CLASS zcl_dynamic_executor DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      execute_from_json
        IMPORTING
          iv_json         TYPE string
        RETURNING
          VALUE(rv_json)  TYPE string
        RAISING
          cx_sy_dyn_call_illegal_form
          cx_transformation_error.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_fm_call_structure,
        fm_name   TYPE rs38l_fnam,
        importing TYPE REF TO data,
        exporting TYPE REF TO data,
        changing  TYPE REF TO data,
        tables    TYPE REF TO data,
      END OF ty_fm_call_structure.

    METHODS:
      build_dynamic_fm_structure
        IMPORTING
          iv_fm_name            TYPE rs38l_fnam
        RETURNING
          VALUE(rs_structure)   TYPE ty_fm_call_structure
        RAISING
          cx_sy_dyn_call_illegal_form,
      execute_function_module
        IMPORTING
          iv_fm_name       TYPE rs38l_fnam
          ir_importing     TYPE REF TO data
          ir_changing      TYPE REF TO data
          ir_tables        TYPE REF TO data
        EXPORTING
          er_exporting     TYPE REF TO data
        RAISING
          cx_sy_dyn_call_illegal_form,
      get_fm_interface
        IMPORTING
          iv_funcname        TYPE rs38l_fnam
        EXPORTING
          et_import          TYPE STANDARD TABLE
          et_export          TYPE STANDARD TABLE
          et_change          TYPE STANDARD TABLE
          et_tables          TYPE STANDARD TABLE
        RAISING
          cx_sy_dyn_call_illegal_form.

ENDCLASS.



CLASS zcl_dynamic_executor IMPLEMENTATION.

  METHOD execute_from_json.
    DATA: ls_call_structure TYPE ty_fm_call_structure,
          lr_data           TYPE REF TO data,
          lv_fm_name        TYPE rs38l_fnam.

    " 1. FM Name aus JSON extrahieren
    CALL TRANSFORMATION id
      SOURCE XML iv_json
      RESULT fm_name = lv_fm_name.

    IF lv_fm_name IS INITIAL.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_form.
    ENDIF.

    " 2. Dynamische Struktur basierend auf FM Signatur aufbauen
    ls_call_structure = build_dynamic_fm_structure( lv_fm_name ).

    " 3. JSON in die dynamische Struktur deserialisieren
    TRY.
        CALL TRANSFORMATION id
          SOURCE XML iv_json
          RESULT importing = ls_call_structure-importing
                 changing  = ls_call_structure-changing
                 tables    = ls_call_structure-tables.
      CATCH cx_transformation_error.
        " Bei Fehler: Struktur komplett neu aufbauen
        CREATE DATA lr_data TYPE ty_fm_call_structure.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<fs_complete>).
        <fs_complete> = ls_call_structure.

        CALL TRANSFORMATION id
          SOURCE XML iv_json
          RESULT complete = <fs_complete>.

        ls_call_structure = <fs_complete>.
    ENDTRY.

    " 4. Funktionsbaustein dynamisch aufrufen
    execute_function_module(
      EXPORTING
        iv_fm_name   = lv_fm_name
        ir_importing = ls_call_structure-importing
        ir_changing  = ls_call_structure-changing
        ir_tables    = ls_call_structure-tables
      IMPORTING
        er_exporting = ls_call_structure-exporting ).

    " 5. Ergebnis-Struktur zurueck nach JSON serialisieren
    CALL TRANSFORMATION id
      SOURCE fm_name   = lv_fm_name
             importing = ls_call_structure-importing
             exporting = ls_call_structure-exporting
             changing  = ls_call_structure-changing
             tables    = ls_call_structure-tables
             success   = abap_true
      RESULT XML rv_json.

  ENDMETHOD.

  METHOD build_dynamic_fm_structure.
    DATA: lt_import    TYPE STANDARD TABLE OF rsimp,
          lt_export    TYPE STANDARD TABLE OF rsimp,
          lt_change    TYPE STANDARD TABLE OF rsimp,
          lt_tables    TYPE STANDARD TABLE OF rsimp,
          ls_import    TYPE rsimp,
          ls_export    TYPE rsimp,
          ls_change    TYPE rsimp,
          ls_table     TYPE rsimp,
          lt_comp_imp  TYPE cl_abap_structdescr=>component_table,
          lt_comp_exp  TYPE cl_abap_structdescr=>component_table,
          lt_comp_chg  TYPE cl_abap_structdescr=>component_table,
          lt_comp_tab  TYPE cl_abap_structdescr=>component_table,
          ls_component TYPE abap_componentdescr,
          lo_type      TYPE REF TO cl_abap_typedescr,
          lo_datadescr TYPE REF TO cl_abap_datadescr,
          lo_struct    TYPE REF TO cl_abap_structdescr,
          lo_table     TYPE REF TO cl_abap_tabledescr.

    rs_structure-fm_name = iv_fm_name.

    " FM Interface abrufen
    get_fm_interface(
      EXPORTING
        iv_funcname = iv_fm_name
      IMPORTING
        et_import   = lt_import
        et_export   = lt_export
        et_change   = lt_change
        et_tables   = lt_tables ).

    " === IMPORTING Parameter Struktur aufbauen ===
    LOOP AT lt_import INTO ls_import.
      CLEAR ls_component.
      ls_component-name = to_upper( ls_import-parameter ).

      IF ls_import-reference IS NOT INITIAL.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_name( ls_import-reference ).
            lo_datadescr ?= lo_type.
            ls_component-type = lo_datadescr.
          CATCH cx_root.
            ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        ENDTRY.
      ELSEIF ls_import-typ IS NOT INITIAL.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_name( ls_import-typ ).
            lo_datadescr ?= lo_type.
            ls_component-type = lo_datadescr.
          CATCH cx_root.
            ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        ENDTRY.
      ELSE.
        ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
      ENDIF.

      APPEND ls_component TO lt_comp_imp.
    ENDLOOP.

    IF lt_comp_imp IS NOT INITIAL.
      lo_struct = cl_abap_structdescr=>create( lt_comp_imp ).
      CREATE DATA rs_structure-importing TYPE HANDLE lo_struct.
    ENDIF.

    " === EXPORTING Parameter Struktur aufbauen ===
    LOOP AT lt_export INTO ls_export.
      CLEAR ls_component.
      ls_component-name = to_upper( ls_export-parameter ).

      IF ls_export-reference IS NOT INITIAL.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_name( ls_export-reference ).
            lo_datadescr ?= lo_type.
            ls_component-type = lo_datadescr.
          CATCH cx_root.
            ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        ENDTRY.
      ELSEIF ls_export-typ IS NOT INITIAL.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_name( ls_export-typ ).
            lo_datadescr ?= lo_type.
            ls_component-type = lo_datadescr.
          CATCH cx_root.
            ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        ENDTRY.
      ELSE.
        ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
      ENDIF.

      APPEND ls_component TO lt_comp_exp.
    ENDLOOP.

    IF lt_comp_exp IS NOT INITIAL.
      lo_struct = cl_abap_structdescr=>create( lt_comp_exp ).
      CREATE DATA rs_structure-exporting TYPE HANDLE lo_struct.
    ENDIF.

    " === CHANGING Parameter Struktur aufbauen ===
    LOOP AT lt_change INTO ls_change.
      CLEAR ls_component.
      ls_component-name = to_upper( ls_change-parameter ).

      IF ls_change-reference IS NOT INITIAL.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_name( ls_change-reference ).
            lo_datadescr ?= lo_type.
            ls_component-type = lo_datadescr.
          CATCH cx_root.
            ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        ENDTRY.
      ELSEIF ls_change-typ IS NOT INITIAL.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_name( ls_change-typ ).
            lo_datadescr ?= lo_type.
            ls_component-type = lo_datadescr.
          CATCH cx_root.
            ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        ENDTRY.
      ELSE.
        ls_component-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
      ENDIF.

      APPEND ls_component TO lt_comp_chg.
    ENDLOOP.

    IF lt_comp_chg IS NOT INITIAL.
      lo_struct = cl_abap_structdescr=>create( lt_comp_chg ).
      CREATE DATA rs_structure-changing TYPE HANDLE lo_struct.
    ENDIF.

    " === TABLES Parameter Struktur aufbauen ===
    LOOP AT lt_tables INTO ls_table.
      CLEAR ls_component.
      ls_component-name = to_upper( ls_table-parameter ).

      IF ls_table-reference IS NOT INITIAL.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_name( ls_table-reference ).
            lo_datadescr ?= lo_type.
            " Fuer Tables: Immer als Tabelle definieren
            lo_table = cl_abap_tabledescr=>create( p_line_type = lo_datadescr ).
            ls_component-type = lo_table.
          CATCH cx_root.
            lo_datadescr ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
            lo_table = cl_abap_tabledescr=>create( p_line_type = lo_datadescr ).
            ls_component-type = lo_table.
        ENDTRY.
      ELSE.
        lo_datadescr ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        lo_table = cl_abap_tabledescr=>create( p_line_type = lo_datadescr ).
        ls_component-type = lo_table.
      ENDIF.

      APPEND ls_component TO lt_comp_tab.
    ENDLOOP.

    IF lt_comp_tab IS NOT INITIAL.
      lo_struct = cl_abap_structdescr=>create( lt_comp_tab ).
      CREATE DATA rs_structure-tables TYPE HANDLE lo_struct.
    ENDIF.

  ENDMETHOD.

  METHOD execute_function_module.
    DATA: lt_ptab    TYPE abap_func_parmbind_tab,
          lt_etab    TYPE abap_func_excpbind_tab,
          ls_ptab    TYPE abap_func_parmbind,
          ls_etab    TYPE abap_func_excpbind,
          lt_import  TYPE STANDARD TABLE OF rsimp,
          lt_export  TYPE STANDARD TABLE OF rsimp,
          lt_change  TYPE STANDARD TABLE OF rsimp,
          lt_tables  TYPE STANDARD TABLE OF rsimp,
          ls_import  TYPE rsimp,
          ls_export  TYPE rsimp,
          ls_change  TYPE rsimp,
          ls_table   TYPE rsimp,
          ls_temp    TYPE ty_fm_call_structure.

    FIELD-SYMBOLS: <fs_any> TYPE any.

    " Interface abrufen
    get_fm_interface(
      EXPORTING
        iv_funcname = iv_fm_name
      IMPORTING
        et_import   = lt_import
        et_export   = lt_export
        et_change   = lt_change
        et_tables   = lt_tables ).

    " Exporting Struktur erstellen
    IF er_exporting IS NOT BOUND.
      ls_temp = build_dynamic_fm_structure( iv_fm_name ).
      er_exporting = ls_temp-exporting.
    ENDIF.

    " === IMPORTING Parameter ===
    IF ir_importing IS BOUND.
      ASSIGN ir_importing->* TO FIELD-SYMBOL(<fs_importing>).
      LOOP AT lt_import INTO ls_import.
        ASSIGN COMPONENT ls_import-parameter OF STRUCTURE <fs_importing> TO <fs_any>.
        IF sy-subrc = 0.
          ls_ptab-name  = ls_import-parameter.
          ls_ptab-kind  = abap_func_exporting.
          GET REFERENCE OF <fs_any> INTO ls_ptab-value.
          APPEND ls_ptab TO lt_ptab.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " === EXPORTING Parameter ===
    IF er_exporting IS BOUND.
      ASSIGN er_exporting->* TO FIELD-SYMBOL(<fs_exporting>).
      LOOP AT lt_export INTO ls_export.
        ASSIGN COMPONENT ls_export-parameter OF STRUCTURE <fs_exporting> TO <fs_any>.
        IF sy-subrc = 0.
          ls_ptab-name  = ls_export-parameter.
          ls_ptab-kind  = abap_func_importing.
          GET REFERENCE OF <fs_any> INTO ls_ptab-value.
          APPEND ls_ptab TO lt_ptab.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " === CHANGING Parameter ===
    IF ir_changing IS BOUND.
      ASSIGN ir_changing->* TO FIELD-SYMBOL(<fs_changing>).
      LOOP AT lt_change INTO ls_change.
        ASSIGN COMPONENT ls_change-parameter OF STRUCTURE <fs_changing> TO <fs_any>.
        IF sy-subrc = 0.
          ls_ptab-name  = ls_change-parameter.
          ls_ptab-kind  = abap_func_changing.
          GET REFERENCE OF <fs_any> INTO ls_ptab-value.
          APPEND ls_ptab TO lt_ptab.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " === TABLES Parameter ===
    IF ir_tables IS BOUND.
      ASSIGN ir_tables->* TO FIELD-SYMBOL(<fs_tables>).
      LOOP AT lt_tables INTO ls_table.
        ASSIGN COMPONENT ls_table-parameter OF STRUCTURE <fs_tables> TO <fs_any>.
        IF sy-subrc = 0.
          ls_ptab-name  = ls_table-parameter.
          ls_ptab-kind  = abap_func_tables.
          GET REFERENCE OF <fs_any> INTO ls_ptab-value.
          APPEND ls_ptab TO lt_ptab.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Exception Table aufbauen - alle Exceptions abfangen
    ls_etab-name = 'OTHERS'.
    ls_etab-value = 1.
    APPEND ls_etab TO lt_etab.

    " Dynamischer Aufruf
    CALL FUNCTION iv_fm_name
      PARAMETER-TABLE lt_ptab
      EXCEPTION-TABLE lt_etab.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_form.
    ENDIF.

  ENDMETHOD.

  METHOD get_fm_interface.
    DATA: lt_enlfdir TYPE STANDARD TABLE OF enlfdir,
          lt_except  TYPE STANDARD TABLE OF rsexc.

    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      EXPORTING
        funcname           = iv_funcname
      TABLES
        exception_list     = lt_except
        export_parameter   = et_export
        import_parameter   = et_import
        changing_parameter = et_change
        tables_parameter   = et_tables
        enlfdir            = lt_enlfdir
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_form.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
