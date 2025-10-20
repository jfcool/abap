*&---------------------------------------------------------------------*
*& Report Z_DEBUG_FM_EXECUTOR
*&---------------------------------------------------------------------*
REPORT z_debug_fm_executor.

DATA: lo_executor      TYPE REF TO zcl_dynamic_executor,
      lv_json_request  TYPE string,
      lv_json_response TYPE string,
      lv_fm_name       TYPE rs38l_fnam.

START-OF-SELECTION.

  WRITE: / '=== Debug Test: Schritt-für-Schritt ==='.
  SKIP 1.

  lv_fm_name = 'RFC_SYSTEM_INFO'.

  TRY.
      " Schritt 1: Prüfe ob FM existiert
      WRITE: / 'Schritt 1: Prüfe FM:', lv_fm_name.
      
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = lv_fm_name
        EXCEPTIONS
          function_not_found = 1
          OTHERS             = 2.
      
      IF sy-subrc = 0.
        WRITE: / '  ✓ FM existiert'.
      ELSE.
        WRITE: / '  ✗ FM existiert NICHT!'.
        RETURN.
      ENDIF.
      SKIP 1.

      " Schritt 2: Hole FM Interface
      WRITE: / 'Schritt 2: Hole FM Interface...'.
      
      DATA: lt_import TYPE STANDARD TABLE OF rsimp,
            lt_export TYPE STANDARD TABLE OF rsimp.
      
      CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
        EXPORTING
          funcname         = lv_fm_name
        TABLES
          import_parameter = lt_import
          export_parameter = lt_export
        EXCEPTIONS
          OTHERS           = 1.
      
      IF sy-subrc = 0.
        WRITE: / '  ✓ Interface geholt'.
        WRITE: / '    Import Parameter:', lines( lt_import ).
        WRITE: / '    Export Parameter:', lines( lt_export ).
      ELSE.
        WRITE: / '  ✗ Fehler beim Holen des Interface!'.
        RETURN.
      ENDIF.
      SKIP 1.

      " Schritt 3: Teste JSON Parsing
      WRITE: / 'Schritt 3: Teste JSON Parsing...'.
      
      lv_json_request = '{ "fm_name": "RFC_SYSTEM_INFO" }'.
      
      DATA: lv_test_fm TYPE rs38l_fnam.
      
      CALL TRANSFORMATION id
        SOURCE XML lv_json_request
        RESULT fm_name = lv_test_fm.
      
      WRITE: / '  ✓ JSON geparst, FM Name:', lv_test_fm.
      SKIP 1.

      " Schritt 4: Teste unsere Klasse
      WRITE: / 'Schritt 4: Teste ZCL_DYNAMIC_EXECUTOR...'.
      
      CREATE OBJECT lo_executor.
      WRITE: / '  ✓ Executor Objekt erstellt'.
      SKIP 1.

      " Schritt 5: Führe execute_from_json aus
      WRITE: / 'Schritt 5: Führe execute_from_json aus...'.
      
      lv_json_response = lo_executor->execute_from_json( lv_json_request ).
      
      WRITE: / '  ✓ Ausführung erfolgreich!'.
      SKIP 1.
      
      WRITE: / 'Response:'.
      WRITE: / lv_json_response.

    CATCH cx_sy_dyn_call_illegal_form INTO DATA(lx_dyn).
      WRITE: / '✗ Dynamic Call Fehler:'.
      WRITE: / '  ', lx_dyn->get_text( ).
      WRITE: / '  Kernel Error ID:', lx_dyn->kernel_errid.
      
    CATCH cx_transformation_error INTO DATA(lx_trans).
      WRITE: / '✗ Transformation Fehler:'.
      WRITE: / '  ', lx_trans->get_text( ).
      
    CATCH cx_root INTO DATA(lx_root).
      WRITE: / '✗ Allgemeiner Fehler:'.
      WRITE: / '  ', lx_root->get_text( ).
      WRITE: / '  Exception Class:', cl_abap_typedescr=>describe_by_object_ref( lx_root )->get_relative_name( ).
  ENDTRY.
