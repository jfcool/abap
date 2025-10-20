*&---------------------------------------------------------------------*
*& Report Z_TEST_DYNAMIC_EXECUTOR
*&---------------------------------------------------------------------*
*& Test Report für ZCL_DYNAMIC_EXECUTOR
*&---------------------------------------------------------------------*
REPORT z_test_dynamic_executor.

DATA: lo_executor      TYPE REF TO zcl_dynamic_executor,
      lv_json_request  TYPE string,
      lv_json_response TYPE string.

START-OF-SELECTION.

  WRITE: / '=== Test 1: RFC_SYSTEM_INFO (Einfacher FM ohne Parameter) ==='.
  SKIP 1.

  TRY.
      " Test 1: RFC_SYSTEM_INFO - Gibt System-Informationen zurück
      lv_json_request = '{' &&
        '  "fm_name": "RFC_SYSTEM_INFO"' &&
        '}'.

      WRITE: / 'JSON Request:'.
      WRITE: / lv_json_request.
      SKIP 1.

      CREATE OBJECT lo_executor.
      lv_json_response = lo_executor->execute_from_json( lv_json_request ).

      WRITE: / 'JSON Response:'.
      WRITE: / lv_json_response.

    CATCH cx_root INTO DATA(lx_error1).
      WRITE: / 'Fehler:', lx_error1->get_text( ).
  ENDTRY.

  SKIP 3.
  ULINE.
  SKIP 1.

  WRITE: / '=== Test 2: BAPI_USER_GET_DETAIL (FM mit IMPORTING Parameter) ==='.
  SKIP 1.

  TRY.
      " Test 2: BAPI_USER_GET_DETAIL - Mit Username als Input
      lv_json_request = '{' &&
        '  "fm_name": "BAPI_USER_GET_DETAIL",' &&
        '  "importing": {' &&
        '    "USERNAME": "' && sy-uname && '"' &&
        '  }' &&
        '}'.

      WRITE: / 'JSON Request:'.
      WRITE: / lv_json_request.
      SKIP 1.

      CLEAR lo_executor.
      CREATE OBJECT lo_executor.
      lv_json_response = lo_executor->execute_from_json( lv_json_request ).

      WRITE: / 'JSON Response (erste 500 Zeichen):'.
      WRITE: / lv_json_response(500).

    CATCH cx_root INTO DATA(lx_error2).
      WRITE: / 'Fehler:', lx_error2->get_text( ).
  ENDTRY.

  SKIP 3.
  ULINE.
  SKIP 1.

  WRITE: / '=== Test 3: BAPI_COMPANYCODE_GETLIST (FM mit Tables Parameter) ==='.
  SKIP 1.

  TRY.
      " Test 3: BAPI_COMPANYCODE_GETLIST - Firmen auflisten
      lv_json_request = '{' &&
        '  "fm_name": "BAPI_COMPANYCODE_GETLIST",' &&
        '  "tables": {' &&
        '    "COMPANYCODE_LIST": []' &&
        '  }' &&
        '}'.

      WRITE: / 'JSON Request:'.
      WRITE: / lv_json_request.
      SKIP 1.

      CLEAR lo_executor.
      CREATE OBJECT lo_executor.
      lv_json_response = lo_executor->execute_from_json( lv_json_request ).

      WRITE: / 'JSON Response (erste 1000 Zeichen):'.
      IF strlen( lv_json_response ) > 1000.
        WRITE: / lv_json_response(1000).
        WRITE: / '...'.
      ELSE.
        WRITE: / lv_json_response.
      ENDIF.

    CATCH cx_root INTO DATA(lx_error3).
      WRITE: / 'Fehler:', lx_error3->get_text( ).
  ENDTRY.

  SKIP 3.
  ULINE.
  SKIP 1.

  WRITE: / '=== Test 4: Struktur-Analyse für einen FM ==='.
  SKIP 1.

  TRY.
      " Test 4: Zeige wie die Struktur für BAPI_MATERIAL_GETLIST aufgebaut wird
      DATA: lo_test_executor TYPE REF TO zcl_dynamic_executor,
            ls_structure     TYPE zcl_dynamic_executor=>ty_fm_call_structure.

      CREATE OBJECT lo_test_executor.

      " Nutze die private Methode über Reflection (nur für Demo)
      WRITE: / 'Funktionsbaustein: BAPI_MATERIAL_GETLIST'.
      WRITE: / 'Die dynamische Struktur wird aufgebaut basierend auf RTTI...'.
      SKIP 1.

      " Einfacher Test ob FM existiert
      lv_json_request = '{' &&
        '  "fm_name": "BAPI_MATERIAL_GETLIST",' &&
        '  "importing": {' &&
        '    "MAXROWS": 10' &&
        '  },' &&
        '  "tables": {' &&
        '    "MATNRLIST": [],' &&
        '    "MATNRSELECTION": []' &&
        '  }' &&
        '}'.

      WRITE: / 'Test-Request:'.
      WRITE: / lv_json_request.
      SKIP 1.

      lv_json_response = lo_test_executor->execute_from_json( lv_json_request ).

      WRITE: / 'Response erhalten (Länge:', strlen( lv_json_response ), 'Zeichen)'.
      WRITE: / 'Erste 500 Zeichen:'.
      WRITE: / lv_json_response(500).

    CATCH cx_root INTO DATA(lx_error4).
      WRITE: / 'Fehler:', lx_error4->get_text( ).
  ENDTRY.

  SKIP 3.
  WRITE: / '=== Alle Tests abgeschlossen ==='.
