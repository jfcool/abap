*&---------------------------------------------------------------------*
*& Report Z_TEST_FM_EXECUTOR
*&---------------------------------------------------------------------*
REPORT z_test_fm_executor.

DATA: lo_executor      TYPE REF TO zcl_dynamic_executor,
      lv_json_request  TYPE string,
      lv_json_response TYPE string.

START-OF-SELECTION.

  WRITE: / '*** Test 1: RFC_SYSTEM_INFO ***'.
  SKIP 1.

  TRY.
      lv_json_request = '{ "fm_name": "RFC_SYSTEM_INFO" }'.

      WRITE: / 'Request:', lv_json_request.
      SKIP 1.

      CREATE OBJECT lo_executor.
      lv_json_response = lo_executor->execute_from_json( lv_json_request ).

      WRITE: / 'Response:'.
      WRITE: / lv_json_response.
      SKIP 2.

      WRITE: / '✓ Test 1 erfolgreich!'.

    CATCH cx_root INTO DATA(lx_error1).
      WRITE: / '✗ Fehler:', lx_error1->get_text( ).
  ENDTRY.

  SKIP 2.
  ULINE.
  SKIP 1.

  WRITE: / '*** Test 2: BAPI_USER_GET_DETAIL ***'.
  SKIP 1.

  TRY.
      lv_json_request = '{ "fm_name": "BAPI_USER_GET_DETAIL", "importing": { "USERNAME": "' && sy-uname && '" } }'.

      WRITE: / 'Request:', lv_json_request.
      SKIP 1.

      CLEAR lo_executor.
      CREATE OBJECT lo_executor.
      lv_json_response = lo_executor->execute_from_json( lv_json_request ).

      WRITE: / 'Response (erste 500 Zeichen):'.
      IF strlen( lv_json_response ) > 500.
        WRITE: / lv_json_response(500).
      ELSE.
        WRITE: / lv_json_response.
      ENDIF.
      SKIP 2.

      WRITE: / '✓ Test 2 erfolgreich!'.

    CATCH cx_root INTO DATA(lx_error2).
      WRITE: / '✗ Fehler:', lx_error2->get_text( ).
  ENDTRY.

  SKIP 2.
  WRITE: / '=== Alle Tests abgeschlossen ==='.
