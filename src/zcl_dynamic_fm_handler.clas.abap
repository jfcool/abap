CLASS zcl_dynamic_fm_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

ENDCLASS.



CLASS zcl_dynamic_fm_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA: lv_json_request  TYPE string,
          lv_json_response TYPE string,
          lv_error_text    TYPE string,
          lv_error_json    TYPE string,
          lo_executor      TYPE REF TO zcl_dynamic_executor.

    TRY.
        " CORS Headers setzen
        server->response->set_header_field( name = 'Access-Control-Allow-Origin' value = '*' ).
        server->response->set_header_field( name = 'Access-Control-Allow-Methods' value = 'POST, GET, OPTIONS' ).
        server->response->set_header_field( name = 'Access-Control-Allow-Headers' value = 'Content-Type' ).

        " OPTIONS Request fuer CORS Preflight
        IF server->request->get_method( ) = 'OPTIONS'.
          server->response->set_status( code = 200 reason = 'OK' ).
          RETURN.
        ENDIF.

        " JSON Request Body lesen
        lv_json_request = server->request->get_cdata( ).

        " Executor erstellen und ausfuehren
        CREATE OBJECT lo_executor.
        lv_json_response = lo_executor->execute_from_json( lv_json_request ).

        " Response zuruecksenden
        server->response->set_status( code = 200 reason = 'OK' ).
        server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=utf-8' ).
        server->response->set_cdata( lv_json_response ).

      CATCH cx_root INTO DATA(lx_error).
        " Fehler als JSON zurueckgeben
        lv_error_text = escape( val = lx_error->get_text( ) format = cl_abap_format=>e_json_string ).
        CONCATENATE '{ "error": "' lv_error_text '", "success": false }' INTO lv_error_json.
        server->response->set_status( code = 500 reason = 'Internal Server Error' ).
        server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=utf-8' ).
        server->response->set_cdata( lv_error_json ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
