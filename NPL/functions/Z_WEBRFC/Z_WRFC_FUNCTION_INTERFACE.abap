FUNCTION Z_WRFC_FUNCTION_INTERFACE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      QUERY_STRING STRUCTURE  W3QUERY
*"      HTML STRUCTURE  W3HTML
*"      MIME STRUCTURE  W3MIME
*"  CHANGING
*"     VALUE(CONTENT_TYPE) TYPE  W3PARAM-CONT_TYPE DEFAULT
*"       'application/json'
*"     VALUE(CONTENT_LENGTH) TYPE  W3PARAM-CONT_LEN
*"     VALUE(RETURN_CODE) TYPE  W3PARAM-RET_CODE
*"----------------------------------------------------------------------
  " http(s)://<your system>:<your port>/sap/bc/webrfc?_FUNCTION=Z_SAMPLERFC&name=<your name>

  " SMW0 -> rilascio funzioni

  " http://127.0.0.1:8000/sap/bc/webrfc?_FUNCTION=Z_WRFC_INTERFACE&callback=jsonCallback&method=Z_WRFC_GET_DUMPS


  TRY.
      DATA(lo_interface) = NEW zcl_wrfc_interface( query_string[] ).
      lo_interface->get_function_interface( ).
      html[] = lo_interface->get_html( ).
    CATCH cx_root INTO DATA(lx_root).
  ENDTRY.


ENDFUNCTION.

* struttura file jsonp
*   iv_callback({
*     "errors": [
*       { "type": "...", "message": "..." },
*     ],
*     "results": {
*       "field1": "...",
*        ...
*       "fieldn": "...",
*       "structure1": { "key1": "...", ..., "keyn": "..." },
*       ...,
*       "structuren": { "key1": "...", ..., "keyn": "..." },
*       "array1": [
*         { "key1": "...", ..., "keyn": "..." },
*         ...,
*         { "key1": "...", ..., "keyn": "..." },
*       ],
*       ...,
*       "arrayn": [
*         { "key1": "...", ..., "keyn": "..." },
*         ...,
*         { "key1": "...", ..., "keyn": "..." },
*       ]
*     }
*   });
