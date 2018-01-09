FUNCTION z_wrfc_table.
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
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

  " http://mnibm09.novellini.it:8066/sap/bc/webrfc?_FUNCTION=Z_SAMPLERFC&callback=jsonCallback&sqlTable=MARC&sqlWhere=MATNR = 'ACF090-071026'


  FIELD-SYMBOLS: <ls_data> TYPE any.
  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.
  TYPES: BEGIN OF ty_s_work, buffer(30000), END OF ty_s_work.
*  DATA: ls_work TYPE ty_s_work.

  DATA: lo_struct_descr   TYPE REF TO cl_abap_structdescr,
        lo_table_descr    TYPE REF TO cl_abap_tabledescr,
        lx_root           TYPE REF TO cx_root,
        ls_components     TYPE abap_compdescr,
        lt_keys           TYPE abap_keydescr_tab,
        ls_string         TYPE string,
        lt_table          TYPE REF TO data,
        ls_table          TYPE REF TO data,
        lt_fields         TYPE TABLE OF string,
        ls_fields         TYPE string.

  DATA: lt_messages TYPE ty_t_messages.

  REFRESH lt_messages.

* parametri
  DATA: iv_callback         TYPE string,
        iv_sql_table        TYPE string,
        iv_sql_where        TYPE string,
        iv_sql_fields       TYPE string,
        iv_from_rec         TYPE i,
        iv_to_rec            TYPE i.

  CLEAR: iv_callback,
         iv_sql_table,
         iv_sql_where,
         iv_sql_fields.

  LOOP AT query_string.
    TRANSLATE query_string-name TO UPPER CASE.
    MODIFY query_string.
  ENDLOOP.

*  SORT query_string DESCENDING.
*  READ TABLE query_string WITH KEY name = 'NAME'.
*  CHECK sy-subrc = 0.
*  name = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'CALLBACK'.
  iv_callback = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'SQLTABLE'.
  "CHECK sy-subrc = 0.
*  IF ( sy-subrc <> 0 ).
*    return_code = 100.
*    RETURN.
*  ENDIF.
  iv_sql_table = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'SQLWHERE'.
  "CHECK sy-subrc = 0.
  iv_sql_where = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'SQLFIELDS'.
  "CHECK sy-subrc = 0.
  iv_sql_fields = query_string-value.
  IF ( iv_sql_fields IS INITIAL ).
    iv_sql_fields = '*'.
  ENDIF.

  REFRESH: lt_fields.
  SPLIT iv_sql_fields AT ' ' INTO TABLE lt_fields.
  LOOP AT lt_fields INTO ls_fields.
    IF ( ls_fields = space ).
      DELETE lt_fields.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'FROM_REC'.
  TRY.
      iv_from_rec = query_string-value.
    CATCH cx_root INTO lx_root.
      iv_from_rec = 0.
  ENDTRY.
  IF ( iv_from_rec IS INITIAL ).
    iv_from_rec = 1.
  ENDIF.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'TO_REC'.
  TRY.
      iv_to_rec = query_string-value.
    CATCH cx_root INTO lx_root.
      iv_to_rec = 0.
  ENDTRY.
  IF ( iv_to_rec IS INITIAL ).
    iv_to_rec = 10.
  ENDIF.

  IF ( iv_callback IS INITIAL ).
    PERFORM add_message USING    'E' 'Specificare funzione di callback'
                        CHANGING lt_messages[].
  ENDIF.
  IF ( iv_sql_table IS INITIAL ).
    PERFORM add_message USING    'E' 'Specificare tabella di estrazione'
                        CHANGING lt_messages[].
  ENDIF.

  IF ( NOT iv_sql_fields IS INITIAL ).
    lo_struct_descr ?= cl_abap_structdescr=>describe_by_name( iv_sql_table ).

*    ASSIGN ls_work TO <ls_data> CASTING TYPE (iv_sql_table).



    CREATE DATA ls_table TYPE HANDLE lo_struct_descr.
    ASSIGN ls_table->* TO <ls_data>.

    lo_table_descr ?= cl_abap_tabledescr=>create( p_line_type  = lo_struct_descr
                                                  p_table_kind = cl_abap_tabledescr=>tablekind_hashed
                                                  p_unique     = abap_true
                                                  p_key        = lt_keys
                                                  p_key_kind   = cl_abap_tabledescr=>keydefkind_default ).

    CREATE DATA lt_table TYPE HANDLE lo_table_descr.
    ASSIGN lt_table->* TO <lt_data>.


    DATA: lo_exception TYPE REF TO cx_root,
          lv_msg       TYPE ty_s_messages-msg.
    TRY.
*    SELECT * FROM (iv_sql_table) INTO TABLE <lt_data> WHERE (iv_sql_where).
        SELECT (iv_sql_fields) FROM (iv_sql_table) INTO CORRESPONDING FIELDS OF TABLE <lt_data> WHERE (iv_sql_where).
        IF ( sy-subrc <> 0 ).
          PERFORM add_message USING    'E' 'Nessun risultato'
                              CHANGING lt_messages[].
        ENDIF.
      CATCH cx_root INTO lo_exception.
        lv_msg = lo_exception->get_text( ).
        PERFORM add_message USING    'E' lv_msg
                            CHANGING lt_messages[].
        RETURN.
    ENDTRY.

*    PERFORM create_jsonp USING    iv_callback
*                                  <lt_data>[]
*                                  lo_struct_descr->components[]
*                                  lt_fields[]
*                                  lt_messages[]
**                                  '10'
*                                  1
*                                  10
*                         CHANGING html[].

    " apre tag funcione di callback
    PERFORM jsonp_open_callback USING    iv_callback
                                CHANGING html[].

** errors -------------------------------------------------------
*  " apre tag errors
*  PERFORM JSONP_OPEN_ERRORS CHANGING CT_JSONP[].
*  " costruzione errors
*  PERFORM JSONP_BUILD_ERRORS USING    IT_MESSAGES[]
*                             CHANGING CT_JSONP[].
*  " chiude tag errors
*  PERFORM JSONP_CLOSE_ERRORS CHANGING CT_JSONP[].
*
** columns ------------------------------------------------------
*  " apre tag columns
*  PERFORM JSONP_OPEN_COLUMNS CHANGING CT_JSONP[].
*  " costruisce columns
*  PERFORM JSONP_BUILD_COLUMNS USING    IT_COMPONENTS[]
*                                       IT_FIELDS[]
*                              CHANGING CT_JSONP[].
*  " chiude tag columns
*  PERFORM JSONP_CLOSE_COLUMNS CHANGING CT_JSONP[].

* results ------------------------------------------------------
    " apre tag results
    PERFORM jsonp_open_results CHANGING html[].
    " costruisce results
    PERFORM jsonp_build_results USING    iv_sql_table
                                         <lt_data>[]
*                                         lo_struct_descr->components[]
                                         lt_fields[]
*                                       IV_ROWS
                                         iv_from_rec
                                         iv_to_rec
                                CHANGING html[].
    " chiude tag results
    PERFORM jsonp_close_results CHANGING html[].

    " chiude tab funzione di callback
    PERFORM jsonp_close_callback USING iv_callback
                                 CHANGING html[].

  ELSE.
    PERFORM create_jsonp_2 USING    iv_callback
                                    lt_messages[]
                           CHANGING html[].
  ENDIF.

*refresh html.
*html = 'jsoncallback({"errors": [], "columns": [], "results": []});'.
*append html.

ENDFUNCTION.

* struttura file jsonp
*   iv_callback({
*     "errors": [
*       { "type": "...", "message": "..." },
*     ],
*     "results": {
*       "iv_sql_table": [
*         { "field1": "...", ..., "fieldn": "..." },
*         ...,
*         { "field11": "...", ..., "fieldn": "..." },
*       ]
*     }
*   });)
