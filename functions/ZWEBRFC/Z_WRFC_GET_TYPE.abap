FUNCTION z_wrfc_get_type.
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
        lo_data_descr     TYPE REF TO cl_abap_datadescr,
        lx_root           TYPE REF TO cx_root,
        ls_components     TYPE abap_compdescr,
        lt_keys           TYPE abap_keydescr_tab,
        ls_string         TYPE string,
        lt_table          TYPE REF TO data,
        ls_table          TYPE REF TO data.

  DATA: lt_messages TYPE ty_t_messages.

  REFRESH lt_messages.

* parametri
  DATA: iv_callback         TYPE string,
        iv_type             TYPE string.
*        IV_SQL_TABLE        TYPE STRING,
*        IV_SQL_WHERE        TYPE STRING,
*        IV_SQL_FIELDS       TYPE STRING,
*        IV_FROM_REC         TYPE I,
*        IV_TO_REC            TYPE I.

  CLEAR: iv_callback,
         iv_type.

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
  READ TABLE query_string WITH KEY name = 'TYPE'.
  iv_type = query_string-value.

  IF ( iv_callback IS INITIAL ).
    PERFORM add_message USING    'E' 'Specificare funzione di callback'
                        CHANGING lt_messages[].
  ENDIF.

  IF ( iv_type IS INITIAL ).
    PERFORM add_message USING    'E' 'Specificare tipo'
                        CHANGING lt_messages[].
  ENDIF.

  IF ( NOT iv_type IS INITIAL ).
*    LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_NAME( IV_SQL_TABLE ).
*
**    ASSIGN ls_work TO <ls_data> CASTING TYPE (iv_sql_table).
*
*
*
*    CREATE DATA LS_TABLE TYPE HANDLE LO_STRUCT_DESCR.
*    ASSIGN LS_TABLE->* TO <LS_DATA>.
*
*    LO_TABLE_DESCR ?= CL_ABAP_TABLEDESCR=>CREATE( P_LINE_TYPE  = LO_STRUCT_DESCR
*                                                  P_TABLE_KIND = CL_ABAP_TABLEDESCR=>TABLEKIND_HASHED
*                                                  P_UNIQUE     = ABAP_TRUE
*                                                  P_KEY        = LT_KEYS
*                                                  P_KEY_KIND   = CL_ABAP_TABLEDESCR=>KEYDEFKIND_DEFAULT ).
*
*    CREATE DATA LT_TABLE TYPE HANDLE LO_TABLE_DESCR.
*    ASSIGN LT_TABLE->* TO <LT_DATA>.
*
*
*    DATA: LO_EXCEPTION TYPE REF TO CX_ROOT,
*          LV_MSG       TYPE TY_S_MESSAGES-MSG.
*    TRY.
**    SELECT * FROM (iv_sql_table) INTO TABLE <lt_data> WHERE (iv_sql_where).
*        SELECT (IV_SQL_FIELDS) FROM (IV_SQL_TABLE) INTO CORRESPONDING FIELDS OF TABLE <LT_DATA> WHERE (IV_SQL_WHERE).
*        IF ( SY-SUBRC <> 0 ).
*          PERFORM ADD_MESSAGE USING    'E' 'Nessun risultato'
*                              CHANGING LT_MESSAGES[].
*        ENDIF.
*      CATCH CX_ROOT INTO LO_EXCEPTION.
*        LV_MSG = LO_EXCEPTION->GET_TEXT( ).
*        PERFORM ADD_MESSAGE USING    'E' LV_MSG
*                            CHANGING LT_MESSAGES[].
*        RETURN.
*    ENDTRY.

    " apre tag funcione di callback
    PERFORM jsonp_open_callback USING    iv_callback
                                CHANGING html[].

* results ------------------------------------------------------
    " apre tag results
    PERFORM jsonp_open_results CHANGING html[].
    " costruisce results

    DATA: lref_value TYPE REF TO data,
          lv_type    TYPE w3_qvalue.
    lv_type = iv_type.
    PERFORM create_parameter USING    lv_type
                                      ''
                                      ''
                             CHANGING lref_value.

    DATA: lv_param          TYPE REF TO data,
          lv_value          TYPE string.
    FIELD-SYMBOLS: <lv_param> TYPE any.
    TRY.
      TRY.
          CREATE DATA lv_param TYPE (lv_type).
        CATCH cx_sy_create_data_error INTO lx_root.
          CREATE DATA lv_param LIKE lv_type.
      ENDTRY.
      ASSIGN lv_param->* TO <lv_param>.
      TRY.
          lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( <lv_param> ).
          cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_value
                                     CHANGING  ca_data = <lv_param> ).
        CATCH cx_root.
          TRY.
              lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <lv_param> ).
              cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_value
                                         CHANGING  ca_data = <lv_param> ).
            CATCH cx_root.
              TRY.
                  lo_data_descr ?= cl_abap_datadescr=>describe_by_data( <lv_param> ).
                CATCH cx_root.
                  PERFORM add_message USING    'E' 'Specificare un tipo valido in SAP'
                                      CHANGING lt_messages[].
                  PERFORM create_jsonp_2 USING    iv_callback
                                                  lt_messages[]
                                         CHANGING html[].
              ENDTRY.
          ENDTRY.
      ENDTRY.
    ENDTRY.

*    PERFORM JSONP_BUILD_RESULTS USING    IV_SQL_TABLE
*                                         <LT_DATA>[]
*                                         LO_STRUCT_DESCR->COMPONENTS[]
*                                         LT_FIELDS[]
**                                       IV_ROWS
*                                         IV_FROM_REC
*                                         IV_TO_REC
*                                CHANGING HTML[].
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
