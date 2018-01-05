FUNCTION Z_WRFC_TABLE.
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


  FIELD-SYMBOLS: <LS_DATA> TYPE ANY.
  FIELD-SYMBOLS: <LT_DATA> TYPE ANY TABLE.
  TYPES: BEGIN OF TY_S_WORK, BUFFER(30000), END OF TY_S_WORK.
*  DATA: ls_work TYPE ty_s_work.

  DATA: LO_STRUCT_DESCR   TYPE REF TO CL_ABAP_STRUCTDESCR,
        LO_TABLE_DESCR    TYPE REF TO CL_ABAP_TABLEDESCR,
        LX_ROOT           TYPE REF TO CX_ROOT,
        LS_COMPONENTS     TYPE ABAP_COMPDESCR,
        LT_KEYS           TYPE ABAP_KEYDESCR_TAB,
        LS_STRING         TYPE STRING,
        LT_TABLE          TYPE REF TO DATA,
        LS_TABLE          TYPE REF TO DATA,
        LT_FIELDS         TYPE TABLE OF STRING,
        LS_FIELDS         TYPE STRING.

  DATA: LT_MESSAGES TYPE TY_T_MESSAGES.

  REFRESH LT_MESSAGES.

* parametri
  DATA: IV_CALLBACK         TYPE STRING,
        IV_SQL_TABLE        TYPE STRING,
        IV_SQL_WHERE        TYPE STRING,
        IV_SQL_FIELDS       TYPE STRING,
        IV_FROM_REC         TYPE I,
        IV_TO_REC            TYPE I.

  CLEAR: IV_CALLBACK,
         IV_SQL_TABLE,
         IV_SQL_WHERE,
         IV_SQL_FIELDS.

  LOOP AT QUERY_STRING.
    TRANSLATE QUERY_STRING-NAME TO UPPER CASE.
    MODIFY QUERY_STRING.
  ENDLOOP.

*  SORT query_string DESCENDING.
*  READ TABLE query_string WITH KEY name = 'NAME'.
*  CHECK sy-subrc = 0.
*  name = query_string-value.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'CALLBACK'.
  IV_CALLBACK = QUERY_STRING-VALUE.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'SQLTABLE'.
  "CHECK sy-subrc = 0.
*  IF ( sy-subrc <> 0 ).
*    return_code = 100.
*    RETURN.
*  ENDIF.
  IV_SQL_TABLE = QUERY_STRING-VALUE.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'SQLWHERE'.
  "CHECK sy-subrc = 0.
  IV_SQL_WHERE = QUERY_STRING-VALUE.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'SQLFIELDS'.
  "CHECK sy-subrc = 0.
  IV_SQL_FIELDS = QUERY_STRING-VALUE.
  IF ( IV_SQL_FIELDS IS INITIAL ).
    IV_SQL_FIELDS = '*'.
  ENDIF.

  REFRESH: LT_FIELDS.
  SPLIT IV_SQL_FIELDS AT ' ' INTO TABLE LT_FIELDS.
  LOOP AT LT_FIELDS INTO LS_FIELDS.
    IF ( LS_FIELDS = SPACE ).
      DELETE LT_FIELDS.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'FROM_REC'.
  TRY.
      IV_FROM_REC = QUERY_STRING-VALUE.
    CATCH CX_ROOT INTO LX_ROOT.
      IV_FROM_REC = 0.
  ENDTRY.
  IF ( IV_FROM_REC IS INITIAL ).
    IV_FROM_REC = 1.
  ENDIF.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'TO_REC'.
  TRY.
      IV_TO_REC = QUERY_STRING-VALUE.
    CATCH CX_ROOT INTO LX_ROOT.
      IV_TO_REC = 0.
  ENDTRY.
  IF ( IV_TO_REC IS INITIAL ).
    IV_TO_REC = 10.
  ENDIF.

  IF ( IV_CALLBACK IS INITIAL ).
    PERFORM ADD_MESSAGE USING    'E' 'Specificare funzione di callback'
                        CHANGING LT_MESSAGES[].
  ENDIF.
  IF ( IV_SQL_TABLE IS INITIAL ).
    PERFORM ADD_MESSAGE USING    'E' 'Specificare tabella di estrazione'
                        CHANGING LT_MESSAGES[].
  ENDIF.

  IF ( NOT IV_SQL_FIELDS IS INITIAL ).
    LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_NAME( IV_SQL_TABLE ).

*    ASSIGN ls_work TO <ls_data> CASTING TYPE (iv_sql_table).



    CREATE DATA LS_TABLE TYPE HANDLE LO_STRUCT_DESCR.
    ASSIGN LS_TABLE->* TO <LS_DATA>.

    LO_TABLE_DESCR ?= CL_ABAP_TABLEDESCR=>CREATE( P_LINE_TYPE  = LO_STRUCT_DESCR
                                                  P_TABLE_KIND = CL_ABAP_TABLEDESCR=>TABLEKIND_HASHED
                                                  P_UNIQUE     = ABAP_TRUE
                                                  P_KEY        = LT_KEYS
                                                  P_KEY_KIND   = CL_ABAP_TABLEDESCR=>KEYDEFKIND_DEFAULT ).

    CREATE DATA LT_TABLE TYPE HANDLE LO_TABLE_DESCR.
    ASSIGN LT_TABLE->* TO <LT_DATA>.


    DATA: LO_EXCEPTION TYPE REF TO CX_ROOT,
          LV_MSG       TYPE TY_S_MESSAGES-MSG.
    TRY.
*    SELECT * FROM (iv_sql_table) INTO TABLE <lt_data> WHERE (iv_sql_where).
        SELECT (IV_SQL_FIELDS) FROM (IV_SQL_TABLE) INTO CORRESPONDING FIELDS OF TABLE <LT_DATA> WHERE (IV_SQL_WHERE).
        IF ( SY-SUBRC <> 0 ).
          PERFORM ADD_MESSAGE USING    'E' 'Nessun risultato'
                              CHANGING LT_MESSAGES[].
        ENDIF.
      CATCH CX_ROOT INTO LO_EXCEPTION.
        LV_MSG = LO_EXCEPTION->GET_TEXT( ).
        PERFORM ADD_MESSAGE USING    'E' LV_MSG
                            CHANGING LT_MESSAGES[].
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
    PERFORM JSONP_OPEN_CALLBACK USING    IV_CALLBACK
                                CHANGING HTML[].

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
    PERFORM JSONP_OPEN_RESULTS CHANGING HTML[].
    " costruisce results
    PERFORM JSONP_BUILD_RESULTS USING    IV_SQL_TABLE
                                         <LT_DATA>[]
                                         LO_STRUCT_DESCR->COMPONENTS[]
                                         LT_FIELDS[]
*                                       IV_ROWS
                                         IV_FROM_REC
                                         IV_TO_REC
                                CHANGING HTML[].
    " chiude tag results
    PERFORM JSONP_CLOSE_RESULTS CHANGING HTML[].

    " chiude tab funzione di callback
    PERFORM JSONP_CLOSE_CALLBACK USING IV_CALLBACK
                                 CHANGING HTML[].

  ELSE.
    PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                    LT_MESSAGES[]
                           CHANGING HTML[].
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
