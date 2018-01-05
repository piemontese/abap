FUNCTION Z_WRFC_GET_TYPE.
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
        LO_DATA_DESCR     TYPE REF TO CL_ABAP_DATADESCR,
        LX_ROOT           TYPE REF TO CX_ROOT,
        LS_COMPONENTS     TYPE ABAP_COMPDESCR,
        LT_KEYS           TYPE ABAP_KEYDESCR_TAB,
        LS_STRING         TYPE STRING,
        LT_TABLE          TYPE REF TO DATA,
        LS_TABLE          TYPE REF TO DATA.

  DATA: LT_MESSAGES TYPE TY_T_MESSAGES.

  REFRESH LT_MESSAGES.

* parametri
  DATA: IV_CALLBACK         TYPE STRING,
        IV_TYPE             TYPE STRING.
*        IV_SQL_TABLE        TYPE STRING,
*        IV_SQL_WHERE        TYPE STRING,
*        IV_SQL_FIELDS       TYPE STRING,
*        IV_FROM_REC         TYPE I,
*        IV_TO_REC            TYPE I.

  CLEAR: IV_CALLBACK,
         IV_TYPE.

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
  READ TABLE QUERY_STRING WITH KEY NAME = 'TYPE'.
  IV_TYPE = QUERY_STRING-VALUE.

  IF ( IV_CALLBACK IS INITIAL ).
    PERFORM ADD_MESSAGE USING    'E' 'Specificare funzione di callback'
                        CHANGING LT_MESSAGES[].
  ENDIF.

  IF ( IV_TYPE IS INITIAL ).
    PERFORM ADD_MESSAGE USING    'E' 'Specificare tipo'
                        CHANGING LT_MESSAGES[].
  ENDIF.

  IF ( NOT IV_TYPE IS INITIAL ).
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
    PERFORM JSONP_OPEN_CALLBACK USING    IV_CALLBACK
                                CHANGING HTML[].

* results ------------------------------------------------------
    " apre tag results
    PERFORM JSONP_OPEN_RESULTS CHANGING HTML[].
    " costruisce results

    DATA: LREF_VALUE TYPE REF TO DATA,
          LV_TYPE    TYPE W3_QVALUE.
    LV_TYPE = IV_TYPE.
    PERFORM CREATE_PARAMETER USING    LV_TYPE
                                      ''
                             CHANGING LREF_VALUE.

    DATA: LV_PARAM          TYPE REF TO DATA,
          LV_VALUE          TYPE STRING.
    FIELD-SYMBOLS: <LV_PARAM> TYPE ANY.
    TRY.
      TRY.
          CREATE DATA LV_PARAM TYPE (LV_TYPE).
        CATCH CX_SY_CREATE_DATA_ERROR INTO LX_ROOT.
          CREATE DATA LV_PARAM LIKE LV_TYPE.
      ENDTRY.
      ASSIGN LV_PARAM->* TO <LV_PARAM>.
      TRY.
          LO_TABLE_DESCR ?= CL_ABAP_TABLEDESCR=>DESCRIBE_BY_DATA( <LV_PARAM> ).
          CL_FDT_JSON=>JSON_TO_DATA( EXPORTING IV_JSON = LV_VALUE
                                     CHANGING  CA_DATA = <LV_PARAM> ).
        CATCH CX_ROOT.
          TRY.
              LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( <LV_PARAM> ).
              CL_FDT_JSON=>JSON_TO_DATA( EXPORTING IV_JSON = LV_VALUE
                                         CHANGING  CA_DATA = <LV_PARAM> ).
            CATCH CX_ROOT.
              TRY.
                  LO_DATA_DESCR ?= CL_ABAP_DATADESCR=>DESCRIBE_BY_DATA( <LV_PARAM> ).
                CATCH CX_ROOT.
                  PERFORM ADD_MESSAGE USING    'E' 'Specificare un tipo valido in SAP'
                                      CHANGING LT_MESSAGES[].
                  PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                                  LT_MESSAGES[]
                                         CHANGING HTML[].
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
