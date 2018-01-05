FUNCTION Z_WRFC_INTERFACE.
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

  " SMW0 -> rilascio funzioni

  " http://mnibm09.novellini.it:8066/sap/bc/webrfc?_FUNCTION=Z_WRFC_INTERFACE&callback=jsonCallback&method=C_PDM_GET_PLANTS&fields=WERKS

  FIELD-SYMBOLS: <LS_DATA> TYPE ANY,
                 <LT_DATA> TYPE ANY TABLE,
                 <LV_DATA> TYPE ANY.
  TYPES: BEGIN OF TY_S_WORK, BUFFER(30000), END OF TY_S_WORK.
  DATA: LS_WORK TYPE TY_S_WORK.

  DATA: LO_STRUCT_DESCR   TYPE REF TO CL_ABAP_STRUCTDESCR,
        LO_TABLE_DESCR    TYPE REF TO CL_ABAP_TABLEDESCR,
        LO_DATA_DESCR     TYPE REF TO CL_ABAP_DATADESCR,
        LS_COMPONENTS     TYPE ABAP_COMPDESCR,
        LT_COMPONENTS     TYPE ABAP_COMPDESCR_TAB,
        LT_KEYS           TYPE ABAP_KEYDESCR_TAB,
        LV_FIELD          TYPE C LENGTH 50,
        LS_STRING         TYPE STRING,
        LT_TABLE          TYPE REF TO DATA,
        LS_TABLE          TYPE REF TO DATA,
        LT_FIELDS         TYPE TABLE OF STRING,
        LS_FIELDS         TYPE STRING,
        LS_MESSAGES       TYPE TY_S_MESSAGES,
        LV_SNAME          TYPE STRING,
        LV_QUERY_NAME     TYPE W3_QNAME,
        LV_QUERY_NULL     TYPE W3_QNAME.

  DATA: LX_ROOT TYPE REF TO CX_ROOT.

  DATA: LT_MESSAGES TYPE TY_T_MESSAGES.

  CLEAR: LV_SNAME.

  REFRESH LT_MESSAGES.

* parametri
  DATA: IV_CALLBACK       TYPE STRING,
        IV_METHOD         TYPE STRING,
        IV_FIELDS         TYPE STRING,
*        IV_ROWS           TYPE STRING,
        IV_FROM_REC       TYPE I,
        IV_TO_REC         TYPE I.

  CLEAR: IV_CALLBACK,
         IV_METHOD,
         IV_FIELDS,
*         IV_ROWS,
         IV_FROM_REC,
         IV_TO_REC.

  LOOP AT QUERY_STRING.
    TRANSLATE QUERY_STRING-NAME TO UPPER CASE.
    CLEAR: LV_QUERY_NAME.
    " usa il pipe per parametro passato in righe multiple
    " es. matnr|0 matnr|1 ... matnr|n
    SPLIT QUERY_STRING-NAME AT '|' INTO LV_QUERY_NAME LV_QUERY_NULL.
    IF ( NOT LV_QUERY_NAME IS INITIAL ).
      QUERY_STRING-NAME = LV_QUERY_NAME.
    ENDIF.
    MODIFY QUERY_STRING.
  ENDLOOP.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'CALLBACK'.
  IV_CALLBACK = QUERY_STRING-VALUE.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'METHOD'.
  IV_METHOD = QUERY_STRING-VALUE.

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'FIELDS'.
  IV_FIELDS = QUERY_STRING-VALUE.
  IF ( IV_FIELDS IS INITIAL ).
    IV_FIELDS = '*'.
  ENDIF.

*  CLEAR: QUERY_STRING.
*  READ TABLE QUERY_STRING WITH KEY NAME = 'ROWS'.
*  IV_ROWS = QUERY_STRING-VALUE.
*  IF ( IV_ROWS IS INITIAL ).
*    IV_ROWS = '20'.
*  ENDIF.

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


  REFRESH: LT_FIELDS.
  SPLIT IV_FIELDS AT ' ' INTO TABLE LT_FIELDS.
  LOOP AT LT_FIELDS INTO LS_FIELDS.
    IF ( LS_FIELDS = SPACE ).
      DELETE LT_FIELDS.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  IF ( IV_CALLBACK IS INITIAL ).
    PERFORM ADD_MESSAGE USING    'E' 'Specificare funzione di callback'
                        CHANGING LT_MESSAGES[].
  ENDIF.



  DATA: LS_INTERFACE TYPE RSFBINTFV,
        LS_PARAM     TYPE RSFBPARA,
        LV_NAME      TYPE EU_LNAME.
  LV_NAME = IV_METHOD.
  CL_FB_FUNCTION_UTILITY=>METH_GET_INTERFACE( EXPORTING IM_NAME      = LV_NAME
                                              IMPORTING EX_INTERFACE = LS_INTERFACE
                                              EXCEPTIONS ERROR_OCCURED = 1
                                                         OBJECT_NOT_EXISTING = 2 ).
  IF ( SY-SUBRC = 0 ).
    DATA: LT_REP      TYPE SWBSE_MAX_LINE_TAB,
          LT_IMPORT   TYPE RSFB_IMP,
          LT_CHANGE   TYPE RSFB_CHA,
          LT_EXPORT   TYPE RSFB_EXP,
          LT_TABLES   TYPE RSFB_TBL,
          LT_EXCEPT   TYPE RSFB_EXC,
          LS_IMPORT   TYPE RSIMP,
          LS_CHANGE   TYPE RSCHA,
          LS_EXPORT   TYPE RSEXP,
          LS_PARA     TYPE RSFBPARA,  "rstbl,
          LS_EXCEPT   TYPE RSEXC,
          LV_FUNCNAME TYPE RS38L-NAME.

    LV_FUNCNAME = IV_METHOD.

    DATA: FUNC          TYPE STRING,
          TEXT_TAB      LIKE STANDARD TABLE OF LINE,
          FILETYPE(10)  TYPE C,
          LT_PARAMS     TYPE ABAP_FUNC_PARMBIND_TAB,
          LS_PARAMS     TYPE ABAP_FUNC_PARMBIND,
          LT_EXCEPTIONS TYPE ABAP_FUNC_EXCPBIND_TAB,
          LS_EXCEPTIONS TYPE ABAP_FUNC_EXCPBIND.

    FUNC = IV_METHOD.

    REFRESH: LT_PARAMS, LT_EXCEPTIONS.

    DATA: LREF_DATA TYPE REF TO DATA,
          LV_QUERY  TYPE STRING.

    FIELD-SYMBOLS: <DATA> TYPE ANY.

    LOOP AT LS_INTERFACE-IMPORT INTO LS_PARA.
      CLEAR: LV_QUERY.
      LOOP AT QUERY_STRING WHERE NAME EQ LS_PARA-PARAMETER.
        LV_QUERY = LV_QUERY && QUERY_STRING-VALUE.
      ENDLOOP.
      IF ( SY-SUBRC = 0 ).
        CLEAR: LS_PARAMS.
        LS_PARAMS-NAME = LS_PARA-PARAMETER.
        LS_PARAMS-KIND = ABAP_FUNC_EXPORTING.
        PERFORM CREATE_PARAMETER USING    LS_PARA-STRUCTURE
                                          LV_QUERY
                                 CHANGING LS_PARAMS-VALUE.
        INSERT LS_PARAMS INTO TABLE LT_PARAMS.
      ELSE.
      ENDIF.
    ENDLOOP.

    LOOP AT LS_INTERFACE-EXPORT INTO LS_PARA.
      CLEAR: LS_PARAMS.
      LS_PARAMS-NAME = LS_PARA-PARAMETER.
      LS_PARAMS-KIND = ABAP_FUNC_IMPORTING.
      PERFORM CREATE_PARAMETER USING    LS_PARA-STRUCTURE
                                        ''
                               CHANGING LS_PARAMS-VALUE.
*        FIELD-SYMBOLS: <lv_value> TYPE any.
*        ASSIGN query_string-value TO <lv_value>.
*        GET REFERENCE OF <lv_value> INTO ls_params-value.
      INSERT LS_PARAMS INTO TABLE LT_PARAMS.
    ENDLOOP.

    LOOP AT LS_INTERFACE-CHANGE INTO LS_PARA.
      CLEAR: LV_QUERY.
      LOOP AT QUERY_STRING WHERE NAME = LS_PARA-PARAMETER.
        LV_QUERY = LV_QUERY && QUERY_STRING-VALUE.
      ENDLOOP.
      CLEAR: LS_PARAMS.
      LS_PARAMS-NAME = LS_PARA-PARAMETER.
      LS_PARAMS-KIND = ABAP_FUNC_CHANGING.
      PERFORM CREATE_PARAMETER USING    LS_PARA-STRUCTURE
                                        LV_QUERY
                               CHANGING LS_PARAMS-VALUE.
      INSERT LS_PARAMS INTO TABLE LT_PARAMS.
    ENDLOOP.

    LOOP AT LS_INTERFACE-TABLES INTO LS_PARA.
      CLEAR: LV_QUERY.
      LOOP AT QUERY_STRING WHERE NAME = LS_PARA-PARAMETER.
        LV_QUERY = LV_QUERY && QUERY_STRING-VALUE.
      ENDLOOP.
      CLEAR: LS_PARAMS.
      LS_PARAMS-NAME = LS_PARA-PARAMETER.
      LS_PARAMS-KIND = ABAP_FUNC_TABLES.
      PERFORM CREATE_TABLE USING    LS_PARA-STRUCTURE
                                    LV_QUERY
                           CHANGING LS_PARAMS-VALUE.
      INSERT LS_PARAMS INTO TABLE LT_PARAMS.
    ENDLOOP.

    LOOP AT LS_INTERFACE-EXCEPT INTO LS_PARA.
      CLEAR: LS_EXCEPTIONS.
      LS_EXCEPTIONS-VALUE = SY-TABIX.
      LS_EXCEPTIONS-NAME = LS_PARA-PARAMETER.
      INSERT LS_EXCEPTIONS INTO TABLE LT_EXCEPTIONS.
    ENDLOOP.

    IF ( LT_FIELDS[] IS INITIAL ).
      LS_FIELDS = '*'.
      APPEND LS_FIELDS TO LT_FIELDS.
    ENDIF.

    TRY.
*        CL_FB_FUNCTION_UTILITY=>METH_ED_GENERATE_CALL( EXPORTING FUNCNAME    = LV_FUNCNAME
*                                                                 P_IF_IMPORT = LT_IMPORT
*                                                                 P_IF_CHANGE = LT_CHANGE
*                                                                 P_IF_EXPORT = LT_EXPORT
*                                                                 P_IF_TABLES = LT_TABLES
*                                                                 P_IF_EXCEPT = LT_EXCEPT
*                                                       CHANGING  REP = LT_REP ).
        DATA: LO_ROOT TYPE REF TO CX_ROOT.
        CALL FUNCTION FUNC
          PARAMETER-TABLE LT_PARAMS
          EXCEPTION-TABLE LT_EXCEPTIONS.
        PERFORM JSON_ADD_RESULT USING    LT_PARAMS[]
                                CHANGING HTML[].

        " apre tag funcione di callback
        PERFORM JSONP_OPEN_CALLBACK USING    IV_CALLBACK
                                    CHANGING HTML[].

        " apre tag results
        PERFORM JSONP_OPEN_RESULTS CHANGING HTML[].

        LOOP AT LT_PARAMS INTO LS_PARAMS.
          READ TABLE LS_INTERFACE-TABLES WITH KEY PARAMETER = LS_PARAMS-NAME TRANSPORTING NO FIELDS.
          IF ( SY-SUBRC = 0 ).
            LV_SNAME = LS_PARAMS-NAME.
            TRY.
                ASSIGN LS_PARAMS-VALUE->* TO <LT_DATA>.
                LOOP AT <LT_DATA> ASSIGNING <LS_DATA>.
                  EXIT.
                ENDLOOP.
                LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( <LS_DATA> ).
              CATCH CX_ROOT.
            ENDTRY.

            " costruisce results
            PERFORM JSONP_BUILD_RESULTS USING    LV_SNAME
                                                 <LT_DATA>[]
                                                 LO_STRUCT_DESCR->COMPONENTS[]
                                                 LT_FIELDS[]
*                                                 IV_ROWS
                                                 IV_FROM_REC
                                                 IV_TO_REC
                                        CHANGING HTML[].
*            PERFORM JSON_CREATE_FROM_DATA USING    IV_CALLBACK
*                                                   LS_PARAMS-NAME
*                                                   <LT_DATA>[]
*                                          CHANGING HTML[].


          ENDIF.
          READ TABLE LS_INTERFACE-EXPORT WITH KEY PARAMETER = LS_PARAMS-NAME TRANSPORTING NO FIELDS.
          IF ( SY-SUBRC = 0 ).
            LV_SNAME = LS_PARAMS-NAME.
            TRY.
                ASSIGN LS_PARAMS-VALUE->* TO <LS_DATA>.
                LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( <LS_DATA> ).

                PERFORM JSONP_BUILD_RESULTS USING    LV_SNAME
                                                     <LS_DATA>
                                                     LO_STRUCT_DESCR->COMPONENTS[]
                                                     LT_FIELDS[]
*                                                     IV_ROWS
                                                     IV_FROM_REC
                                                     IV_TO_REC
                                            CHANGING HTML[].
              CATCH CX_ROOT.
                TRY.
                    ASSIGN LS_PARAMS-VALUE->* TO <LS_DATA>.
                    LO_DATA_DESCR ?= CL_ABAP_DATADESCR=>DESCRIBE_BY_DATA( <LS_DATA> ).

                    HTML-LINE = '"' && LV_SNAME && '":"' && <LS_DATA> && '",'..
                    APPEND HTML.
                  CATCH CX_ROOT.
                ENDTRY.
            ENDTRY.

*            ASSIGN <LS_DATA> TO <LV_DATA>.
*            PERFORM JSON_CREATE_FROM_DATA USING    IV_CALLBACK
*                                                   LS_PARAMS-NAME
*                                                   <LV_DATA>
*                                          CHANGING HTML[].

          ENDIF.
          READ TABLE LS_INTERFACE-CHANGE WITH KEY PARAMETER = LS_PARAMS-NAME TRANSPORTING NO FIELDS.
          IF ( SY-SUBRC = 0 ).
            LV_SNAME = LS_PARAMS-NAME.
            TRY.
                ASSIGN LS_PARAMS-VALUE->* TO <LS_DATA>.
                LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( <LS_DATA> ).

                PERFORM JSONP_BUILD_RESULTS USING    LV_SNAME
                                                     <LS_DATA>
                                                     LO_STRUCT_DESCR->COMPONENTS[]
                                                     LT_FIELDS[]
*                                                     IV_ROWS
                                                     IV_FROM_REC
                                                     IV_TO_REC
                                            CHANGING HTML[].
              CATCH CX_ROOT.
                TRY.
                    ASSIGN LS_PARAMS-VALUE->* TO <LS_DATA>.
                    LO_DATA_DESCR ?= CL_ABAP_DATADESCR=>DESCRIBE_BY_DATA( <LS_DATA> ).

                    HTML-LINE = '"' && LV_SNAME && '":"' && <LS_DATA> && '",'..
                    APPEND HTML.
                  CATCH CX_ROOT.
                ENDTRY.
            ENDTRY.

*            ASSIGN <LS_DATA> TO <LV_DATA>.
*            PERFORM JSON_CREATE_FROM_DATA USING    IV_CALLBACK
*                                                   LS_PARAMS-NAME
*                                                   <LV_DATA>
*                                          CHANGING HTML[].

          ENDIF.

        ENDLOOP.

        " chiude tag results
        PERFORM JSONP_CLOSE_RESULTS CHANGING HTML[].

        " apre tag funcione di callback
        PERFORM JSONP_CLOSE_CALLBACK USING    IV_CALLBACK
                                     CHANGING HTML[].

      CATCH CX_ROOT INTO LO_ROOT.
        DATA: LV_MSG TYPE TY_S_MESSAGES-MSG.
        LV_MSG = LO_ROOT->GET_TEXT( ).
        PERFORM ADD_MESSAGE USING    'E' LV_MSG
                            CHANGING LT_MESSAGES[].

        PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                        LT_MESSAGES[]
                               CHANGING HTML[].
    ENDTRY.
  ELSE.
    CASE SY-SUBRC.
      WHEN 1.
        PERFORM ADD_MESSAGE USING    'E' 'Error'
                            CHANGING LT_MESSAGES[].
      WHEN 2.
        PERFORM ADD_MESSAGE USING    'E' 'Function not exist'
                            CHANGING LT_MESSAGES[].
    ENDCASE.

    PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                    LT_MESSAGES[]
                           CHANGING HTML[].
  ENDIF.

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
