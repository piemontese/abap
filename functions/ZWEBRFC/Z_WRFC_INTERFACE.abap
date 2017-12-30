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
        LV_SNAME          TYPE STRING.

  DATA: LT_MESSAGES TYPE TY_T_MESSAGES.

  CLEAR: LV_SNAME.

  REFRESH LT_MESSAGES.

* parametri
  DATA: IV_CALLBACK       TYPE STRING,
        IV_METHOD         TYPE STRING,
        IV_FIELDS         TYPE STRING,
        IV_ROWS           TYPE STRING.

  CLEAR: IV_CALLBACK,
         IV_METHOD,
         IV_FIELDS,
         IV_ROWS.

  LOOP AT QUERY_STRING.
    TRANSLATE QUERY_STRING-NAME TO UPPER CASE.
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

  CLEAR: QUERY_STRING.
  READ TABLE QUERY_STRING WITH KEY NAME = 'ROWS'.
  IV_ROWS = QUERY_STRING-VALUE.
  IF ( IV_ROWS IS INITIAL ).
    IV_ROWS = '20'.
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

    DATA: LREF_DATA TYPE REF TO DATA.

    FIELD-SYMBOLS: <DATA> TYPE ANY.

    LOOP AT LS_INTERFACE-IMPORT INTO LS_PARA.
      CLEAR: QUERY_STRING.
      READ TABLE QUERY_STRING WITH KEY NAME = LS_PARA-PARAMETER.
      IF ( SY-SUBRC = 0 ).
        CLEAR: LS_PARAMS.
        LS_PARAMS-NAME = LS_PARA-PARAMETER.
        LS_PARAMS-KIND = ABAP_FUNC_EXPORTING.
        PERFORM CREATE_PARAMETER USING    LS_PARA-STRUCTURE
                                          QUERY_STRING-VALUE
                                 CHANGING LS_PARAMS-VALUE.
*        FIELD-SYMBOLS: <lv_value> TYPE any.
*        ASSIGN query_string-value TO <lv_value>.
*        GET REFERENCE OF <lv_value> INTO ls_params-value.
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

    LOOP AT LS_INTERFACE-TABLES INTO LS_PARA.
      CLEAR: LS_PARAMS.
      LS_PARAMS-NAME = LS_PARA-PARAMETER.
      LS_PARAMS-KIND = ABAP_FUNC_TABLES.
      PERFORM CREATE_TABLE USING    LS_PARA-STRUCTURE
                           CHANGING LS_PARAMS-VALUE.
*      GET REFERENCE OF text_tab INTO ls_params-value.
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
                                                 IV_ROWS
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
                                                     IV_ROWS
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
        CLEAR: LS_MESSAGES.
        LS_MESSAGES-TYPE = 'E'.
        LS_MESSAGES-MSG = LO_ROOT->GET_TEXT( ).
        APPEND LS_MESSAGES TO LT_MESSAGES.

        PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                        LT_MESSAGES[]
                               CHANGING HTML[].
    ENDTRY.
  ELSE.
    CLEAR: LS_MESSAGES.
    LS_MESSAGES-TYPE = 'E'.
    CASE SY-SUBRC.
      WHEN 1.
        LS_MESSAGES-MSG = 'Error'.
      WHEN 2.
        LS_MESSAGES-MSG = 'Function not exist'.
    ENDCASE.
    APPEND LS_MESSAGES TO LT_MESSAGES.

    PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                    LT_MESSAGES[]
                           CHANGING HTML[].
  ENDIF.

ENDFUNCTION.

* struttura file jsonp
*    callback({
*        "errors": [
*            { "type": "type" },
*            { "message": "msg" },
*        ],
*        "columns": [
*            { "columnn": "name1" },
*            { "columnn": "name2" },
*            { "columnn": "name3" }
*            { "columnn": "name4" }
*        ],
*        "results": [
*            {
*                "rows": [
*                    { "name": "name1", "value": "value1" },
*                    { "name": "name2", "value": "value2" },
*                    { "name": "name3", "value": "value3" },
*                    { "name": "name4", "value": "value4" }
*                ]
*            }
*        ]
*    })
