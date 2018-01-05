FUNCTION Z_WRFC_FUNCTION_INTERFACE.
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

  " http://mnibm09.novellini.it:8066/sap/bc/webrfc?_FUNCTION=Z_WRFC_FUNCTION_INTERFACE&callback=jsonCallback&method=C_PDM_GET_PLANTS

  FIELD-SYMBOLS: <LS_DATA> TYPE ANY.
  FIELD-SYMBOLS: <LT_DATA> TYPE ANY TABLE.
  TYPES: BEGIN OF TY_S_WORK, BUFFER(30000), END OF TY_S_WORK.
  DATA: LS_WORK TYPE TY_S_WORK.

  DATA: LX_ROOT           TYPE REF TO CX_ROOT,
        LO_STRUCT_DESCR   TYPE REF TO CL_ABAP_STRUCTDESCR,
        LO_STRUCT_DESCR_2 TYPE REF TO CL_ABAP_STRUCTDESCR,
        LO_TABLE_DESCR    TYPE REF TO CL_ABAP_TABLEDESCR,
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

  REFRESH LT_MESSAGES.

  CLEAR: LV_SNAME.

* parametri
  DATA: IV_CALLBACK       TYPE STRING,
        IV_METHOD         TYPE STRING,
        IV_FIELDS         TYPE STRING.

  CLEAR: IV_CALLBACK,
         IV_METHOD,
         IV_FIELDS.

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
    " apre tag funcione di callback
    PERFORM JSONP_OPEN_CALLBACK USING    IV_CALLBACK
                                CHANGING HTML[].

    TRY.
        FIELD-SYMBOLS <LV_FIELD> TYPE ANY.
        LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( LS_INTERFACE ).

** columns ------------------------------------------------------
*        " apre tag columns
*        PERFORM jsonp_open_columns CHANGING html[].
*        " costruisce columns
*        PERFORM jsonp_build_columns USING    lo_struct_descr->components[]
*                                             lt_fields[]
*                                    CHANGING html[].
*        " chiude tag columns
*        PERFORM jsonp_close_columns CHANGING html[].

        " apre tag results
        PERFORM JSONP_OPEN_RESULTS CHANGING HTML[].

        LOOP AT LO_STRUCT_DESCR->COMPONENTS INTO LS_COMPONENTS WHERE NAME = 'IMPORT'
                                                               OR    NAME = 'EXPORT'
                                                               OR    NAME = 'CHANGE'
                                                               OR    NAME = 'TABLES'
                                                               OR    NAME = 'EXCEPT'.
          LV_FIELD = 'ls_interface-' && LS_COMPONENTS-NAME.
          ASSIGN (LV_FIELD) TO <LT_DATA>.
          IF ( SY-SUBRC = 0 ).
            LV_SNAME = LS_COMPONENTS-NAME.
            HTML-LINE = '"' && LS_COMPONENTS-NAME && '":{'.
            APPEND HTML.
            LOOP AT <LT_DATA> ASSIGNING <LS_DATA>.
              TRY.
                  LO_STRUCT_DESCR_2 ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( <LS_DATA> ).
* results ------------------------------------------------------
*                  " apre tag results
*                  PERFORM jsonp_open_results CHANGING html[].
                  " costruisce results
                  DATA:  LS_COMPONENTS_2    TYPE ABAP_COMPDESCR,
                         LS_COMPONENTS_3    TYPE ABAP_COMPDESCR,
                         LV_PAR TYPE STRING.
                  FIELD-SYMBOLS: <LV_PAR> TYPE ANY,
                                 <LV_PAR2> TYPE ANY.
                  LOOP AT LO_STRUCT_DESCR_2->COMPONENTS INTO LS_COMPONENTS_2 WHERE NAME = 'PARAMETER'.
                    LV_PAR = '<LS_DATA>-' && LS_COMPONENTS_2-NAME.
                    ASSIGN (LV_PAR) TO <LV_PAR>.
                    CHECK SY-SUBRC = 0.
                    DATA: LO_DATA   TYPE REF TO CL_ABAP_DATADESCR,
                          LO_STRUCT TYPE REF TO CL_ABAP_STRUCTDESCR,
                          LO_TABLE  TYPE REF TO CL_ABAP_TABLEDESCR,
                          LV_TYPE   TYPE STRING.
                    CLEAR: LV_TYPE.
                    LV_PAR = '<LS_DATA>-structure'.
                    ASSIGN (LV_PAR) TO <LV_PAR2>.
                    CHECK SY-SUBRC = 0.
                    IF ( NOT <LV_PAR2> IS INITIAL ).
                      TRY.
                          LO_TABLE ?= CL_ABAP_TABLEDESCR=>DESCRIBE_BY_NAME( <LV_PAR2> ).
                          LV_TYPE = '"table"'.
*                        HTML-LINE = '"table": {'.
*                        APPEND HTML.
*                        DATA:  LS_COMPONENTS_3    TYPE ABAP_COMPDESCR.
*                        LOOP AT LO_TABLE->COMPONENTS INTO LS_COMPONENTS_3.
*                        ENDLOOP.
*                        HTML-LINE = '}'.
*                        APPEND HTML.
                        CATCH CX_ROOT.
                          TRY.
                              LO_STRUCT ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_NAME( <LV_PAR2> ).
                              LV_TYPE = '"structure"'.
*                              HTML-LINE = '"structure": {'.
*                              APPEND HTML.
*                              LOOP AT LO_STRUCT->COMPONENTS INTO LS_COMPONENTS_3.
*                              ENDLOOP.
*                              HTML-LINE = '}'.
*                              APPEND HTML.
                            CATCH CX_ROOT.
                              TRY.
                                  LO_DATA ?= CL_ABAP_DATADESCR=>DESCRIBE_BY_NAME( <LV_PAR2> ).
                                  LV_TYPE = '"field"'.
*                                  HTML-LINE = '"' && <LV_PAR> && '": "' && <LV_PAR2> &&  '",'.
*                                  APPEND HTML.
                                CATCH CX_ROOT.
                              ENDTRY.
                          ENDTRY.
                      ENDTRY.
                    ENDIF.
                    HTML-LINE = '"' && <LV_PAR> && '": "' && <LV_PAR2> &&  '",'.
                    APPEND HTML.
                  ENDLOOP.
*                  PERFORM JSONP_BUILD_RESULTS USING    LV_SNAME
*                                                       <LT_DATA>[]
*                                                       LO_STRUCT_DESCR_2->COMPONENTS[]
*                                                       LT_FIELDS[]
**                                                       '10'
*                                                       1
*                                                       10
*                                              CHANGING HTML[].
*                  PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING HTML[].

                CATCH CX_ROOT INTO LX_ROOT.
                  CLEAR: LS_MESSAGES.
                  LS_MESSAGES-TYPE = 'E'.
                  LS_MESSAGES-MSG = LX_ROOT->GET_TEXT( ).
                  APPEND LS_MESSAGES TO LT_MESSAGES.

                  PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                                  LT_MESSAGES[]
                                         CHANGING HTML[].
                  RETURN.
              ENDTRY.
            ENDLOOP.
            PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING HTML[].
            HTML-LINE = '},'.
            APPEND HTML.
          ENDIF.
        ENDLOOP.
        PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING HTML[].

        " chiude tag results
        PERFORM JSONP_CLOSE_RESULTS CHANGING HTML[].
      CATCH CX_ROOT.
        CLEAR: LS_MESSAGES.
        LS_MESSAGES-TYPE = 'E'.
        LS_MESSAGES-MSG = LX_ROOT->GET_TEXT( ).
        APPEND LS_MESSAGES TO LT_MESSAGES.

        PERFORM CREATE_JSONP_2 USING    IV_CALLBACK
                                        LT_MESSAGES[]
                               CHANGING HTML[].
        RETURN.
    ENDTRY.

    " apre tag funcione di callback
    PERFORM JSONP_CLOSE_CALLBACK USING IV_CALLBACK
                                 CHANGING HTML[].

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
*   iv_callback({
*     "errors": [
*       { "type": "...", "message": "..." },
*     ],
*     "results": {
*       "exporting": {
*         "field1": "...",
*          ...
*         "fieldn": "...",
*         "structure1": { "key1": "...", ..., "keyn": "..." },
*         ...,
*         "structuren": { "key1": "...", ..., "keyn": "..." },
*         "table1": [
*           { "key1": "...", ..., "keyn": "..." },
*           ...,
*           { "key1": "...", ..., "keyn": "..." },
*         ],
*         ...,
*         "tablen": [
*           { "key1": "...", ..., "keyn": "..." },
*           ...,
*           { "key1": "...", ..., "keyn": "..." },
*         ]
*       },
*       "importing": {
*       },
*       "changing": {
*       },
*       "tables": {
*       },
*     }
*   });
