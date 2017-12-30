*----------------------------------------------------------------------*
***INCLUDE LZ_SAMPLERFCF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CREATE_JSONP
*&---------------------------------------------------------------------*
FORM CREATE_JSONP  USING VALUE(IV_CALLBACK)   TYPE STRING
                         VALUE(IT_DATA)       TYPE ANY TABLE
                         VALUE(IT_COMPONENTS) TYPE ABAP_COMPDESCR_TAB
                         VALUE(IT_FIELDS)     TYPE TABLE
                         VALUE(IT_MESSAGES)   TYPE TY_T_MESSAGES
                         VALUE(IV_ROWS)       TYPE STRING
                   CHANGING    CT_JSONP       TYPE TY_T_W3HTML.


  DATA: HTMLDOC       TYPE W3HTML,
        LS_FIELDS     TYPE STRING,
        LV_FIELD      TYPE C LENGTH 50,
        LS_COMPONENTS TYPE ABAP_COMPDESCR,
        LV_SNAME      TYPE STRING.

  FIELD-SYMBOLS: <LS_DATA> TYPE ANY.

  CLEAR: LV_SNAME.

  " apre tag funcione di callback
  PERFORM JSONP_OPEN_CALLBACK USING    IV_CALLBACK
                              CHANGING CT_JSONP[].

* errors -------------------------------------------------------
  " apre tag errors
  PERFORM JSONP_OPEN_ERRORS CHANGING CT_JSONP[].
  " costruzione errors
  PERFORM JSONP_BUILD_ERRORS USING    IT_MESSAGES[]
                             CHANGING CT_JSONP[].
  " chiude tag errors
  PERFORM JSONP_CLOSE_ERRORS CHANGING CT_JSONP[].

* columns ------------------------------------------------------
  " apre tag columns
  PERFORM JSONP_OPEN_COLUMNS CHANGING CT_JSONP[].
  " costruisce columns
  PERFORM JSONP_BUILD_COLUMNS USING    IT_COMPONENTS[]
                                       IT_FIELDS[]
                              CHANGING CT_JSONP[].
  " chiude tag columns
  PERFORM JSONP_CLOSE_COLUMNS CHANGING CT_JSONP[].

* results ------------------------------------------------------
  " apre tag results
  PERFORM JSONP_OPEN_RESULTS CHANGING CT_JSONP[].
  " costruisce results
  PERFORM JSONP_BUILD_RESULTS USING    LV_SNAME
                                       IT_DATA[]
                                       IT_COMPONENTS[]
                                       IT_FIELDS[]
                                       IV_ROWS
                              CHANGING CT_JSONP[].
  " chiude tag results
  PERFORM JSONP_CLOSE_RESULTS CHANGING CT_JSONP[].

  " chiude tab funzione di callback
  PERFORM JSONP_CLOSE_CALLBACK USING IV_CALLBACK
                               CHANGING CT_JSONP[].

ENDFORM.                    " CREATE_JSONP

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_CALLBACK
*&---------------------------------------------------------------------*
FORM JSONP_OPEN_CALLBACK  USING VALUE(IV_CALLBACK) TYPE STRING
                          CHANGING    CT_JSONP     TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

  CLEAR: LS_JSONP.
  IF ( NOT IV_CALLBACK IS INITIAL ).
    LS_JSONP-LINE = IV_CALLBACK && '({'.
  ELSE.
    LS_JSONP-LINE = '{'.
  ENDIF.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_OPEN_CALLBACK

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_ERRORS
*&---------------------------------------------------------------------*
FORM JSONP_OPEN_ERRORS  CHANGING    CT_JSONP     TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

  LS_JSONP = '"errors": ['.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_OPEN_ERRORS

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_RESULTS
*&---------------------------------------------------------------------*
FORM JSONP_OPEN_RESULTS  CHANGING    CT_JSONP     TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

*  ls_jsonp = '{"results": ['.
*  LS_JSONP = '"results": ['.
  LS_JSONP = '"results": {'.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_OPEN_RESULTS

*&---------------------------------------------------------------------*
*&      Form  JSONP_BUILD_RESULTS
*&---------------------------------------------------------------------*
FORM JSONP_BUILD_RESULTS  USING VALUE(IV_SNAME)      TYPE STRING
                                VALUE(IV_DATA)       TYPE ANY
                                VALUE(IT_COMPONENTS) TYPE ABAP_COMPDESCR_TAB
                                VALUE(IT_FIELDS)     TYPE TABLE
                                VALUE(IV_ROWS)       TYPE STRING
                          CHANGING    CT_JSONP       TYPE TY_T_W3HTML.

  FIELD-SYMBOLS: <VALUE>   TYPE ANY,
                 <LS_DATA> TYPE ANY.

  DATA: LT_JSONP      TYPE TY_T_W3HTML,
        LS_JSONP      TYPE W3HTML,
        LS_FIELDS     TYPE STRING,
        LV_FIELD      TYPE C LENGTH 50,
        LV_STR_NAME   TYPE STRING,
        LV_STR_VAL    TYPE STRING,
        LS_COMPONENTS TYPE ABAP_COMPDESCR,
        LV_LINES      TYPE I,
        LV_TABIX      TYPE I,
        LV_COMMA      TYPE BOOL.

  DATA: LO_STRUCT_DESCR TYPE REF TO CL_ABAP_STRUCTDESCR,
        LO_TABLE_DESCR  TYPE REF TO CL_ABAP_TABLEDESCR,
        LO_DATA_DESCR   TYPE REF TO CL_ABAP_DATADESCR,
        LX_ROOT         TYPE REF TO CX_ROOT.

  TRY.
      LO_TABLE_DESCR ?= CL_ABAP_TABLEDESCR=>DESCRIBE_BY_DATA( IV_DATA ).

      FIELD-SYMBOLS: <LT_DATA> TYPE ANY TABLE.
      ASSIGN IV_DATA TO <LT_DATA>.

      IF ( NOT IV_SNAME IS INITIAL ).
        LS_JSONP-LINE = '"' && IV_SNAME && '": ['.
        INSERT LS_JSONP INTO TABLE CT_JSONP.
      ENDIF.

      DATA: LV_COUNT TYPE I.
      LV_COUNT = 1.
*      LOOP AT IT_DATA ASSIGNING <LS_DATA>.
      LOOP AT <LT_DATA> ASSIGNING <LS_DATA>.
        LV_TABIX = SY-TABIX.
        CHECK LV_COUNT <= IV_ROWS.
*    LS_JSONP = '"item": ['.
        LS_JSONP = '{'.
        INSERT LS_JSONP INTO TABLE CT_JSONP.

        DESCRIBE TABLE IT_FIELDS LINES LV_LINES.

        REFRESH: LT_JSONP.
        LOOP AT IT_FIELDS INTO LS_FIELDS.
          LV_TABIX = SY-TABIX.
          IF ( LS_FIELDS <> '*' ).
            CLEAR: LV_COMMA.
            IF ( LV_TABIX < LV_LINES ).
              LV_COMMA = 'X'.
            ENDIF.
            READ TABLE IT_COMPONENTS INTO LS_COMPONENTS WITH KEY NAME = LS_FIELDS.
            IF ( SY-SUBRC = 0 ).
              PERFORM JSONP_ADD_ROW_RESULTS USING     <LS_DATA>
                                                      LS_COMPONENTS
                                                      'X'  "lv_comma
*                                            CHANGING  CT_JSONP[].
                                            CHANGING  LT_JSONP[].
            ENDIF.
          ELSE.
*            REFRESH: LT_JSONP.
            LOOP AT IT_COMPONENTS INTO LS_COMPONENTS.
              LV_TABIX = SY-TABIX.
              CLEAR: LV_COMMA.
              IF ( LV_TABIX < LV_LINES ).
                LV_COMMA = 'X'.
              ENDIF.
              PERFORM JSONP_ADD_ROW_RESULTS USING     <LS_DATA>
                                                      LS_COMPONENTS
                                                      'X'   "lv_comma
*                                            CHANGING  CT_JSONP[].
                                            CHANGING  LT_JSONP[].
            ENDLOOP.
*            DATA: LV_STR       TYPE STRING,
*                  LS_JSONP_TMP TYPE W3HTML.
*            LV_STR = ''.
*            LOOP AT LT_JSONP INTO LS_JSONP_TMP.
*              IF ( STRLEN( LV_STR ) > 200 ).
*                LS_JSONP-LINE = LV_STR.
*                APPEND LS_JSONP TO CT_JSONP.
*                LV_STR = ''.
*              ENDIF.
*              LV_STR = LV_STR && LS_JSONP_TMP-LINE.
*            ENDLOOP.
*            IF ( STRLEN( LV_STR ) > 0 ).
*              LS_JSONP-LINE = LV_STR.
*              APPEND LS_JSONP TO CT_JSONP.
*            ENDIF.
*            PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING CT_JSONP[].
          ENDIF.
        ENDLOOP.
        DATA: LV_STR       TYPE STRING,
              LS_JSONP_TMP TYPE W3HTML.
        LV_STR = ''.
        LOOP AT LT_JSONP INTO LS_JSONP_TMP.
          IF ( STRLEN( LV_STR ) > 200 ).
            LS_JSONP-LINE = LV_STR.
            APPEND LS_JSONP TO CT_JSONP.
            LV_STR = ''.
          ENDIF.
          LV_STR = LV_STR && LS_JSONP_TMP-LINE.
        ENDLOOP.
        IF ( STRLEN( LV_STR ) > 0 ).
          LS_JSONP-LINE = LV_STR.
          APPEND LS_JSONP TO CT_JSONP.
        ENDIF.
        PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING CT_JSONP[].

        LS_JSONP = '},'.
        INSERT LS_JSONP INTO TABLE CT_JSONP.
        ADD 1 TO LV_COUNT.
      ENDLOOP.

      PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING CT_JSONP[].

      IF ( NOT IV_SNAME IS INITIAL ).
        LS_JSONP = '],'.
        INSERT LS_JSONP INTO TABLE CT_JSONP.
      ENDIF.
    CATCH CX_ROOT.
      TRY.
          LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( IV_DATA ).

          IF ( NOT IV_SNAME IS INITIAL ).
            LS_JSONP-LINE = '"' && IV_SNAME && '": {'.
            INSERT LS_JSONP INTO TABLE CT_JSONP.
          ENDIF.

*          LS_JSONP = '{'.
*          INSERT LS_JSONP INTO TABLE CT_JSONP.
          LOOP AT IT_COMPONENTS INTO LS_COMPONENTS.
            LV_TABIX = SY-TABIX.
            CLEAR: LV_COMMA.
            IF ( LV_TABIX < LV_LINES ).
              LV_COMMA = 'X'.
            ENDIF.
            PERFORM JSONP_ADD_ROW_RESULTS USING     IV_DATA
                                                    LS_COMPONENTS
                                                    'X'   "lv_comma
                                          CHANGING  CT_JSONP[].
          ENDLOOP.

          PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING CT_JSONP[].

          IF ( NOT IV_SNAME IS INITIAL ).
            LS_JSONP = '},'.
            INSERT LS_JSONP INTO TABLE CT_JSONP.
          ENDIF.
        CATCH CX_ROOT.
          TRY.
              LO_DATA_DESCR ?= CL_ABAP_DATADESCR=>DESCRIBE_BY_DATA( IV_DATA ).
            CATCH CX_ROOT.
          ENDTRY.
      ENDTRY.
  ENDTRY.

*  PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING CT_JSONP[].
*
*  IF ( NOT IV_SNAME IS INITIAL ).
*    LS_JSONP = '],'.
*    INSERT LS_JSONP INTO TABLE CT_JSONP.
*  ENDIF.

ENDFORM.                    " JSONP_BUILD_RESULTS

*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_ERRORS
*&---------------------------------------------------------------------*
FORM JSONP_CLOSE_ERRORS  CHANGING CT_JSONP TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

  LS_JSONP = '],'.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_CLOSE_ERRORS

*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_RESULTS
*&---------------------------------------------------------------------*
FORM JSONP_CLOSE_RESULTS  CHANGING CT_JSONP TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

  PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING CT_JSONP[].

*  LS_JSONP = ']}'.
  LS_JSONP = '}'.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_CLOSE_RESULTS*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_CALLBACK
*&---------------------------------------------------------------------*
FORM JSONP_CLOSE_CALLBACK  USING VALUE(IV_CALLBACK) TYPE STRING
                           CHANGING CT_JSONP TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

  PERFORM JSONP_REMOVE_EXTRA_COMMA CHANGING CT_JSONP[].

  IF ( NOT IV_CALLBACK IS INITIAL ).
    LS_JSONP = '});'.
  ELSE.
    LS_JSONP = '}'.
  ENDIF.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_CLOSE_CALLBACK
*&---------------------------------------------------------------------*
*&      Form  ADD_MESSAGE
*&---------------------------------------------------------------------*
FORM ADD_MESSAGE  USING VALUE(IV_TYPE)    TYPE TY_S_MESSAGES-TYPE
                        VALUE(IV_MSG)     TYPE TY_S_MESSAGES-MSG
                  CHANGING    CT_MESSAGES TYPE TY_T_MESSAGES.

  DATA: LS_MESSAGES TYPE TY_S_MESSAGES.

  CLEAR: LS_MESSAGES.
  LS_MESSAGES-TYPE = IV_TYPE.
  LS_MESSAGES-MSG  = IV_MSG.

  APPEND LS_MESSAGES TO CT_MESSAGES.

ENDFORM.                    " ADD_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  JSONP_BUILD_ERRORS
*&---------------------------------------------------------------------*
FORM JSONP_BUILD_ERRORS  USING VALUE(IT_MESSAGES) TYPE TY_T_MESSAGES
                         CHANGING    CT_JSONP     TYPE TY_T_W3HTML.

  DATA: LS_MESSAGES TYPE TY_S_MESSAGES,
        LS_JSONP    TYPE W3HTML,
        LV_LINES    TYPE I,
        LV_TABIX    TYPE I.

  DESCRIBE TABLE IT_MESSAGES LINES LV_LINES.

  LOOP AT IT_MESSAGES INTO LS_MESSAGES.
    LV_TABIX = SY-TABIX.
    CLEAR: LS_JSONP.
    CONCATENATE '{ "type":"' LS_MESSAGES-TYPE '", "msg":"' LS_MESSAGES-MSG '" }' INTO LS_JSONP.
    IF ( LV_TABIX < LV_LINES ).
      CONCATENATE LS_JSONP ',' INTO LS_JSONP.
    ENDIF.
    APPEND LS_JSONP TO CT_JSONP.
  ENDLOOP.

ENDFORM.                    " JSONP_BUILD_ERRORS

*&---------------------------------------------------------------------*
*&      Form  CREATE_JSONP_2
*&---------------------------------------------------------------------*
FORM CREATE_JSONP_2 USING VALUE(IV_CALLBACK)   TYPE STRING
                          VALUE(IT_MESSAGES)   TYPE TY_T_MESSAGES
                    CHANGING    CT_JSONP       TYPE TY_T_W3HTML.


  DATA: HTMLDOC       TYPE W3HTML.

  " apre tag funcione di callback
  PERFORM JSONP_OPEN_CALLBACK USING    IV_CALLBACK
                              CHANGING CT_JSONP[].
  " apre tag errors
  PERFORM JSONP_OPEN_ERRORS CHANGING CT_JSONP[].
  " costruzione errors
  PERFORM JSONP_BUILD_ERRORS USING    IT_MESSAGES[]
                             CHANGING CT_JSONP[].
  " chiude tag errors
  PERFORM JSONP_CLOSE_ERRORS CHANGING CT_JSONP[].
  " apre tag results
  PERFORM JSONP_OPEN_RESULTS CHANGING CT_JSONP[].
  " chiude tag results
  PERFORM JSONP_CLOSE_RESULTS CHANGING CT_JSONP[].
  " chiude tab funzione di callback
  PERFORM JSONP_CLOSE_CALLBACK USING IV_CALLBACK
                               CHANGING CT_JSONP[].

ENDFORM.                    " CREATE_JSONP_2

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_COLUMNS
*&---------------------------------------------------------------------*
FORM JSONP_OPEN_COLUMNS  CHANGING    CT_JSONP     TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

  LS_JSONP = '"columns": ['.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_OPEN_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_COLUMNS
*&---------------------------------------------------------------------*
FORM JSONP_CLOSE_COLUMNS    CHANGING CT_JSONP TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML.

  LS_JSONP = '],'.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_CLOSE_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  JSONP_BUILD_COLUMNS
*&---------------------------------------------------------------------*
FORM JSONP_BUILD_COLUMNS  USING VALUE(IT_COMPONENTS) TYPE ABAP_COMPDESCR_TAB
                                VALUE(IT_FIELDS)     TYPE TABLE
                          CHANGING    CT_JSONP       TYPE TY_T_W3HTML.

  FIELD-SYMBOLS: <VALUE>   TYPE ANY,
                 <LS_DATA> TYPE ANY.

  DATA: LS_JSONP      TYPE W3HTML,
        LS_FIELDS     TYPE STRING,
        LS_COMPONENTS TYPE ABAP_COMPDESCR,
        LV_LINES      TYPE I,
        LV_TABIX      TYPE I,
        LV_COMMA      TYPE BOOL.

  DESCRIBE TABLE IT_FIELDS LINES LV_LINES.

  LOOP AT IT_FIELDS INTO LS_FIELDS.
    LV_TABIX = SY-TABIX.
    IF ( LS_FIELDS <> '*' ).
      CLEAR: LV_COMMA.
      IF ( LV_TABIX < LV_LINES ).
        LV_COMMA = 'X'.
      ENDIF.
      READ TABLE IT_COMPONENTS INTO LS_COMPONENTS WITH KEY NAME = LS_FIELDS.
      IF ( SY-SUBRC = 0 ).
        PERFORM JSONP_ADD_ROW_COLUMNS USING     LS_COMPONENTS
                                                'X'  "lv_comma
                                      CHANGING  CT_JSONP[].
      ENDIF.
    ELSE.
      DESCRIBE TABLE IT_COMPONENTS LINES LV_LINES.
      LOOP AT IT_COMPONENTS INTO LS_COMPONENTS.
        LV_TABIX = SY-TABIX.
        CLEAR: LV_COMMA.
        IF ( LV_TABIX < LV_LINES ).
          LV_COMMA = 'X'.
        ENDIF.
        PERFORM JSONP_ADD_ROW_COLUMNS USING     LS_COMPONENTS
                                                'X'  "lv_comma
                                      CHANGING  CT_JSONP[].
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " JSONP_BUILD_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  jsonp_add_row_results
*&---------------------------------------------------------------------*
FORM JSONP_ADD_ROW_RESULTS  USING VALUE(IS_DATA)       TYPE ANY
                                  VALUE(IS_COMPONENTS) TYPE ABAP_COMPDESCR
                                  VALUE(IV_COMMA)      TYPE BOOL
                            CHANGING    CT_JSONP       TYPE TY_T_W3HTML.

  FIELD-SYMBOLS: <VALUE>   TYPE ANY.

  DATA: LS_JSONP      TYPE W3HTML,
        LV_STR_NAME   TYPE STRING,
        LV_STR_VAL    TYPE STRING,
        LV_FIELD      TYPE C LENGTH 50.

  CLEAR: LS_JSONP-LINE.

  LV_STR_NAME = IS_COMPONENTS-NAME.
  CONCATENATE '"' LV_STR_NAME '"' INTO LV_STR_NAME.

  LV_FIELD = 'is_data-'.
  CONCATENATE LV_FIELD IS_COMPONENTS-NAME INTO LV_FIELD.
  ASSIGN (LV_FIELD) TO <VALUE>.
  LV_STR_VAL = <VALUE>.
  CONCATENATE '"' LV_STR_VAL '"' INTO LV_STR_VAL.

*  CONCATENATE '{"name": ' lv_str_name ', "value": ' lv_str_val '}' INTO ls_jsonp-line.
  CONCATENATE LV_STR_NAME ': ' LV_STR_VAL INTO LS_JSONP-LINE.
  IF ( NOT IV_COMMA IS INITIAL ).
    CONCATENATE LS_JSONP ',' INTO LS_JSONP.
  ENDIF.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " jsonp_add_row_results

*&---------------------------------------------------------------------*
*&      Form  JSONP_ADD_ROW_COLUMNS
*&---------------------------------------------------------------------*
FORM JSONP_ADD_ROW_COLUMNS  USING VALUE(IS_COMPONENTS) TYPE ABAP_COMPDESCR
                                  VALUE(IV_COMMA)      TYPE BOOL
                            CHANGING    CT_JSONP       TYPE TY_T_W3HTML.

  DATA: LS_JSONP      TYPE W3HTML.

  CLEAR: LS_JSONP-LINE.

  CONCATENATE '{ "column": "' IS_COMPONENTS-NAME '" }' INTO LS_JSONP-LINE.
  IF ( NOT IV_COMMA IS INITIAL ).
    CONCATENATE LS_JSONP ',' INTO LS_JSONP.
  ENDIF.
  INSERT LS_JSONP INTO TABLE CT_JSONP.

ENDFORM.                    " JSONP_ADD_ROW_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  create_table
*&---------------------------------------------------------------------*
FORM CREATE_TABLE USING VALUE(IV_FIELD) TYPE C
                  CHANGING    CT_DATA   TYPE REF TO DATA.

  DATA: LO_STRUCT_DESCR   TYPE REF TO CL_ABAP_STRUCTDESCR,
        LO_TABLE_DESCR    TYPE REF TO CL_ABAP_TABLEDESCR,
        LT_KEYS           TYPE ABAP_KEYDESCR_TAB,
        LT_TABLE          TYPE REF TO DATA,
        LS_TABLE          TYPE REF TO DATA.

  FIELD-SYMBOLS: <LS_DATA> TYPE ANY.
  FIELD-SYMBOLS: <LT_DATA> TYPE ANY TABLE.
  TYPES: BEGIN OF TY_S_WORK,
           BUFFER(30000),
          END OF TY_S_WORK,
          TY_T_WORK TYPE TABLE OF TY_S_WORK.

  TRY.
      LO_STRUCT_DESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_NAME( IV_FIELD ).

      CREATE DATA LS_TABLE TYPE HANDLE LO_STRUCT_DESCR.
      ASSIGN LS_TABLE->* TO <LS_DATA>.

      LO_TABLE_DESCR ?= CL_ABAP_TABLEDESCR=>CREATE( P_LINE_TYPE  = LO_STRUCT_DESCR
                                                    P_TABLE_KIND = CL_ABAP_TABLEDESCR=>TABLEKIND_HASHED
                                                    P_UNIQUE     = ABAP_TRUE
                                                    P_KEY        = LT_KEYS
                                                    P_KEY_KIND   = CL_ABAP_TABLEDESCR=>KEYDEFKIND_DEFAULT ).

      CREATE DATA LT_TABLE TYPE HANDLE LO_TABLE_DESCR.
      ASSIGN LT_TABLE->* TO <LT_DATA>.

    CATCH CX_ROOT.
  ENDTRY.

  CT_DATA = LT_TABLE.

ENDFORM.                    " create_table

*&---------------------------------------------------------------------*
*&      Form  JSON_ADD_RESULT
*&---------------------------------------------------------------------*
FORM JSON_ADD_RESULT  USING VALUE(IT_PARAMS) TYPE ABAP_FUNC_PARMBIND_TAB
                      CHANGING    CT_JSONP   TYPE TY_T_W3HTML.

*  DATA: ls_params    TYPE abap_func_parmbind,
*        ls_interface TYPE rsfbintfv,
*        lv_sname     TYPE string.
*
*  LOOP AT it_params INTO ls_params.
*    READ TABLE ls_interface-tables WITH KEY parameter = ls_params-name TRANSPORTING NO FIELDS.
*    IF ( sy-subrc = 0 ).
*      lv_sname = ls_params-name.
*      TRY.
*          ASSIGN ls_params-value->* TO <lt_data>.
*          LOOP AT <lt_data> ASSIGNING <ls_data>.
*            EXIT.
*          ENDLOOP.
*          lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
*        CATCH cx_root.
*      ENDTRY.
*
*      DATA: lv_json TYPE string.
*      lv_json = cl_fdt_json=>data_to_json( ia_data = <lt_data> ).
*      lv_json = '{"' && ls_params-name && '":' && lv_json && '}'.
*      DATA: lv_len      TYPE i VALUE 255,
*            lv_json_len TYPE i,
*            lv_times    TYPE i,
*            lv_start    TYPE i,
*            lv_end      TYPE i,
*            lv_finish   TYPE abap_bool.
*      lv_json_len = strlen( lv_json ).
*      lv_times = lv_json_len / lv_len + 1.
*      lv_start = 0.
*      lv_end = lv_len.
*      lv_finish = abap_false.
*      DO lv_times TIMES.
*        DATA: lv_str  TYPE string.
*        lv_str = substring( val = lv_json off = lv_start len = lv_len )..
*        html = lv_str.
*        APPEND html.
*        IF ( lv_finish = abap_true ).
*          EXIT.
*        ENDIF.
*        IF ( lv_end < lv_json_len - lv_len ).
*          ADD lv_len TO lv_start.
*          ADD lv_len TO lv_end.
*        ELSE.
*          ADD lv_len TO lv_start.
*          lv_end  = lv_json_len.
*          lv_finish = abap_true.   " termina ciclo all'iterazione successiva
*          lv_len = lv_end - lv_start.
*        ENDIF.
*      ENDDO.
*      CONTINUE.
*
*      " apre tag funcione di callback
*      PERFORM jsonp_open_callback USING    iv_callback
*                                  CHANGING html[].
*
** columns ------------------------------------------------------
*      " apre tag columns
*      PERFORM jsonp_open_columns CHANGING html[].
*      " costruisce columns
*      PERFORM jsonp_build_columns USING    lo_struct_descr->components[]
*                                           lt_fields[]
*                                  CHANGING html[].
*      " chiude tag columns
*      PERFORM jsonp_close_columns CHANGING html[].
*
** results ------------------------------------------------------
*      " apre tag results
*      PERFORM jsonp_open_results CHANGING html[].
*      " costruisce results
*      PERFORM jsonp_build_results USING    lv_sname
*                                           <lt_data>[]
*                                           lo_struct_descr->components[]
*                                           lt_fields[]
*                                  CHANGING html[].
*      " chiude tag results
*      PERFORM jsonp_close_results CHANGING html[].
*
*      " apre tag funcione di callback
*      PERFORM jsonp_close_callback USING    iv_callback
*                                   CHANGING html[].
*
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " JSON_ADD_RESULT

*&---------------------------------------------------------------------*
*&      Form  CREATE_PARAMETER
*&---------------------------------------------------------------------*
FORM CREATE_PARAMETER USING VALUE(IV_FIELD) TYPE C
                            VALUE(IV_VALUE) TYPE ANY
                      CHANGING    CV_PARAM  TYPE REF TO DATA.

  DATA: LX_ROOT           TYPE REF TO CX_ROOT,
        LT_KEYS           TYPE ABAP_KEYDESCR_TAB,
        LV_PARAM          TYPE REF TO DATA.

  FIELD-SYMBOLS: <LV_PARAM> TYPE ANY.

  TRY.
      TRY.
          CREATE DATA LV_PARAM TYPE (IV_FIELD).
        CATCH CX_SY_CREATE_DATA_ERROR INTO LX_ROOT.
          CREATE DATA LV_PARAM LIKE IV_FIELD.
      ENDTRY.
      ASSIGN LV_PARAM->* TO <LV_PARAM>.

      IF ( NOT IV_VALUE IS INITIAL ).
        <LV_PARAM> = IV_VALUE.
      ENDIF.

    CATCH CX_ROOT INTO LX_ROOT.
  ENDTRY.

  CV_PARAM = LV_PARAM.

ENDFORM.                    " CREATE_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  CREATE_JSON_FROM_DATA
*&---------------------------------------------------------------------*
FORM JSON_CREATE_FROM_DATA  USING VALUE(IV_CALLBACK) TYPE STRING
                                  VALUE(IV_NAME) TYPE ABAP_PARMNAME
                                  VALUE(IV_DATA) TYPE ANY
                            CHANGING    CT_JSONP TYPE TY_T_W3HTML.

  DATA: LV_JSON     TYPE STRING,
        LS_JSONP    TYPE W3HTML,
        LV_LEN      TYPE I VALUE 200, "255,
        LV_LEN_TMP  TYPE I,
        LV_JSON_LEN TYPE I,
        LV_TIMES    TYPE I,
        LV_START    TYPE I,
        LV_END      TYPE I,
        LV_PTR      TYPE I,
        LV_FINISH   TYPE ABAP_BOOL.

  LV_JSON = CL_FDT_JSON=>DATA_TO_JSON( IA_DATA = IV_DATA ).

*  DATA: LO_JSON TYPE REF TO CL_IAC_JSON_STRING.
*  CREATE OBJECT LO_JSON
*    EXPORTING
*      P_VALUE = LV_JSON.
*  LO_JSON->IF_IAC_JSON~GET_STRING_TABLE( CHANGING P_STRING_TABLE = CT_JSONP ).

  IF ( LV_JSON CS '$ROOT' ).
*    LV_JSON = '{"' && IV_NAME && '":"' && IV_DATA && '"}'.
    LV_JSON = '"' && IV_NAME && '":"' && IV_DATA && '",'.
    LS_JSONP = LV_JSON.
    APPEND LS_JSONP TO CT_JSONP.
    RETURN.
  ENDIF.
*  CL_FDT_JSON=>JSON_TO_DATA( EXPORTING IV_JSON = LV_JSON
*                             CHANGING  CA_DATA = CT_JSONP ).
*  LV_JSON = '{"' && IV_NAME && '":' && LV_JSON && '}'.
  LV_JSON = '"' && IV_NAME && '":' && LV_JSON && ','.
*  SPLIT LV_JSON AT ',' INTO TABLE CT_JSONP.
  LV_JSON_LEN = STRLEN( LV_JSON ).
  IF ( LV_LEN > LV_JSON_LEN ).
    LV_LEN = LV_JSON_LEN.
  ENDIF.
  LV_TIMES = LV_JSON_LEN / LV_LEN + 1.
  LV_START = 0.
  LV_END = LV_LEN.
  LV_LEN_TMP = LV_LEN.
  LV_FINISH = ABAP_FALSE.
  DO LV_TIMES TIMES.
    DATA: LV_STR  TYPE STRING.
    LV_STR = SUBSTRING( VAL = LV_JSON OFF = LV_START LEN = LV_LEN_TMP ).
    LV_PTR = STRLEN( LV_STR ) - 1.
*    LV_LEN_TMP = STRLEN( LV_STR ).
*    DO LV_PTR TIMES.
*      IF ( LV_STR+LV_PTR(1) = ',' ).
*        LV_LEN_TMP = LV_LEN_TMP - ( LV_LEN_TMP - LV_PTR ) + 1.
*        EXIT.
*      ENDIF.
*      LV_PTR = LV_PTR - 1.
*    ENDDO.
    LS_JSONP = LV_STR(LV_LEN_TMP).
    APPEND LS_JSONP TO CT_JSONP.
    IF ( LV_FINISH = ABAP_TRUE ).
      EXIT.
    ENDIF.
    IF ( LV_END < LV_JSON_LEN - LV_LEN_TMP ).
      ADD LV_LEN_TMP TO LV_START.
      ADD LV_LEN_TMP TO LV_END.
    ELSE.
      ADD LV_LEN_TMP TO LV_START.
      LV_END  = LV_JSON_LEN.
      IF ( LV_START = LV_END ).
        EXIT.
      ENDIF.
      LV_FINISH = ABAP_TRUE.   " termina ciclo all'iterazione successiva
      LV_LEN_TMP = LV_END - LV_START.
    ENDIF.
  ENDDO.

ENDFORM.                    " JSON_CREATE_FROM_DATA

*&---------------------------------------------------------------------*
*&      Form  JSONP_REMOVE_EXTRA_COMMA
*&---------------------------------------------------------------------*
FORM JSONP_REMOVE_EXTRA_COMMA  CHANGING CT_JSONP TYPE TY_T_W3HTML.

  DATA: LS_JSONP TYPE W3HTML,
        LV_LEN   TYPE I,
        LV_LINES TYPE I.

  DESCRIBE TABLE CT_JSONP LINES LV_LINES.
  IF ( LV_LINES > 0 ).
    READ TABLE CT_JSONP INTO LS_JSONP INDEX LV_LINES.
    LV_LEN = STRLEN( LS_JSONP ) - 1.
    IF ( LS_JSONP+LV_LEN(1) = ',' ).
      LS_JSONP+LV_LEN(1) = ''.
    ENDIF.
    MODIFY CT_JSONP FROM LS_JSONP INDEX LV_LINES.
  ENDIF.

ENDFORM.                    " JSONP_REMOVE_EXTRA_COMMA
