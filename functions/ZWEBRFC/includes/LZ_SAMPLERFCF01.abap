*----------------------------------------------------------------------*
***INCLUDE LZ_SAMPLERFCF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CREATE_JSONP
*&---------------------------------------------------------------------*
FORM create_jsonp  USING value(iv_callback)   TYPE string
                         value(it_data)       TYPE ANY TABLE
                         value(it_components) TYPE abap_compdescr_tab
                         value(it_fields)     TYPE table
                         value(it_messages)   TYPE ty_t_messages
*                         VALUE(IV_ROWS)       TYPE STRING
                         value(iv_from_rec)   TYPE i
                         value(iv_to_rec)     TYPE i
                   CHANGING    ct_jsonp       TYPE ty_t_w3html.


  DATA: htmldoc       TYPE w3html,
        ls_fields     TYPE string,
        lv_field      TYPE c LENGTH 50,
        ls_components TYPE abap_compdescr,
        lv_sname      TYPE string.

  FIELD-SYMBOLS: <ls_data> TYPE any.

  CLEAR: lv_sname.

  " apre tag funcione di callback
  PERFORM jsonp_open_callback USING    iv_callback
                              CHANGING ct_jsonp[].

* errors -------------------------------------------------------
  " apre tag errors
  PERFORM jsonp_open_errors CHANGING ct_jsonp[].
  " costruzione errors
  PERFORM jsonp_build_errors USING    it_messages[]
                             CHANGING ct_jsonp[].
  " chiude tag errors
  PERFORM jsonp_close_errors CHANGING ct_jsonp[].

* columns ------------------------------------------------------
  " apre tag columns
  PERFORM jsonp_open_columns CHANGING ct_jsonp[].
  " costruisce columns
  PERFORM jsonp_build_columns USING    it_components[]
                                       it_fields[]
                              CHANGING ct_jsonp[].
  " chiude tag columns
  PERFORM jsonp_close_columns CHANGING ct_jsonp[].

* results ------------------------------------------------------
  " apre tag results
  PERFORM jsonp_open_results CHANGING ct_jsonp[].
  " costruisce results
  PERFORM jsonp_build_results USING    lv_sname
                                       it_data[]
*                                       it_components[]
                                       it_fields[]
*                                       IV_ROWS
                                       iv_from_rec
                                       iv_to_rec
                              CHANGING ct_jsonp[].
  " chiude tag results
  PERFORM jsonp_close_results CHANGING ct_jsonp[].

  " chiude tab funzione di callback
  PERFORM jsonp_close_callback USING iv_callback
                               CHANGING ct_jsonp[].

ENDFORM.                    " CREATE_JSONP

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_CALLBACK
*&---------------------------------------------------------------------*
FORM jsonp_open_callback  USING value(iv_callback) TYPE string
                          CHANGING    ct_jsonp     TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

  CLEAR: ls_jsonp.
  IF ( NOT iv_callback IS INITIAL ).
    ls_jsonp-line = iv_callback && '({'.
  ELSE.
    ls_jsonp-line = '{'.
  ENDIF.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_OPEN_CALLBACK

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_ERRORS
*&---------------------------------------------------------------------*
FORM jsonp_open_errors  CHANGING    ct_jsonp     TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

  ls_jsonp = '"errors": ['.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_OPEN_ERRORS

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_RESULTS
*&---------------------------------------------------------------------*
FORM jsonp_open_results  CHANGING    ct_jsonp     TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

*  ls_jsonp = '{"results": ['.
*  LS_JSONP = '"results": ['.
  ls_jsonp = '"results": {'.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_OPEN_RESULTS

*&---------------------------------------------------------------------*
*&      Form  JSONP_BUILD_RESULTS
*&---------------------------------------------------------------------*
FORM jsonp_build_results  USING value(iv_sname)      TYPE string
                                value(iv_data)       TYPE any
*                                value(it_components) TYPE abap_compdescr_tab
                                value(it_fields)     TYPE table
*                                VALUE(IV_ROWS)       TYPE STRING
                                value(iv_from_rec)   TYPE i
                                value(iv_to_rec)     TYPE i
                          CHANGING    ct_jsonp       TYPE ty_t_w3html.

  FIELD-SYMBOLS: <value>   TYPE any,
                 <ls_data> TYPE any.

  DATA: lt_jsonp      TYPE ty_t_w3html,
        ls_jsonp      TYPE w3html,
        ls_fields     TYPE string,
        lv_field      TYPE c LENGTH 50,
        lv_str_name   TYPE string,
        lv_str_val    TYPE string,
        ls_components TYPE abap_compdescr,
        lv_lines      TYPE i,
        lv_tabix      TYPE i,
        lv_comma      TYPE bool.

  DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
        lo_table_descr  TYPE REF TO cl_abap_tabledescr,
        lo_data_descr   TYPE REF TO cl_abap_datadescr,
        lx_root         TYPE REF TO cx_root.

  TRY.
      lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( iv_data ).

      FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.
      ASSIGN iv_data TO <lt_data>.

      IF ( NOT iv_sname IS INITIAL ).
        ls_jsonp-line = '"' && iv_sname && '": ['.
        INSERT ls_jsonp INTO TABLE ct_jsonp.
      ENDIF.

      DATA: lv_count TYPE i.
      lv_count = 1.
*      LOOP AT IT_DATA ASSIGNING <LS_DATA>.
      LOOP AT <lt_data> ASSIGNING <ls_data>.
        lv_tabix = sy-tabix.
        lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
*        CHECK LV_COUNT <= IV_ROWS.
        CHECK lv_count BETWEEN iv_from_rec AND iv_to_rec.
*    LS_JSONP = '"item": ['.
        ls_jsonp = '{'.
        INSERT ls_jsonp INTO TABLE ct_jsonp.

        DESCRIBE TABLE it_fields LINES lv_lines.

        REFRESH: lt_jsonp.
        LOOP AT it_fields INTO ls_fields.
          lv_tabix = sy-tabix.
          IF ( ls_fields <> '*' ).
            CLEAR: lv_comma.
            IF ( lv_tabix < lv_lines ).
              lv_comma = 'X'.
            ENDIF.
*            READ TABLE it_components INTO ls_components WITH KEY name = ls_fields.
            READ TABLE lo_struct_descr->components INTO ls_components WITH KEY name = ls_fields.
            IF ( sy-subrc = 0 ).
              PERFORM jsonp_add_row_results USING     <ls_data>
                                                      ls_components
                                                      'X'  "lv_comma
*                                            CHANGING  CT_JSONP[].
                                            CHANGING  lt_jsonp[].
            ENDIF.
          ELSE.
*            REFRESH: LT_JSONP.
*            LOOP AT it_components INTO ls_components.
            LOOP AT lo_struct_descr->components INTO ls_components.
              lv_tabix = sy-tabix.
              CLEAR: lv_comma.
              IF ( lv_tabix < lv_lines ).
                lv_comma = 'X'.
              ENDIF.
              PERFORM jsonp_add_row_results USING     <ls_data>
                                                      ls_components
                                                      'X'   "lv_comma
*                                            CHANGING  CT_JSONP[].
                                            CHANGING  lt_jsonp[].
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
        DATA: lv_str       TYPE string,
              ls_jsonp_tmp TYPE w3html.
        lv_str = ''.
        LOOP AT lt_jsonp INTO ls_jsonp_tmp.
          IF ( strlen( lv_str ) > 200 ).
            ls_jsonp-line = lv_str.
            APPEND ls_jsonp TO ct_jsonp.
            lv_str = ''.
          ENDIF.
          lv_str = lv_str && ls_jsonp_tmp-line.
        ENDLOOP.
        IF ( strlen( lv_str ) > 0 ).
          ls_jsonp-line = lv_str.
          APPEND ls_jsonp TO ct_jsonp.
        ENDIF.
        PERFORM jsonp_remove_extra_comma CHANGING ct_jsonp[].

        ls_jsonp = '},'.
        INSERT ls_jsonp INTO TABLE ct_jsonp.
        ADD 1 TO lv_count.
      ENDLOOP.

      PERFORM jsonp_remove_extra_comma CHANGING ct_jsonp[].

      IF ( NOT iv_sname IS INITIAL ).
        ls_jsonp = '],'.
        INSERT ls_jsonp INTO TABLE ct_jsonp.
      ENDIF.
    CATCH cx_root.
      TRY.
          lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( iv_data ).

          IF ( NOT iv_sname IS INITIAL ).
            ls_jsonp-line = '"' && iv_sname && '": {'.
            INSERT ls_jsonp INTO TABLE ct_jsonp.
          ENDIF.

*          LS_JSONP = '{'.
*          INSERT LS_JSONP INTO TABLE CT_JSONP.
          LOOP AT lo_struct_descr->components INTO ls_components.
            lv_tabix = sy-tabix.
            CLEAR: lv_comma.
            IF ( lv_tabix < lv_lines ).
              lv_comma = 'X'.
            ENDIF.
            PERFORM jsonp_add_row_results USING     iv_data
                                                    ls_components
                                                    'X'   "lv_comma
                                          CHANGING  ct_jsonp[].
          ENDLOOP.

          PERFORM jsonp_remove_extra_comma CHANGING ct_jsonp[].

          IF ( NOT iv_sname IS INITIAL ).
            ls_jsonp = '},'.
            INSERT ls_jsonp INTO TABLE ct_jsonp.
          ENDIF.
        CATCH cx_root.
          TRY.
              lo_data_descr ?= cl_abap_datadescr=>describe_by_data( iv_data ).
            CATCH cx_root.
          ENDTRY.
      ENDTRY.
  ENDTRY.

ENDFORM.                    " JSONP_BUILD_RESULTS

*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_ERRORS
*&---------------------------------------------------------------------*
FORM jsonp_close_errors  CHANGING ct_jsonp TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

  ls_jsonp = '],'.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_CLOSE_ERRORS

*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_RESULTS
*&---------------------------------------------------------------------*
FORM jsonp_close_results  CHANGING ct_jsonp TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

  PERFORM jsonp_remove_extra_comma CHANGING ct_jsonp[].

*  LS_JSONP = ']}'.
  ls_jsonp = '}'.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_CLOSE_RESULTS*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_CALLBACK
*&---------------------------------------------------------------------*
FORM jsonp_close_callback  USING value(iv_callback) TYPE string
                           CHANGING ct_jsonp TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

  PERFORM jsonp_remove_extra_comma CHANGING ct_jsonp[].

  IF ( NOT iv_callback IS INITIAL ).
    ls_jsonp = '});'.
  ELSE.
    ls_jsonp = '}'.
  ENDIF.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_CLOSE_CALLBACK
*&---------------------------------------------------------------------*
*&      Form  ADD_MESSAGE
*&---------------------------------------------------------------------*
FORM add_message  USING value(iv_type)    TYPE ty_s_messages-type
                        value(iv_msg)     TYPE ty_s_messages-msg
                  CHANGING    ct_messages TYPE ty_t_messages.

  DATA: ls_messages TYPE ty_s_messages.

  CLEAR: ls_messages.
  ls_messages-type = iv_type.
  ls_messages-msg  = iv_msg.

  APPEND ls_messages TO ct_messages.

ENDFORM.                    " ADD_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  JSONP_BUILD_ERRORS
*&---------------------------------------------------------------------*
FORM jsonp_build_errors  USING value(it_messages) TYPE ty_t_messages
                         CHANGING    ct_jsonp     TYPE ty_t_w3html.

  DATA: ls_messages TYPE ty_s_messages,
        ls_jsonp    TYPE w3html,
        lv_lines    TYPE i,
        lv_tabix    TYPE i.

  DESCRIBE TABLE it_messages LINES lv_lines.

  LOOP AT it_messages INTO ls_messages.
    lv_tabix = sy-tabix.
    CLEAR: ls_jsonp.
    CONCATENATE '{ "type":"' ls_messages-type '", "msg":"' ls_messages-msg '" }' INTO ls_jsonp.
    IF ( lv_tabix < lv_lines ).
      CONCATENATE ls_jsonp ',' INTO ls_jsonp.
    ENDIF.
    APPEND ls_jsonp TO ct_jsonp.
  ENDLOOP.

ENDFORM.                    " JSONP_BUILD_ERRORS

*&---------------------------------------------------------------------*
*&      Form  CREATE_JSONP_2
*&---------------------------------------------------------------------*
FORM create_jsonp_2 USING value(iv_callback)   TYPE string
                          value(it_messages)   TYPE ty_t_messages
                    CHANGING    ct_jsonp       TYPE ty_t_w3html.


  DATA: htmldoc       TYPE w3html.

  " apre tag funcione di callback
  PERFORM jsonp_open_callback USING    iv_callback
                              CHANGING ct_jsonp[].
  " apre tag errors
  PERFORM jsonp_open_errors CHANGING ct_jsonp[].
  " costruzione errors
  PERFORM jsonp_build_errors USING    it_messages[]
                             CHANGING ct_jsonp[].
  " chiude tag errors
  PERFORM jsonp_close_errors CHANGING ct_jsonp[].
  " apre tag results
  PERFORM jsonp_open_results CHANGING ct_jsonp[].
  " chiude tag results
  PERFORM jsonp_close_results CHANGING ct_jsonp[].
  " chiude tab funzione di callback
  PERFORM jsonp_close_callback USING iv_callback
                               CHANGING ct_jsonp[].

ENDFORM.                    " CREATE_JSONP_2

*&---------------------------------------------------------------------*
*&      Form  JSONP_OPEN_COLUMNS
*&---------------------------------------------------------------------*
FORM jsonp_open_columns  CHANGING    ct_jsonp     TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

  ls_jsonp = '"columns": ['.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_OPEN_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  JSONP_CLOSE_COLUMNS
*&---------------------------------------------------------------------*
FORM jsonp_close_columns    CHANGING ct_jsonp TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html.

  ls_jsonp = '],'.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_CLOSE_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  JSONP_BUILD_COLUMNS
*&---------------------------------------------------------------------*
FORM jsonp_build_columns  USING value(it_components) TYPE abap_compdescr_tab
                                value(it_fields)     TYPE table
                          CHANGING    ct_jsonp       TYPE ty_t_w3html.

  FIELD-SYMBOLS: <value>   TYPE any,
                 <ls_data> TYPE any.

  DATA: ls_jsonp      TYPE w3html,
        ls_fields     TYPE string,
        ls_components TYPE abap_compdescr,
        lv_lines      TYPE i,
        lv_tabix      TYPE i,
        lv_comma      TYPE bool.

  DESCRIBE TABLE it_fields LINES lv_lines.

  LOOP AT it_fields INTO ls_fields.
    lv_tabix = sy-tabix.
    IF ( ls_fields <> '*' ).
      CLEAR: lv_comma.
      IF ( lv_tabix < lv_lines ).
        lv_comma = 'X'.
      ENDIF.
      READ TABLE it_components INTO ls_components WITH KEY name = ls_fields.
      IF ( sy-subrc = 0 ).
        PERFORM jsonp_add_row_columns USING     ls_components
                                                'X'  "lv_comma
                                      CHANGING  ct_jsonp[].
      ENDIF.
    ELSE.
      DESCRIBE TABLE it_components LINES lv_lines.
      LOOP AT it_components INTO ls_components.
        lv_tabix = sy-tabix.
        CLEAR: lv_comma.
        IF ( lv_tabix < lv_lines ).
          lv_comma = 'X'.
        ENDIF.
        PERFORM jsonp_add_row_columns USING     ls_components
                                                'X'  "lv_comma
                                      CHANGING  ct_jsonp[].
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " JSONP_BUILD_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  jsonp_add_row_results
*&---------------------------------------------------------------------*
FORM jsonp_add_row_results  USING value(is_data)       TYPE any
                                  value(is_components) TYPE abap_compdescr
                                  value(iv_comma)      TYPE bool
                            CHANGING    ct_jsonp       TYPE ty_t_w3html.

  FIELD-SYMBOLS: <value>   TYPE any.

  DATA: ls_jsonp      TYPE w3html,
        lv_str_name   TYPE string,
        lv_str_val    TYPE string,
        lv_field      TYPE c LENGTH 50.

  CLEAR: ls_jsonp-line, lv_str_val.

  lv_str_name = is_components-name.
  TRANSLATE lv_str_name TO LOWER CASE.
  CONCATENATE '"' lv_str_name '"' INTO lv_str_name.

  lv_field = 'is_data-'.
  CONCATENATE lv_field is_components-name INTO lv_field.
  ASSIGN (lv_field) TO <value>.
*  LV_STR_VAL = <VALUE>.
  zcl_bc_conversion_exit=>conversion_output( EXPORTING iv_field = <value>
                                             CHANGING  cv_field = lv_str_val ).
  CONCATENATE '"' lv_str_val '"' INTO lv_str_val.

*  CONCATENATE '{"name": ' lv_str_name ', "value": ' lv_str_val '}' INTO ls_jsonp-line.
  CONCATENATE lv_str_name ': ' lv_str_val INTO ls_jsonp-line.
  IF ( NOT iv_comma IS INITIAL ).
    CONCATENATE ls_jsonp ',' INTO ls_jsonp.
  ENDIF.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " jsonp_add_row_results

*&---------------------------------------------------------------------*
*&      Form  JSONP_ADD_ROW_COLUMNS
*&---------------------------------------------------------------------*
FORM jsonp_add_row_columns  USING value(is_components) TYPE abap_compdescr
                                  value(iv_comma)      TYPE bool
                            CHANGING    ct_jsonp       TYPE ty_t_w3html.

  DATA: ls_jsonp      TYPE w3html.

  CLEAR: ls_jsonp-line.

  CONCATENATE '{ "column": "' is_components-name '" }' INTO ls_jsonp-line.
  IF ( NOT iv_comma IS INITIAL ).
    CONCATENATE ls_jsonp ',' INTO ls_jsonp.
  ENDIF.
  INSERT ls_jsonp INTO TABLE ct_jsonp.

ENDFORM.                    " JSONP_ADD_ROW_COLUMNS

*&---------------------------------------------------------------------*
*&      Form  create_table
*&---------------------------------------------------------------------*
FORM create_table USING value(iv_field) TYPE c
                        value(iv_value) TYPE any
                  CHANGING    ct_data   TYPE REF TO data.

  DATA: lo_struct_descr   TYPE REF TO cl_abap_structdescr,
        lo_table_descr    TYPE REF TO cl_abap_tabledescr,
        lt_keys           TYPE abap_keydescr_tab,
        lt_table          TYPE REF TO data,
        ls_table          TYPE REF TO data.

  FIELD-SYMBOLS: <ls_data> TYPE any.
  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.
  TYPES: BEGIN OF ty_s_work,
           buffer(30000),
          END OF ty_s_work,
          ty_t_work TYPE TABLE OF ty_s_work.

  TRY.
      lo_struct_descr ?= cl_abap_structdescr=>describe_by_name( iv_field ).

      CREATE DATA ls_table TYPE HANDLE lo_struct_descr.
      ASSIGN ls_table->* TO <ls_data>.

      lo_table_descr ?= cl_abap_tabledescr=>create( p_line_type  = lo_struct_descr
                                                    p_table_kind = cl_abap_tabledescr=>tablekind_hashed
                                                    p_unique     = abap_true
                                                    p_key        = lt_keys
                                                    p_key_kind   = cl_abap_tabledescr=>keydefkind_default ).

      CREATE DATA lt_table TYPE HANDLE lo_table_descr.
      ASSIGN lt_table->* TO <lt_data>.

    CATCH cx_root.
  ENDTRY.

  ct_data = lt_table.

ENDFORM.                    " create_table

*&---------------------------------------------------------------------*
*&      Form  JSON_ADD_RESULT
*&---------------------------------------------------------------------*
FORM json_add_result  USING value(it_params) TYPE abap_func_parmbind_tab
                      CHANGING    ct_jsonp   TYPE ty_t_w3html.

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
FORM create_parameter USING value(iv_field) TYPE c
                            value(iv_type)  TYPE c
                            value(iv_value) TYPE any
                      CHANGING    cv_param  TYPE REF TO data.

  DATA: lx_root           TYPE REF TO cx_root,
        lt_keys           TYPE abap_keydescr_tab,
        lv_param          TYPE REF TO data,
        lo_data_descr     TYPE REF TO cl_abap_datadescr,
        lo_struct_descr   TYPE REF TO cl_abap_structdescr,
        lo_table_descr    TYPE REF TO cl_abap_tabledescr,
        lo_ref_descr2     TYPE REF TO cl_abap_refdescr,
        ls_components     TYPE abap_compdescr,
        lv_value          TYPE string.

  FIELD-SYMBOLS: <lv_param> TYPE any.

  lv_value = iv_value.

  TRY.
      TRY.
          IF ( iv_type = 'TYPE REF TO' ).
            CREATE DATA lv_param TYPE REF TO (iv_field).
          ELSE.
            CREATE DATA lv_param TYPE (iv_field).
          ENDIF.
        CATCH cx_root INTO lx_root.
          CREATE DATA lv_param LIKE iv_field.
      ENDTRY.
      ASSIGN lv_param->* TO <lv_param>.
      IF ( iv_type <> 'TYPE REF TO' ).
        TRY.
            lo_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lv_param ).
            cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_value
                                       CHANGING  ca_data = <lv_param> ).
          CATCH cx_root.
            TRY.
                lo_struct_descr ?= cl_abap_structdescr=>describe_by_data_ref( lv_param ).
                cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_value
                                           CHANGING  ca_data = <lv_param> ).
              CATCH cx_root.
                TRY.
                    lo_data_descr ?= cl_abap_datadescr=>describe_by_data_ref( lv_param ).
                    IF ( NOT iv_value IS INITIAL ).
                      zcl_bc_conversion_exit=>conversion_input( EXPORTING iv_field = iv_value
                                                                CHANGING  cv_field = <lv_param> ).
                    ENDIF.
                  CATCH cx_root.
                ENDTRY.
            ENDTRY.
        ENDTRY.
      ENDIF.

    CATCH cx_root INTO lx_root.
  ENDTRY.

  cv_param = lv_param.

ENDFORM.                    " CREATE_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  CREATE_JSON_FROM_DATA
*&---------------------------------------------------------------------*
FORM json_create_from_data  USING value(iv_callback) TYPE string
                                  value(iv_name) TYPE abap_parmname
                                  value(iv_data) TYPE any
                            CHANGING    ct_jsonp TYPE ty_t_w3html.

  DATA: lv_json     TYPE string,
        ls_jsonp    TYPE w3html,
        lv_len      TYPE i VALUE 200, "255,
        lv_len_tmp  TYPE i,
        lv_json_len TYPE i,
        lv_times    TYPE i,
        lv_start    TYPE i,
        lv_end      TYPE i,
        lv_ptr      TYPE i,
        lv_finish   TYPE abap_bool.

  lv_json = cl_fdt_json=>data_to_json( ia_data = iv_data ).

*  DATA: LO_JSON TYPE REF TO CL_IAC_JSON_STRING.
*  CREATE OBJECT LO_JSON
*    EXPORTING
*      P_VALUE = LV_JSON.
*  LO_JSON->IF_IAC_JSON~GET_STRING_TABLE( CHANGING P_STRING_TABLE = CT_JSONP ).

  IF ( lv_json CS '$ROOT' ).
*    LV_JSON = '{"' && IV_NAME && '":"' && IV_DATA && '"}'.
    lv_json = '"' && iv_name && '":"' && iv_data && '",'.
    ls_jsonp = lv_json.
    APPEND ls_jsonp TO ct_jsonp.
    RETURN.
  ENDIF.
*  CL_FDT_JSON=>JSON_TO_DATA( EXPORTING IV_JSON = LV_JSON
*                             CHANGING  CA_DATA = CT_JSONP ).
*  LV_JSON = '{"' && IV_NAME && '":' && LV_JSON && '}'.
  lv_json = '"' && iv_name && '":' && lv_json && ','.
*  SPLIT LV_JSON AT ',' INTO TABLE CT_JSONP.
  lv_json_len = strlen( lv_json ).
  IF ( lv_len > lv_json_len ).
    lv_len = lv_json_len.
  ENDIF.
  lv_times = lv_json_len / lv_len + 1.
  lv_start = 0.
  lv_end = lv_len.
  lv_len_tmp = lv_len.
  lv_finish = abap_false.
  DO lv_times TIMES.
    DATA: lv_str  TYPE string.
    lv_str = substring( val = lv_json off = lv_start len = lv_len_tmp ).
    lv_ptr = strlen( lv_str ) - 1.
*    LV_LEN_TMP = STRLEN( LV_STR ).
*    DO LV_PTR TIMES.
*      IF ( LV_STR+LV_PTR(1) = ',' ).
*        LV_LEN_TMP = LV_LEN_TMP - ( LV_LEN_TMP - LV_PTR ) + 1.
*        EXIT.
*      ENDIF.
*      LV_PTR = LV_PTR - 1.
*    ENDDO.
    ls_jsonp = lv_str(lv_len_tmp).
    APPEND ls_jsonp TO ct_jsonp.
    IF ( lv_finish = abap_true ).
      EXIT.
    ENDIF.
    IF ( lv_end < lv_json_len - lv_len_tmp ).
      ADD lv_len_tmp TO lv_start.
      ADD lv_len_tmp TO lv_end.
    ELSE.
      ADD lv_len_tmp TO lv_start.
      lv_end  = lv_json_len.
      IF ( lv_start = lv_end ).
        EXIT.
      ENDIF.
      lv_finish = abap_true.   " termina ciclo all'iterazione successiva
      lv_len_tmp = lv_end - lv_start.
    ENDIF.
  ENDDO.

ENDFORM.                    " JSON_CREATE_FROM_DATA

*&---------------------------------------------------------------------*
*&      Form  JSONP_REMOVE_EXTRA_COMMA
*&---------------------------------------------------------------------*
FORM jsonp_remove_extra_comma  CHANGING ct_jsonp TYPE ty_t_w3html.

  DATA: ls_jsonp TYPE w3html,
        lv_len   TYPE i,
        lv_lines TYPE i.

  DESCRIBE TABLE ct_jsonp LINES lv_lines.
  IF ( lv_lines > 0 ).
    READ TABLE ct_jsonp INTO ls_jsonp INDEX lv_lines.
    lv_len = strlen( ls_jsonp ) - 1.
    IF ( ls_jsonp+lv_len(1) = ',' ).
      ls_jsonp+lv_len(1) = ''.
    ENDIF.
    MODIFY ct_jsonp FROM ls_jsonp INDEX lv_lines.
  ENDIF.

ENDFORM.                    " JSONP_REMOVE_EXTRA_COMMA
