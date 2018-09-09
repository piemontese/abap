class ZCL_WRFC_JSONP definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_CALLBACK type STRING
      !IS_RESULTS type ZBC_TY_S_FM_RESULTS
      !IO_FUNCTION type ref to ZCL_BC_FUNCTION_MODULE
      !IT_FIELDS type ZWRFC_TY_T_FIELDS
      !IT_MESSAGES type ZWRFC_TY_T_MESSAGES
      !IV_FROM_REC type I
      !IV_TO_REC type I
      !IV_CAMEL_CASE type STRING
      !IV_ESCAPE_URL type STRING default 'false'
      !IV_WEBRFC_USER type ZWRFC_USER default SPACE
      !IV_WEBRFC_LOG type STRING default 'false' .
  methods GET_JSON_TABLE
    returning
      value(RT_TABLE) type W3HTMLTAB .
  methods GET_JSON_STRING
    returning
      value(RV_JSON_STRING) type STRING .
  methods BUILD .
  methods BUILD_ERRORS .
  methods BUILD_INTERFACE .
  PROTECTED SECTION.
private section.

  data GV_CALLBACK type STRING .
  data GV_JSONP type STRING .
  data GT_JSONP type W3HTMLTAB .
  data GS_RESULTS type ZBC_TY_S_FM_RESULTS .
  data GO_FUNCTION type ref to ZCL_BC_FUNCTION_MODULE .
  data GT_DICTIONARY type ZWRFC_TY_T_DICTIONARY .
  data GT_FIELDS type ZWRFC_TY_T_FIELDS .
  data GV_FROM_REC type I .
  data GV_TO_REC type I .
  data GV_CAMEL_CASE type STRING .
  data GT_MESSAGES type ZWRFC_TY_T_MESSAGES .
  data GV_ESCAPE_URL type ABAP_BOOL .
  data GV_WEBRFC_USER type ZWRFC_USER .
  data GV_WEBRFC_LOG type STRING .

  methods OPEN_CALLBACK .
  methods CLOSE_CALLBACK .
  methods REMOVE_EXTRA_COMMA .
  methods OPEN_RESULTS .
  methods CLOSE_RESULTS .
  methods BUILD_RESULTS
    importing
      !IV_SNAME type ABAP_PARMNAME
      !IV_DATA type ANY
      !IT_FIELDS type TABLE
      !IV_FROM_REC type I
      !IV_TO_REC type I
    changing
      !CT_JSONP type W3HTMLTAB
      !CT_DICTIONARY type ZWRFC_TY_T_DICTIONARY .
  methods ADD_ROW_RESULTS
    importing
      !IS_DATA type ANY
      !IS_COMPONENTS type ABAP_COMPONENTDESCR
      !IV_COMMA type BOOL
    changing
      !CT_JSONP type W3HTMLTAB
      !CT_DICTIONARY type ZWRFC_TY_T_DICTIONARY .
  methods DICTIONARY .
  methods ADD_TO_DICTIONARY
    importing
      !IV_NAME type STRING
      !IV_VALUE type ANY optional
      !IV_FIELD type STRING optional
    changing
      !CT_DICTIONARY type ZWRFC_TY_T_DICTIONARY
    raising
      resumable(ZCX_BC_EXCEPTION) .
  methods CAMEL_CASE
    changing
      !CV_STR_NAME type STRING .
  methods OPEN_ERRORS .
  methods CLOSE_ERRORS .
  methods BUILD_ERRS .
  methods BUILD_SYSTEM_INFO .
  methods BUILD_USER_INFO .
  methods BUILD_WEBRFC_INFO .
  methods OPEN_KEY
    importing
      !IV_NAME type STRING .
  methods CLOSE_KEY .
ENDCLASS.



CLASS ZCL_WRFC_JSONP IMPLEMENTATION.


  METHOD add_row_results.
    FIELD-SYMBOLS: <value>   TYPE any.

    DATA: ls_jsonp    TYPE w3html,
          lv_str_name TYPE string,
          lv_name     TYPE string,
          lv_str_val  TYPE string,
          lv_field    TYPE c LENGTH 50.

    CLEAR: ls_jsonp-line, lv_str_val.

    lv_str_name = lv_name = is_components-name.
    TRANSLATE lv_str_name TO LOWER CASE.
    TRANSLATE lv_name TO LOWER CASE.
    camel_case( CHANGING cv_str_name = lv_str_name ).
    CONCATENATE '"' lv_str_name '"' INTO lv_str_name.

    lv_field = 'is_data-'.
    CONCATENATE lv_field is_components-name INTO lv_field.
    ASSIGN (lv_field) TO <value>.

    zcl_bc_conversion_exit=>conversion_output( EXPORTING iv_field = <value>
                                               CHANGING  cv_field = lv_str_val ).

    " encode uri
    IF ( gv_escape_url = 'true' ).
      cl_http_utility=>escape_url( EXPORTING unescaped = lv_str_val
                                   RECEIVING escaped = lv_str_val ).

    ENDIF.
    CONCATENATE '"' lv_str_val '"' INTO lv_str_val.

    IF ( lv_str_name = '""' ).
      ls_jsonp-line = lv_str_val.
    ELSE.
      CONCATENATE lv_str_name ': ' lv_str_val INTO ls_jsonp-line.
    ENDIF.

    IF ( NOT iv_comma IS INITIAL ).
      CONCATENATE ls_jsonp ',' INTO ls_jsonp.
    ENDIF.
    INSERT ls_jsonp INTO TABLE ct_jsonp.

    add_to_dictionary( EXPORTING iv_name = lv_name
                                 iv_value = <value>
                       changing ct_dictionary = ct_dictionary ).

  ENDMETHOD.


  METHOD add_to_dictionary.
    READ TABLE ct_dictionary WITH KEY name = iv_name TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.

    DATA: ls_dictionary TYPE zwrfc_ty_s_dictionary,
          lo_datadescr  TYPE REF TO cl_abap_datadescr,
          lo_elemdescr  TYPE REF TO cl_abap_elemdescr.

    CLEAR: ls_dictionary.

    ls_dictionary-name = iv_name.

    TRY.
*        IF ( NOT iv_value IS INITIAL ).
*          lo_datadescr ?= cl_abap_datadescr=>describe_by_data( iv_value ).
*          lo_elemdescr ?= cl_abap_elemdescr=>describe_by_data( iv_value ).
*        ELSEIF ( NOT iv_field IS INITIAL ).
*          lo_datadescr ?= cl_abap_datadescr=>describe_by_name( iv_field ).
*          lo_elemdescr ?= cl_abap_elemdescr=>describe_by_name( iv_field ).
*        ELSE.
*          RETURN.
*        ENDIF.
        lo_datadescr ?= cl_abap_datadescr=>describe_by_data( iv_value ).
        lo_elemdescr ?= cl_abap_elemdescr=>describe_by_data( iv_value ).

        ls_dictionary-length = lo_elemdescr->output_length.
        TRY.
            "DATA: ls_elem TYPE dfies.
            DATA(ls_elem) = lo_elemdescr->get_ddic_field( p_langu = sy-langu ).
            CONDENSE ls_dictionary-length.
            CASE ls_elem-inttype.
              WHEN 'C'.
                ls_dictionary-type = 'string'.
              WHEN 'I' OR 'b'.
                ls_dictionary-type = 'integer'.
            ENDCASE.
            ls_dictionary-description  = ls_elem-fieldtext.
            ls_dictionary-header_descr = ls_elem-reptext.
            ls_dictionary-small_descr  = ls_elem-scrtext_s.
            ls_dictionary-medium_descr = ls_elem-scrtext_m.
            ls_dictionary-long_descr   = ls_elem-scrtext_l.
          CATCH cx_root.
        ENDTRY.
      CATCH cx_root.
    ENDTRY.

    IF ( gv_escape_url = 'true' ).
      cl_http_utility=>escape_url( EXPORTING unescaped = ls_dictionary-length
                                   RECEIVING escaped = ls_dictionary-length ).
    ENDIF.

    IF ( ls_dictionary-description IS INITIAL OR ls_dictionary-description = '.' ).
      ls_dictionary-description = ls_dictionary-name.
    ENDIF.
    IF ( gv_escape_url = 'true' ).
      cl_http_utility=>escape_url( EXPORTING unescaped = ls_dictionary-description
                                   RECEIVING escaped = ls_dictionary-description ).
    ENDIF.

    IF ( ls_dictionary-header_descr IS INITIAL OR ls_dictionary-header_descr = '.'  ).
      ls_dictionary-header_descr = ls_dictionary-name.
    ENDIF.
    IF ( gv_escape_url = 'true' ).
      cl_http_utility=>escape_url( EXPORTING unescaped = ls_dictionary-header_descr
                                   RECEIVING escaped = ls_dictionary-header_descr ).
    ENDIF.

    IF ( ls_dictionary-small_descr IS INITIAL OR ls_dictionary-small_descr = '.'  ).
      ls_dictionary-small_descr = ls_dictionary-name.
    ENDIF.
    IF ( gv_escape_url = 'true' ).
      cl_http_utility=>escape_url( EXPORTING unescaped = ls_dictionary-small_descr
                                   RECEIVING escaped = ls_dictionary-small_descr ).
    ENDIF.

    IF ( ls_dictionary-medium_descr IS INITIAL OR ls_dictionary-medium_descr = '.'  ).
      ls_dictionary-medium_descr = ls_dictionary-name.
    ENDIF.
    IF ( gv_escape_url = 'true' ).
      cl_http_utility=>escape_url( EXPORTING unescaped = ls_dictionary-medium_descr
                                   RECEIVING escaped = ls_dictionary-medium_descr ).
    ENDIF.

    IF ( ls_dictionary-long_descr IS INITIAL OR ls_dictionary-long_descr = '.'  ).
      ls_dictionary-long_descr = ls_dictionary-name.
    ENDIF.
    IF ( gv_escape_url = 'true' ).
      cl_http_utility=>escape_url( EXPORTING unescaped = ls_dictionary-long_descr
                                   RECEIVING escaped = ls_dictionary-long_descr ).

      cl_http_utility=>escape_url( EXPORTING unescaped = ls_dictionary-name
                                   RECEIVING escaped = ls_dictionary-name ).
    ENDIF.

    IF ( NOT ls_dictionary IS INITIAL ).
      APPEND ls_dictionary TO ct_dictionary.
    ENDIF.
  ENDMETHOD.


  METHOD build.
    CLEAR: gv_jsonp, gt_jsonp, gt_dictionary.

    open_callback( ).
    open_results( ).

    DATA: ls_jsonp        TYPE w3html,
          lo_struct_descr TYPE REF TO cl_abap_structdescr.
    CLEAR: ls_jsonp.
    LOOP AT gs_results-t_params INTO DATA(ls_params).
      DATA: ls_tables TYPE rsfbpara.
      FIELD-SYMBOLS: <ls_data> TYPE any,
                     <lt_data> TYPE ANY TABLE,
                     <lv_data> TYPE any.
      READ TABLE go_function->get_interface( )-tables WITH KEY parameter = ls_params-name INTO ls_tables.
      IF ( sy-subrc = 0 ).
        DATA(lv_sname) = ls_params-name.
        TRY.
            ASSIGN ls_params-value->* TO <lt_data>.
            LOOP AT <lt_data> ASSIGNING <ls_data>.
              EXIT.
            ENDLOOP.
            IF ( sy-subrc <> 0 ).   " tabella vuota
              ls_jsonp-line = '"' && lv_sname && '": [],'.
              APPEND ls_jsonp TO gt_jsonp.
              CONTINUE.
            ENDIF.
            lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
          CATCH cx_root.
        ENDTRY.

        " costruisce results
        build_results( EXPORTING iv_sname    = lv_sname
                                 iv_data     = <lt_data>[]
                                 it_fields   = gt_fields[]
                                 iv_from_rec = gv_from_rec
                                 iv_to_rec   = gv_to_rec
                       CHANGING ct_jsonp      = gt_jsonp
                                ct_dictionary = gt_dictionary ).


      ENDIF.
      READ TABLE go_function->get_interface( )-export WITH KEY parameter = ls_params-name TRANSPORTING NO FIELDS.
      IF ( sy-subrc = 0 ).
        lv_sname = ls_params-name.
        TRY.
            ASSIGN ls_params-value->* TO <ls_data>.
            DATA: lo_table_descr TYPE REF TO cl_abap_tabledescr.
            lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( <ls_data> ).
            " costruisce results
            build_results( EXPORTING iv_sname    = lv_sname
                                     iv_data     =   <ls_data>
                                     it_fields   = gt_fields[]
                                     iv_from_rec = gv_from_rec
                                     iv_to_rec   = gv_to_rec
                           CHANGING ct_jsonp      = gt_jsonp
                                    ct_dictionary = gt_dictionary ).

          CATCH cx_root.
            TRY.
                ASSIGN ls_params-value->* TO <ls_data>.
                lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).

                build_results( EXPORTING iv_sname = lv_sname
                                         iv_data  = <ls_data>
                                         it_fields = gt_fields[]
                                         iv_from_rec = gv_from_rec
                                         iv_to_rec = gv_to_rec
                        CHANGING ct_jsonp = gt_jsonp
                                 ct_dictionary = gt_dictionary  ).
              CATCH cx_root.
                TRY.
                    ASSIGN ls_params-value->* TO <ls_data>.
                    DATA: lo_data_descr TYPE REF TO cl_abap_datadescr.
                    lo_data_descr ?= cl_abap_datadescr=>describe_by_data( <ls_data> ).

                    ls_jsonp-line = '"' && lv_sname && '":"' && <ls_data> && '",'.
                    APPEND ls_jsonp TO gt_jsonp.
                  CATCH cx_root.
                ENDTRY.
            ENDTRY.
        ENDTRY.

      ENDIF.

      READ TABLE go_function->get_interface( )-change WITH KEY parameter = ls_params-name TRANSPORTING NO FIELDS.
      IF ( sy-subrc = 0 ).
        lv_sname = ls_params-name.
        TRY.
            ASSIGN ls_params-value->* TO <ls_data>.
            lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).

            build_results( EXPORTING iv_sname = lv_sname
                                     iv_data =  <ls_data>
                                     it_fields = gt_fields[]
                                     iv_from_rec = gv_from_rec
                                     iv_to_rec = gv_to_rec
                           CHANGING ct_jsonp      = gt_jsonp
                                    ct_dictionary = gt_dictionary ).
          CATCH cx_root.
            TRY.
                ASSIGN ls_params-value->* TO <ls_data>.
                lo_data_descr ?= cl_abap_datadescr=>describe_by_data( <ls_data> ).

                ls_jsonp-line = '"' && lv_sname && '":"' && <ls_data> && '",'..
                APPEND ls_jsonp TO gt_jsonp.
              CATCH cx_root.
            ENDTRY.
        ENDTRY.

      ENDIF.

    ENDLOOP.

    close_results( ).
    build_system_info( ).
    build_user_info( ).
    build_webrfc_info( ).
    dictionary( ).
    close_callback( ).

  ENDMETHOD.


  METHOD build_errors.
    " apre tag funcione di callback
    open_callback( ).
    " apre tag errors
    open_errors( ).
    " costruzione errors
    build_errs( ).
    " chiude tag errors
    close_errors( ).
    " apre tag results
    open_results( ).
    " chiude tag results
    close_results( ).
    " chiude tab funzione di callback
    close_callback( ).
  ENDMETHOD.


  METHOD build_errs.
    DATA: ls_jsonp TYPE w3html,
          lv_lines TYPE i,
          lv_tabix TYPE i.

    DESCRIBE TABLE gt_messages LINES lv_lines.

    LOOP AT gt_messages INTO DATA(ls_messages).
      lv_tabix = sy-tabix.
      CLEAR: ls_jsonp.
      CONCATENATE '{ "type":"' ls_messages-type '", "msg":"' ls_messages-msg '" }' INTO ls_jsonp.
      IF ( lv_tabix < lv_lines ).
        CONCATENATE ls_jsonp ',' INTO ls_jsonp.
      ENDIF.
      APPEND ls_jsonp TO gt_jsonp.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_interface.
    DATA(ls_interface) =  go_function->get_interface( ).

    DATA: ls_param        TYPE rsfbpara,
          lv_name         TYPE eu_lname,
          lo_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_data> TYPE any.
    FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.

    IF ( NOT ls_interface IS INITIAL ).
      " apre tag funcione di callback
      open_callback( ).

      TRY.
          FIELD-SYMBOLS <lv_field> TYPE any.
          lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( ls_interface ).

          " apre tag results
          open_results( ).

          LOOP AT lo_struct_descr->get_components( ) INTO DATA(ls_components) WHERE name = 'IMPORT'
                                                                              OR    name = 'EXPORT'
                                                                              OR    name = 'CHANGE'
                                                                              OR    name = 'TABLES'
                                                                              OR    name = 'EXCEPT'.
            DATA(lv_field) = 'ls_interface-' && ls_components-name.
            ASSIGN (lv_field) TO <lt_data>.
            IF ( sy-subrc = 0 ).
              DATA: lv_sname TYPE string,
                    ls_jsonp TYPE w3html.

              lv_sname = ls_components-name.
              ls_jsonp-line = '"' && ls_components-name && '":{'.
              APPEND ls_jsonp TO gt_jsonp.
              LOOP AT <lt_data> ASSIGNING <ls_data>.
                TRY.
                    DATA: lo_struct_descr_2 TYPE REF TO cl_abap_structdescr.
                    lo_struct_descr_2 ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
* results ------------------------------------------------------
                    " costruisce results
                    DATA: ls_components_2 TYPE abap_compdescr,
                          ls_components_3 TYPE abap_compdescr,
                          lv_par          TYPE string.
                    FIELD-SYMBOLS: <lv_par>  TYPE any,
                                   <lv_par2> TYPE any.
                    LOOP AT lo_struct_descr_2->components INTO ls_components_2 WHERE name = 'PARAMETER'.
                      lv_par = '<LS_DATA>-' && ls_components_2-name.
                      ASSIGN (lv_par) TO <lv_par>.
                      CHECK sy-subrc = 0.
                      DATA: lo_data   TYPE REF TO cl_abap_datadescr,
                            lo_struct TYPE REF TO cl_abap_structdescr,
                            lo_table  TYPE REF TO cl_abap_tabledescr,
                            lv_type   TYPE string.
                      CLEAR: lv_type.
                      lv_par = '<LS_DATA>-structure'.
                      ASSIGN (lv_par) TO <lv_par2>.
                      CHECK sy-subrc = 0.
                      IF ( NOT <lv_par2> IS INITIAL ).
                        TRY.
                            lo_table ?= cl_abap_tabledescr=>describe_by_name( <lv_par2> ).
                            lv_type = '"table"'.
                          CATCH cx_root.
                            TRY.
                                lo_struct ?= cl_abap_structdescr=>describe_by_name( <lv_par2> ).
                                lv_type = '"structure"'.
                              CATCH cx_root.
                                TRY.
                                    lo_data ?= cl_abap_datadescr=>describe_by_name( <lv_par2> ).
                                    lv_type = '"field"'.
*                                  HTML-LINE = '"' && <LV_PAR> && '": "' && <LV_PAR2> &&  '",'.
*                                  APPEND HTML.
                                  CATCH cx_root.
                                ENDTRY.
                            ENDTRY.
                        ENDTRY.
                      ENDIF.
                      ls_jsonp-line = '"' && <lv_par> && '": "' && <lv_par2> &&  '",'.
                      APPEND ls_jsonp TO gt_jsonp.
                      DATA: lv_name2  TYPE string,
                            lv_field2 TYPE string.
                      lv_name2 = <lv_par>.
                      lv_field2 = <lv_par2>.
*                      add_to_dictionary( EXPORTING iv_name = lv_name2
*                                                   iv_field = lv_field2
*                                         CHANGING ct_dictionary = gt_dictionary ).
                    ENDLOOP.

                  CATCH cx_root INTO DATA(lx_root).
*                    CLEAR: ls_messages.
*                    ls_messages-type = 'E'.
*                    ls_messages-msg = lx_root->get_text( ).
*                    APPEND ls_messages TO lt_messages.
*
*                    PERFORM create_jsonp_2 USING    iv_callback
*                                                    lt_messages[]
*                                           CHANGING html[].
                    RETURN.
                ENDTRY.
              ENDLOOP.
              remove_extra_comma( ).
              ls_jsonp-line = '},'.
              APPEND ls_jsonp TO gt_jsonp.
            ENDIF.
          ENDLOOP.
          remove_extra_comma( ).

          " chiude tag results
          close_results( ).
        CATCH cx_root INTO lx_root.
      ENDTRY.

      dictionary( ).
      close_callback( ).
    ENDIF.
  ENDMETHOD.


  METHOD build_results.
    FIELD-SYMBOLS: <value>   TYPE any.

    DATA: lt_jsonp      TYPE w3htmltab,
          ls_jsonp      TYPE w3html,
          ls_fields     TYPE string,
          lv_field      TYPE string,  "c LENGTH 50,
          lv_str_name   TYPE string,
          lv_str_val    TYPE string,
          ls_components TYPE abap_componentdescr,
          lv_lines      TYPE i,
          lv_tabix      TYPE i,
          lv_comma      TYPE bool.

    FIELD-SYMBOLS: <lv_field> TYPE any.

    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_table_descr  TYPE REF TO cl_abap_tabledescr,
          lo_data_descr   TYPE REF TO cl_abap_datadescr,
          lx_root         TYPE REF TO cx_root,
          lv_has_struct   TYPE abap_bool.

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
        LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
          lv_tabix = sy-tabix.
          TRY.
              lv_has_struct = abap_true.
              CHECK lv_count BETWEEN iv_from_rec AND iv_to_rec.
              TRY.
                  lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
                  ls_jsonp = '{'.
                  INSERT ls_jsonp INTO TABLE ct_jsonp.
                CATCH cx_root.
                  lv_has_struct = abap_false.
              ENDTRY.
*            CHECK lv_count BETWEEN iv_from_rec AND iv_to_rec.
*            ls_jsonp = '{'.
*            INSERT ls_jsonp INTO TABLE ct_jsonp.

              DESCRIBE TABLE it_fields LINES lv_lines.

              REFRESH: lt_jsonp.
              IF ( lv_has_struct = abap_true ).
                LOOP AT it_fields INTO ls_fields.
                  lv_tabix = sy-tabix.
                  IF ( ls_fields <> '*' ).
                    CLEAR: lv_comma.
                    IF ( lv_tabix < lv_lines ).
                      lv_comma = 'X'.
                    ENDIF.
                    READ TABLE lo_struct_descr->get_components( ) INTO ls_components WITH KEY name = ls_fields.
                    IF ( sy-subrc = 0 ).
                      add_row_results( EXPORTING is_data = <ls_data>
                                                 is_components = ls_components
                                                 iv_comma =  'X'  "lv_comma
                                       CHANGING ct_jsonp = lt_jsonp[]
                                                ct_dictionary =  ct_dictionary[] ).
                    ENDIF.
                  ELSE.
                    LOOP AT lo_struct_descr->get_components( ) INTO ls_components.
                      lv_tabix = sy-tabix.
                      lv_field = '<ls_data>-' && ls_components-name.
                      ASSIGN (lv_field) TO <lv_field>.
                      TRY.
                          lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( <lv_field> ).
                          DATA: lv_name TYPE abap_parmname.
                          lv_name = ls_components-name.
                          TRANSLATE lv_name TO LOWER CASE.
                          build_results( EXPORTING iv_sname =    lv_name
                                                   iv_data =  <lv_field>
                                                   it_fields =   it_fields[]
                                                   iv_from_rec = iv_from_rec
                                                   iv_to_rec = iv_to_rec
                                         CHANGING ct_jsonp = lt_jsonp[]
                                                  ct_dictionary = ct_dictionary[] ).
                          DATA: lv_index TYPE i.
                          DESCRIBE TABLE lt_jsonp LINES lv_index.
                          READ TABLE lt_jsonp INTO ls_jsonp INDEX lv_index.
                          IF ( sy-subrc = 0 ).
                            IF ( NOT ls_jsonp-line CA '],' ).
                              ls_jsonp-line = ls_jsonp-line && '],'.
                              MODIFY lt_jsonp FROM ls_jsonp INDEX lv_index.
                            ENDIF.
                          ENDIF.
                        CATCH cx_root.
                          TRY.
                              lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <lv_field> ).
                              CHECK 1 = 1.
                            CATCH cx_root.
                              CLEAR: lv_comma.
                              IF ( lv_tabix < lv_lines ).
                                lv_comma = 'X'.
                              ENDIF.
                              add_row_results( EXPORTING is_data       = <ls_data>
                                                         is_components = ls_components
                                                         iv_comma      = 'X'   "lv_comma
                                               CHANGING ct_jsonp      = lt_jsonp[]
                                                        ct_dictionary = ct_dictionary[] ).
                          ENDTRY.
                      ENDTRY.
                      "              CLEAR: lv_comma.
                      "              IF ( lv_tabix < lv_lines ).
                      "                lv_comma = 'X'.
                      "              ENDIF.
                      "              PERFORM jsonp_add_row_results USING     <ls_data>
                      "                                                      ls_components
                      "                                                      'X'   "lv_comma
                      "                                            CHANGING  lt_jsonp[].
                    ENDLOOP.
                  ENDIF.
                ENDLOOP.
                DATA: lv_str       TYPE string,
                      ls_jsonp_tmp TYPE w3html.
*              lv_str = ''.
*              LOOP AT lt_jsonp INTO ls_jsonp_tmp.
*                IF ( strlen( lv_str ) > 200 ).
*                  ls_jsonp-line = lv_str.
*                  APPEND ls_jsonp TO ct_jsonp.
*                  lv_str = ''.
*                ENDIF.
*                lv_str = lv_str && ls_jsonp_tmp-line.
*              ENDLOOP.
*              IF ( strlen( lv_str ) > 0 ).
*                ls_jsonp-line = lv_str.
*                APPEND ls_jsonp TO ct_jsonp.
*              ENDIF.
                APPEND LINES OF lt_jsonp TO ct_jsonp.
                remove_extra_comma( ).

                ls_jsonp = '},'.
                INSERT ls_jsonp INTO TABLE ct_jsonp.
              ELSE.
                IF ( iv_sname = 'steps' ).
                  "                break novedev.
                ENDIF.
                lv_str_val = <ls_data>.
                lv_str = lv_str_val.
                " encode uri
                IF ( gv_escape_url = 'true' ).
                  cl_http_utility=>escape_url( EXPORTING unescaped = lv_str_val
                                               RECEIVING escaped = lv_str ).
                ENDIF.
                ls_jsonp-line = '"' && lv_str && '",'.
                APPEND ls_jsonp TO ct_jsonp.
              ENDIF.
*            DATA: lv_str       TYPE string,
*                  ls_jsonp_tmp TYPE w3html.
*            lv_str = ''.
*            LOOP AT lt_jsonp INTO ls_jsonp_tmp.
*              IF ( strlen( lv_str ) > 200 ).
*                ls_jsonp-line = lv_str.
*                APPEND ls_jsonp TO ct_jsonp.
*                lv_str = ''.
*              ENDIF.
*              lv_str = lv_str && ls_jsonp_tmp-line.
*            ENDLOOP.
*            IF ( strlen( lv_str ) > 0 ).
*              ls_jsonp-line = lv_str.
*              APPEND ls_jsonp TO ct_jsonp.
*            ENDIF.
*            PERFORM jsonp_remove_extra_comma CHANGING ct_jsonp[].
*
*            ls_jsonp = '},'.
*            INSERT ls_jsonp INTO TABLE ct_jsonp.
            CATCH cx_root.
*            CHECK 1 = 1.
*            ls_jsonp-line = '"' && <ls_data> && '",'.
*            APPEND ls_jsonp TO ct_jsonp.
          ENDTRY.
          ADD 1 TO lv_count.
        ENDLOOP.

        remove_extra_comma( ).

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

            LOOP AT lo_struct_descr->get_components( ) INTO ls_components.
              lv_tabix = sy-tabix.
              CLEAR: lv_comma.
              IF ( lv_tabix < lv_lines ).
                lv_comma = 'X'.
              ENDIF.
              add_row_results( EXPORTING is_data       = iv_data
                                         is_components = ls_components
                                         iv_comma      = 'X'   "lv_comma
                               CHANGING ct_jsonp     = ct_jsonp[]
                                        ct_dictionary = ct_dictionary[] ).
            ENDLOOP.

            remove_extra_comma( ).

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
  ENDMETHOD.


  METHOD build_system_info.
    open_key( 'systemInfo' ).
    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr.
    TRY.
        lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( sy ).
      CATCH cx_root.
    ENDTRY.
    LOOP AT lo_struct_descr->get_components( ) INTO DATA(ls_components) WHERE name = 'MANDT'
                                                                        OR    name = 'LANGU'
                                                                        OR    name = 'SYSID'
                                                                        OR    name = 'HOST'
                                                                        OR    name = 'OPSYS'
                                                                        OR    name = 'DBSYS'
                                                                        OR    name = 'DATUM'
                                                                        OR    name = 'UZEIT'
                                                                        OR    name = 'UNAME'
                                                                        OR    name = 'ZONLO'
                                                                        OR    name = 'DATLO'
                                                                        OR    name = 'TIMLO'
                                                                        OR    name = 'SAPRL'.
      me->add_row_results( EXPORTING is_data = sy
                                     is_components = ls_components
                                     iv_comma      = 'X'   "lv_comma
                           CHANGING ct_jsonp      = gt_jsonp[]
                                    ct_dictionary = gt_dictionary[] ).
    ENDLOOP.
    close_key( ).
    remove_extra_comma( ).
  ENDMETHOD.


  METHOD build_user_info.
    open_key( 'userInfo' ).
    SELECT  adrp~name_first AS first_name,
                  adrp~name_last AS last_name,
                  adrp~name_text AS full_name,
                  adr6~smtp_addr AS email,
                  adr2~tel_number AS telephone_number,
                  adr2~tel_number AS mobile_number,
                  adr2~r3_user
           FROM usr21 INNER JOIN adrp ON adrp~persnumber = usr21~persnumber
                      LEFT JOIN adr6 ON adr6~persnumber = adrp~persnumber
                      LEFT JOIN adr2 ON adr2~persnumber = adrp~persnumber AND
                                        adr6~date_from <= @sy-datum AND
                                        adr2~date_from <= @sy-datum "AND
                                        "adr2~r3_user = '3'
           WHERE usr21~bname    =  @sy-uname
           AND   adrp~date_from <= @sy-datum
           AND   adrp~date_to   >= @sy-datum
           AND   adr6~date_from <= @sy-datum
           INTO TABLE @DATA(lt_user_info).
    TRY.
        DATA(ls_user_info) = lt_user_info[ 1 ].
      CATCH cx_root.
    ENDTRY.
    LOOP AT lt_user_info INTO DATA(ls_usr).
      CASE ls_usr-r3_user.
        WHEN '1'.
          ls_user_info-telephone_number = ls_usr-telephone_number.
        WHEN '3'.
          ls_user_info-mobile_number = ls_usr-mobile_number.
      ENDCASE.
    ENDLOOP.
    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr.
    TRY.
        lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( ls_user_info ).
      CATCH cx_root.
    ENDTRY.
    LOOP AT lo_struct_descr->get_components( ) INTO DATA(ls_components) where name <> 'R3_USER'.
      me->add_row_results( EXPORTING is_data = ls_user_info
                                     is_components = ls_components
                                     iv_comma      = 'X'   "lv_comma
                           CHANGING ct_jsonp      = gt_jsonp[]
                                    ct_dictionary = gt_dictionary[] ).
    ENDLOOP.
    close_key( ).
    remove_extra_comma( ).
  ENDMETHOD.


  METHOD build_webrfc_info.
    IF ( NOT gv_webrfc_user IS INITIAL ).
      open_key( 'webrfcUserInfo' ).
      SELECT usr, user_type, first_name, last_name, full_name, email, phone_number, mobile_number
             FROM zwrfc_users
             WHERE usr =  @gv_webrfc_user
             INTO TABLE @DATA(lt_user_info).
      TRY.
          DATA(ls_user_info) = lt_user_info[ 1 ].
        CATCH cx_root.
      ENDTRY.
      DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
            lo_data_descr   TYPE REF TO cl_abap_datadescr.
      TRY.
          lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( ls_user_info ).
          lo_data_descr ?= cl_abap_datadescr=>describe_by_data( ls_user_info-user_type ).
*          lo_data_descr->get_ddic_object(
        CATCH cx_root.
      ENDTRY.
      LOOP AT lo_struct_descr->get_components( ) INTO DATA(ls_components) WHERE name <> 'R3_USER'.
        me->add_row_results( EXPORTING is_data = ls_user_info
                                       is_components = ls_components
                                       iv_comma      = 'X'   "lv_comma
                             CHANGING ct_jsonp      = gt_jsonp[]
                                      ct_dictionary = gt_dictionary[] ).
      ENDLOOP.
      close_key( ).
      remove_extra_comma( ).
    ENDIF.
  ENDMETHOD.


  METHOD camel_case.
    DATA: lv_len      TYPE i,
          lv_pos      TYPE i,
          lv_upper    TYPE c LENGTH 1,
          lv_str_name TYPE string.

    IF ( gv_camel_case = 'true' AND cv_str_name CA '_' ).
      lv_str_name = cv_str_name.
      lv_pos = 0.
      lv_len = strlen( cv_str_name ).
      CLEAR: cv_str_name.
      DO lv_len TIMES.
        IF ( lv_pos >= lv_len ).
          EXIT.
        ENDIF.
        IF ( lv_str_name+lv_pos(1) = '_' ).
          ADD 1 TO lv_pos.
          lv_upper = lv_str_name+lv_pos(1).
          TRANSLATE lv_upper TO UPPER CASE.
          cv_str_name = cv_str_name && lv_upper.
          ADD 1 TO lv_pos.
        ENDIF.
        cv_str_name = cv_str_name && lv_str_name+lv_pos(1).
        ADD 1 TO lv_pos.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD close_callback.
    DATA: ls_jsonp TYPE w3html.

    remove_extra_comma( ).

    IF ( NOT gv_callback IS INITIAL ).
      ls_jsonp = '});'.
    ELSE.
      ls_jsonp = '}'.
    ENDIF.
    gv_jsonp = gv_jsonp && ls_jsonp-line.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD close_errors.
    DATA: ls_jsonp TYPE w3html.

    ls_jsonp = '],'.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD close_key.
    DATA: ls_jsonp TYPE w3html.

    remove_extra_comma( ).

    ls_jsonp = '},'.
    gv_jsonp = gv_jsonp && ls_jsonp-line.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD close_results.
    DATA: ls_jsonp TYPE w3html.

    remove_extra_comma( ).

    ls_jsonp = '}'.
    gv_jsonp = gv_jsonp && ls_jsonp-line.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD constructor.
    gv_callback    = iv_callback.
    gs_results     = is_results.
    go_function    = io_function.
    gt_fields[]    = it_fields[].
    gt_messages[]  = it_messages[].
    gv_from_rec    = iv_from_rec.
    gv_to_rec      = iv_to_rec.
    gv_camel_case  = iv_camel_case.
    gv_escape_url  = iv_escape_url.
    gv_webrfc_user = iv_webrfc_user.
    gv_webrfc_log  = iv_webrfc_log.
    CLEAR: gv_jsonp, gt_jsonp, gt_dictionary.
    TRANSLATE gv_camel_case TO LOWER CASE.
  ENDMETHOD.


  METHOD dictionary.
    DATA: lv_name        TYPE string,
          ls_dictionary  TYPE zwrfc_ty_s_dictionary,
          ls_jsonp       TYPE w3html,
          ls_components  TYPE abap_compdescr,
          lv_field       TYPE string,
          lo_structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <value> TYPE string.

    TRY.
        lo_structdescr ?= cl_abap_structdescr=>describe_by_data( ls_dictionary ).
      CATCH cx_root.
    ENDTRY.

    remove_extra_comma( ).

    ls_jsonp-line = ', "dictionary": ['  ##no_text.
    APPEND ls_jsonp TO gt_jsonp.

    CLEAR: lv_name.
    LOOP AT gt_dictionary INTO ls_dictionary.
      IF ( lv_name <> ls_dictionary-name ).
        ls_jsonp-line = '{'.
        APPEND ls_jsonp TO gt_jsonp.

        READ TABLE gt_dictionary INTO DATA(ls_dict) WITH KEY name = ls_dictionary-name.
        CHECK sy-subrc = 0.

        LOOP AT lo_structdescr->components INTO ls_components.  " WHERE name <> ls_dictionary-name.
          TRANSLATE ls_components-name TO LOWER CASE.
          lv_field = 'ls_dictionary-' && ls_components-name.

          ASSIGN (lv_field) TO <value>.
          CHECK sy-subrc = 0.

*          ls_jsonp-line = '"' && ls_components-name && '": "' && <value> && '",'.
          DATA: lv_comp_name TYPE string.
          lv_comp_name = ls_components-name.
          camel_case( CHANGING cv_str_name = lv_comp_name ).
          ls_jsonp-line = '"' && lv_comp_name && '": "' && <value> && '",'.
          APPEND ls_jsonp TO gt_jsonp.
        ENDLOOP.
        remove_extra_comma( ).

        ls_jsonp-line = '},'.
        APPEND ls_jsonp TO gt_jsonp.
      ENDIF.
      lv_name = ls_dictionary-name.
    ENDLOOP.

    remove_extra_comma( ).

    ls_jsonp-line = ']'.
    APPEND ls_jsonp TO gt_jsonp.
  ENDMETHOD.


  METHOD get_json_string.
    CLEAR: rv_json_string.
    LOOP AT gt_jsonp INTO DATA(ls_jsonp).
      rv_json_string = rv_json_string && ls_jsonp-line.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_JSON_TABLE.
    rt_table[] = gt_jsonp[].
  ENDMETHOD.


  METHOD open_callback.
    DATA: ls_jsonp TYPE w3html.

    CLEAR: ls_jsonp.
    IF ( NOT gv_callback IS INITIAL ).
      ls_jsonp-line = gv_callback && '({'.
    ELSE.
      ls_jsonp-line = '{'.
    ENDIF.
    gv_jsonp = ls_jsonp-line.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD open_errors.
    DATA: ls_jsonp TYPE w3html.

    ls_jsonp = '"errors": ['.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD open_key.
    DATA: ls_jsonp TYPE w3html.

    ls_jsonp-line = ',"' && iv_name && '": {'.
    gv_jsonp = gv_jsonp && ls_jsonp-line.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD open_results.
    DATA: ls_jsonp TYPE w3html.

    ls_jsonp = '"results": {'.
    gv_jsonp = gv_jsonp && ls_jsonp-line.
    INSERT ls_jsonp INTO TABLE gt_jsonp.
  ENDMETHOD.


  METHOD remove_extra_comma.
    DATA: ls_jsonp TYPE w3html,
          lv_len   TYPE i,
          lv_lines TYPE i.

    DESCRIBE TABLE gt_jsonp LINES lv_lines.
    IF ( lv_lines > 0 ).
      READ TABLE gt_jsonp INTO ls_jsonp INDEX lv_lines.
      lv_len = strlen( ls_jsonp ) - 1.
      IF ( ls_jsonp+lv_len(1) = ',' ).
        ls_jsonp+lv_len(1) = ''.
      ENDIF.
      MODIFY gt_jsonp FROM ls_jsonp INDEX lv_lines.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
