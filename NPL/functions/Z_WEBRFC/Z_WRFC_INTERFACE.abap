FUNCTION z_wrfc_interface.
*"----------------------------------------------------------------------
*"*"Local Interface:
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

  " http://127.0.0.1:8000/sap/bc/webrfc?_FUNCTION=Z_WRFC_INTERFACE&callback=jsonCallback&method=Z_WRFC_GET_DUMPS

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lt_data> TYPE ANY TABLE,
                 <lv_data> TYPE any.
  TYPES: BEGIN OF ty_s_work, buffer(30000), END OF ty_s_work.
  DATA: ls_work TYPE ty_s_work.

  DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
        lo_table_descr  TYPE REF TO cl_abap_tabledescr,
        lo_data_descr   TYPE REF TO cl_abap_datadescr,
        ls_components   TYPE abap_compdescr,
        lt_components   TYPE abap_compdescr_tab,
        lt_keys         TYPE abap_keydescr_tab,
        lv_field        TYPE c LENGTH 50,
        ls_string       TYPE string,
        lt_table        TYPE REF TO data,
        ls_table        TYPE REF TO data,
        lt_fields       TYPE TABLE OF string,
        ls_fields       TYPE string,
        ls_messages     TYPE ty_s_messages,
        lv_sname        TYPE string,
        lv_query_name   TYPE w3_qname,
        lv_query_null   TYPE w3_qname.

  DATA: lx_root TYPE REF TO cx_root.

  DATA: lt_messages   TYPE ty_t_messages,
        lt_dictionary TYPE ty_t_dictionary.

  CLEAR: lv_sname.

  REFRESH: lt_messages, lt_dictionary.

* parametri
  DATA: iv_callback TYPE string,
        iv_method   TYPE string,
        iv_fields   TYPE string,
*        IV_ROWS           TYPE STRING,
        iv_from_rec TYPE i,
        iv_to_rec   TYPE i.

  CLEAR: gv_camel_case,
         iv_callback,
         iv_method,
         iv_fields,
*         IV_ROWS,
         iv_from_rec,
         iv_to_rec.

  LOOP AT query_string.
    TRANSLATE query_string-name TO UPPER CASE.
    CLEAR: lv_query_name.
    " usa il pipe per parametro passato in righe multiple
    " es. matnr|0 matnr|1 ... matnr|n
    SPLIT query_string-name AT '|' INTO lv_query_name lv_query_null.
    IF ( NOT lv_query_name IS INITIAL ).
      query_string-name = lv_query_name.
    ENDIF.
    MODIFY query_string.
  ENDLOOP.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'CALLBACK'.
  iv_callback = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'METHOD'.
  iv_method = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'CAMEL_CASE'.
  gv_camel_case = query_string-value.
  TRANSLATE gv_camel_case TO LOWER CASE.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'FIELDS'.
  iv_fields = query_string-value.
  IF ( iv_fields IS INITIAL ).
    iv_fields = '*'.
  ENDIF.

*  CLEAR: QUERY_STRING.
*  READ TABLE QUERY_STRING WITH KEY NAME = 'ROWS'.
*  IV_ROWS = QUERY_STRING-VALUE.
*  IF ( IV_ROWS IS INITIAL ).
*    IV_ROWS = '20'.
*  ENDIF.

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
    iv_to_rec = 999.
  ENDIF.


  REFRESH: lt_fields.
  SPLIT iv_fields AT ' ' INTO TABLE lt_fields.
  LOOP AT lt_fields INTO ls_fields.
    IF ( ls_fields = space ).
      DELETE lt_fields.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  IF ( iv_callback IS INITIAL ).
    PERFORM add_message USING    'E' 'Specificare funzione di callback'
                        CHANGING lt_messages[].
  ENDIF.



  DATA: "ls_interface TYPE rsfbintfv,
    ls_param TYPE rsfbpara,
    lv_name  TYPE eu_lname.
  lv_name = iv_method.
*  cl_fb_function_utility=>meth_get_interface( EXPORTING im_name      = lv_name
*                                              IMPORTING ex_interface = ls_interface
*                                              EXCEPTIONS error_occured = 1
*                                                         object_not_existing = 2 ).
*  IF ( sy-subrc = 0 ).
  DATA: lt_rep      TYPE swbse_max_line_tab,
        lt_import   TYPE rsfb_imp,
        lt_change   TYPE rsfb_cha,
        lt_export   TYPE rsfb_exp,
        lt_tables   TYPE rsfb_tbl,
        lt_except   TYPE rsfb_exc,
        ls_import   TYPE rsimp,
        ls_change   TYPE rscha,
        ls_export   TYPE rsexp,
        ls_para     TYPE rsfbpara,  "rstbl,
        ls_except   TYPE rsexc,
        lv_funcname TYPE rs38l-name.

  lv_funcname = iv_method.

  DATA: func         TYPE string,
        text_tab     LIKE STANDARD TABLE OF line,
        filetype(10) TYPE c.
*          ls_params     TYPE abap_func_parmbind,
*          ls_exceptions TYPE abap_func_excpbind.

  func = iv_method.


  TRY.
      DATA: lo_root TYPE REF TO cx_root.

      DATA: lt_fm_parameters TYPE zbc_ty_t_fm_parameters.
      LOOP AT query_string INTO DATA(ls_query).
        DATA: ls_fm_parameters TYPE zbc_ty_s_fm_parameters.
        MOVE-CORRESPONDING ls_query TO ls_fm_parameters.
        APPEND ls_fm_parameters TO lt_fm_parameters.
      ENDLOOP.
      DATA(lo_function) = NEW zcl_bc_function_module( iv_method     = lv_name
                                                      it_parameters = lt_fm_parameters ).
*      DATA(ls_results) = lo_function->get_results( ).
      DATA(ls_results) = lo_function->call( ).

      DATA(lo_jsonp) = NEW zcl_wrfc_jsonp( iv_callback = iv_callback
                                           is_results  = ls_results ).

      PERFORM json_add_result USING    ls_results-t_params[]  "lt_params[]
                              CHANGING html[].

      " apre tag funcione di callback
      PERFORM jsonp_open_callback USING    iv_callback
                                  CHANGING html[].

      " apre tag results
      PERFORM jsonp_open_results CHANGING html[].

*        LOOP AT lt_params INTO ls_params.
      LOOP AT ls_results-t_params INTO DATA(ls_params).
*          READ TABLE ls_interface-tables WITH KEY parameter = ls_params-name TRANSPORTING NO FIELDS.
        DATA: ls_tables TYPE rsfbpara.
        READ TABLE lo_function->get_interface( )-tables WITH KEY parameter = ls_params-name INTO ls_tables.
        IF ( sy-subrc = 0 ).
          lv_sname = ls_params-name.
          TRY.
              ASSIGN ls_params-value->* TO <lt_data>.
              LOOP AT <lt_data> ASSIGNING <ls_data>.
                EXIT.
              ENDLOOP.
              IF ( sy-subrc <> 0 ).   " tabella vuota
                html-line = '"' && lv_sname && '": [],'.
                APPEND html.
                CONTINUE.
              ENDIF.
              lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
            CATCH cx_root.
          ENDTRY.

          " costruisce results
          PERFORM jsonp_build_results USING    lv_sname
                                               <lt_data>[]
                                               lt_fields[]
                                               iv_from_rec
                                               iv_to_rec
                                      CHANGING html[]
                                               lt_dictionary[].
*            PERFORM JSON_CREATE_FROM_DATA USING    IV_CALLBACK
*                                                   LS_PARAMS-NAME
*                                                   <LT_DATA>[]
*                                          CHANGING HTML[].


        ENDIF.
        READ TABLE lo_function->get_interface( )-export WITH KEY parameter = ls_params-name TRANSPORTING NO FIELDS.
        IF ( sy-subrc = 0 ).
          lv_sname = ls_params-name.
          TRY.
              ASSIGN ls_params-value->* TO <ls_data>.
              lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( <ls_data> ).
              " costruisce results
              PERFORM jsonp_build_results USING    lv_sname
                                                   <ls_data>
*                                                 LO_STRUCT_DESCR->COMPONENTS[]
                                                   lt_fields[]
*                                                 IV_ROWS
                                                   iv_from_rec
                                                   iv_to_rec
                                          CHANGING html[]
                                                   lt_dictionary[].

            CATCH cx_root.
              TRY.
                  ASSIGN ls_params-value->* TO <ls_data>.
                  lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).

                  PERFORM jsonp_build_results USING    lv_sname
                                                       <ls_data>
*                                                     LO_STRUCT_DESCR->COMPONENTS[]
                                                       lt_fields[]
*                                                     IV_ROWS
                                                       iv_from_rec
                                                       iv_to_rec
                                              CHANGING html[]
                                                       lt_dictionary[].
                CATCH cx_root.
                  TRY.
                      ASSIGN ls_params-value->* TO <ls_data>.
                      lo_data_descr ?= cl_abap_datadescr=>describe_by_data( <ls_data> ).

                      html-line = '"' && lv_sname && '":"' && <ls_data> && '",'.
                      APPEND html.
                    CATCH cx_root.
                  ENDTRY.
              ENDTRY.
          ENDTRY.

*            ASSIGN <LS_DATA> TO <LV_DATA>.
*            PERFORM JSON_CREATE_FROM_DATA USING    IV_CALLBACK
*                                                   LS_PARAMS-NAME
*                                                   <LV_DATA>
*                                          CHANGING HTML[].

        ENDIF.
        READ TABLE lo_function->get_interface( )-change WITH KEY parameter = ls_params-name TRANSPORTING NO FIELDS.
        IF ( sy-subrc = 0 ).
          lv_sname = ls_params-name.
          TRY.
              ASSIGN ls_params-value->* TO <ls_data>.
              lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).

              PERFORM jsonp_build_results USING    lv_sname
                                                   <ls_data>
*                                                     LO_STRUCT_DESCR->COMPONENTS[]
                                                   lt_fields[]
*                                                     IV_ROWS
                                                   iv_from_rec
                                                   iv_to_rec
                                          CHANGING html[]
                                                   lt_dictionary[].
            CATCH cx_root.
              TRY.
                  ASSIGN ls_params-value->* TO <ls_data>.
                  lo_data_descr ?= cl_abap_datadescr=>describe_by_data( <ls_data> ).

                  html-line = '"' && lv_sname && '":"' && <ls_data> && '",'..
                  APPEND html.
                CATCH cx_root.
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
      PERFORM jsonp_close_results CHANGING html[].

      PERFORM jsonp_dictionary USING    lt_dictionary[]
                               CHANGING html[].

      " apre tag funcione di callback
      PERFORM jsonp_close_callback USING    iv_callback
                                   CHANGING html[].

    CATCH zcx_bc_exception INTO DATA(lx_exception).
      DATA: lv_msg TYPE ty_s_messages-msg.
      lv_msg = lx_exception->get_text( ).
      PERFORM add_message USING    'E' lv_msg
                          CHANGING lt_messages[].

      PERFORM create_jsonp_2 USING    iv_callback
                                      lt_messages[]
                             CHANGING html[].
  ENDTRY.
*  ELSE.
*    CASE sy-subrc.
*      WHEN 1.
*        PERFORM add_message USING    'E' 'Error'
*                            CHANGING lt_messages[].
*      WHEN 2.
*        PERFORM add_message USING    'E' 'Function not exist'
*                            CHANGING lt_messages[].
*    ENDCASE.
*
*    PERFORM create_jsonp_2 USING    iv_callback
*                                    lt_messages[]
*                           CHANGING html[].
*  ENDIF.

  DATA: lv_string TYPE string.
  CLEAR: lv_string.
  LOOP AT html.
    lv_string = lv_string && html-line.
  ENDLOOP.
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
