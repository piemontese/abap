FUNCTION z_wrfc_function_interface.
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

  FIELD-SYMBOLS: <ls_data> TYPE any.
  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.
  TYPES: BEGIN OF ty_s_work, buffer(30000), END OF ty_s_work.
  DATA: ls_work TYPE ty_s_work.

  DATA: lx_root           TYPE REF TO cx_root,
        lo_struct_descr   TYPE REF TO cl_abap_structdescr,
        lo_struct_descr_2 TYPE REF TO cl_abap_structdescr,
        lo_table_descr    TYPE REF TO cl_abap_tabledescr,
        ls_components     TYPE abap_compdescr,
        lt_components     TYPE abap_compdescr_tab,
        lt_keys           TYPE abap_keydescr_tab,
        lv_field          TYPE c LENGTH 50,
        ls_string         TYPE string,
        lt_table          TYPE REF TO data,
        ls_table          TYPE REF TO data,
        lt_fields         TYPE TABLE OF string,
        ls_fields         TYPE string,
        ls_messages       TYPE ty_s_messages,
        lv_sname          TYPE string.

  DATA: lt_messages TYPE ty_t_messages.

  REFRESH lt_messages.

  CLEAR: lv_sname.

* parametri
  DATA: iv_callback       TYPE string,
        iv_method         TYPE string,
        iv_fields         TYPE string.

  CLEAR: iv_callback,
         iv_method,
         iv_fields.

  LOOP AT query_string.
    TRANSLATE query_string-name TO UPPER CASE.
    MODIFY query_string.
  ENDLOOP.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'CALLBACK'.
  iv_callback = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'METHOD'.
  iv_method = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'FIELDS'.
  iv_fields = query_string-value.
  IF ( iv_fields IS INITIAL ).
    iv_fields = '*'.
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



  DATA: ls_interface TYPE rsfbintfv,
        ls_param     TYPE rsfbpara,
        lv_name      TYPE eu_lname.
  lv_name = iv_method.
  cl_fb_function_utility=>meth_get_interface( EXPORTING im_name      = lv_name
                                              IMPORTING ex_interface = ls_interface
                                              EXCEPTIONS error_occured = 1
                                                         object_not_existing = 2 ).
  IF ( sy-subrc = 0 ).
    " apre tag funcione di callback
    PERFORM jsonp_open_callback USING    iv_callback
                                CHANGING html[].

    TRY.
        FIELD-SYMBOLS <lv_field> TYPE any.
        lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( ls_interface ).

* columns ------------------------------------------------------
        " apre tag columns
        PERFORM jsonp_open_columns CHANGING html[].
        " costruisce columns
        PERFORM jsonp_build_columns USING    lo_struct_descr->components[]
                                             lt_fields[]
                                    CHANGING html[].
        " chiude tag columns
        PERFORM jsonp_close_columns CHANGING html[].

        " apre tag results
        PERFORM jsonp_open_results CHANGING html[].

        LOOP AT lo_struct_descr->components INTO ls_components.
          lv_field = 'ls_interface-' && ls_components-name.
          ASSIGN (lv_field) TO <lt_data>.
          IF ( sy-subrc = 0 ).
*            LOOP AT <lt_data> ASSIGNING <ls_data>.
*            ENDLOOP.
            lv_sname = ls_components-name.
            TRY.
                LOOP AT <lt_data> ASSIGNING <ls_data>.
                  EXIT.
                ENDLOOP.
                IF ( sy-subrc = 0 ).
                  lo_struct_descr_2 ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
* results ------------------------------------------------------
*                  " apre tag results
*                  PERFORM jsonp_open_results CHANGING html[].
                  " costruisce results
                  PERFORM jsonp_build_results USING    lv_sname
                                                       <lt_data>[]
                                                       lo_struct_descr_2->components[]
                                                       lt_fields[]
*                                                       '10'
                                                       1
                                                       10
                                              CHANGING html[].
*                  " chiude tag results
*                  PERFORM jsonp_close_results CHANGING html[].
                ENDIF.

              CATCH cx_root INTO lx_root.
                CLEAR: ls_messages.
                ls_messages-type = 'E'.
                ls_messages-msg = lx_root->get_text( ).
                APPEND ls_messages TO lt_messages.

                PERFORM create_jsonp_2 USING    iv_callback
                                                lt_messages[]
                                       CHANGING html[].
                RETURN.
            ENDTRY.

          ENDIF.
        ENDLOOP.

        " chiude tag results
        PERFORM jsonp_close_results CHANGING html[].
      CATCH cx_root.
        CLEAR: ls_messages.
        ls_messages-type = 'E'.
        ls_messages-msg = lx_root->get_text( ).
        APPEND ls_messages TO lt_messages.

        PERFORM create_jsonp_2 USING    iv_callback
                                        lt_messages[]
                               CHANGING html[].
        RETURN.
    ENDTRY.

    " apre tag funcione di callback
    PERFORM jsonp_close_callback USING iv_callback
                                 CHANGING html[].

  ELSE.
    CLEAR: ls_messages.
    ls_messages-type = 'E'.
    CASE sy-subrc.
      WHEN 1.
        ls_messages-msg = 'Error'.
      WHEN 2.
        ls_messages-msg = 'Function not exist'.
    ENDCASE.
    APPEND ls_messages TO lt_messages.

    PERFORM create_jsonp_2 USING    iv_callback
                                    lt_messages[]
                           CHANGING html[].
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
