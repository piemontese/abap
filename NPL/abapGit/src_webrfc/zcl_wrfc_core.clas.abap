class ZCL_WRFC_CORE definition
  public
  create public .

public section.

  constants C_PARAM_CALLBACK type STRING value 'CALLBACK' ##NO_TEXT.
  constants C_PARAM_METHOD type STRING value 'METHOD' ##NO_TEXT.
  constants C_PARAM_CAMEL_CASE type STRING value 'CAMEL_CASE' ##NO_TEXT.
  constants C_PARAM_WEBRFC_LOGIN type STRING value 'WEBRFC_LOGIN' ##NO_TEXT.
  constants C_PARAM_WEBRFC_USER type STRING value 'WEBRFC_USER' ##NO_TEXT.
  constants C_PARAM_WEBRFC_PASSWORD type STRING value 'WEBRFC_PASSWORD' ##NO_TEXT.
  constants C_PARAM_FIELDS type STRING value 'FIELDS' ##NO_TEXT.
  constants C_PARAM_FROM_REC type STRING value 'FROM_REC' ##NO_TEXT.
  constants C_PARAM_TO_REC type STRING value 'TO_REC' ##NO_TEXT.
  constants C_PARAM_WEBRFC_LOG type STRING value 'WEBRFC_LOG' ##NO_TEXT.
  constants C_PARAM_METHOD_TYPE type STRING value 'METHOD_TYPE' ##NO_TEXT.
  data C_PARAM_ESCAPE_URL type STRING value 'ESCAPE_URL' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IT_QUERY type RRXW3TQUERY
    raising
      resumable(ZCX_BC_EXCEPTION) .
  methods GET_HTML
    returning
      value(RT_HTML) type W3HTMLTAB .
  methods GET_QUERY
    returning
      value(RT_QUERY) type RRXW3TQUERY .
  methods GET_FIELDS
    changing
      value(CT_FIELDS) type ZWRFC_TY_T_FIELDS .
  methods GET_MIME
    returning
      value(RT_MIME) type W3MIMETABTYPE .
  methods ADD_MESSAGE
    importing
      !IV_TYPE type ZWRFC_TY_S_MESSAGES-TYPE
      !IV_MSG type ZWRFC_TY_S_MESSAGES-MSG .
  methods GET_JSON
    returning
      value(RV_JSON) type STRING .
protected section.

  data GT_QUERY type RRXW3TQUERY .
  data GT_HTML type W3HTMLTAB .
  data GT_MIME type W3MIMETABTYPE .
  data GT_FIELDS type ZWRFC_TY_T_FIELDS .
  data GT_MESSAGES type ZWRFC_TY_T_MESSAGES .
  data GV_FROM_REC type I value 0 ##NO_TEXT.
  data GV_TO_REC type I value 100 ##NO_TEXT.
  data GV_CAMEL_CASE type STRING .
  data GV_CALLBACK type STRING value SPACE ##NO_TEXT.
  data GV_ESCAPE_URL type STRING .
  data GV_JSON type STRING .
  data GV_WEBRFC_LOGIN type STRING value ABAP_FALSE ##NO_TEXT.
  data GV_WEBRFC_USER type ZWRFC_USER value ABAP_FALSE ##NO_TEXT.
  data GV_WEBRFC_PASSWORD type ZWRFC_PASSWORD value ABAP_FALSE ##NO_TEXT.
  data GV_WEBRFC_LOG type STRING .
  data GV_METHOD_TYPE type ABAP_BOOL value 'F' ##NO_TEXT.
  data GT_CALL_PARAMS type ZWRFC_TY_T_CALL_PARAMS .
  constants C_PARAM_SQLTABLE type STRING value 'SQLTABLE' ##NO_TEXT.
  constants C_PARAM_SQLWHERE type STRING value 'SQLWHERE' ##NO_TEXT.
  constants C_PARAM_CLASS type STRING value 'CLASS' ##NO_TEXT.
private section.
ENDCLASS.



CLASS ZCL_WRFC_CORE IMPLEMENTATION.


  METHOD add_message.
    DATA: ls_messages TYPE zwrfc_ty_s_messages.

    CLEAR: ls_messages.
    ls_messages-type = iv_type.
    ls_messages-msg  = iv_msg.

    APPEND ls_messages TO gt_messages.
  ENDMETHOD.


  METHOD constructor.
    CLEAR: gt_call_params, gt_html, gt_mime, gt_fields, gt_messages, gv_json.

    SELECT zwrfc_call_param~* INTO TABLE @gt_call_params FROM zwrfc_call_param.

    gt_query[] = it_query[].

    SORT gt_query.
    DELETE ADJACENT DUPLICATES FROM gt_query.
    LOOP AT gt_query ASSIGNING FIELD-SYMBOL(<ls_query>).
      DATA(lv_index) = sy-tabix.
      TRANSLATE <ls_query>-name TO UPPER CASE.

      DATA: lv_query_name TYPE w3_qname,
            lv_query_null TYPE w3_qname.

      CLEAR: lv_query_name.
      " usa il pipe per parametro passato in righe multiple
      " es. matnr|0 matnr|1 ... matnr|n
      SPLIT <ls_query>-name AT '|' INTO lv_query_name lv_query_null.
      IF ( NOT lv_query_name IS INITIAL ).
        <ls_query>-name = lv_query_name.
      ENDIF.

      TRY.
          DATA(lv_name) = gt_call_params[ name = <ls_query>-name ]-name.
        CATCH cx_root.
          DELETE gt_query INDEX lv_index.
      ENDTRY.
    ENDLOOP.

    LOOP AT gt_call_params INTO DATA(ls_call_params).
      TRY.
          DATA(ls_query) = gt_query[ name = ls_call_params-name ].
        CATCH cx_root.
          ls_query-name = ls_call_params-name.
          ls_query-value = ls_call_params-value.
          APPEND ls_query TO gt_query.
      ENDTRY.
    ENDLOOP.

    READ TABLE gt_query ASSIGNING <ls_query> WITH KEY name = c_param_method.
    IF ( sy-subrc = 0 ).
      TRANSLATE <ls_query>-value TO UPPER CASE.
    ENDIF.

    READ TABLE gt_query ASSIGNING <ls_query> WITH KEY name = c_param_class.
    IF ( sy-subrc = 0 ).
      TRANSLATE <ls_query>-value TO UPPER CASE.
    ENDIF.

    TRY.
        gv_callback = gt_query[ name = c_param_callback ]-value.
      CATCH cx_root.
        MESSAGE s001(z_webrfc) INTO DATA(lv_msg).
        add_message( iv_type = 'E' iv_msg = lv_msg ).
    ENDTRY.

    TRY.
        gv_from_rec = gt_query[ name = c_param_from_rec ]-value.
      CATCH cx_root.
        gv_from_rec = 0.
    ENDTRY.

    TRY.
        gv_to_rec = gt_query[ name = c_param_to_rec ]-value.
      CATCH cx_root.
        gv_to_rec = 100.
    ENDTRY.

    TRY.
        gv_escape_url = gt_query[ name = c_param_escape_url ]-value.
      CATCH cx_root.
        gv_escape_url = abap_true.
    ENDTRY.

    TRY.
        DATA(lv_fields) = gt_query[ name = c_param_fields ]-value.
        IF ( lv_fields IS INITIAL ).
          lv_fields = '*'.
        ENDIF.
      CATCH cx_root.
        lv_fields = '*'.
    ENDTRY.
    SPLIT lv_fields AT ' ' INTO TABLE gt_fields.
    LOOP AT gt_fields INTO DATA(ls_fields).
      IF ( ls_fields = space ).
        DELETE gt_fields.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    TRY.
        gv_camel_case = gt_query[ name = c_param_camel_case ]-value.
      CATCH cx_root.
        gv_camel_case = 'false'.
    ENDTRY.

    TRY.
        gv_webrfc_login = gt_query[ name = c_param_webrfc_login ]-value.
      CATCH cx_root.
        gv_webrfc_login = space.
    ENDTRY.

    TRY.
        gv_webrfc_user = gt_query[ name = c_param_webrfc_user ]-value.
      CATCH cx_root.
        gv_webrfc_user = space.
    ENDTRY.

    TRY.
        gv_webrfc_password = gt_query[ name = c_param_webrfc_password ]-value.
      CATCH cx_root.
        gv_webrfc_password = space.
    ENDTRY.

    TRY.
        gv_webrfc_log = gt_query[ name = c_param_webrfc_log ]-value.
      CATCH cx_root.
*        gv_webrfc_log = space.
    ENDTRY.

    TRY.
        gv_method_type = gt_query[ name = c_param_method_type ]-value.
      CATCH cx_root.
*        gv_method_type = space.
    ENDTRY.
  ENDMETHOD.


  METHOD get_fields.
    ct_fields[] = gt_fields[].
  ENDMETHOD.


  METHOD get_html.
    rt_html[] = gt_html[].
  ENDMETHOD.


  METHOD get_json.
    rv_json = gv_json.
  ENDMETHOD.


  METHOD get_mime.
    rt_mime[] = gt_mime[].
  ENDMETHOD.


  METHOD get_query.
    rt_query[] = gt_query[].
  ENDMETHOD.
ENDCLASS.
