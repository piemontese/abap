class ZCL_WRFC_INTERFACE definition
  public
  inheriting from ZCL_WRFC_CORE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_QUERY type RRXW3TQUERY
    raising
      resumable(ZCX_BC_EXCEPTION) .
  methods CALL
    returning
      value(RV_ERROR) type I .
  methods GET_FUNCTION_INTERFACE .
protected section.
private section.

  data GV_METHOD type STRING value SPACE ##NO_TEXT.
ENDCLASS.



CLASS ZCL_WRFC_INTERFACE IMPLEMENTATION.


  METHOD call.
    rv_error = 0.
    TRY.
        IF ( gv_webrfc_log = 'true' ).
          DATA: lv_logid TYPE zwrfc_logid.
          zcl_bc_numerator_handler=>get_next( EXPORTING iv_range_nr = '01'
                                                        iv_object   = 'ZWRFC_LOG'
                                              IMPORTING  ev_number  = lv_logid ).
          DATA(ls_wrfc_logt) = VALUE zwrfc_logt( mandt       = sy-mandt
                                                 logid       = lv_logid
                                                 interface   = 'ZCL_WRFC_INTERFACE'
                                                 method      = gv_method
                                                 method_type = gv_method_type
                                                 logdate     = sy-datum
                                                 logtime     = sy-uzeit
                                                 bname       = sy-uname
                                                 webrfc_user = gv_webrfc_user ).
          INSERT zwrfc_logt FROM ls_wrfc_logt.
          COMMIT WORK.
        ENDIF.
        DATA: lt_fm_parameters TYPE zbc_ty_t_fm_parameters.
        REFRESH: lt_fm_parameters.
        LOOP AT gt_query INTO DATA(ls_query).
          IF ( ls_query-name <> 'WEBRFC_PASSWORD' ).
            DATA(ls_wrfc_logd_in) = VALUE zwrfc_logd( mandt     = sy-mandt
                                                      logid     = lv_logid
                                                      logcount  = sy-tabix
                                                      direction = 'I'
                                                      name      = ls_query-name
                                                      value     = ls_query-value ).
            INSERT zwrfc_logd FROM ls_wrfc_logd_in.
            COMMIT WORK.
          ENDIF.
          DATA: ls_fm_parameters TYPE zbc_ty_s_fm_parameters.
          MOVE-CORRESPONDING ls_query TO ls_fm_parameters.
          APPEND ls_fm_parameters TO lt_fm_parameters.
        ENDLOOP.
        IF ( gv_webrfc_login = 'true' ).
          DATA(lv_password) = cl_http_utility=>decode_base64( CONV string( gv_webrfc_password ) ).
          zcl_wrfc_user=>login( iv_user     = CONV string( gv_webrfc_user )
                                iv_password = CONV string( lv_password ) ).
        ENDIF.
        DATA: lv_name  TYPE eu_lname.
*        lv_name = gt_query[ name = c_param_method ]-value.
        lv_name = gv_method.
        DATA(lo_function) = NEW zcl_bc_function_module( iv_method     = lv_name
                                                        it_parameters = lt_fm_parameters ).
        DATA(ls_results) = lo_function->call( ).

        DATA(lo_json) = NEW zcl_bc_json( ).
        lo_json->add( iv_name  = 'ABC'
                      iv_value = ls_results ).

        DATA(lo_jsonp) = NEW zcl_wrfc_jsonp( iv_callback    = gv_callback
                                             io_function    = lo_function
                                             is_results     = ls_results
                                             it_fields      = gt_fields
                                             it_messages    = gt_messages[]
                                             iv_from_rec    = gv_from_rec
                                             iv_to_rec      = gv_to_rec
                                             iv_camel_case  = gv_camel_case
                                             iv_escape_url  = gv_escape_url
                                             iv_webrfc_user = gv_webrfc_user
                                             iv_webrfc_log  = gv_webrfc_log ).
        lo_jsonp->build( ).
        gt_html[] = lo_jsonp->get_json_table( ).
        gv_json = lo_jsonp->get_json_string( ).
        DATA(lv_jsonpp) = lo_jsonp->get_json_string( ).

        DATA(ls_wrfc_logd_out) = VALUE zwrfc_logd( mandt     = sy-mandt
                                                   logid     = lv_logid
                                                   logcount  = 1
                                                   direction = 'O'
                                                   name      = 'JSON'
                                                   value     = lo_jsonp->get_json_string( ) ).
        INSERT zwrfc_logd FROM ls_wrfc_logd_out.
        COMMIT WORK.
      CATCH zcx_bc_exception INTO DATA(lx_exception).
        ls_wrfc_logt-msgtype = 'E'.
        ls_wrfc_logt-message = lx_exception->get_text( ).
        MODIFY zwrfc_logt FROM ls_wrfc_logt.

        add_message( iv_type = 'E' iv_msg = lx_exception->get_text( ) ).

        DATA(lo_jsonp_err) = NEW zcl_wrfc_jsonp( iv_callback   = gv_callback
                                                 io_function   = lo_function
                                                 is_results    = ls_results
                                                 it_fields     = gt_fields
                                                 it_messages   = gt_messages[]
                                                 iv_from_rec   = gv_from_rec
                                                 iv_to_rec     = gv_to_rec
                                                 iv_camel_case = gv_camel_case
                                                 iv_escape_url = gv_escape_url ).
        lo_jsonp_err->build_errors( ).
        gt_html[] = lo_jsonp_err->get_json_table( ).
        DATA(lv_jsonp) = lo_jsonp_err->get_json_string( ).
        rv_error = 1.

        ls_wrfc_logd_out = VALUE zwrfc_logd( mandt     = sy-mandt
                                 logid     = lv_logid
                                 logcount  = 1
                                 direction = 'O'
                                 name      = 'JSON'
                                 value     = lo_jsonp_err->get_json_string( ) ).
        INSERT zwrfc_logd FROM ls_wrfc_logd_out.
        COMMIT WORK.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( it_query = it_query ).

    TRY.
        gv_method = gt_query[ name = c_param_method ]-value.
      CATCH cx_root.
        MESSAGE s002(z_webrfc) INTO DATA(lv_msg).
        add_message( iv_type = 'E' iv_msg = lv_msg ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_function_interface.
    TRY.
        DATA: lt_fm_parameters TYPE zbc_ty_t_fm_parameters.
        REFRESH: lt_fm_parameters.
        LOOP AT gt_query INTO DATA(ls_query).
          DATA: ls_fm_parameters TYPE zbc_ty_s_fm_parameters.
          MOVE-CORRESPONDING ls_query TO ls_fm_parameters.
          APPEND ls_fm_parameters TO lt_fm_parameters.
        ENDLOOP.
        DATA: lv_name  TYPE eu_lname.
*        lv_name = gt_query[ name = c_param_method ]-value.
        lv_name = gv_method.
        DATA(lo_function) = NEW zcl_bc_function_module( iv_method     = lv_name
                                                        it_parameters = lt_fm_parameters ).
        DATA: ls_results TYPE zbc_ty_s_fm_results.
        CLEAR: ls_results.

        DATA(lo_jsonp) = NEW zcl_wrfc_jsonp( iv_callback = gv_callback
                                             io_function = lo_function
                                             is_results  = ls_results
                                             it_fields   = gt_fields
                                             it_messages = gt_messages[]
                                             iv_from_rec = gv_from_rec
                                             iv_to_rec   = gv_to_rec
                                             iv_camel_case = gv_camel_case
                                             iv_escape_url = gv_escape_url ).
        lo_jsonp->build_interface( ).
        gt_html[] = lo_jsonp->get_json_table( ).
        DATA(lv_jsonpp) = lo_jsonp->get_json_string( ).

      CATCH zcx_bc_exception INTO DATA(lx_exception).
        add_message( iv_type = 'E' iv_msg = lx_exception->get_text( ) ).

        DATA(lo_jsonp_err) = NEW zcl_wrfc_jsonp( iv_callback = gv_callback
                                                 io_function = lo_function
                                                 is_results  = ls_results
                                                 it_fields   = gt_fields
                                                 it_messages = gt_messages[]
                                                 iv_from_rec = gv_from_rec
                                                 iv_to_rec   = gv_to_rec
                                                 iv_camel_case = gv_camel_case
                                                 iv_escape_url = gv_escape_url ).
        lo_jsonp_err->build_errors( ).
        gt_html[] = lo_jsonp_err->get_json_table( ).
        DATA(lv_jsonp) = lo_jsonp_err->get_json_string( ).

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
