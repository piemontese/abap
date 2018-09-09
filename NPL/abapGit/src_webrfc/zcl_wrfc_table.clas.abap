class ZCL_WRFC_TABLE definition
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
  methods GET_DATA .
protected section.
private section.

  data GV_TABLE type TABLENAME .
  data GV_WHERE type STRING .
ENDCLASS.



CLASS ZCL_WRFC_TABLE IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    super->constructor( it_query = it_query ).

    TRY.
        gv_table = gt_query[ name = c_param_sqltable ]-value.
      CATCH cx_root.
        MESSAGE s002(z_webrfc) INTO DATA(lv_msg).
        add_message( iv_type = 'E' iv_msg = lv_msg ).
    ENDTRY.

    TRY.
        gv_where = gt_query[ name = c_param_sqlwhere ]-value.
      CATCH cx_root.
    ENDTRY.

*    TRY.
*        gv_fields = gt_query[ name = c_param_fields ]-value.
*      CATCH cx_root.
*    ENDTRY.
  ENDMETHOD.


  METHOD get_data.
*    TRY.
*        DATA: lt_fm_parameters TYPE zbc_ty_t_fm_parameters.
*        REFRESH: lt_fm_parameters.
*        LOOP AT gt_query INTO DATA(ls_query).
*          DATA: ls_fm_parameters TYPE zbc_ty_s_fm_parameters.
*          MOVE-CORRESPONDING ls_query TO ls_fm_parameters.
*          APPEND ls_fm_parameters TO lt_fm_parameters.
*        ENDLOOP.
*        DATA: lv_name  TYPE eu_lname.
**        lv_name = gt_query[ name = c_param_method ]-value.
*        lv_name = gv_method.
*        DATA(lo_function) = NEW zcl_bc_function_module( iv_method     = lv_name
*                                                        it_parameters = lt_fm_parameters ).
*        DATA: ls_results TYPE zbc_ty_s_fm_results.
*        CLEAR: ls_results.
*
*        DATA(lo_jsonp) = NEW zcl_wrfc_jsonp( iv_callback = gv_callback
*                                             io_function = lo_function
*                                             is_results  = ls_results
*                                             it_fields   = gt_fields
*                                             it_messages = gt_messages[]
*                                             iv_from_rec = gv_from_rec
*                                             iv_to_rec   = gv_to_rec
*                                             iv_camel_case = gv_camel_case
*                                             iv_escape_url = gv_escape_url ).
*        lo_jsonp->build_interface( ).
*        gt_html[] = lo_jsonp->get_json_table( ).
*        DATA(lv_jsonpp) = lo_jsonp->get_json_string( ).
*
*      CATCH zcx_bc_exception INTO DATA(lx_exception).
*        add_message( iv_type = 'E' iv_msg = lx_exception->get_text( ) ).
*
*        DATA(lo_jsonp_err) = NEW zcl_wrfc_jsonp( iv_callback = gv_callback
*                                                 io_function = lo_function
*                                                 is_results  = ls_results
*                                                 it_fields   = gt_fields
*                                                 it_messages = gt_messages[]
*                                                 iv_from_rec = gv_from_rec
*                                                 iv_to_rec   = gv_to_rec
*                                                 iv_camel_case = gv_camel_case
*                                                 iv_escape_url = gv_escape_url ).
*        lo_jsonp_err->build_errors( ).
*        gt_html[] = lo_jsonp_err->get_json_table( ).
*        DATA(lv_jsonp) = lo_jsonp_err->get_json_string( ).
*
*    ENDTRY.
  ENDMETHOD.
ENDCLASS.
