*&---------------------------------------------------------------------*
*& Report ZCL_BC_CONVERSION_EXIT_SAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcl_bc_conversion_exit_sample.



START-OF-SELECTION.
  TYPES: BEGIN OF ty_s_data,
           meins  TYPE meins,
           s_mara TYPE mara,
           t_mara TYPE wrf_mara_tty,
         END OF ty_s_data.
  DATA: lv_meins_in TYPE meins VALUE 'ST',
        ls_mara_in  TYPE mara,
        lt_mara_in  TYPE TABLE OF mara,
        ls_data_in  type ty_s_data.

  DATA: lv_meins_out TYPE c LENGTH 3,
        ls_mara_out  TYPE mara,
        lt_mara_out  TYPE TABLE OF mara,
        ls_data_out  type ty_s_data.


  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_mara_in FROM mara.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_mara_in FROM mara UP TO 2 ROWS.

  ls_data_in-meins  = lv_meins_in.
  ls_data_in-s_mara = ls_mara_in.
  ls_data_in-t_mara = lt_mara_in.


  REFRESH: lt_mara_out.
  clear: ls_data_out.

  TRY.
      zcl_bc_conversion_exit=>conversion_output( EXPORTING iv_field = lv_meins_in
                                                 CHANGING  cv_field = lv_meins_out ).
      zcl_bc_conversion_exit=>conversion_output( EXPORTING iv_field = ls_mara_in
                                                 CHANGING  cv_field = ls_mara_out ).
      zcl_bc_conversion_exit=>conversion_output( EXPORTING iv_field = lt_mara_in[]
                                                 CHANGING  cv_field = lt_mara_out[] ).
      zcl_bc_conversion_exit=>conversion_output( EXPORTING iv_field = ls_data_in
                                                 CHANGING  cv_field = ls_data_out ).
*      WRITE: / 'lv_meins_in = ', lv_meins_in, '  lv_meins_out = ', lv_meins_out.
    CATCH cx_sy_move_cast_error.
  ENDTRY.
