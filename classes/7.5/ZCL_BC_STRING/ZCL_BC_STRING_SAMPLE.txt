*&---------------------------------------------------------------------*
*& Report ZCL_BC_CONVERSION_EXIT_SAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcl_bc_string_sample.

PARAMETERS: p_string TYPE string,
            p_sep    TYPE string..


START-OF-SELECTION.
  DATA: lo_string TYPE REF TO zcl_bc_string,
        lt_split  TYPE zcl_bc_string=>ty_t_string.

  TRY.
      lo_string = NEW #( iv_string = p_string ).
      lo_string->split( EXPORTING iv_sep    = p_sep
                        CHANGING  ct_split  = lt_split ).
    CATCH cx_root.
  ENDTRY.

  WRITE: / lo_string->length( ).
  LOOP AT lt_split INTO DATA(ls_split).
    WRITE: / ls_split.
  ENDLOOP.
  WRITE: / lo_string->add( ' jjgjhgjhg' ).

