*&---------------------------------------------------------------------*
*& Report ZBC_PROGRAM_SAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwrfc_maintain_users.

INCLUDE zwrfc_maintain_users_sel.          " selections
INCLUDE zwrfc_maintain_users_class.        " classes
INCLUDE zwrfc_maintain_users_top.          " global data


INITIALIZATION.
  go_program = NEW lcl_program( iv_program  = sy-repid
                                iv_pfstatus = 'STANDARD_FULLSCREEN' ).

AT SELECTION-SCREEN OUTPUT.
  TRY.
      go_program->modify_screen( ).
    CATCH cx_root INTO DATA(lcx_exception).
      MESSAGE lcx_exception->get_text( ) TYPE 'S'.
  ENDTRY.

START-OF-SELECTION.
  TRY.

*     Esecuzione del programma
      go_program->run( ).
      DATA: lt_data TYPE zcl_wrfc_maintain_users=>ty_t_data.
      go_program->get_data( IMPORTING et_data = lt_data ).

    CATCH cx_root INTO DATA(lcx_exception).
      MESSAGE lcx_exception->get_text( ) TYPE 'S'.
  ENDTRY.
