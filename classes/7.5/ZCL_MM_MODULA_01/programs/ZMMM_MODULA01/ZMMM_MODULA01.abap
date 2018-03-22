*&---------------------------------------------------------------------*
*& Report  ZMMM_MOD_MONITOR
*&
*&---------------------------------------------------------------------*
*&
*& Tcode:  ZMMM_MODULA01          Esportazione anagrafica materiale
*& Classe: ZCL_MM_MODULA_01
*&
*&---------------------------------------------------------------------*
REPORT zmmm_modula01.

INCLUDE zmmm_modula01_sel                    .    " selections
INCLUDE zmmm_modula01_class                  .    " classes
INCLUDE zmmm_modula01_top                    .    " global data


INITIALIZATION.
  go_program = NEW lcl_program( iv_program  = sy-repid
                                iv_pfstatus = 'STANDARD_FULLSCREEN').

AT SELECTION-SCREEN OUTPUT.
  TRY.
      go_program->modify_screen( ).
    CATCH cx_root INTO DATA(lcx_exception).
      MESSAGE lcx_exception->get_text( ) TYPE 'S'.
  ENDTRY.

START-OF-SELECTION.
  TRY.
      go_program->run( ).
    CATCH cx_root INTO DATA(lcx_exception).
      MESSAGE lcx_exception->get_text( ) TYPE 'S'.
  ENDTRY.
