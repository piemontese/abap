*&---------------------------------------------------------------------*
*& Report: ZPPP_ECTR_NORM
*& Tcode:  ZPPP_ECTR_NORM
*&---------------------------------------------------------------------*
*& Il programma a partire dai dati non ancora elaborati nella tabella
*& ZPP_MODBOM (ZPP_MODBOM-ZFLAG1 = X e ZPP_MODBOM-ZFLAG2 = SPACE) alimenta:
*& (Step 1) la tabella di riepilogo ZPP_PDM_NOR_BOM
*& (Step 2) le tabelle ZPP_PDM_MON_ANAG e ZPP_PDM_MON_BOM per
*& il Cruscotto Produzione.
*& Si ricorda che ZPPP_ECTR_NORM deve girare dopo il programma
*& ZPPP_ECTR_TO_SAP (che crea il file elaborato da ZPP_PDM_TO_SAP)
*& per generare le BOM di tipo 2.
*&---------------------------------------------------------------------*
REPORT zppp_ectr_norm.

INCLUDE zppp_ectr_norm_sel                    .    " selections
INCLUDE zppp_ectr_norm_class                  .    " classes
INCLUDE zppp_ectr_norm_top                    .    " global data

INITIALIZATION.
  go_program = NEW lcl_program( iv_program  = sy-repid
                                iv_pfstatus = 'STANDARD_FULLSCREEN').

AT SELECTION-SCREEN OUTPUT.
  TRY.
      go_program->modify_screen( ).
    CATCH cx_root INTO DATA(lcx_exception).
      MESSAGE lcx_exception->get_text( ) TYPE 'S'.
  ENDTRY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo1.
  go_program->set_variant( CHANGING cv_variant = p_layo1 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo2.
  go_program->set_variant( CHANGING cv_variant = p_layo2 ).

START-OF-SELECTION.
  TRY.

*     Esecuzione del programma
      go_program->run( ).   "<------

    CATCH cx_root INTO DATA(lcx_exception).
      MESSAGE lcx_exception->get_text( ) TYPE 'S'.
  ENDTRY.
