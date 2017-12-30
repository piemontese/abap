*&---------------------------------------------------------------------*
*& Report  ZPP_PDM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE zpp_sap_to_pdm_top.    " global Data
INCLUDE zpp_sap_to_pdm_sel.    " selections



INITIALIZATION.
  " istanzia la classe
  go_program = NEW #( iv_program = sy-repid ).

AT SELECTION-SCREEN OUTPUT.
  " modifica attributi screen
  go_program->modify_screen( ).

START-OF-SELECTION.
  " esegue programma
  go_program->execute( ).
  FREE go_program.
