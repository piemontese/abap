*&---------------------------------------------------------------------*
*& Report  ZPP_PDM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE zpp_pdm_to_sap_top.     " global Data
INCLUDE zpp_pdm_to_sap_sel.     " selections

INITIALIZATION.
  " istanzia alv
  go_salv = NEW #( iv_program = sy-repid ).
  " istanzia la classe
  go_program = NEW #( iv_program      = sy-repid
                      iv_pfstatus     = 'MAIN'
                      iv_pfstatus_log = 'LOG' ).
  go_program->set_salv( io_salv = go_salv ).

AT SELECTION-SCREEN OUTPUT.

*DP-07_10_2016 Inizio
  CASE abap_true.
    WHEN p_mat.
      CLEAR p_dfill.
      CLEAR p_dlabl.
      CLEAR p_dpack.
      REFRESH s_matnr1.
*     IF sy-uname <> 'KOSMEDEV'.
      s_matnr1-sign   = 'I'.
      s_matnr1-option = 'CP'.
      s_matnr1-low    = '5*'.
*       s_matnr1-high   = '5*'.
      APPEND s_matnr1.
      s_matnr1-sign   = 'I'.
      s_matnr1-option = 'CP'.
      s_matnr1-low    = '8*'.
*      s_matnr1-high   = '8*'.
      APPEND s_matnr1.
*     ENDIF.   "IF sy-uname <> 'KOSMEDEV'.
    WHEN p_bom.
      CLEAR s_matnr1.
      REFRESH s_matnr1.
      CLEAR p_mfill.
      CLEAR p_mlabl.
      CLEAR p_mpack.
  ENDCASE.
*DP-07_10_2016 Fine

  " modifica attributi screen
  go_program->modify_screen( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  go_program->get_salv( )->set_variant( CHANGING cv_variant = p_layout ).

START-OF-SELECTION.
  " esegue programma
  go_program->execute( ).
  FREE go_program.
