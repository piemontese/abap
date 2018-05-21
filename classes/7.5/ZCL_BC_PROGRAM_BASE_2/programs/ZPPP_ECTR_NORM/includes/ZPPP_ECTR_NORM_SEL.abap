*&---------------------------------------------------------------------*
*&  Include           ZPPP_ECTR_TO_SAP_SEL
*&---------------------------------------------------------------------*

TABLES: marc, zpp_albero, prps.

SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME.
SELECT-OPTIONS: s_dispo FOR marc-dispo,
                s_root  FOR zpp_albero-rootname,
                s_matnr FOR marc-matnr,
                s_posid FOR prps-posid.
SELECTION-SCREEN END OF BLOCK 000.
PARAMETERS: p_test AS CHECKBOX DEFAULT abap_true.
