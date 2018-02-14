*&---------------------------------------------------------------------*
*&  Include           ZFI_JPK_VAT_POLAND_SEL
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME TITLE text-t00.
PARAMETERS:     p_bukrs TYPE t001-bukrs OBLIGATORY.
SELECT-OPTIONS: s_date FOR gv_extract_date MODIF ID mc2 NO-EXTENSION.
PARAMETERS :    p_gjahr TYPE fipl_extractyear MODIF ID mc2.
SELECTION-SCREEN END OF BLOCK 000.

SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE text-t01.
PARAMETERS:     p_file TYPE string.
SELECTION-SCREEN END OF BLOCK 001.
PARAMETERS: p_show AS CHECKBOX DEFAULT abap_true,
            p_save AS CHECKBOX DEFAULT abap_true.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM select_file CHANGING p_file.
