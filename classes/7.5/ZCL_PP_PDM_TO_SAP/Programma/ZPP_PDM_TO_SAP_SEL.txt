*&---------------------------------------------------------------------*
*&  Include           ZPP_PDM_SEL
*&---------------------------------------------------------------------*
TABLES: mara, marc.

* TEXT-s00 Selezioni per lettura file
SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME TITLE TEXT-s00.
* Ricezione anagrafica materiale
PARAMETERS: p_mat  RADIOBUTTON GROUP rad USER-COMMAND rad DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK 002 WITH FRAME.
SELECT-OPTIONS: s_matnr1 FOR mara-matnr MODIF ID mat NO-DISPLAY.
PARAMETERS: p_mfill TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_mfill MODIF ID mat,   " invio anagrafica SAP -> PDM Filling
            p_mlabl TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_mlabl MODIF ID mat,   " invio anagrafica SAP -> PDM Labelling
            p_mpack TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_mpack MODIF ID mat.   " invio anagrafica SAP -> PDM Packaging
SELECTION-SCREEN END OF BLOCK 002.
* Ricezione distinte base
PARAMETERS: p_bom  RADIOBUTTON GROUP rad.
SELECTION-SCREEN BEGIN OF BLOCK 003 WITH FRAME.
PARAMETERS: p_dfill  TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_dfill MODIF ID bom,   " invio distinte base PDM -> SAP Filling
            p_dlabl  TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_dlabl MODIF ID bom,   " invio distinte base PDM -> SAP  Labelling
            p_dpack  TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_dpack MODIF ID bom,   " invio distinte base PDM -> SAP Packaging
            p_dtbase TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_dtbase MODIF ID bom
                                   DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK 003.
PARAMETERS: p_layout TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_layout.
SELECTION-SCREEN END OF BLOCK 000.

* TEXT-s01 Selezioni per rielaborazione degli errori
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-s01.
PARAMETERS: p_err   TYPE zcl_pp_pdm_to_sap=>ty_s_sel-p_err USER-COMMAND usr.    " rielaborazione errori
SELECT-OPTIONS:  s_matnr FOR mara-matnr MODIF ID err,
                 s_datum FOR sy-datum MODIF ID err,
                 s_uzeit FOR sy-uzeit MODIF ID err.
SELECTION-SCREEN END OF BLOCK 001.
