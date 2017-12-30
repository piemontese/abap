*&---------------------------------------------------------------------*
*&  Include           ZPP_PDM_SEL
*&---------------------------------------------------------------------*
TABLES: mara, marc, mvke.

SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME TITLE TEXT-s00.
PARAMETERS: p_full  TYPE zcl_pp_sap_to_pdm=>ty_s_sel-p_full RADIOBUTTON GROUP rad     " estrazione full
                                                     USER-COMMAND usr DEFAULT 'X',
            p_delta TYPE zcl_pp_sap_to_pdm=>ty_s_sel-p_delta RADIOBUTTON GROUP rad.    " estrazione delta
SELECTION-SCREEN END OF BLOCK 000.

SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-s01.
PARAMETERS: p_tofill TYPE zcl_pp_sap_to_pdm=>ty_s_sel-p_tofill,   " invio anagrafica SAP -> PDM Filling
            p_tolabl TYPE zcl_pp_sap_to_pdm=>ty_s_sel-p_tolabl,   " invio anagrafica SAP -> PDM Labelling
            p_topack TYPE zcl_pp_sap_to_pdm=>ty_s_sel-p_topack.   " invio anagrafica SAP -> PDM Packaging
SELECTION-SCREEN END OF BLOCK 001.

SELECTION-SCREEN BEGIN OF BLOCK 002 WITH FRAME TITLE TEXT-s02.
SELECT-OPTIONS:  s_werks  FOR marc-werks DEFAULT '6301',
                 s_matnr  FOR mara-matnr,
                 s_mstae  FOR mara-mstae,
                 s_mmsta  FOR marc-mmsta,
                 s_dispo  FOR marc-dispo,
                 s_laeda  FOR mara-laeda MODIF ID lae,
*                 s_mstav  FOR mara-mstav. "DP-06_10_2016
                 s_vmsta  FOR mvke-vmsta. "DP-06_10_2016
SELECTION-SCREEN END OF BLOCK 002.
