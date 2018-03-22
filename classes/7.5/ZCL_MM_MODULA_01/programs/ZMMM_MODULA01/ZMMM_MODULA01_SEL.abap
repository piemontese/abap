*&---------------------------------------------------------------------*
*&  Include           ZMMM_MOD_MONITOR_SEL
*&---------------------------------------------------------------------*

TABLES: mara, marc.

SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME.
PARAMETERS: p_bukrs TYPE t001-bukrs OBLIGATORY MODIF ID bur,
            p_werks TYPE marc-werks OBLIGATORY MODIF ID wer,
            p_lgort TYPE mard-lgort OBLIGATORY MATCHCODE OBJECT h_t001l MODIF ID lgo,
            p_lgpbe TYPE mard-lgpbe OBLIGATORY MODIF ID lgp.
SELECT-OPTIONS: s_matnr FOR marc-matnr MODIF ID mat, " OBLIGATORY,
                s_mtart FOR mara-mtart NO-DISPLAY,
                s_matkl FOR mara-matkl NO-DISPLAY,
                s_ekgrp FOR marc-ekgrp NO-DISPLAY,
                s_dispo FOR marc-dispo NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK 000.

SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME.
PARAMETERS: p_sendfi AS CHECKBOX DEFAULT abap_false,     " Invio file (default space)
            p_updubi AS CHECKBOX DEFAULT space,         " aggiorna ubicazioni (default blank)
            p_canubi AS CHECKBOX USER-COMMAND usr,      " cancella ubicazioni ( solo stock O)
            p_sendde AS CHECKBOX MODIF ID del.          " invia anagrafiche cancellate (solo se p_canubi = X)
SELECTION-SCREEN END OF BLOCK 001.
