*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZWRFC_CALL_PARAM
*   generation date: 21.07.2018 at 13:11:46
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZWRFC_CALL_PARAM   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
