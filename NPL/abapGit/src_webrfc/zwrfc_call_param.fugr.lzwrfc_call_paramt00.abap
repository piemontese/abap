*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 21.07.2018 at 12:58:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZWRFC_CALL_PARAM................................*
DATA:  BEGIN OF STATUS_ZWRFC_CALL_PARAM              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWRFC_CALL_PARAM              .
CONTROLS: TCTRL_ZWRFC_CALL_PARAM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWRFC_CALL_PARAM              .
TABLES: ZWRFC_CALL_PARAM               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
