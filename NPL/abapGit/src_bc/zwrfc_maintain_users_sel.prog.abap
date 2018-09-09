*&---------------------------------------------------------------------*
*& Include          ZBC_GET_DUMPS_SEL
*&---------------------------------------------------------------------*

TABLES: rsdumpinfo.

SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME.
SELECT-OPTIONS: s_date FOR rsdumpinfo-sydate,
                s_time FOR rsdumpinfo-sytime,
                s_host FOR rsdumpinfo-syhost,
                s_user FOR rsdumpinfo-syuser.
SELECTION-SCREEN END OF BLOCK 000.
