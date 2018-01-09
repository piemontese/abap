FUNCTION Z_WRFC_TEST .
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  IMPORTING
*"     VALUE(IT_MATNR) TYPE  RANGES_MATNR OPTIONAL
*"     VALUE(IS_MATNR2) TYPE  RANGE_MATNR OPTIONAL
*"  EXPORTING
*"     VALUE(ES_MATNR2) TYPE  RANGE_MATNR
*"  TABLES
*"      ET_MATNR STRUCTURE  RANGE_MATNR OPTIONAL
*"----------------------------------------------------------------------


  ET_MATNR[] = IT_MATNR[].
  ES_MATNR2 = IS_MATNR2.


ENDFUNCTION.
