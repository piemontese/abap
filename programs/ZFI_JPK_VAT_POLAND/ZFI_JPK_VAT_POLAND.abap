*&---------------------------------------------------------------------*
*& Report  ZFI_JPK_VAT_POLAND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfi_jpk_vat_poland.

INCLUDE zfi_jpk_vat_poland_class.       " xml class
INCLUDE zfi_jpk_vat_poland_top.         " data declarations
INCLUDE zfi_jpk_vat_poland_sel.         " selections
INCLUDE zfi_jpk_vat_poland_f01.         " forms




START-OF-SELECTION.
  PERFORM __main__.
