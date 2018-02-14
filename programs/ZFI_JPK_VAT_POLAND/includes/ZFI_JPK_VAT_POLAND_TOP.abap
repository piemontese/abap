*&---------------------------------------------------------------------*
*&  Include           ZFI_JPK_VAT_POLAND_TOP
*&---------------------------------------------------------------------*

DATA: gv_extract_date    TYPE fipl_extractdate.

DATA: go_jpk_poland TYPE REF TO lcl_jpk_poland_3.

DATA: gt_fipld_vat_h   TYPE lcl_jpk_poland_3=>ty_t_fipld_vat_h,
      gt_fipld_vat_i   TYPE lcl_jpk_poland_3=>ty_t_fipld_vat_i,
      gt_fipld_vat_sum TYPE lcl_jpk_poland_3=>ty_t_fipld_vat_sum.
