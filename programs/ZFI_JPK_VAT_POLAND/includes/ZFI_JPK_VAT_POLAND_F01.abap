*&---------------------------------------------------------------------*
*&  Include           ZFI_JPK_VAT_POLAND_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  __MAIN__
*&---------------------------------------------------------------------*
FORM __main__ .

  PERFORM get_data.
  PERFORM xml_build.
  PERFORM xlm_show.
  PERFORM xml_download.

ENDFORM.                    " __MAIN__

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data .

  PERFORM get_fipld_vat_h.
  PERFORM get_fipld_vat_i.
  PERFORM get_fipld_vat_sum.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  XLM_SHOW
*&---------------------------------------------------------------------*
FORM xlm_show.

  CHECK p_show = abap_true.

  DATA: lx_root TYPE REF TO cx_root.
  TRY.
      go_jpk_poland->get_document( )->show( ).
    CATCH cx_root INTO lx_root.
      DATA: lv_msg TYPE string.
      lv_msg = lx_root->get_text( ).
      MESSAGE i390(00) WITH lv_msg.
  ENDTRY.

ENDFORM.                    " XLM_SHOW

*&---------------------------------------------------------------------*
*&      Form  XML_DOWNLOAD
*&---------------------------------------------------------------------*
FORM xml_download .

  CHECK p_save = abap_true.

  DATA: lx_root TYPE REF TO cx_root.
  TRY.
      go_jpk_poland->get_document( )->download( iv_filename = p_file ).
    CATCH cx_root INTO lx_root.
      DATA: lv_msg TYPE string.
      lv_msg = lx_root->get_text( ).
      MESSAGE i390(00) WITH lv_msg.
  ENDTRY.

ENDFORM.                    " XML_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  XML_BUILD
*&---------------------------------------------------------------------*
FORM xml_build .

  FREE go_jpk_poland.

  DATA: lx_root TYPE REF TO cx_root.
  TRY.
      CREATE OBJECT go_jpk_poland
        EXPORTING
          iv_rootname = 'JPK'.

*      " Naglowek node
*      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'Naglowek' iv_value = '' ).
*
*      " Podmiot1 node
*      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'Podmiot1' iv_value = '' ).
*
*      " SprzedazWiersz node
*      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'SprzedazWiersz' iv_value = '' ).
*
*      " SprzedazCtrl node
*      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'SprzedazCtrl' iv_value = '' ).
*
*      " ZakupWiersz node
*      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'ZakupWiersz' iv_value = '' ).
*
*      " ZakupCtrl node
*      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'ZakupCtrl' iv_value = '' ).

      PERFORM xml_add_data.

      go_jpk_poland->build_dom( ).
    CATCH cx_root INTO lx_root.
      DATA: lv_msg TYPE string.
      lv_msg = lx_root->get_text( ).
      MESSAGE i390(00) WITH lv_msg.
  ENDTRY.


ENDFORM.                    " XML_BUILD

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE
*&---------------------------------------------------------------------*
FORM select_file  CHANGING cv_file TYPE string.

  DATA: lt_file_table TYPE filetable,
        ls_file_table TYPE file_table,
        lv_rc         TYPE i.

  REFRESH: lt_file_table.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select file'
      default_extension       = 'xml'
      default_filename        = 'JPK_VAT.xml'
*     file_filter             =
*     with_encoding           =
      initial_directory       = 'C:\JPK_VAT'
*     multiselection          =
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
*     user_action             =
*     file_encoding           =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE lt_file_table INTO ls_file_table INDEX 1.

  cv_file = ls_file_table-filename.

ENDFORM.                    " SELECT_FILE

*&---------------------------------------------------------------------*
*&      Form  GET_FIPLD_VAT_H
*&---------------------------------------------------------------------*
FORM get_fipld_vat_h .

  REFRESH: gt_fipld_vat_h.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_fipld_vat_h
           FROM fipld_vat_h
           WHERE bukrs       EQ p_bukrs
           AND   gjahr       EQ p_gjahr
           AND   vatdate     IN s_date.

ENDFORM.                    " GET_FIPLD_VAT_H

*&---------------------------------------------------------------------*
*&      Form  GET_FIPLD_VAT_I
*&---------------------------------------------------------------------*
FORM get_fipld_vat_i .

  REFRESH: gt_fipld_vat_i.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_fipld_vat_i
           FROM fipld_vat_i
           FOR ALL ENTRIES IN gt_fipld_vat_h
           WHERE bukrs    = gt_fipld_vat_h-bukrs
           AND   belnr    = gt_fipld_vat_h-belnr
           AND   gjahr    = gt_fipld_vat_h-gjahr
           AND   ext_uuid = gt_fipld_vat_h-ext_uuid..

ENDFORM.                    " GET_FIPLD_VAT_I

*&---------------------------------------------------------------------*
*&      Form  GET_FIPLD_VAT_SUM
*&---------------------------------------------------------------------*
FORM get_fipld_vat_sum .

  REFRESH: gt_fipld_vat_sum.

  DATA: ls_fipld_vat_h TYPE lcl_jpk_poland_3=>ty_s_fipld_vat_h.

  READ TABLE gt_fipld_vat_h INTO ls_fipld_vat_h INDEX 1.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_fipld_vat_sum
         FROM fipld_vat_sum
         WHERE bukrs    = ls_fipld_vat_h-bukrs
         AND   gjahr    = ls_fipld_vat_h-gjahr
         AND   ext_uuid = ls_fipld_vat_h-ext_uuid..


ENDFORM.                    " GET_FIPLD_VAT_SUM

*&---------------------------------------------------------------------*
*&      Form  XML_ADD_DATA
*&---------------------------------------------------------------------*
FORM xml_add_data .

  DATA: ls_fipld_vat_h   TYPE lcl_jpk_poland_3=>ty_s_fipld_vat_h,
        ls_fipld_vat_i   TYPE lcl_jpk_poland_3=>ty_s_fipld_vat_i,
        ls_fipld_vat_sum TYPE lcl_jpk_poland_3=>ty_s_fipld_vat_sum,
        lv_char          TYPE char100,
        lv_name          TYPE string,
        lv_value         TYPE string.

  LOOP AT gt_fipld_vat_h INTO ls_fipld_vat_h.

    IF ( NOT ls_fipld_vat_h-customer_id IS INITIAL ).
      " SprzedazWiersz node
      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'SprzedazWiersz' iv_value = '' ).
      " SprzedazWiersz childs
      go_jpk_poland->insert_element( iv_parent = 'SprzedazWiersz' iv_name = 'NazwaKontrahenta' iv_value = lcl_jpk_poland_3=>char2string( ls_fipld_vat_h-customer_id ) ).
      go_jpk_poland->insert_element( iv_parent = 'SprzedazWiersz' iv_name = 'DowodSprzedazy' iv_value = lcl_jpk_poland_3=>char2string( ls_fipld_vat_h-doc_number )  ).
      go_jpk_poland->insert_element( iv_parent = 'SprzedazWiersz' iv_name = 'DataWystawienia' iv_value = lcl_jpk_poland_3=>format_date( ls_fipld_vat_h-invoice_date ) ).
      go_jpk_poland->insert_element( iv_parent = 'SprzedazWiersz' iv_name = 'DataSprzedazy' iv_value = lcl_jpk_poland_3=>format_date( ls_fipld_vat_h-delivery_date ) ).

    ENDIF.

    IF ( NOT ls_fipld_vat_h-suplier_id IS INITIAL ).
      " ZakupWiersz node
      go_jpk_poland->insert_element( iv_parent = '' iv_name = 'ZakupWiersz' iv_value = '' ).
      " ZakupWiersz childs
      go_jpk_poland->insert_element( iv_parent = 'ZakupWiersz' iv_name = 'NazwaDostawcy' iv_value = lcl_jpk_poland_3=>char2string( ls_fipld_vat_h-suplier_id ) ).
      go_jpk_poland->insert_element( iv_parent = 'ZakupWiersz' iv_name = 'DowodZakupu' iv_value = lcl_jpk_poland_3=>char2string( ls_fipld_vat_h-doc_number ) ).
      go_jpk_poland->insert_element( iv_parent = 'ZakupWiersz' iv_name = 'DataZakupu' iv_value = lcl_jpk_poland_3=>format_date( ls_fipld_vat_h-invoice_date ) ).
      go_jpk_poland->insert_element( iv_parent = 'ZakupWiersz' iv_name = 'DataWplywu' iv_value = lcl_jpk_poland_3=>format_date( ls_fipld_vat_h-reindat ) ).
    ENDIF.

    LOOP AT gt_fipld_vat_i INTO ls_fipld_vat_i WHERE bukrs    = ls_fipld_vat_h-bukrs
                                               AND   belnr    = ls_fipld_vat_h-belnr
                                               AND   gjahr    = ls_fipld_vat_h-gjahr
                                               AND   ext_uuid = ls_fipld_vat_h-ext_uuid.

      IF ( NOT ls_fipld_vat_h-customer_id IS INITIAL ).
        " SprzedazWiersz childs
        CASE ls_fipld_vat_i-taxcode_off.
          WHEN 'K_10' OR 'K_11' OR 'K_12' OR 'K_13' OR 'K_14' OR 'K_15' OR 'K_17' OR 'K_19' OR 'K_21' OR
               'K_22' OR 'K_23' OR 'K_25' OR 'K_27' OR 'K_29' OR 'K_31' OR 'K_32' OR 'K_34' OR 'K_43' OR
               'K_45'.
            CHECK NOT ls_fipld_vat_i-tax_base IS INITIAL.
            lv_value = ls_fipld_vat_i-tax_base.

          WHEN 'K_16' OR 'K_18' OR 'K_20' OR 'K_24' OR 'K_26' OR 'K_28' OR 'K_30' OR 'K_33' OR 'K_35' OR
               'K_36' OR 'K_38' OR 'K_39' OR 'K_44' OR 'K_46'.
            CHECK NOT ls_fipld_vat_i-tax_amount IS INITIAL.
            lv_value = ls_fipld_vat_i-tax_amount.

        ENDCASE.

        " SprzedazWiersz childs
        go_jpk_poland->insert_element( iv_parent = 'SprzedazWiersz' iv_name = lcl_jpk_poland_3=>char2string( ls_fipld_vat_i-taxcode_off ) iv_value = lv_value ).
      ENDIF.

      IF ( NOT ls_fipld_vat_h-suplier_id IS INITIAL ).
        " ZakupWiersz childs
        CASE ls_fipld_vat_i-taxcode_off.
          WHEN 'K_10' OR 'K_11' OR 'K_12' OR 'K_13' OR 'K_14' OR 'K_15' OR 'K_17' OR 'K_19' OR 'K_21' OR
               'K_22' OR 'K_23' OR 'K_25' OR 'K_27' OR 'K_29' OR 'K_31' OR 'K_32' OR 'K_34' OR 'K_43' OR
               'K_45'.
            CHECK NOT ls_fipld_vat_i-tax_base IS INITIAL.
            lv_value = ls_fipld_vat_i-tax_base.

          WHEN 'K_16' OR 'K_18' OR 'K_20' OR 'K_24' OR 'K_26' OR 'K_28' OR 'K_30' OR 'K_33' OR 'K_35' OR
               'K_36' OR 'K_38' OR 'K_39' OR 'K_44' OR 'K_46'.
            CHECK NOT ls_fipld_vat_i-tax_amount IS INITIAL.
            lv_value = ls_fipld_vat_i-tax_amount.

        ENDCASE.

        " SprzedazWiersz childs
        go_jpk_poland->insert_element( iv_parent = 'ZakupWiersz' iv_name = lcl_jpk_poland_3=>char2string( ls_fipld_vat_i-taxcode_off ) iv_value = lv_value ).
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " XML_ADD_DATA
