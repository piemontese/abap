*&---------------------------------------------------------------------*
*&  Include           ZFI_JPK_VAT_POLAND_CLASS
*&---------------------------------------------------------------------*

INCLUDE zbc_xml.                        " class xml

*----------------------------------------------------------------------*
*       CLASS lcl_jpk_poland_3 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_jpk_poland_3 DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_attributes,
             node TYPE string,
             name TYPE string,
             value TYPE string,
           END OF ty_s_attributes,
           BEGIN OF ty_s_elements,
             parent TYPE string,
             name   TYPE string,
             value  TYPE string,
             prefix TYPE string,
           END OF ty_s_elements,
            ty_s_fipld_vat_h   TYPE fipld_vat_h,
            ty_s_fipld_vat_i   TYPE fipld_vat_i,
            ty_s_fipld_vat_sum TYPE fipld_vat_sum,
            ty_t_attributes    TYPE TABLE OF ty_s_attributes,
            ty_t_elements      TYPE TABLE OF ty_s_elements,
            ty_t_fipld_vat_h   TYPE TABLE OF fipld_vat_h,
            ty_t_fipld_vat_i   TYPE TABLE OF fipld_vat_i,
            ty_t_fipld_vat_sum TYPE TABLE OF fipld_vat_sum.


    METHODS: constructor IMPORTING iv_rootname   TYPE string
                                   iv_namespace  TYPE string DEFAULT space
                                   iv_prefix     TYPE string DEFAULT space
                                   it_elements   TYPE ty_t_elements OPTIONAL
                                   it_attributes TYPE ty_t_attributes OPTIONAL,
             insert_attribute IMPORTING iv_node TYPE string
                                        iv_name   TYPE string
                                        iv_value  TYPE string,
             insert_element IMPORTING iv_parent TYPE string
                                      iv_name   TYPE string
                                      iv_value  TYPE string,
             build_dom,
             get_document RETURNING value(ro_document) TYPE REF TO lcl_xml.

    CLASS-METHODS:
                 char2string IMPORTING iv_char TYPE c
                             RETURNING value(rv_string) TYPE string,
                 format_date IMPORTING iv_date TYPE datum
                             RETURNING value(rv_formatted_date) TYPE string.

  PRIVATE SECTION.
    DATA: go_xml TYPE REF TO lcl_xml,
          gt_attributes TYPE ty_t_attributes,
          gt_elements TYPE ty_t_elements.
ENDCLASS.                    "lcl_jpk_poland_3 DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_jpk_poland_3 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_jpk_poland_3 IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT me->go_xml
      EXPORTING
        iv_rootname = 'JPK'.

    me->go_xml->add_root_attribute( iv_name = 'xmnls' iv_value = 'http://jpk.mf.gov.pl/wzor/2017/11/13/1113/' ).

    IF ( NOT it_elements[] IS INITIAL ).
      gt_elements[] = it_elements[].
    ENDIF.

    IF ( NOT it_attributes[] IS INITIAL ).
      gt_attributes[] = it_attributes[].
    ENDIF.

  ENDMETHOD.                    "constructor

  METHOD insert_attribute.
    DATA: ls_attributes TYPE me->ty_s_attributes.

    CLEAR: ls_attributes.
    ls_attributes-node  = iv_node.
    ls_attributes-name  = iv_name.
    ls_attributes-value = iv_value.
    APPEND ls_attributes TO me->gt_attributes.
  ENDMETHOD.                    "insert_attribute

  METHOD build_dom.
    DATA: ls_elements TYPE me->ty_s_elements,
          ls_attributes TYPE me->ty_s_attributes.
    LOOP AT me->gt_elements INTO ls_elements.
      IF ( ls_elements-value IS INITIAL ).
        me->go_xml->add_node( iv_parent = ls_elements-parent
                              iv_name   = ls_elements-name ).
      ELSE.
        me->go_xml->add_child( iv_name   = ls_elements-name
                               iv_value  = ls_elements-value ).
      ENDIF.
      LOOP AT me->gt_attributes INTO ls_attributes WHERE node = ls_elements-name.
        me->go_xml->add_attribute( iv_node  = ls_attributes-node
                                   iv_name  = ls_attributes-name
                                   iv_value = ls_attributes-value ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.                    "build_dom

  METHOD char2string.
    rv_string = iv_char.
  ENDMETHOD.                    "char2string

  METHOD format_date.
    IF ( NOT iv_date IS INITIAL ).
      rv_formatted_date = iv_date(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).
    ENDIF.
  ENDMETHOD.                    "format_date

  METHOD get_document.
    ro_document = me->go_xml.
  ENDMETHOD.                    "get_document

  METHOD insert_element.
    DATA: ls_elements TYPE me->ty_s_elements.

    CLEAR: ls_elements.
    ls_elements-parent = iv_parent.
    ls_elements-name   = iv_name.
    ls_elements-value  = iv_value.
    APPEND ls_elements TO me->gt_elements.
  ENDMETHOD.                    "insert_element
ENDCLASS.                    "lcl_jpk_poland_3 IMPLEMENTATION
