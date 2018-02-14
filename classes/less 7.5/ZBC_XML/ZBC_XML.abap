*&---------------------------------------------------------------------*
*&  Include           ZBC_XML
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_rootname  TYPE string
                                   iv_namespace TYPE string DEFAULT space
                                   iv_prefix    TYPE string DEFAULT space,
             get_document RETURNING value(rv_document) TYPE REF TO if_ixml_document,
             add_node     IMPORTING iv_parent          TYPE string OPTIONAL
                                    iv_name            TYPE string,
             add_child    IMPORTING iv_parent          TYPE string OPTIONAL
                                    iv_name            TYPE string
                                    iv_value           TYPE string,
             add_root_attribute IMPORTING iv_name      TYPE string
                                          iv_value     TYPE string,
             add_attribute IMPORTING iv_node           TYPE string
                                     iv_name           TYPE string
                                     iv_value          TYPE string,
             download IMPORTING iv_filename            TYPE string,
             show.
  PRIVATE SECTION.
    METHODS: convert_dom_to_xml IMPORTING io_dom        TYPE REF TO if_ixml_document
                                RETURNING value(rv_xml) TYPE string.

    TYPES : my_type TYPE c LENGTH 60.
    CONSTANTS mc_character_set TYPE string VALUE 'TEST*'.   "#EC NOTEXT
    DATA: gv_rootname  TYPE string,
          gv_namespace TYPE string,
          gv_prefix    TYPE string.
    DATA: go_ixml TYPE REF TO if_ixml,
          go_ixml_document TYPE REF TO if_ixml_document,
          go_ixml_ostream TYPE REF TO if_ixml_ostream,
          go_ixml_renderer TYPE REF TO if_ixml_renderer,
          go_ixml_encoding TYPE REF TO if_ixml_encoding,
          go_ixml_node_parent TYPE REF TO if_ixml_node,
          go_ixml_element TYPE REF TO if_ixml_element,
          go_ixml_element_sub TYPE REF TO if_ixml_element .
ENDCLASS.               "LCL_XML

*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_xml
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_xml IMPLEMENTATION.
  METHOD constructor.
    DATA: lif_ixml_stream_factory TYPE REF TO if_ixml_stream_factory,
          lt_string TYPE TABLE OF my_type.

    me->gv_rootname  = iv_rootname.
    me->gv_namespace = iv_namespace.
    me->gv_prefix    = iv_prefix.

**-- Create the Main Factory
    me->go_ixml = cl_ixml=>create( ).

**-- Create the Initial Document
    me->go_ixml_document = go_ixml->create_document( ).

**-- Create an Output Stream
    lif_ixml_stream_factory = go_ixml->create_stream_factory( ).
    me->go_ixml_ostream = lif_ixml_stream_factory->create_ostream_itable( table = lt_string ).

**-- Create an Encoding
    me->go_ixml_encoding = go_ixml->create_encoding( byte_order    = 0
                                                     character_set = mc_character_set ).

**-- Set the Encoding
    me->go_ixml_ostream->set_encoding( encoding = go_ixml_encoding ).

**-- Create a Renderer
    me->go_ixml_renderer = go_ixml->create_renderer( document = go_ixml_document
                                                     ostream  = go_ixml_ostream ).

**-- Create the Root Node
    me->go_ixml_element = go_ixml_document->create_element( name = iv_rootname ).

**-- Append the Root element to the Document
    me->go_ixml_document->append_child( new_child = go_ixml_element ).

**-- Define a Parent Node
    me->go_ixml_node_parent = go_ixml_element.

  ENDMETHOD.                    "constructor

  METHOD get_document.
    rv_document = go_ixml_document .
  ENDMETHOD.                    "get_document

  METHOD add_node.
    me->go_ixml_element = me->go_ixml_document->create_element( name = iv_name ).
    IF ( NOT iv_parent IS INITIAL ).
      DATA: lo_node TYPE REF TO if_ixml_node_collection.
      lo_node = me->go_ixml_document->get_elements_by_tag_name( iv_parent ).
      lo_node->get_item( index = 0 )->append_child( new_child = go_ixml_element ).
    ELSE.
      me->go_ixml_node_parent->append_child( new_child = go_ixml_element ).
    ENDIF.
  ENDMETHOD.                    "add_node

  METHOD add_child.
    me->go_ixml_element_sub = go_ixml_document->create_element( name = iv_name ).
    me->go_ixml_element_sub->set_value( value = iv_value ).
    IF ( NOT iv_parent IS INITIAL ).
      DATA: lo_node TYPE REF TO if_ixml_node_collection.
      lo_node = me->go_ixml_document->get_elements_by_tag_name( iv_parent ).
      lo_node->get_item( index = 0 )->append_child( new_child = go_ixml_element_sub ).
      me->go_ixml_element_sub->set_value( value = iv_value ).
    ELSE.
      me->go_ixml_element->append_child( new_child = go_ixml_element_sub ).
    ENDIF.

*    me->go_ixml_element_sub->set_value( value = iv_value ).
*    me->go_ixml_element->append_child( new_child = go_ixml_element_sub ).
  ENDMETHOD.                    "add_node

  METHOD add_root_attribute.
    IF ( me->go_ixml_element->get_name( ) = gv_rootname ).
      me->go_ixml_element->set_attribute( name = iv_name value = iv_value ).
    ENDIF.
  ENDMETHOD.                    "add_attribute

  METHOD add_attribute.
    IF ( me->go_ixml_element->get_name( ) = iv_node ).
      me->go_ixml_element->set_attribute( name = iv_name value = iv_value ).
    ELSEIF ( me->go_ixml_element_sub->get_name( ) = iv_node ).
      me->go_ixml_element_sub->set_attribute( name = iv_name value = iv_value ).
    ENDIF.
  ENDMETHOD.                    "add_attribute

  METHOD download.
    DATA: lv_xml TYPE string,
          lt_table TYPE TABLE OF string.

    lv_xml = me->convert_dom_to_xml( io_dom = go_ixml_document ).

    REFRESH: lt_table.
    APPEND lv_xml TO lt_table.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
*    bin_filesize              =
        filename                  = iv_filename
*    filetype                  = 'ASC'
*    append                    = SPACE
*    write_field_separator     = SPACE
*    header                    = '00'
*    trunc_trailing_blanks     = SPACE
*    write_lf                  = 'X'
*    col_select                = SPACE
*    col_select_mask           = SPACE
*    dat_mode                  = SPACE
*    confirm_overwrite         = SPACE
*    no_auth_check             = SPACE
*    codepage                  = SPACE
*    ignore_cerr               = ABAP_TRUE
*    replacement               = '#'
*    write_bom                 = SPACE
*    trunc_trailing_blanks_eol = 'X'
*    wk1_n_format              = SPACE
*    wk1_n_size                = SPACE
*    wk1_t_format              = SPACE
*    wk1_t_size                = SPACE
*    show_transfer_status      = 'X'
*    fieldnames                =
*    write_lf_after_last_line  = 'X'
*    virus_scan_profile        = '/SCET/GUI_DOWNLOAD'
*  IMPORTING
*    filelength                =
      CHANGING
        data_tab                  = lt_table
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
            .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.                    "download

  METHOD show.
    DATA: lv_xml TYPE string.
    lv_xml = me->convert_dom_to_xml( io_dom = go_ixml_document ).

**--Displaying xml
    cl_abap_browser=>show_xml( xml_string =  lv_xml ).  " XML in String
  ENDMETHOD.                    "show

  METHOD convert_dom_to_xml.
    DATA : lv_xstring    TYPE        xstring,
           convin        TYPE REF TO cl_abap_conv_in_ce..

    CALL FUNCTION 'SDIXML_DOM_TO_XML'
      EXPORTING
        document      = io_dom
      IMPORTING
        xml_as_string = lv_xstring
      EXCEPTIONS
        no_document   = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
**--Exception Handling

    ENDIF.

**--convert xstring to string
    cl_abap_conv_in_ce=>create( EXPORTING encoding    = 'UTF-8'
                                          endian      = 'L'
                                          ignore_cerr = 'X'
                                          replacement = '#'
                                          input       = lv_xstring
                                RECEIVING conv        = convin ). " Xstring

    convin->read( IMPORTING data = rv_xml ).                  " String
  ENDMETHOD.                    "convert_dom_to_xml
ENDCLASS.               "lcl_xml
