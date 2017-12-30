*&---------------------------------------------------------------------*
*& Report  ZBC_UPLOAD_IMPORT_CLASS_XML
*&
*&---------------------------------------------------------------------*
*& Programma per scaricare e importare sul/dal disco locale una classe
*& con i relativi metodi.
*& Il file generato è in formato XML.
*&
*& Versione 1.0 - 8 marzo 2016
*&---------------------------------------------------------------------*
REPORT  zbc_upload_import_class_xml.

TABLES: t002.
TYPE-POOLS: seok, seop.

DATA:
  corr_mode,

  g_xml            TYPE REF TO cl_xml_document,
* Extracted structures (from XML file)
  gt_locals_src    TYPE	seop_source_string,
  gt_locals_def    TYPE	seop_source_string,
  gt_locals_imp    TYPE	seop_source_string,
  gt_locals_mac    TYPE	seop_source_string,

  gs_class         TYPE vseoclass,
  gt_attributes    TYPE seoo_attributes_r,
  gt_methods       TYPE seoo_methods_r,
  gt_events        TYPE seoo_events_r,
  gt_types         TYPE seoo_types_r,
  gt_parameters    TYPE seos_parameters_r,
  gt_exceps        TYPE seos_exceptions_r,
  gt_implementings TYPE seor_implementings_r,
  gs_inheritance   TYPE vseoextend,
  gt_redefinitions TYPE seor_redefinitions_r,
  gt_impl_details  TYPE seor_redefinitions_r,
  gt_friendships   TYPE seof_friendships_r,
  gt_typepusages   TYPE seot_typepusages_r,
  gt_clsdeferrds   TYPE seot_clsdeferrds_r,
  gt_intdeferrds   TYPE seot_intdeferrds_r,
  gt_aliases       TYPE seoo_aliases_r,
  gt_interfaces    TYPE seok_int_typeinfos,
  gs_includes      TYPE seop_methods_w_include,
  gt_texts         TYPE TABLE OF textpool,
  gt_source        TYPE seop_source,
  gt_type_source   TYPE	seop_source,
  gt_mtd_source    TYPE seo_method_source_table,
  wa_mtd_source    TYPE seo_method_source.

DATA:
  typkey        TYPE seocmpkey,
  single_source TYPE seop_source,
  type_source   TYPE seop_source.

FIELD-SYMBOLS:
  <type>        TYPE seoo_type_r,
  <source_line> TYPE LINE OF seop_source,
  <info>        TYPE seok_cls_typeinfo_by_vis.

FIELD-SYMBOLS:
  <include> TYPE LINE OF seop_methods_w_include.

CLASS lcx_xml_error DEFINITION DEFERRED.

DATA:
  ex TYPE REF TO lcx_xml_error.
*----------------------------------------------------------------------*
*       CLASS lCX_xml_error  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_error DEFINITION
  INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA: error TYPE string.
    METHODS: constructor IMPORTING VALUE(i_error) TYPE string OPTIONAL.
ENDCLASS.                    "lCX_xml_error  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS lcx_xml_section_error  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_section_error DEFINITION
  INHERITING FROM lcx_xml_error.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING VALUE(i_section) TYPE string OPTIONAL
      .
ENDCLASS.                    "lCX_xml_error  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS lcx_xml_error IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_error IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    error = i_error.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcx_xml_error IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcx_xml__section_error IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_section_error IMPLEMENTATION.
  METHOD constructor.
    DATA:
      msg TYPE string.
    CONCATENATE 'File does not contain obligatory section'
                i_section
                INTO msg
                SEPARATED BY space.
    CALL METHOD super->constructor
      EXPORTING
        i_error = msg.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcx_xml__section_error IMPLEMENTATION

CONSTANTS:
  c_xml_root          TYPE string VALUE 'CLASS_ROOT',
  c_xml_public        TYPE string VALUE 'PUBLIC_SECTION',
  c_xml_protected     TYPE string VALUE 'PROTECTED_SECTION',
  c_xml_private       TYPE string VALUE 'PRIVATE_SECTION',
  c_xml_inheritance   TYPE string VALUE 'INHERITANCE',
  c_xml_friends       TYPE string VALUE 'FRIENDS',
  c_xml_interfaces    TYPE string VALUE 'INTERFACES',
  c_xml_redefinitions TYPE string VALUE 'REDEFINITIONS',
  c_xml_typesource    TYPE string VALUE 'TYPE_SOURCE',
  c_xml_locals_src    TYPE string VALUE 'LOCALS_SRC',
  c_xml_locals_def    TYPE string VALUE 'LOCALS_DEF',
  c_xml_locals_imp    TYPE string VALUE 'LOCALS_IMP',
  c_xml_locals_mac    TYPE string VALUE 'LOCALS_MAC',
  c_xml_method_src    TYPE string VALUE 'METHOD_SOURCE'.

CONSTANTS:
  c_xml_attributes    TYPE string VALUE 'ATTRIBUTES',
  c_xml_methods       TYPE string VALUE 'METHODS',
  c_xml_events        TYPE string VALUE 'EVENTS',
  c_xml_types         TYPE string VALUE 'TYPES',
  c_xml_parameters    TYPE string VALUE 'PARAMETERS',
  c_xml_exceps        TYPE string VALUE 'EXCEPTIONS',
  c_xml_implementings TYPE string VALUE 'IMPLEMENTINGS',
  c_xml_impl_details  TYPE string VALUE 'IMPL_DETAILS',
  c_xml_friendships   TYPE string VALUE 'FRIENDSHIPS',
  c_xml_typepusages   TYPE string VALUE 'TYPEPUSAGES',
  c_xml_clsdeferrds   TYPE string VALUE 'CLSDEFERRDS',
  c_xml_intdeferrds   TYPE string VALUE 'INTDEFERRDS',
  c_xml_aliases       TYPE string VALUE 'ALIASES',
  c_xml_text_pool     TYPE string VALUE 'TEXT_POOL'.

PARAMETERS: p_rad1 RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND abc,
            p_rad2 RADIOBUTTON GROUP rad1.



*ESPORTAZIONE CLASSE IN XML
SELECTION-SCREEN SKIP.
PARAMETERS: class TYPE seoclskey.
PARAMETERS: p_show AS CHECKBOX.
SELECT-OPTIONS: s_langu FOR t002-spras
                        DEFAULT sy-langu
                        NO INTERVALS.


*IMPORTAZIONE CLASSE IN XML
PARAMETERS:
  xmlfile TYPE localfile.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name CS 'S_LANGU'.
      IF NOT p_rad2 IS INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name CS 'P_SHOW'.
      IF NOT p_rad2 IS INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name CS 'CLASS'.
      IF NOT p_rad2 IS INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


  LOOP AT SCREEN.
    IF screen-name CS 'XMLFILE'.
      IF NOT p_rad1 IS INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR xmlfile.
  DATA:
    file_table TYPE filetable,
    action     TYPE i,
    rc         TYPE sysubrc.

  FIELD-SYMBOLS:
    <file> TYPE file_table.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_extension       = 'xml'
      file_filter             = cl_gui_frontend_services=>filetype_xml
    CHANGING
      file_table              = file_table
      rc                      = rc
      user_action             = action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK action = cl_gui_frontend_services=>action_ok.

  READ TABLE file_table INDEX 1 ASSIGNING <file>.

  xmlfile = <file>.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR class.
  DATA:
    clsname TYPE seoclass-clsname.
  clsname = class.

  CALL FUNCTION 'F4_OBJECTS_CLASS'
    EXPORTING
      object = clsname
    IMPORTING
      result = clsname.

  class = clsname.

AT SELECTION-SCREEN ON class.
  DATA:
    clskey  TYPE  seoclskey.

  clskey-clsname = class.

  CHECK class IS NOT INITIAL.
* Check if active version of class exists
  CALL FUNCTION 'SEO_CLASS_GET'
    EXPORTING
      clskey       = clskey
      version      = '1'
      state        = '1'
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_interface = 3
      model_only   = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



START-OF-SELECTION.

  IF p_rad2 IS NOT INITIAL.
    IF class IS INITIAL.
      MESSAGE s208(00) WITH TEXT-001 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF p_rad1 IS NOT INITIAL.
    PERFORM class_upload_xml.
  ENDIF.

  IF p_rad2 IS NOT INITIAL.
    PERFORM class_download.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  CLASS_UPLOAD_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM class_upload_xml .


  DATA:
    retcode     TYPE sysubrc,
    source_node TYPE REF TO if_ixml_node,
    xml         TYPE REF TO cl_xml_document.

  CREATE OBJECT xml.

  TRY.
      CALL METHOD xml->import_from_file
        EXPORTING
          filename = xmlfile
        RECEIVING
          retcode  = retcode.

      IF retcode NE xml->c_ok.
        DATA:
          msg TYPE string.
        CASE retcode.
          WHEN xml->c_no_ixml.
            msg = 'Questo non è un file in XML'.
          WHEN xml->c_failed.
            msg = 'Impossibile analizzare il file XML'.
          WHEN xml->c_not_found.
            msg = 'File XML non trovato'.
        ENDCASE.
        RAISE EXCEPTION TYPE lcx_xml_error
          EXPORTING
            i_error = msg.
      ENDIF.

      DEFINE load_section.
        CALL METHOD xml->get_data
          EXPORTING
            name       = &1
          IMPORTING
            retcode    = retcode
          CHANGING
            dataobject = &2.

        IF retcode NE xml->c_ok.
          RAISE EXCEPTION TYPE lcx_xml_section_error
            EXPORTING i_section = &1.
        ENDIF.
      END-OF-DEFINITION.

      load_section:
        c_xml_root gs_class,

        c_xml_attributes gt_attributes,
        c_xml_methods gt_methods,
        c_xml_events gt_events,
        c_xml_types gt_types,
        c_xml_parameters gt_parameters,
        c_xml_exceps gt_exceps,
        c_xml_implementings gt_implementings,
        c_xml_impl_details gt_impl_details,
        c_xml_friendships gt_friendships,
        c_xml_typepusages gt_typepusages,
        c_xml_clsdeferrds gt_clsdeferrds,
        c_xml_intdeferrds gt_intdeferrds,
        c_xml_aliases gt_aliases,
        c_xml_inheritance gs_inheritance,
        c_xml_interfaces gt_interfaces,
        c_xml_redefinitions gt_redefinitions,

        c_xml_typesource gt_type_source,

*        c_xml_text_pool gt_texts,

        c_xml_locals_src gt_locals_src,
        c_xml_locals_def gt_locals_def,
        c_xml_locals_mac gt_locals_mac,
        c_xml_locals_imp gt_locals_imp.

* Now find method implementation node containing sources
* for each method as a subnode
      CALL METHOD xml->find_node
        EXPORTING
          name = c_xml_method_src
        RECEIVING
          node = source_node.

      IF source_node IS INITIAL.
        RAISE EXCEPTION TYPE lcx_xml_section_error
          EXPORTING
            i_section = c_xml_methods.
      ENDIF.

      source_node = source_node->get_first_child( ).

      DATA:
        name1      TYPE string,
        name2      TYPE string,
        xml_sep(5) VALUE '_--7E'.

      WHILE NOT source_node IS INITIAL.

        CLEAR wa_mtd_source.

        name1 = source_node->get_name( ).
        SPLIT name1 AT xml_sep INTO name1 name2.

        IF name2 IS INITIAL.
          wa_mtd_source-cpdname = name1.
        ELSE.
          CONCATENATE name1 name2 INTO wa_mtd_source-cpdname
                      SEPARATED BY '~'.
        ENDIF.

        CALL METHOD xml->get_node_data
          EXPORTING
            node       = source_node
          IMPORTING
            dataobject = wa_mtd_source-source
            retcode    = retcode.

        IF retcode = xml->c_ok.
          APPEND wa_mtd_source TO gt_mtd_source.
        ENDIF.

        source_node = source_node->get_next( ).
      ENDWHILE.

    CATCH lcx_xml_error INTO ex.
      MESSAGE ex->error TYPE 'E'.
  ENDTRY.

  DATA:
    answer,
    fields    TYPE TABLE OF sval WITH HEADER LINE,
    parameter	TYPE TABLE OF spar WITH HEADER LINE,
    clskey    TYPE seoclskey.

  clskey-clsname = gs_class-clsname.

  corr_mode = 'I'.
  DO.
    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = clskey
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4
        OTHERS       = 5.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    REFRESH parameter.
    parameter-param = 'CLASS'.
    parameter-value = clskey-clsname.
    APPEND parameter.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Classe già esistente'
        text_question  = 'Classe &CLASS& già esistente. Sovrascrivere?'
        default_button = '2'
      IMPORTING
        answer         = answer
      TABLES
        parameter      = parameter
      EXCEPTIONS
        text_not_found = 0
        OTHERS         = 0.

    CASE answer.
      WHEN '1'.
        corr_mode = 'U'.
        EXIT.
      WHEN 'A'.
        RETURN.
    ENDCASE.

    REFRESH fields.
    fields-tabname = 'SEOCLSKEY'.
    fields-fieldname = 'CLSNAME'.
    fields-value = clskey-clsname.
    APPEND fields.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Inserire il nome della classe'
      IMPORTING
        returncode      = answer
      TABLES
        fields          = fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF answer = 'A'.
      RETURN.
    ENDIF.

    READ TABLE fields INDEX 1.
    clskey-clsname = fields-value.

  ENDDO.

  IF clskey-clsname NE gs_class-clsname.
    gs_class-clsname = clskey-clsname.
    gs_class-author = sy-uname.
    gs_class-changedby = sy-uname.
    gs_class-changedon = sy-datum.

    gs_inheritance-clsname = clskey-clsname.

    PERFORM rename_class TABLES gt_implementings USING clskey-clsname.
    PERFORM rename_class TABLES gt_attributes    USING clskey-clsname.
    PERFORM rename_class TABLES gt_methods       USING clskey-clsname.
    PERFORM rename_class TABLES gt_events        USING clskey-clsname.
    PERFORM rename_class TABLES gt_types         USING clskey-clsname.
    PERFORM rename_class TABLES gt_parameters    USING clskey-clsname.
    PERFORM rename_class TABLES gt_exceps        USING clskey-clsname.
    PERFORM rename_class TABLES gt_aliases       USING clskey-clsname.
    PERFORM rename_class TABLES gt_typepusages   USING clskey-clsname.
    PERFORM rename_class TABLES gt_clsdeferrds   USING clskey-clsname.
    PERFORM rename_class TABLES gt_intdeferrds   USING clskey-clsname.

    PERFORM rename_class TABLES gt_redefinitions USING clskey-clsname.
    PERFORM rename_class TABLES gt_impl_details  USING clskey-clsname.
    PERFORM rename_class TABLES gt_friendships   USING clskey-clsname.
  ENDIF.

  DATA:
    devclass LIKE  tadir-devclass,
    korrnum  LIKE  e070-trkorr.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object              = gs_class-clsname
      object_class        = 'CLAS'
      mode                = corr_mode
      global_lock         = 'X'
      master_language     = gs_class-langu
    IMPORTING
      devclass            = devclass
      korrnum             = korrnum
    EXCEPTIONS
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      OTHERS              = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA:
    overwrite TYPE seox_boolean.

  IF corr_mode = 'U'.
    overwrite = seox_true.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Generazione della Classe'.

  CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
    EXPORTING
      corrnr          = korrnum
      devclass        = devclass
      version         = seoc_version_inactive
      overwrite       = overwrite
    IMPORTING
      korrnr          = korrnum
    CHANGING
      class           = gs_class
      inheritance     = gs_inheritance
      redefinitions   = gt_redefinitions
      implementings   = gt_implementings
      impl_details    = gt_impl_details
      attributes      = gt_attributes
      methods         = gt_methods
      events          = gt_events
      types           = gt_types
      type_source     = gt_type_source
      parameters      = gt_parameters
      exceps          = gt_exceps
      aliases         = gt_aliases
      typepusages     = gt_typepusages
      clsdeferrds     = gt_clsdeferrds
      intdeferrds     = gt_intdeferrds
      friendships     = gt_friendships
    EXCEPTIONS
      existing        = 1
      is_interface    = 2
      db_error        = 3
      component_error = 4
      no_access       = 5
      other           = 6
      OTHERS          = 7.

  IF sy-subrc <> 0.
    MESSAGE 'Errore nella generazione della classe' TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Generazione della Classe Locale'.

  CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
    EXPORTING
      clskey                 = clskey
      force                  = seox_true
      corrnr                 = korrnum
      implementation         = gt_locals_src
      locals_def             = gt_locals_def
      locals_imp             = gt_locals_imp
      locals_mac             = gt_locals_mac
    EXCEPTIONS
      not_existing           = 1
      model_only             = 2
      locals_not_generated   = 3
      locals_not_initialised = 4
      OTHERS                 = 5.

  IF sy-subrc <> 0.
    MESSAGE 'Errore nella generazione della classe Locale' TYPE 'E'.
  ENDIF.

  FIELD-SYMBOLS:
    <mtd_source> TYPE seo_method_source.

  DATA:
    status_line(128),
    mtdkey TYPE seocpdkey.

  LOOP AT gt_mtd_source ASSIGNING <mtd_source>.

    CONCATENATE 'Generazione implementazione dei metodi'
                <mtd_source>-cpdname
                INTO status_line
                SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = status_line.

    mtdkey-clsname = clskey-clsname.
    mtdkey-cpdname = <mtd_source>-cpdname.
    CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
      EXPORTING
        mtdkey                         = mtdkey
        force                          = seox_true
        suppress_corr                  = seox_true
        implementation_expanded        = <mtd_source>-source
        corrnr                         = korrnum
        without_method_frame           = seox_true
      EXCEPTIONS
        not_existing                   = 1
        model_only                     = 2
        include_existing               = 3
        method_imp_not_generated       = 4
        method_imp_not_initialised     = 5
        _internal_class_not_existing   = 6
        _internal_method_overflow      = 7
        cancelled                      = 8
        method_is_abstract_implemented = 9
        method_is_final_implemented    = 10
        internal_error_insert_report   = 11
        OTHERS                         = 12.

*    IF sy-subrc <> 0.
*      MESSAGE 'Error generating class' TYPE 'E'.
*    ENDIF.
  ENDLOOP.

* Now find text pool node containing textpools for
* different languages as subnodes
  DATA:
    langu        TYPE sy-langu,
    include_name TYPE programm.

  TRY.
      CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
        EXPORTING
          clskey   = clskey
        IMPORTING
          progname = include_name.

      CALL METHOD xml->find_node
        EXPORTING
          name = c_xml_text_pool
        RECEIVING
          node = source_node.

      IF source_node IS INITIAL.
        RAISE EXCEPTION TYPE lcx_xml_section_error
          EXPORTING
            i_section = c_xml_text_pool.
      ENDIF.

      source_node = source_node->get_first_child( ).

      WHILE NOT source_node IS INITIAL.
        langu = source_node->get_name( ).

        REFRESH gt_texts.
        CALL METHOD xml->get_node_data
          EXPORTING
            node       = source_node
          IMPORTING
            dataobject = gt_texts
            retcode    = retcode.

        source_node = source_node->get_next( ).

        CHECK retcode = xml->c_ok
        AND   NOT gt_texts IS INITIAL.

        CONCATENATE 'Generazione Classe text pool per la lingua'
                    langu
                    INTO msg
                    SEPARATED BY space.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text = msg.

        INSERT TEXTPOOL include_name
               FROM gt_texts
               LANGUAGE langu.
      ENDWHILE.

    CATCH lcx_xml_section_error.
      MESSAGE 'Il file XML non contiene Text pool' TYPE 'S'.
  ENDTRY.

  SET PARAMETER ID 'CLASS' FIELD clskey-clsname.
  CALL TRANSACTION 'SE24'.

ENDFORM.                    " CLASS_UPLOAD_XML

*&---------------------------------------------------------------------*
*&      Form  rename_class
*&---------------------------------------------------------------------*
*       Renames class references in arbitrary table
*----------------------------------------------------------------------*
*      -->P_TABLE    some generation structure
*      -->P_NAME     new class name
*----------------------------------------------------------------------*
FORM rename_class  TABLES   p_table
                   USING    p_name.
  FIELD-SYMBOLS:
    <field> TYPE any,
    <line>  TYPE any.

  DEFINE set_field.
    ASSIGN COMPONENT &1 OF STRUCTURE <line> TO <field>.
    IF sy-subrc = 0.
      <field> = &2.
    ENDIF.
  END-OF-DEFINITION.

  LOOP AT p_table ASSIGNING <line>.
    set_field 'CLSNAME' p_name.
    set_field 'AUTHOR' sy-uname.
    set_field 'CHANGEDBY' sy-uname.
  ENDLOOP.
ENDFORM.                    " rename_class

*&---------------------------------------------------------------------*
*&      Form  class_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM class_download.

  CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
    EXPORTING
      clskey        = class
      version       = seoc_version_active
    IMPORTING
      class         = gs_class
      attributes    = gt_attributes
      methods       = gt_methods
      events        = gt_events
      types         = gt_types
      parameters    = gt_parameters
      exceps        = gt_exceps
      implementings = gt_implementings
      inheritance   = gs_inheritance
      redefinitions = gt_redefinitions
      impl_details  = gt_impl_details
      friendships   = gt_friendships
      typepusages   = gt_typepusages
      clsdeferrds   = gt_clsdeferrds
      intdeferrds   = gt_intdeferrds
      aliases       = gt_aliases
    EXCEPTIONS
      not_existing  = 1
      is_interface  = 2
      model_only    = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Load all method include names
  CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
    EXPORTING
      clskey                       = class
    IMPORTING
      includes                     = gs_includes
    EXCEPTIONS
      _internal_class_not_existing = 1
      OTHERS                       = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Gathering together internal type source
  LOOP AT gt_types ASSIGNING <type>.

    typkey-clsname = <type>-clsname.
    typkey-cmpname = <type>-cmpname.

    REFRESH single_source.

    CALL FUNCTION 'SEO_CLASS_GET_TYPE_SOURCE'
      EXPORTING
        typkey                       = typkey
      IMPORTING
        source                       = single_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        not_edited                   = 3
        OTHERS                       = 4.

    CHECK sy-subrc = 0 AND NOT single_source[] IS INITIAL.

    <type>-srcrow1 = lines( type_source ) + 1.
    <type>-srccolumn1 = 0.

    LOOP AT single_source ASSIGNING <source_line>.
      CONDENSE <source_line>.
      <type>-srccolumn2 = strlen( <source_line> ).
      APPEND <source_line> TO type_source.
    ENDLOOP.

    <type>-srcrow2 = lines( type_source ).

  ENDLOOP.

  DATA:
    mtd_name      TYPE string,
    class_root    TYPE REF TO if_ixml_node,
    section       TYPE REF TO if_ixml_node,
    l_description TYPE sdok_descr.

  l_description = gs_class-descript.

  CREATE OBJECT g_xml
    EXPORTING
      description = l_description
      object_type = cl_xml_document=>c_bor_classtype
      object_name = gs_class-clsname.

* Simply generate sections of XML document


* First create root section
  g_xml->set_data( name       = c_xml_root
                   dataobject = gs_class ).

  class_root = g_xml->find_node( name   = c_xml_root ).

* All other sections will be under the root (see parent_node)
  DEFINE set_section_data.
    g_xml->set_data( name = &1
                     parent_node = class_root
                     dataobject  = &2 ).
  END-OF-DEFINITION.

  set_section_data:
    c_xml_attributes    gt_attributes,
    c_xml_methods       gt_methods,
    c_xml_events        gt_events,
    c_xml_types         gt_types,
    c_xml_parameters    gt_parameters,
    c_xml_exceps        gt_exceps,
    c_xml_implementings gt_implementings,
    c_xml_impl_details  gt_impl_details,
    c_xml_friendships   gt_friendships,
    c_xml_typepusages   gt_typepusages,
    c_xml_clsdeferrds   gt_clsdeferrds,
    c_xml_intdeferrds   gt_intdeferrds,
    c_xml_aliases       gt_aliases,
    c_xml_inheritance   gs_inheritance,
    c_xml_interfaces    gt_interfaces,
    c_xml_redefinitions gt_redefinitions,
    c_xml_typesource    type_source.

* Extract local definitions (classes, macros)
  DEFINE set_locals.
    REFRESH single_source.
    REFRESH single_source.
    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = class
        inctype                      = &1
      IMPORTING
        source                       = single_source
      EXCEPTIONS
        _internal_class_not_existing = 0
        not_existing                 = 0
        OTHERS                       = 0.

    g_xml->set_data( name = &2
                     parent_node = class_root
                     dataobject  = single_source ).

  END-OF-DEFINITION.

  DATA:
    include_name TYPE programm.

  CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
    EXPORTING
      clskey   = class
      limu     = seok_limu_locals
    IMPORTING
      progname = include_name.

  set_locals:
    include_name c_xml_locals_src,
    seop_ext_class_locals_def c_xml_locals_def,
    seop_ext_class_locals_imp c_xml_locals_imp,
    seop_ext_class_macros c_xml_locals_mac.

* Extract text pool
  DATA:
    name     TYPE string,
    gt_texts TYPE TABLE OF textpool.

  CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
    EXPORTING
      clskey   = class
    IMPORTING
      progname = include_name.

  section = g_xml->create_simple_element( name   = c_xml_text_pool
                                          parent = class_root ).

  SELECT spras FROM t002 INTO t002-spras.
    REFRESH gt_texts.
    READ TEXTPOOL include_name INTO gt_texts
                               LANGUAGE t002-spras.

    CHECK sy-subrc = 0.

    name = t002-spras.
    g_xml->set_data( name = name
                     parent_node = section
                     dataobject  = gt_texts ).

  ENDSELECT.

* Create section for method source.
* Then each metod as a single node under the common section
  section = g_xml->create_simple_element( name   = c_xml_method_src
                                          parent = class_root ).

  LOOP AT gs_includes ASSIGNING <include>.

    REFRESH gt_source.
    CALL FUNCTION 'SEO_METHOD_GET_SOURCE'
      EXPORTING
        mtdkey                        = <include>-cpdkey
        state                         = 'A'
      IMPORTING
        source                        = gt_source
      EXCEPTIONS
        _internal_method_not_existing = 1
        _internal_class_not_existing  = 2
        version_not_existing          = 3
        inactive_new                  = 4
        inactive_deleted              = 5
        OTHERS                        = 6.


    CHECK sy-subrc = 0.

    mtd_name = <include>-cpdkey-cpdname.

    g_xml->set_data( name = mtd_name
                     parent_node = section
                     dataobject  = gt_source[] ).

  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Now display resulting XML-file or export it into workstation file
  mtd_name = class.

  IF p_show = 'X'.
    g_xml->display( ).
    EXIT.
  ENDIF.

  DATA:
    user_action TYPE i,
    path        TYPE string,
    full_path   TYPE string,
    file_name   TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = 'xml'
      default_file_name    = mtd_name
      file_filter          = cl_gui_frontend_services=>filetype_xml
    CHANGING
      filename             = file_name
      path                 = path
      fullpath             = full_path
      user_action          = user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK user_action = cl_gui_frontend_services=>action_ok.

  DATA:
    localfile TYPE localfile.

  localfile = full_path.

  g_xml->export_to_file( filename = localfile ).

ENDFORM.                    "class_download

