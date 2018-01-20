FUNCTION z_wrfc_get_menu .
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  EXPORTING
*"     VALUE(ET_MENU_ITEM) TYPE  ZWRFC_MENU_ITEM_T
*"     VALUE(ET_MENU_ACTION) TYPE  ZWRFC_MENU_ACTION_T
*"     VALUE(ET_MENU_METHOD) TYPE  ZWRFC_MENU_METHOD_T
*"----------------------------------------------------------------------
  " http(s)://<your system>:<your port>/sap/bc/webrfc?_FUNCTION=Z_SAMPLERFC&name=<your name>

  " http://mnibm09.novellini.it:8066/sap/bc/webrfc?_FUNCTION=Z_SAMPLERFC&callback=jsonCallback&sqlTable=MARC&sqlWhere=MATNR = 'ACF090-071026'

  DATA: lt_menu_item TYPE TABLE OF ty_s_menu_item,
        lt_fields    TYPE TABLE OF string,
        ls_menu_item TYPE ty_s_menu_item,
        lv_json      TYPE string.

  REFRESH: lt_menu_item, lt_fields.
  refresh: et_menu_action, et_menu_item, et_menu_method.

  APPEND '*' TO lt_fields.

  lv_json = '['
      && '{ parent: "", item: "item_1", description: "Item 1", action: "item_1_1", auth: "auth1", visible: false },'
      && '{ parent: "", item: "item_2", description: "Item 2", action: "item_2_1", auth: "auth2", visible: false },'
      && '{ parent: "", item: "item_3", description: "Item 3", action: "item_3_1", auth: "auth3", visible: false },'
      && '{ parent: "", item: "item_4", description: "Item 4", action: "item_4_1", auth: "auth4", visible: false },'
      && '{ parent: "", item: "item_5", description: "Item 5", action: "item_5_1", auth: "auth5", visible: false },'
      && '{ parent: "item_1", item: "item_1_1", description: "Item 1 1", action: "item_1_1_1", auth: "auth1", visible: false },'
      && '{ parent: "item_1", item: "item_1_2", description: "Item 1 2", action: "item_1_2_1", auth: "auth1", visible: false},'
      && '{ parent: "item_1", item: "item_1_3", description: "Item 1 3", action: "item_1_3_1", auth: "auth1", visible: false},'
      && '{ parent: "item_2", item: "item_2_1", description: "Item 2 1", action: "item_2_1_1", auth: "auth2", visible: false},'
      && '{ parent: "item_1_1", item: "item_1_1_1", description: "Item 1 1 1", action: "item_1_1_1_1", auth: "auth1", visible: false }'
      && ']'.
  cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_json
                             CHANGING  ca_data = et_menu_item ).

  lv_json = '['
      && '{ parent: "item_1_1_1", item: "action_1_1_1", title: "Action 1 1 1", method: "Z_METHOD_1_1_1" },'
      && '{ parent: "item_1_2", item: "action_1_2", title: "Action 1 2", method: "Z_METHOD_1_2" },'
      && '{ parent: "item_1_3", item: "action_1_3", title: "Action 1 3", method: "Z_METHOD_1_3" },'
      && '{ parent: "item_2_1", item: "action_2_1", title: "Action 2 1", method: "Z_METHOD_2_1" },'
      && '{ parent: "item_3", item: "action_3", title: "Action 3", method: "Z_METHOD_3" },'
      && '{ parent: "item_4", item: "action_4", title: "Action 4", method: "Z_METHOD_4" },'
      && '{ parent: "item_5", item: "action_5", title: "Action 5", method: "Z_METHOD_5" }'
      && ']'.
  cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_json
                             CHANGING  ca_data = et_menu_action ).

  lv_json = '['
      && '{ method: "Z_METHOD_1_1_1", fields: ['
      &&        '{ field: "plant", description: "Plant", type: IFieldType.select, inputType: null, required: true, value: "1000", defaultValue: "1000", length: 4, data: [ "", "1000", "2000", "3000"], minlength: "0", maxlength: "4", step: 1, valid: false'
      &&    ' },'
      &&        '{ field: "storageLoc", description: "Storage location", type: IFieldType.select, inputType: null, required: true, value: "2000", defaultValue: "2000", length: 4, data: [ "1000", "2000", "3000", "4000"], minlength: "0", maxlength: "4",'
      &&           'step: 1,'
      &&           'valid: false }'
      &&      '],'
      &&      'steps: [ "1" ],'
      &&      'repeat: false'
      &&    '},'
      &&    '{ method: "Z_METHOD_3", fields: ['
      &&        '{ field: "material", description: "Material", type: IFieldType.textbox, inputType: TextboxType.text, required: true, value: "100-002", defaultValue: "100-002", length: 18, data: [], minlength: "6", maxlength: "18", step: 1, '
      &&           'valid: false },'
      &&        '{ field: "plant", description: "Plant", type: IFieldType.select, inputType: null, required: true, value: "1000", defaultValue: "1000", length: 4, data: [ "1000", "2000", "3000"], minlength: "0", maxlength: "4", step: 1, valid: false },'
      &&        '{ field: "storageLoc", description: "Storage location", type: IFieldType.select, inputType: null, required: true, value: "", defaultValue: "", length: 4, data: [ "1001", "1002", "1003"], minlength: "0", maxlength: "4", step: 2, '
      &&           'valid: false },'
      &&        '{ field: "note", description: "Note", type: IFieldType.textarea, inputType: null, required: false, value: "", defaultValue: "", length: 200, data: [], minlength: "0", maxlength: "1000", step: 3, valid: false }'
      &&      '],'
      &&      'steps: [ "1", "2", "3" ],'
      &&      'repeat: true'
      &&    '},'
      &&    '{ method: "Z_METHOD_2_1", fields: ['
      &&        '{ field: "user", description: "User", type: IFieldType.textbox, inputType: TextboxType.text, required: true, value: '', defaultValue: '', length: 20, data: [], minlength: "5", maxlength: "20", step: 1, valid: false },'
      &&        '{ field: "pwd", description: "Password", type: IFieldType.textbox, inputType: TextboxType.password, required: true, value: '', defaultValue: '', length: 20, data: [], minlength: "10", maxlength: "20", step: 1, valid: false }'
      &&      '],'
      &&      'steps: [ "1" ],'
      &&      'repeat: false'
      &&    '},'
      &&    '{ method: "Z_METHOD_4", fields: ['
      &&        '{ field: "material", description: "Material", type: IFieldType.textbox, inputType: TextboxType.text, required: true, value: "100-001", defaultValue: "100-001", length: 18, data: [], minlength: "0", maxlength: "18", step: 1, '
      &&           'valid: false },'
      &&        '{ field: "quantity", description: "Quantity", type: IFieldType.textbox, inputType: TextboxType.number, required: true, value: "0", defaultValue: "0", length: 10, data: [], minlength: "1", maxlength: "10", step: 1, valid: false }'
      &&      '],'
      &&    '  steps: [ "1" ],'
      &&    '  repeat: false'
      &&    '},'
      &&    '{ method: "Z_METHOD_5", fields: ['
      &&        '{ field: "vendor", description: "Vendor", type: IFieldType.textbox, inputType: "text", required: true, value: "", defaultValue: "", length: 10, data: [], minlength: "5", maxlength: "10", step: 1, valid: false }'
      &&      '],'
      &&      'steps: [ "1"],'
      &&      'repeat: false'
      &&    '}'
      && ']'.
  cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_json
                             CHANGING  ca_data = et_menu_method ).


  RETURN.

  FIELD-SYMBOLS: <ls_data> TYPE any.
  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.
  TYPES: BEGIN OF ty_s_work,
           buffer(30000),
          END OF ty_s_work,
          ty_t_work TYPE TABLE OF ty_s_work.



  break novedev.
  ASSIGN ('lt_menu_item[]') TO <lt_data>[].
  IF ( sy-subrc = 0 ).
*    ASSIGN et_menu_item->* TO  <lt_data>[].
*  ENDIF.

    DATA: lo_struct_descr   TYPE REF TO cl_abap_structdescr,
          lo_table_descr    TYPE REF TO cl_abap_tabledescr,
          lt_keys           TYPE abap_keydescr_tab,
          lt_table          TYPE REF TO data,
          ls_table          TYPE REF TO data.

    TRY.
        lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( ls_menu_item ).

        CREATE DATA ls_table TYPE HANDLE lo_struct_descr.
        ASSIGN ls_table->* TO <ls_data>.

        lo_table_descr ?= cl_abap_tabledescr=>create( p_line_type  = lo_struct_descr
                                                      p_table_kind = cl_abap_tabledescr=>tablekind_hashed
                                                      p_unique     = abap_true
                                                      p_key        = lt_keys
                                                      p_key_kind   = cl_abap_tabledescr=>keydefkind_default ).

*        CREATE DATA et_menu_item TYPE HANDLE lo_table_descr.
*        ASSIGN et_menu_item->* TO <lt_data>.
        <lt_data> = lt_menu_item.

      CATCH cx_root.
    ENDTRY.
  ENDIF.

*  DATA: ls_work TYPE ty_s_work.

  DATA: lo_data_descr     TYPE REF TO cl_abap_datadescr,
        lx_root           TYPE REF TO cx_root,
        ls_components     TYPE abap_compdescr,
        ls_string         TYPE string.

  DATA: lt_messages TYPE ty_t_messages.

  REFRESH lt_messages.



ENDFUNCTION.
