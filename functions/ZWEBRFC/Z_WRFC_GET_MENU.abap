FUNCTION z_wrfc_get_menu.
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  TABLES
*"      QUERY_STRING STRUCTURE  W3QUERY
*"      HTML STRUCTURE  W3HTML
*"      MIME STRUCTURE  W3MIME
*"  CHANGING
*"     VALUE(CONTENT_TYPE) TYPE  W3PARAM-CONT_TYPE DEFAULT
*"       'application/json'
*"     VALUE(CONTENT_LENGTH) TYPE  W3PARAM-CONT_LEN
*"     VALUE(RETURN_CODE) TYPE  W3PARAM-RET_CODE
*"----------------------------------------------------------------------
  " http(s)://<your system>:<your port>/sap/bc/webrfc?_FUNCTION=Z_SAMPLERFC&name=<your name>

  " http://mnibm09.novellini.it:8066/sap/bc/webrfc?_FUNCTION=Z_SAMPLERFC&callback=jsonCallback&sqlTable=MARC&sqlWhere=MATNR = 'ACF090-071026'

  DATA: lt_menu_item TYPE TABLE OF ty_s_menu_item,
        lt_fields    TYPE TABLE OF string,
        ls_menu_item TYPE ty_s_menu_item,
        lv_json      TYPE string.

  REFRESH: lt_menu_item, lt_fields..

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
                             CHANGING  ca_data = lt_menu_item ).


  FIELD-SYMBOLS: <ls_data> TYPE any.
  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.
  TYPES: BEGIN OF ty_s_work, buffer(30000), END OF ty_s_work.
*  DATA: ls_work TYPE ty_s_work.

  DATA: lo_struct_descr   TYPE REF TO cl_abap_structdescr,
        lo_table_descr    TYPE REF TO cl_abap_tabledescr,
        lo_data_descr     TYPE REF TO cl_abap_datadescr,
        lx_root           TYPE REF TO cx_root,
        ls_components     TYPE abap_compdescr,
        lt_keys           TYPE abap_keydescr_tab,
        ls_string         TYPE string,
        lt_table          TYPE REF TO data,
        ls_table          TYPE REF TO data.

  DATA: lt_messages TYPE ty_t_messages.

  REFRESH lt_messages.

* parametri
  DATA: iv_callback         TYPE string,
        iv_type             TYPE string.
*        IV_SQL_TABLE        TYPE STRING,
*        IV_SQL_WHERE        TYPE STRING,
*        IV_SQL_FIELDS       TYPE STRING,
*        IV_FROM_REC         TYPE I,
*        IV_TO_REC            TYPE I.

  CLEAR: iv_callback,
         iv_type.

  LOOP AT query_string.
    TRANSLATE query_string-name TO UPPER CASE.
    MODIFY query_string.
  ENDLOOP.

*  SORT query_string DESCENDING.
*  READ TABLE query_string WITH KEY name = 'NAME'.
*  CHECK sy-subrc = 0.
*  name = query_string-value.

  CLEAR: query_string.
  READ TABLE query_string WITH KEY name = 'CALLBACK'.
  iv_callback = query_string-value.


  " apre tag funcione di callback
  PERFORM jsonp_open_callback USING    iv_callback
                              CHANGING html[].

* results ------------------------------------------------------
  " apre tag results
  PERFORM jsonp_open_results CHANGING html[].
  " costruisce results

  PERFORM jsonp_build_results USING    'menuItem'
                                       lt_menu_item[]
*                                         it_components[]
                                       lt_fields[]
*                                         IV_ROWS
                                       1
                                       1000
                              CHANGING html[].

  " chiude tag results
  PERFORM jsonp_close_results CHANGING html[].

  " chiude tab funzione di callback
  PERFORM jsonp_close_callback USING iv_callback
                               CHANGING html[].



ENDFUNCTION.

* struttura file jsonp
*   iv_callback({
*     "errors": [
*       { "type": "...", "message": "..." },
*     ],
*     "results": {
*       "iv_sql_table": [
*         { "field1": "...", ..., "fieldn": "..." },
*         ...,
*         { "field11": "...", ..., "fieldn": "..." },
*       ]
*     }
*   });)
