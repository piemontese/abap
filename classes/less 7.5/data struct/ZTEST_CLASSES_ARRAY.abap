*&---------------------------------------------------------------------*
*& Report  ZTEST_CLASSES_ARRAY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztest_classes_array.



*----------------------------------------------------------------------*
*       CLASS lcl_array DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_array DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_data,
             index TYPE i,
             item  TYPE REF TO data,
           END OF ty_s_data.

    METHODS:
      constructor,
      insert IMPORTING iv_data TYPE REF TO data,
      get_array EXPORTING et_data TYPE table,
      get_item IMPORTING iv_index TYPE i
               RETURNING value(rv_item) TYPE REF TO data,
      get_item_by_index IMPORTING iv_index TYPE i
                        RETURNING value(rv_item) TYPE REF TO data,
      get_index RETURNING value(rv_index) TYPE i,
      swap IMPORTING iv_index_1 TYPE i
                     iv_index_2 TYPE i.

  PRIVATE SECTION.
    DATA: gv_index       TYPE i,
          gref_items     TYPE TABLE OF REF TO ty_s_data,
          gref_items_tmp TYPE TABLE OF REF TO ty_s_data.
ENDCLASS.                    "lcl_array DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_array IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_array IMPLEMENTATION  .
  METHOD constructor.
    gv_index = 0.
    CLEAR: gref_items, gref_items_tmp.
  ENDMETHOD.                    "constructor
  METHOD insert.
    ADD 1 TO gv_index.
    DATA: lref_data TYPE REF TO ty_s_data.
    CREATE DATA lref_data.
    lref_data->*-index = gv_index.
    lref_data->*-item = iv_data.
    APPEND lref_data TO gref_items.
  ENDMETHOD.                    "insert
  METHOD get_array.
    DATA: lref_data TYPE REF TO ty_s_data.
    REFRESH: et_data.
    CREATE DATA lref_data.
    LOOP AT gref_items INTO lref_data.
*      APPEND lref_data->*-item TO et_data.
    ENDLOOP.
  ENDMETHOD.                    "get_array
  METHOD get_item.
    DATA: lref_data TYPE REF TO ty_s_data.
    CREATE DATA lref_data.
    READ TABLE gref_items INTO lref_data INDEX iv_index.
    IF ( sy-subrc = 0 ).
      rv_item = lref_data->*-item.
    ENDIF.
  ENDMETHOD.                    "get_item
  METHOD get_item_by_index.
    IF ( gref_items_tmp[] IS INITIAL ).
      gref_items_tmp[] = gref_items[].
    ENDIF.
    DATA: lref_data TYPE REF TO ty_s_data.
    CREATE DATA lref_data.
    DATA: lv_tabix TYPE i.
    LOOP AT gref_items_tmp INTO lref_data.
      lv_tabix = sy-tabix.
      IF ( lref_data->*-index = iv_index ).
        rv_item = lref_data->*-item.
        DELETE gref_items_tmp INDEX lv_tabix.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_item_by_index
  METHOD get_index.
    rv_index = gv_index.
  ENDMETHOD.                    "get_index
  METHOD swap.
    DATA: lref_data_1 TYPE REF TO ty_s_data,
          lref_data_2 TYPE REF TO ty_s_data.
    CREATE DATA lref_data_1.
    READ TABLE gref_items INTO lref_data_1 INDEX iv_index_1.
    IF ( sy-subrc = 0 ).
      DATA: lv_index_1.
      lv_index_1 = sy-tabix.
      CREATE DATA lref_data_2.
      READ TABLE gref_items INTO lref_data_2 INDEX iv_index_2.
      IF ( sy-subrc = 0 ).
        DATA: lv_index_2.
        lv_index_2 = sy-tabix.
*        MODIFY gref_items FROM lref_data_1 INDEX lref_data_2->*-index.
*        MODIFY gref_items FROM lref_data_2 INDEX lref_data_1->*-index.
        MODIFY gref_items FROM lref_data_1 INDEX lv_index_2.
        MODIFY gref_items FROM lref_data_2 INDEX lv_index_1.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "swap
ENDCLASS.                    "lcl_array IMPLEMENTATION


START-OF-SELECTION.
  DATA: ldref      TYPE REF TO string,
        lo_array   TYPE REF TO lcl_array.

  CREATE OBJECT lo_array.


  CREATE DATA ldref.
  ldref->* = 'string 1'.
  lo_array->insert( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 2'.
  lo_array->insert( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 3'.
  lo_array->insert( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 4'.
  lo_array->insert( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 5'.
  lo_array->insert( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 6'.
  lo_array->insert( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 7'.
  lo_array->insert( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 8'.
  lo_array->insert( ldref ).



  DATA: ldref_tmp TYPE REF TO data.
  CREATE DATA ldref_tmp TYPE string.
  ldref_tmp = lo_array->get_item( 2 ).


  WRITE / 'Array'.
  DO lo_array->get_index( ) TIMES.
    ldref ?= lo_array->get_item( sy-index ).
    WRITE / ldref->*.
  ENDDO.
  WRITE /.

  WRITE / 'Swap(1, 3)'.
  lo_array->swap( iv_index_1 = 1 iv_index_2 = 3 ).
  DO lo_array->get_index( ) TIMES.
    ldref ?= lo_array->get_item( sy-index ).
    WRITE / ldref->*.
  ENDDO.
  WRITE /.

  WRITE / 'Swap(1, 2)'.
  lo_array->swap( iv_index_1 = 1 iv_index_2 = 2 ).
  DO lo_array->get_index( ) TIMES.
    ldref ?= lo_array->get_item( sy-index ).
    WRITE / ldref->*.
  ENDDO.
  WRITE /.

  WRITE / 'Walk by array index'.
  DO lo_array->get_index( ) TIMES.
    ldref ?= lo_array->get_item_by_index( sy-index ).
    WRITE / ldref->*.
  ENDDO.
  WRITE /.

  WRITE / 'Walk by array index'.
  DO lo_array->get_index( ) TIMES.
    ldref ?= lo_array->get_item_by_index( sy-index ).
    WRITE / ldref->*.
  ENDDO.
  WRITE /.

  WRITE / 'Walk by differences'.
  DO lo_array->get_index( ) TIMES.
    ldref ?= lo_array->get_item_by_index( sy-index ).
    DATA: ldref_2 TYPE REF TO string.
    ldref_2 ?= lo_array->get_item( sy-index ).
    IF ( ldref <> ldref_2 ).
      WRITE: / ldref->*, ' => ', ldref_2->*.
    ENDIF.
  ENDDO.
  WRITE /.

  DATA: lt_array TYPE TABLE OF REF TO string.
  lo_array->get_array( IMPORTING et_data = lt_array ).

  CHECK 1 = 1.
