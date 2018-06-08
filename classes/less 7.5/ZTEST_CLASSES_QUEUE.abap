*&---------------------------------------------------------------------*
*& Report  ZTEST_CLASSES_QUEUE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztest_classes_queue.


*----------------------------------------------------------------------*
*       CLASS lcl_queue DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_queue DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      push IMPORTING iv_data TYPE REF TO data,
      pop RETURNING value(ev_data) TYPE REF TO data,
      is_empty RETURNING value(ev_ret) TYPE i,
      set_data IMPORTING iv_data TYPE REF TO data,
      set_next IMPORTING io_queue TYPE REF TO lcl_queue,
      set_base IMPORTING io_queue TYPE REF TO lcl_queue,
      get_data RETURNING value(ev_data) TYPE REF TO data,
      get_base RETURNING value(eo_queue) TYPE REF TO lcl_queue,
      get_next RETURNING value(eo_queue) TYPE REF TO lcl_queue.
  PRIVATE SECTION.
    DATA: gv_data TYPE REF TO data,
          go_next TYPE REF TO lcl_queue,
          go_base  TYPE REF TO lcl_queue,
          go_last TYPE REF TO lcl_queue.
ENDCLASS.                    "lcl_queue DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_queue IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_queue IMPLEMENTATION.
  METHOD constructor.
    FREE: go_next, go_base.
  ENDMETHOD.                    "constructor
  METHOD push.
    IF ( go_base IS BOUND ).
      CREATE OBJECT go_next.
      go_next = go_base.
    ENDIF.
    CREATE OBJECT go_base.
    go_base->set_data( iv_data ).
    go_base->set_next( io_queue = go_next ).
  ENDMETHOD.                    "push
  METHOD pop.
    DATA: lo_queue TYPE REF TO lcl_queue,
          lo_queue_last TYPE REF TO lcl_queue.
    lo_queue = go_base.
    WHILE ( lo_queue IS BOUND ).
      IF ( lo_queue = go_last ).
        EXIT.
      ENDIF.
      IF ( lo_queue IS BOUND ).
        lo_queue_last = lo_queue.
      ENDIF.
      ev_data = lo_queue->get_data( ).
      lo_queue = lo_queue->get_next( ).
    ENDWHILE.
    go_last = lo_queue_last.
    IF ( go_base = go_last ).
      FREE go_base.
      RETURN..
    ENDIF.
*    go_base = go_base->get_next( ).
  ENDMETHOD.                    "pop
  METHOD is_empty.
    ev_ret = -1.
    IF ( NOT go_base IS BOUND ).
      ev_ret = 0.
    ENDIF.
  ENDMETHOD.                    "is_empty
  METHOD get_base.
    IF ( go_base IS BOUND ).
      eo_queue = go_base.
    ENDIF.
  ENDMETHOD.                    "get_base
  METHOD get_next.
    IF ( go_next IS BOUND ).
      eo_queue = go_next.
    ENDIF.
  ENDMETHOD.                    "get_next
  METHOD set_data.
    gv_data = iv_data.
  ENDMETHOD.                    "set_data
  METHOD set_next.
    go_next = io_queue.
  ENDMETHOD.                    "set_next
  METHOD set_base.
    go_base = io_queue.
  ENDMETHOD.                    "set_base
  METHOD get_data.
    ev_data = gv_data.
  ENDMETHOD.                    "get_data
ENDCLASS.                    "lcl_queue IMPLEMENTATION


START-OF-SELECTION.
  DATA: ldref     TYPE REF TO string,
        lo_queue  TYPE REF TO lcl_queue.

  CREATE OBJECT lo_queue.


  CREATE DATA ldref.
  ldref->* = 'string 1'.
  lo_queue->push( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 2'.
  lo_queue->push( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 3'.
  lo_queue->push( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 4'.
  lo_queue->push( ldref ).


  WRITE / 'Pop'.
  WHILE ( lo_queue->is_empty( ) <> 0 ).
    ldref ?= lo_queue->pop( ).
    WRITE / ldref->*.
  ENDWHILE.
  WRITE /.

  FREE lo_queue.
  CREATE OBJECT lo_queue.

  DATA: lt_vbap TYPE TABLE OF vbap,
        ls_vbap TYPE vbap.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vbap
           FROM vbap
           WHERE vbeln = '1000000038'.

  LOOP AT lt_vbap INTO ls_vbap.
    DATA: lref_vbap TYPE REF TO vbap.
    CREATE DATA lref_vbap.
    lref_vbap->* = ls_vbap.
    lo_queue->push( lref_vbap ).
  ENDLOOP.

  WRITE / 'Pop'.
  WHILE ( lo_queue->is_empty( ) <> 0 ).
    lref_vbap ?= lo_queue->pop( ).
    WRITE: / lref_vbap->*-vbeln, lref_vbap->*-posnr, lref_vbap->*-matnr, lref_vbap->*-kwmeng.
  ENDWHILE.
