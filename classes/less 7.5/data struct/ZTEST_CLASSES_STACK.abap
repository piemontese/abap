*&---------------------------------------------------------------------*
*& Report  ZTEST_CLASSES_STACK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztest_classes_stack.


*----------------------------------------------------------------------*
*       CLASS lcl_stack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_stack DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      push IMPORTING iv_data TYPE REF TO data,
      pop RETURNING value(ev_data) TYPE REF TO data,
      is_empty RETURNING value(ev_ret) TYPE i,
      set_data IMPORTING iv_data TYPE REF TO data,
      set_prev IMPORTING io_stack TYPE REF TO lcl_stack,
      set_top IMPORTING io_stack TYPE REF TO lcl_stack,
      get_data RETURNING value(ev_data) TYPE REF TO data,
      get_top RETURNING value(eo_stack) TYPE REF TO lcl_stack,
      get_prev RETURNING value(eo_stack) TYPE REF TO lcl_stack.
  PRIVATE SECTION.
    DATA: gv_data TYPE REF TO data,
          go_prev TYPE REF TO lcl_stack,
          go_top  TYPE REF TO lcl_stack.
ENDCLASS.                    "lcl_stack DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_stack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_stack IMPLEMENTATION.
  METHOD constructor.
    FREE: go_prev, go_top.
  ENDMETHOD.                    "constructor
  METHOD push.
    IF ( go_top IS BOUND ).
      CREATE OBJECT go_prev.
      go_prev = go_top.
    ENDIF.
    CREATE OBJECT go_top.
    go_top->set_data( iv_data ).
    go_top->set_prev( io_stack = go_prev ).
  ENDMETHOD.                    "push
  METHOD pop.
    IF ( go_top IS BOUND ).
      ev_data = go_top->get_data( ).
      go_top = go_top->get_prev( ).
    ENDIF.
  ENDMETHOD.                    "pop
  METHOD is_empty.
    ev_ret = -1.
    IF ( NOT go_top IS BOUND ).
      ev_ret = 0.
    ENDIF.
  ENDMETHOD.                    "is_empty
  METHOD get_top.
    IF ( go_top IS BOUND ).
      eo_stack = go_top.
    ENDIF.
  ENDMETHOD.                    "get_top
  METHOD get_prev.
    IF ( go_prev IS BOUND ).
      eo_stack = go_prev.
    ENDIF.
  ENDMETHOD.                    "get_prev
  METHOD set_data.
    gv_data = iv_data.
  ENDMETHOD.                    "set_data
  METHOD set_prev.
    go_prev = io_stack.
  ENDMETHOD.                    "set_prev
  METHOD set_top.
    go_top = io_stack.
  ENDMETHOD.                    "set_top
  METHOD get_data.
    ev_data = gv_data.
  ENDMETHOD.                    "get_data
ENDCLASS.                    "lcl_stack IMPLEMENTATION


START-OF-SELECTION.
  DATA: ldref     TYPE REF TO string,
        lo_stack  TYPE REF TO lcl_stack.

  FIELD-SYMBOLS <data> TYPE any.

  CREATE OBJECT lo_stack.


  CREATE DATA ldref.
  ldref->* = 'string 1'.
  lo_stack->push( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 2'.
  lo_stack->push( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 3'.
  lo_stack->push( ldref ).

  CREATE DATA ldref.
  ldref->* = 'string 4'.
  lo_stack->push( ldref ).


  WHILE ( lo_stack->is_empty( ) <> 0 ).
    ldref ?= lo_stack->pop( ).
    ASSIGN ldref->* TO <data>.
    WRITE / <data>.
  ENDWHILE.

  FREE lo_stack.
  CREATE OBJECT lo_stack.

  DATA: lt_vbap TYPE TABLE OF vbap,
        ls_vbap TYPE vbap.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vbap
           FROM vbap
           WHERE vbeln = '1000000038'.

  LOOP AT lt_vbap INTO ls_vbap.
    DATA: lref_vbap TYPE REF TO data.
    CREATE DATA lref_vbap TYPE vbap.
    FIELD-SYMBOLS: <ls_vbap> TYPE vbap.
    ASSIGN ('ls_vbap') TO <ls_vbap>.
    ASSIGN lref_vbap->* TO <ls_vbap>.
    <ls_vbap> = ls_vbap.
    lo_stack->push( lref_vbap ).
  ENDLOOP.

  WHILE ( lo_stack->is_empty( ) <> 0 ).
    lref_vbap ?= lo_stack->pop( ).
    ASSIGN lref_vbap->* TO <ls_vbap>.
    WRITE: / <ls_vbap>-vbeln, <ls_vbap>-posnr, <ls_vbap>-matnr, <ls_vbap>-kwmeng.
  ENDWHILE.

  FREE lo_stack.
