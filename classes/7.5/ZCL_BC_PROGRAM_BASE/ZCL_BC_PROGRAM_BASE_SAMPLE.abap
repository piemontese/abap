*&---------------------------------------------------------------------*
*& Report zcl_bc_program_base_sample
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcl_bc_program_base_sample.


CLASS lcl_program DEFINITION INHERITING FROM zcl_bc_program_base.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_program TYPE sy-repid.
ENDCLASS.

CLASS lcl_program IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_program = iv_program ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_program TYPE REF TO lcl_program.

  lo_program = NEW #( iv_program = sy-repid  ).
