*&---------------------------------------------------------------------*
*& Include ZPP_PDM_TOP                                       Report ZPP_PDM
*&
*&---------------------------------------------------------------------*
REPORT zpp_pdm.

CLASS lcl_salv DEFINITION INHERITING FROM zcl_bc_alv_base.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_program TYPE sy-repid.
ENDCLASS.

CLASS lcl_salv IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_program = iv_program ).
  ENDMETHOD.
ENDCLASS.

DATA: go_program TYPE REF TO zcl_pp_pdm_to_sap,
      go_salv    TYPE REF TO lcl_salv.
