*&---------------------------------------------------------------------*
*&  Include           ZMMM_MODULA01_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_program DEFINITION INHERITING FROM zcl_mm_modula_01.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_program  TYPE sy-repid
                            iv_pfstatus TYPE sy-pfkey.
  PRIVATE SECTION.
    METHODS:
      modify_selections.
ENDCLASS.

CLASS lcl_program IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_program  = iv_program
                        iv_pfstatus = iv_pfstatus ).
    initialization( ).
    modify_selections( ).
  ENDMETHOD.
  METHOD modify_selections.
    DATA(ls_sel) = get_sel( ).
    FIELD-SYMBOLS: <sel> TYPE any.
    ASSIGN ls_sel TO <sel>.
    IF ( sy-subrc = 0 ).
      DATA: lo_structdescr TYPE REF TO cl_abap_structdescr.
      TRY.
          lo_structdescr ?= cl_abap_structdescr=>describe_by_data( <sel> ).
          LOOP AT lo_structdescr->get_components( ) INTO DATA(ls_components).
            DATA(lv_param) = '<sel>-' && ls_components-name.
            FIELD-SYMBOLS: <param>         TYPE any,
                           <parameter>     TYPE any,
                           <select_option> TYPE table.
            ASSIGN (lv_param) TO <param>.
            IF ( sy-subrc = 0 ).
              ASSIGN (ls_components-name) TO <parameter>.
              IF ( sy-subrc = 0 ).
                DATA: lo_tabledescr TYPE REF TO cl_abap_tabledescr.
                TRY.
                    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( <param> ).
                    DATA(lv_selecct_option) = ls_components-name && '[]'.
                    ASSIGN (lv_selecct_option) TO <select_option>.
                    <select_option>[] = <param>.
                  CATCH cx_root.
                    <parameter> = <param>.
                ENDTRY.
              ENDIF.
            ENDIF.
          ENDLOOP.
        CATCH cx_root.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
