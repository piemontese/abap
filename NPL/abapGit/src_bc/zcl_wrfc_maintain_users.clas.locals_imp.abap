*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_sender TYPE REF TO zcl_wrfc_maintain_users
                            io_alv    TYPE REF TO cl_salv_table
                            io_sel    TYPE REF TO cl_salv_selections,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
  PRIVATE SECTION.
    DATA: go_sender TYPE REF TO zcl_wrfc_maintain_users,
          go_alv    TYPE REF TO cl_salv_table,
          go_sel    TYPE REF TO cl_salv_selections.
*    METHODS:
*      edit.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD constructor.
    go_sender = io_sender.
    go_alv = io_alv.
    go_sel = io_sel.
  ENDMETHOD.
  METHOD on_user_command.
    DATA: lo_selections TYPE REF TO cl_salv_selections,
          lo_salv       TYPE REF TO cl_salv_table.
    DATA: lt_rows TYPE salv_t_row.
    DATA: ls_rows TYPE i.
    DATA: message TYPE string.
    CASE e_salv_function.
      WHEN 'EDIT'.
        TRY.
            go_sender->edit_user( iv_show_alv = abap_true ).
            go_sender->set_refresh_data( abap_true ).
          CATCH cx_root INTO DATA(lcx_root).
            MESSAGE lcx_root->get_text( ) TYPE 'S'.
        ENDTRY.
      WHEN 'ADD'.
        TRY.
            go_sender->add_user( iv_show_alv = abap_true ).
            go_sender->set_refresh_data( abap_true ).
          CATCH cx_root INTO lcx_root.
            MESSAGE lcx_root->get_text( ) TYPE 'S'.
        ENDTRY.
      WHEN 'REMOVDE'.
        TRY.
            go_sender->remove_user( iv_show_alv = abap_true ).
            go_sender->set_refresh_data( abap_true ).
          CATCH cx_root INTO lcx_root.
            MESSAGE lcx_root->get_text( ) TYPE 'S'.
        ENDTRY.
*      WHEN 'REFRESH'.
*        go_sender->set_refresh_data( abap_true ).
*        go_sender->refresh_data( ).
*        go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
*      WHEN '&&BACK'.
*        LEAVE TO SCREEN 0.
    ENDCASE.
    TRY.
        IF ( go_sender->get_refresh_data( ) = abap_true ).
          go_sender->refresh_data( ).
        ENDIF.
        go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
