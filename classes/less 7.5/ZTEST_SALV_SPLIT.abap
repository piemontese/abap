*&---------------------------------------------------------------------*
*& Report  ZTEST_SALV_SPLIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztest_salv_split.

TABLES: vbak.

TYPES: BEGIN OF ty_s_vbak,
         vbeln TYPE vbak-vbeln,
         vkorg TYPE vbak-vkorg,
         vtweg TYPE vbak-spart,
         spart TYPE vbak-spart,
         kunnr TYPE vbak-kunnr,
       END OF ty_s_vbak,
       BEGIN OF ty_s_vbap,
         posnr TYPE vbap-posnr,
         matnr TYPE vbap-matnr,
         arktx TYPE vbap-arktx,
         kwmeng TYPE vbap-kwmeng,
         vrkme TYPE vbap-vrkme,
       END OF ty_s_vbap,
       ty_t_vbak TYPE TABLE OF ty_s_vbak,
       ty_t_vbap TYPE TABLE OF ty_s_vbap.

DATA: g_docking_container_1 TYPE REF TO cl_gui_docking_container,
      g_docking_container_2 TYPE REF TO cl_gui_docking_container,
      g_docking_container_3 TYPE REF TO cl_gui_docking_container,
      g_docking_container_4 TYPE REF TO cl_gui_docking_container,
      go_salv_1 TYPE REF TO cl_salv_table,
      go_salv_2 TYPE REF TO cl_salv_table,
      go_salv_3 TYPE REF TO cl_salv_table,
      go_salv_4 TYPE REF TO cl_salv_table,
      gt_vbak TYPE ty_t_vbak,
      gt_vbap TYPE ty_t_vbap,
      ok_code TYPE sy-ucomm.


*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_user_command FOR EVENT added_function OF cl_salv_events
                             IMPORTING e_salv_function,
             on_double_click_1 FOR EVENT double_click OF cl_salv_events_table
                               IMPORTING row column,
             on_double_click_2 FOR EVENT double_click OF cl_salv_events_table
                               IMPORTING row column.
ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'BUTTON1'.
        MESSAGE s398(00) WITH 'Button1 clicked'.
      WHEN 'BUTTON2'.
        MESSAGE s398(00) WITH 'Button3 clicked'.
      WHEN 'BUTTON3'.
        MESSAGE s398(00) WITH 'Button2 clicked'.
    ENDCASE.
  ENDMETHOD.                    "on_user_command

  METHOD on_double_click_1.
    DATA: ls_vbak TYPE ty_s_vbak.
    READ TABLE gt_vbak INTO ls_vbak INDEX row.
    IF ( sy-subrc = 0 ).
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_vbap
               FROM vbap
               WHERE vbeln = ls_vbak-vbeln.
      TRY.
          CALL METHOD go_salv_2->refresh( refresh_mode = if_salv_c_refresh=>full ).
          CALL METHOD go_salv_3->refresh( refresh_mode = if_salv_c_refresh=>full ).
          CALL METHOD go_salv_4->refresh( refresh_mode = if_salv_c_refresh=>full ).
        CATCH cx_salv_data_error.
      ENDTRY.
    ENDIF.
  ENDMETHOD.                    "on_double_click

  METHOD on_double_click_2.
  ENDMETHOD.                    "on_double_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME.
SELECT-OPTIONS: s_vbeln FOR vbak-vbeln.
SELECTION-SCREEN END OF BLOCK 000.

START-OF-SELECTION.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_vbak UP TO 50 ROWS
           FROM vbak
           WHERE vbeln IN s_vbeln.

  CREATE OBJECT g_docking_container_4
    EXPORTING
      repid     = sy-repid
      dynnr     = '100'
      extension = 1200
      ratio     = 30
      side      = cl_gui_docking_container=>dock_at_right.

  CREATE OBJECT g_docking_container_3
    EXPORTING
      repid     = sy-repid
      dynnr     = '100'
      extension = 1200
      ratio     = 30
      side      = cl_gui_docking_container=>dock_at_right.

  CREATE OBJECT g_docking_container_1
    EXPORTING
      repid     = sy-repid
      dynnr     = '100'
      extension = 150
      side      = cl_gui_docking_container=>dock_at_top.

  CREATE OBJECT g_docking_container_2
    EXPORTING
      repid     = sy-repid
      dynnr     = '100'
      extension = 1200
      side      = cl_gui_docking_container=>dock_at_bottom.

*  CREATE OBJECT g_docking_container_4
*    EXPORTING
*      repid     = sy-repid
*      dynnr     = '100'
**     extension = 1200
*      side      = cl_gui_docking_container=>dock_at_top.

  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container  = g_docking_container_1
        IMPORTING
          r_salv_table = go_salv_1
        CHANGING
          t_table      = gt_vbak.

      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container  = g_docking_container_2
        IMPORTING
          r_salv_table = go_salv_2
        CHANGING
          t_table      = gt_vbap.

      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container  = g_docking_container_3
        IMPORTING
          r_salv_table = go_salv_3
        CHANGING
          t_table      = gt_vbap.

      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container  = g_docking_container_4
        IMPORTING
          r_salv_table = go_salv_4
        CHANGING
          t_table      = gt_vbap.


*      go_salv_1->set_screen_status( pfstatus = 'STANDARD_FULLSCREEN'
*                                    report = sy-repid
*                                    set_functions = go_salv_1->c_functions_all ).
*
*      go_salv_2->set_screen_status( pfstatus = 'STANDARD_FULLSCREEN'
*                                    report = sy-repid
*                                    set_functions = go_salv_2->c_functions_all ).
*
*      go_salv_3->set_screen_status( pfstatus = 'STANDARD_FULLSCREEN'
*                                    report = sy-repid
*                                    set_functions = go_salv_3->c_functions_all ).

      DATA: lo_event_handler TYPE REF TO lcl_event_handler.
      DATA: lo_events_1 TYPE REF TO cl_salv_events_table,
            lo_events_2 TYPE REF TO cl_salv_events_table,
            lo_events_3 TYPE REF TO cl_salv_events_table,
            lo_events_4 TYPE REF TO cl_salv_events_table.

      lo_events_1 = go_salv_1->get_event( ).
      lo_events_2 = go_salv_2->get_event( ).
      lo_events_3 = go_salv_3->get_event( ).
      lo_events_4 = go_salv_4->get_event( ).

      CREATE OBJECT lo_event_handler.

      SET HANDLER lo_event_handler->on_user_command FOR lo_events_1.
      SET HANDLER lo_event_handler->on_user_command FOR lo_events_2.
      SET HANDLER lo_event_handler->on_user_command FOR lo_events_3.
      SET HANDLER lo_event_handler->on_user_command FOR lo_events_4.
      SET HANDLER lo_event_handler->on_double_click_1 FOR lo_events_1.
      SET HANDLER lo_event_handler->on_double_click_2 FOR lo_events_1.

      DATA: lo_functions_1 TYPE REF TO cl_salv_functions_list,
            lo_functions_2 TYPE REF TO cl_salv_functions_list,
            lo_functions_3 TYPE REF TO cl_salv_functions_list,
            lo_functions_4 TYPE REF TO cl_salv_functions_list.

      lo_functions_1 = go_salv_1->get_functions( ).
      lo_functions_1->set_default( abap_true ).
      lo_functions_2 = go_salv_2->get_functions( ).
      lo_functions_2->set_default( abap_true ).
      lo_functions_3 = go_salv_3->get_functions( ).
      lo_functions_3->set_default( abap_true ).
      lo_functions_4 = go_salv_4->get_functions( ).
      lo_functions_4->set_default( abap_true ).
      DATA: l_text       TYPE string,
            l_icon       TYPE string.
      l_text = 'testo'.
      l_icon = icon_complete.
      TRY.
          lo_functions_1->add_function(
            name     = 'BUTTON1'
            icon     = l_icon
            text     = l_text
            tooltip  = 'Update Equipment Cost'
            position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions_2->add_function(
            name     = 'BUTTON2'
            icon     = l_icon
            text     = l_text
            tooltip  = 'Update Equipment Cost'
            position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions_3->add_function(
            name     = 'BUTTON3'
            icon     = l_icon
            text     = l_text
            tooltip  = 'Update Equipment Cost'
            position = if_salv_c_function_position=>right_of_salv_functions ).
        CATCH cx_salv_existing cx_salv_wrong_call.

      ENDTRY.
    CATCH cx_salv_msg.
  ENDTRY.

  go_salv_1->display( ).
  go_salv_2->display( ).
  go_salv_3->display( ).
  go_salv_4->display( ).

  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'E' OR
         'ECAN' OR
         'ENDE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
