FUNCTION-POOL z_samplerfc.                  "MESSAGE-ID ..

TYPES: BEGIN OF ty_s_messages,
         type TYPE c LENGTH 1,
         msg  TYPE c LENGTH 255,
       END OF ty_s_messages.

TYPES: ty_t_w3html   TYPE TABLE OF w3html,
       ty_t_messages TYPE TABLE OF ty_s_messages.


*&---------------------------------------------------------------------*
*&       Class LCL_JSON
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_json DEFINITION.
  PUBLIC SECTION.
*    METHODS: constructor.
ENDCLASS.               "LCL_JSON


*----------------------------------------------------------------------*
*       CLASS lcl_function DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_function DEFINITION.

ENDCLASS.                    "lcl_function DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_menu DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_menu DEFINITION.

ENDCLASS.                    "lcl_menu DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_table DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_table DEFINITION.

ENDCLASS.                    "lcl_table DEFINITION

*&---------------------------------------------------------------------*
*&       Class LCL_WEBRFC
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_webrfc DEFINITION.
  PUBLIC SECTION.
    DATA: go_json     TYPE REF TO lcl_json,
          go_function TYPE REF TO lcl_function,
          go_menu     TYPE REF TO lcl_menu,
          go_table    TYPE REF TO lcl_table.

ENDCLASS.               "LCL_WEBRFC

* INCLUDE LZ_SAMPLERFCD...                   " Local class definition
