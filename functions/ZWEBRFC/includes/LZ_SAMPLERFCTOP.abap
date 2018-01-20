FUNCTION-POOL z_samplerfc.                  "MESSAGE-ID ..

TYPES: BEGIN OF ty_s_messages,
         type        TYPE c LENGTH 1,
         msg         TYPE c LENGTH 255,
       END OF ty_s_messages,
       "
       BEGIN OF ty_s_menu_item,
         parent      TYPE string,
         item        TYPE string,
         description TYPE string,
         action      TYPE string,
         auth        TYPE string,
         visible     TYPE string,
       END OF ty_s_menu_item,
       "
       BEGIN OF ty_s_menu_action,
         parent      TYPE string,
         item        TYPE string,
         title       TYPE string,
         method      TYPE string,
       END OF ty_s_menu_action,
       "
       BEGIN OF ty_s_method_field,
         field       TYPE string,
         description TYPE string,
         type        TYPE string,
         required    TYPE string,
         value       TYPE string,
         length      TYPE string,
         data        TYPE string,  "REF TO data,
         minlength   TYPE string,
         maxlength   TYPE string,
         step        TYPE string,
         valid       TYPE string,
       END OF ty_s_method_field,
       ty_t_method_fields TYPE TABLE OF ty_s_method_field,
       "
       BEGIN OF ty_s_method_action,
         method      TYPE string,
         description TYPE string,
       END OF ty_s_method_action,
       "
       BEGIN OF ty_s_methods,
         method      TYPE string,
         fields      TYPE string,  "REF TO data,
         steps       TYPE string,  "REF TO data,
       END OF ty_s_methods.




TYPES: ty_t_w3html   TYPE TABLE OF w3html,
       ty_t_messages TYPE TABLE OF ty_s_messages.

DATA: gv_camel_case TYPE string.


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
