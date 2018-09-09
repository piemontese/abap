TYPE-POOL zwrfc .

TYPES:
  " query parameter type
  BEGIN OF zwrfc_ty_s_query_params,
    name  TYPE string,   " parameter name
    value TYPE REF TO data,   " parmeter value
  END OF zwrfc_ty_s_query_params,
  " webrfc messages type
  BEGIN OF zwrfc_ty_s_messages,
    type TYPE c LENGTH 1,
    msg  TYPE string, c LENGTH 255,
  END OF zwrfc_ty_s_messages,
  " webrfc dictionary type
  BEGIN OF zwrfc_ty_s_dictionary,
    name         TYPE string,
    type         TYPE string,
    length       TYPE string,
    description  TYPE string,
    header_descr TYPE string,
    small_descr  TYPE string,
    medium_descr TYPE string,
    long_descr   TYPE string,
  END OF zwrfc_ty_s_dictionary ,
  " field list
  zwrfc_ty_s_fields TYPE string,
  " webrfc users
  zwrfc_ty_s_users  TYPE zwrfc_users.

TYPES:
  " Call params table type
  zwrfc_ty_t_call_params  TYPE TABLE OF zwrfc_call_param,
  " query parrameters table type
  zwrfc_ty_t_query_params TYPE TABLE OF zwrfc_ty_s_query_params,
  " messages table type
  zwrfc_ty_t_messages     TYPE TABLE OF zwrfc_ty_s_messages,
  " webrfc dictionary table type
  zwrfc_ty_t_dictionary   TYPE TABLE OF zwrfc_ty_s_dictionary,
  " field list table type
  zwrfc_ty_t_fields       TYPE TABLE OF zwrfc_ty_s_fields,
  " webrfc users table type
  zwrfc_ty_t_users        TYPE TABLE OF zwrfc_ty_s_users.
