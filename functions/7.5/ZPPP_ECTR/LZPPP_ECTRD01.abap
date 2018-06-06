*----------------------------------------------------------------------*
***INCLUDE LZPPP_ECTRD01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class lcl_base
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_base DEFINITION.
  PUBLIC SECTION.
*    types: begin of ty_s_params,
*             ...,
*           end of ty_s_params.

    METHODS:
      constructor IMPORTING iv_msgid TYPE sy-msgid OPTIONAL
                  RAISING   zcx_bc_exception,
      is_initial IMPORTING iv_value TYPE any
                           iv_msgno TYPE sy-msgno OPTIONAL
                           iv_msgv1 TYPE c OPTIONAL
                           iv_msgv2 TYPE c OPTIONAL
                           iv_msgv3 TYPE c OPTIONAL
                           iv_msgv4 TYPE c OPTIONAL
                 RAISING   zcx_bc_exception,
      is_table_initial IMPORTING it_table TYPE ANY TABLE
                                 iv_msgno TYPE sy-msgno OPTIONAL
                                 iv_msgv1 TYPE c OPTIONAL
                                 iv_msgv2 TYPE c OPTIONAL
                                 iv_msgv3 TYPE c OPTIONAL
                                 iv_msgv4 TYPE c OPTIONAL
                       RAISING   zcx_bc_exception,
      is_exists IMPORTING iv_table  TYPE string
                          it_fields TYPE zsbc_fields_t
                          iv_msgno  TYPE sy-msgno OPTIONAL
                          iv_msgv1  TYPE c OPTIONAL
                          iv_msgv2  TYPE c OPTIONAL
                          iv_msgv3  TYPE c OPTIONAL
                          iv_msgv4  TYPE c OPTIONAL
                RAISING   zcx_bc_exception,
      check_domain_values IMPORTING iv_field TYPE any
                          RAISING   zcx_bc_exception,
      get_error RETURNING VALUE(rv_errid) TYPE numc3.

  PROTECTED SECTION.
    CLASS-DATA: gv_msgid TYPE sy-msgid,
                gv_errid TYPE n LENGTH 3.
*    DATA: gs_params TYPE ty_s_params.
ENDCLASS.

*&---------------------------------------------------------------------*
*&       Class lcl_Z_MMM_ECTR_MM01
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_z_mmm_ectr_mm01 DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_params_1,
             tipologia        TYPE  zpp_ectr_tipo,
             ambito           TYPE  zpp_ectr_amb,
             maktx            TYPE  makt-maktx,
             maktx2           TYPE  makt-maktx,
             matkl            TYPE  mara-matkl,
             meins            TYPE  mara-meins,
             brgew            TYPE  mara-brgew,
             ntgew            TYPE  mara-ntgew,
             gewei            TYPE  mara-gewei,
             volum            TYPE  mara-volum,
             voleh            TYPE  mara-voleh,
             laeng            TYPE  mara-laeng,
             hoehe            TYPE  mara-hoehe,
             breit            TYPE  mara-breit,
             meabm            TYPE  mara-meabm,
             zzmaterial_descr TYPE  zmm_material_t-zzmaterial_descr,
             document         TYPE  bapi_doc_keys,
           END OF ty_s_params_1.

    METHODS:
      constructor IMPORTING iv_msgid            TYPE sy-msgid OPTIONAL
                            iv_tipologia        TYPE  char1
                            iv_ambito           TYPE  char1
                            iv_maktx            TYPE  makt-maktx
                            iv_maktx2           TYPE  makt-maktx
                            iv_matkl            TYPE  mara-matkl
                            iv_meins            TYPE  mara-meins
                            iv_brgew            TYPE  mara-brgew
                            iv_ntgew            TYPE  mara-ntgew
                            iv_gewei            TYPE  mara-gewei
                            iv_volum            TYPE  mara-volum
                            iv_voleh            TYPE  mara-voleh
                            iv_laeng            TYPE  mara-laeng
                            iv_hoehe            TYPE  mara-hoehe
                            iv_breit            TYPE  mara-breit
                            iv_meabm            TYPE  mara-meabm
                            iv_zzmaterial_descr TYPE  zmm_material_t-zzmaterial_descr
                            is_document         TYPE  bapi_doc_keys
                  RAISING   zcx_bc_exception,
      check_tipologia RAISING   zcx_bc_exception,
      check_ambito RAISING   zcx_bc_exception,
      create_material RETURNING VALUE(rv_matnr) TYPE matnr
                      RAISING   zcx_bc_exception.

  PROTECTED SECTION.
    DATA: gs_params_1 TYPE ty_s_params_1.

    METHODS:
      get_material RETURNING VALUE(rv_matnr) TYPE matnr
                   RAISING   zcx_bc_exception,
      enqueue RAISING   zcx_bc_exception,
      dequeue IMPORTING iv_matnr TYPE matnr OPTIONAL
              RAISING   zcx_bc_exception,
      get_ztmm_ekgrp IMPORTING iv_matnr             TYPE matnr
                     RETURNING VALUE(es_ztmm_ekgrp) TYPE ztmm_ekgrp
                     RAISING   zcx_bc_exception.
ENDCLASS.

*&---------------------------------------------------------------------*
*&       Class lcl_z_ppp_cade_cs01
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_z_ppp_cade_cs01 DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_params_1,
             rootname TYPE zpp_albero-rootname,
             werks    TYPE werks_d,
             t_bom_in TYPE zpp_bom_in_t,
           END OF ty_s_params_1.

    METHODS:
      constructor IMPORTING iv_msgid    TYPE sy-msgid OPTIONAL
                            iv_rootname TYPE zpp_albero-rootname
                            iv_werks    TYPE werks_d
                            it_bom_in   TYPE zpp_bom_in_t
                  RAISING   zcx_bc_exception,
      maintain_bom.

  PRIVATE SECTION.
    DATA: gs_params_1 TYPE ty_s_params_1.

    METHODS:
      create_bom,
      change_bom.
ENDCLASS.

*&---------------------------------------------------------------------*
*&       Class lcl_z_ppp_cade_cs15
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_z_ppp_cade_cs15 DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_params_1,
             werks TYPE werks_d,
             matnr TYPE matnr,
           END OF ty_s_params_1.

    METHODS:
      constructor IMPORTING iv_msgid TYPE sy-msgid OPTIONAL
                            iv_werks TYPE werks_d
                            iv_matnr TYPE matnr
                  RAISING   zcx_bc_exception,
      get_where_used RETURNING VALUE(rt_cs15) TYPE zmm_cs15_t
                     RAISING   zcx_bc_exception.

  PRIVATE SECTION.
    DATA: gs_params_1 TYPE ty_s_params_1,
          gt_cs15     TYPE zmm_cs15_t.

ENDCLASS.
