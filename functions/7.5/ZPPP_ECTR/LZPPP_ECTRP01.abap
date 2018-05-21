*----------------------------------------------------------------------*
***INCLUDE LZPPP_ECTRP01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_base
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_base IMPLEMENTATION.
  METHOD constructor.
    gv_msgid = iv_msgid.
    TRANSLATE gv_msgid TO UPPER CASE.
    gv_errid = '001'.
  ENDMETHOD.

  METHOD is_initial.
    IF ( iv_value IS INITIAL ).
      IF ( gv_msgid IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_bc_exception.
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER iv_msgno WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4..
      ENDIF.
    ENDIF.
    ADD 1 TO gv_errid.
  ENDMETHOD.

  METHOD is_exists.
    DATA(lv_table) = iv_table.
    DATA(lt_fields) = it_fields[].
    DATA: lv_query  TYPE string,
          lv_where  TYPE string,
          lv_fields TYPE string.

    TRANSLATE lv_table TO UPPER CASE.

    CLEAR: lv_query, lv_where, lv_fields.
    LOOP AT lt_fields INTO DATA(ls_fields).
      TRANSLATE ls_fields-name TO UPPER CASE.
      TRANSLATE ls_fields-value TO UPPER CASE.
      IF ( lv_fields IS INITIAL ).
        lv_fields = ls_fields-name.
      ELSE.
        lv_fields = lv_fields && ` ` && ls_fields-name.
      ENDIF.
      IF ( lv_where IS INITIAL ).
        lv_where = ls_fields-name && ` = '` && ls_fields-value && `'`.
      ELSE.
        lv_where = lv_where && ` AND ` && ls_fields-name && ` = '` && ls_fields-value && `'`.
      ENDIF.
    ENDLOOP.

    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lref_table      TYPE REF TO data.
    FIELD-SYMBOLS: <ls_table> TYPE any.

    TRY.
        lo_struct_descr ?= cl_abap_structdescr=>describe_by_name( iv_table ).
      CATCH cx_root INTO DATA(lx_root).
    ENDTRY.

    CREATE DATA lref_table TYPE HANDLE lo_struct_descr.
    ASSIGN lref_table->* TO <ls_table>.

    SELECT SINGLE (lv_fields) INTO <ls_table> FROM (iv_table) WHERE (lv_where).
    IF ( sy-subrc <> 0 ).
      IF ( gv_msgid IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_bc_exception.
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER iv_msgno WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4..
      ENDIF.
    ENDIF.
    ADD 1 TO gv_errid.
  ENDMETHOD.

  METHOD get_error.
    rv_errid = gv_errid.
  ENDMETHOD.
ENDCLASS.               "lcl_base

*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_z_mmm_ectr_mm01
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_z_mmm_ectr_mm01 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_msgid = iv_msgid ).
    gs_params_1-tipologia  = iv_tipologia.
    gs_params_1-ambito     = iv_ambito.
    gs_params_1-maktx      = iv_maktx.
    gs_params_1-maktx2     = iv_maktx2.
    gs_params_1-matkl      = iv_matkl.
    gs_params_1-meins      = iv_meins.
    IF ( iv_gewei = 'g' OR iv_gewei = 'G' ).
      gs_params_1-gewei      = 'KG'.
      gs_params_1-brgew      = iv_brgew / 1000.
      gs_params_1-ntgew      = iv_ntgew / 1000.
    ENDIF.
    gs_params_1-volum      = iv_volum.
    gs_params_1-voleh      = iv_voleh.
    gs_params_1-laeng      = iv_laeng.
    gs_params_1-hoehe      = iv_hoehe.
    gs_params_1-breit      = iv_breit.
    gs_params_1-meabm      = iv_meabm.
    gs_params_1-zzmaterial_descr = iv_zzmaterial_descr.
    gs_params_1-document   = is_document.
  ENDMETHOD.

  METHOD check_tipologia.
    IF ( gs_params_1-tipologia <> 'L' AND
         gs_params_1-tipologia <> 'F' AND
         gs_params_1-tipologia <> 'P' AND
         gs_params_1-tipologia <> 'C' ).
      IF ( gv_msgid IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_bc_exception.
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER '008' WITH gs_params_1-tipologia.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_ambito.
    IF ( gs_params_1-ambito <> '5' AND
         gs_params_1-ambito <> '8' ).
      IF ( gv_msgid IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_bc_exception.
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER '009' WITH gs_params_1-ambito.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_material.
    DATA: ls_headdata             TYPE bapimathead,
          ls_clientdata           TYPE bapi_mara,
          ls_clientdatax          TYPE bapi_marax,
          ls_materialdescription  TYPE bapi_makt,
          ls_materiallongtext     TYPE bapi_mltx,
          ls_salesdata            TYPE bapi_mvke,
          ls_salesdatax           TYPE bapi_mvkex,
          ls_plantdata            TYPE bapi_marc,
          ls_plantdata01          TYPE bapi_marc,   "Tonolli_G - Ticket 9421 - 15/12/2017 - Ins
          ls_plantdatax           TYPE bapi_marcx,
          ls_storagelocationdata  TYPE bapi_mard,
          ls_storagelocationdatax TYPE bapi_mardx,
          ls_valuationdata        TYPE bapi_mbew,
          ls_valuationdatax       TYPE bapi_mbewx,
          ls_taxclassifications   TYPE bapi_mlan,
          ls_unitsofmeasure       TYPE bapi_marm,
          ls_unitsofmeasurex      TYPE bapi_marmx,
          ls_return               TYPE bapiret2,
          lt_materialdescription  TYPE TABLE OF bapi_makt,
          lt_materiallongtext     TYPE TABLE OF bapi_mltx,
          lt_taxclassifications   TYPE TABLE OF bapi_mlan,
          lt_unitsofmeasure       TYPE TABLE OF bapi_marm,
          lt_unitsofmeasurex      TYPE TABLE OF bapi_marmx,
          lt_returnmessages       TYPE TABLE OF bapi_matreturn2,
          lv_matnr                TYPE matnr.
*          ls_ztbc000004           TYPE ztbc000004.

    rv_matnr = get_material( ).

*    ls_ztbc000004 = get_create_param( iv_type = is_data_mat-tipofile ).

    CLEAR: ls_headdata, ls_clientdata, ls_clientdatax, ls_materialdescription, ls_materiallongtext,
           ls_salesdata, ls_salesdatax, ls_taxclassifications, ls_plantdata, ls_plantdatax,
           ls_storagelocationdata, ls_storagelocationdatax, ls_valuationdata, ls_valuationdatax,
           ls_return, ls_plantdata01, ls_unitsofmeasure, ls_unitsofmeasurex.

    REFRESH: lt_materialdescription, lt_materiallongtext, lt_taxclassifications, lt_returnmessages, lt_unitsofmeasure, lt_unitsofmeasurex.

*    CLEAR ls_pdm_am.
*    MOVE-CORRESPONDING is_data_mat TO ls_pdm_am.
    IF ( NOT rv_matnr IS INITIAL ).

      zcl_bc_conversion_exit=>conversion_input( EXPORTING iv_field = rv_matnr
                                                CHANGING  cv_field = lv_matnr ).

*      SELECT SINGLE matnr INTO lv_matnr
*             FROM mara
*             WHERE matnr = is_data_mat-matnr.
*      IF ( sy-subrc = 0 ).
*        ls_pdm_am-anamat_flg = 'Y'.
*        APPEND ls_pdm_am TO ct_pdm_am.
*        RETURN.
*      ENDIF.

      DATA: lv_disgr TYPE marc-disgr.
      SELECT SINGLE webaz dzeit plifz beskz sobsl disgr maabc kondm rgekz
                    lgpro                                                 "Tonolli_G - Ticket 9421 - 15/12/2017 - Ins
             INTO (ls_plantdata-gr_pr_time, ls_plantdata-inhseprodt, ls_plantdata-plnd_delry,
                   ls_plantdata-proc_type, ls_plantdata-spproctype, ls_plantdata-mrp_group,
                   ls_plantdata-abc_id, ls_salesdata-mat_pr_grp, ls_plantdata-backflush,
                   ls_plantdata01-iss_st_loc )                            "Tonolli_G - Ticket 9421 - 15/12/2017 - Ins
             FROM zpp_inter_matkl
             WHERE matkl = gs_params_1-matkl.

      ls_headdata-material = lv_matnr.
      ls_headdata-ind_sector = 'M'.
      ls_headdata-matl_type = 'ZP0' && lv_matnr(1).
      ls_headdata-basic_view = abap_true.
      ls_headdata-sales_view = abap_true.
      ls_headdata-purchase_view = abap_true.
      ls_headdata-mrp_view = abap_true.
      ls_headdata-forecast_view = abap_false.
      ls_headdata-work_sched_view = abap_true.
      ls_headdata-prt_view = abap_false.
      ls_headdata-storage_view = abap_true.
      ls_headdata-warehouse_view = abap_false.
      ls_headdata-quality_view = abap_true.
      ls_headdata-account_view = abap_true.
      ls_headdata-cost_view = abap_true.

*    ls_clientdata-MATL_GROUP = è il gruppo merci del file (CATEGORIA MERC.)
      ls_clientdata-matl_group = gs_params_1-matkl.
      ls_clientdata-base_uom = gs_params_1-meins.
      ls_clientdata-base_uom_iso = ls_clientdata-base_uom.
      ls_clientdata-pur_status = space. "'10'.
*    IF ( is_data_mat-matnr(1) = '5' ).
*      ls_clientdata-item_cat = 'ZMTS'.
*    ELSEIF ( is_data_mat-matnr(1) = '8' ).
*      ls_clientdata-item_cat = 'ZMTO'.
*    ENDIF.
      ls_clientdata-division     = '02'.
      ls_clientdata-pur_valkey   = '3'.

      IF ( NOT gs_params_1-ntgew IS INITIAL ).
        ls_clientdata-net_weight = gs_params_1-ntgew.
      ENDIF.
      IF ( NOT gs_params_1-gewei IS INITIAL ).
        ls_clientdata-unit_of_wt = gs_params_1-gewei.
        ls_clientdata-unit_of_wt_iso = ls_clientdata-unit_of_wt.
      ENDIF.

      ls_clientdatax-matl_group   = abap_true.
      ls_clientdatax-base_uom     = abap_true.
      ls_clientdatax-base_uom_iso = abap_true.
*    ls_clientdatax-item_cat     = abap_true.
      ls_clientdatax-division     = abap_true.
      ls_clientdatax-pur_valkey   = abap_true.
      ls_clientdatax-pur_status   = abap_true.

      IF ( NOT gs_params_1-ntgew IS INITIAL ).
        ls_clientdatax-net_weight = abap_true.
      ENDIF.
      IF ( NOT gs_params_1-gewei IS INITIAL ).
        ls_clientdatax-unit_of_wt = abap_true.
        ls_clientdatax-unit_of_wt_iso = abap_true.
      ENDIF.

      IF ( NOT gs_params_1-brgew IS INITIAL ).
        ls_unitsofmeasure-alt_unit = gs_params_1-meins.
*        ls_unitsofmeasure-alt_unit_iso = gs_params_1-meins.
        ls_unitsofmeasure-gross_wt = gs_params_1-brgew.
        ls_unitsofmeasure-unit_of_wt = gs_params_1-gewei.
*        ls_unitsofmeasure-unit_of_wt_iso = gs_params_1-gewei.

        ls_unitsofmeasurex-alt_unit = gs_params_1-meins.
*        ls_unitsofmeasurex-alt_unit_iso = abap_true.
        ls_unitsofmeasurex-gross_wt = abap_true.
        ls_unitsofmeasurex-unit_of_wt = abap_true.
*        ls_unitsofmeasurex-unit_of_wt_iso = abap_true.
      ENDIF.

      IF ( NOT gs_params_1-volum IS INITIAL ).
        ls_unitsofmeasure-alt_unit = gs_params_1-meins.
*        ls_unitsofmeasure-alt_unit_iso = gs_params_1-meins.
        ls_unitsofmeasure-volume = gs_params_1-volum.
        ls_unitsofmeasure-volumeunit = gs_params_1-voleh.
        ls_unitsofmeasure-volumeunit_iso = gs_params_1-voleh.

        ls_unitsofmeasurex-alt_unit = gs_params_1-meins.
*        ls_unitsofmeasurex-alt_unit_iso = abap_true.
        ls_unitsofmeasurex-volume = abap_true.
        ls_unitsofmeasurex-volumeunit = abap_true.
*        ls_unitsofmeasurex-volumeunit_iso = abap_true.
      ENDIF.

      IF ( NOT gs_params_1-laeng IS INITIAL OR
           NOT gs_params_1-hoehe IS INITIAL OR
           NOT gs_params_1-breit IS INITIAL ).
        ls_unitsofmeasure-alt_unit = gs_params_1-meins.
*        ls_unitsofmeasure-alt_unit_iso = gs_params_1-meins.
        ls_unitsofmeasure-length = gs_params_1-laeng.
        ls_unitsofmeasure-width = gs_params_1-breit.
        ls_unitsofmeasure-height = gs_params_1-hoehe.
        ls_unitsofmeasure-unit_dim = gs_params_1-meabm.
        ls_unitsofmeasure-unit_dim_iso = gs_params_1-meabm.

        ls_unitsofmeasurex-alt_unit = gs_params_1-meins.
*        ls_unitsofmeasurex-alt_unit_iso = abap_true.
        ls_unitsofmeasurex-length = abap_true.
        ls_unitsofmeasurex-width = abap_true.
        ls_unitsofmeasurex-height = abap_true.
        ls_unitsofmeasurex-unit_dim = abap_true.
        ls_unitsofmeasurex-unit_dim_iso = abap_true.
      ENDIF.

      IF ( NOT ls_unitsofmeasure IS INITIAL ).
        APPEND ls_unitsofmeasure TO lt_unitsofmeasure.
      ENDIF.

      IF ( NOT ls_unitsofmeasurex IS INITIAL ).
        APPEND ls_unitsofmeasurex TO lt_unitsofmeasurex.
      ENDIF.



      ls_materialdescription-langu = 'I'.
      ls_materialdescription-matl_desc = gs_params_1-maktx.
      APPEND ls_materialdescription TO lt_materialdescription.

      ls_materiallongtext-text_id = 'GRUN'.
      ls_materiallongtext-text_name = lv_matnr.
      ls_materiallongtext-langu = 'I'.
      ls_materiallongtext-text_line = gs_params_1-maktx2.
      APPEND ls_materiallongtext TO lt_materiallongtext.

      ls_salesdata-sales_org  = '6301'.
      ls_salesdata-distr_chan = '02'.
      ls_salesdata-delyg_plnt = '6301'.
      ls_salesdata-acct_assgt = '04'.
*      IF ( is_data_mat-matnr(1) = '5' ).
*        ls_salesdata-item_cat = 'ZMTS'.
*      ELSEIF ( is_data_mat-matnr(1) = '8' ).
*        ls_salesdata-item_cat = 'ZMTO'.
*      ENDIF.
      ls_salesdata-item_cat = ls_plantdata-mrp_group.

      ls_salesdatax-sales_org  = ls_salesdata-sales_org.
      ls_salesdatax-distr_chan = ls_salesdata-distr_chan.
      ls_salesdatax-delyg_plnt = abap_true.
      ls_salesdatax-acct_assgt = abap_true.
      ls_salesdatax-item_cat   = abap_true.
      ls_salesdatax-mat_pr_grp = abap_true.

      ls_taxclassifications-depcountry = 'IT'.
      ls_taxclassifications-tax_type_1 = 'MWST'.
      ls_taxclassifications-taxclass_1 = '1'.
      APPEND ls_taxclassifications TO lt_taxclassifications.
      ls_taxclassifications-tax_type_1 = 'LCIT'.
      ls_taxclassifications-taxclass_1 = '0'.
      APPEND ls_taxclassifications TO lt_taxclassifications.
*     Tonolli_G - Ticket 9406 - 13/12/2017 - Aliquota split payment anagrafica materiale - Start
      ls_taxclassifications-tax_type_1 = 'ZMW0'.
      ls_taxclassifications-taxclass_1 = '1'.
      APPEND ls_taxclassifications TO lt_taxclassifications.
*     Tonolli_G - Ticket 9406 - 13/12/2017 - Aliquota split payment anagrafica materiale - End

*   PLANTDATA ---------------------------------------------------------------------------------
      ls_plantdata-plant      = '6301'.
      ls_plantdata-availcheck = '02'.
      ls_plantdata-loadinggrp = '0001'.
      ls_plantdata-countryori = 'IT'.
      ls_plantdata-regionorig = 'MN'.
      DATA(ls_ztmm_ekgrp) = get_ztmm_ekgrp( iv_matnr = lv_matnr ).
      ls_plantdata-pur_group = ls_ztmm_ekgrp-ekgrp.
*    SELECT SINGLE ekgrp INTO ls_plantdata-pur_group
*           FROM ztmm_ekgrp
*           WHERE matnr = is_data_mat-matnr.
*    IF ( sy-subrc <> 0 ).
*      SELECT SINGLE ekgrp INTO ls_plantdata-pur_group
*             FROM ztmm_ekgrp
*             WHERE matnr = space.
*    ENDIF.
      ls_plantdata-mrp_type = 'PD'.
      ls_plantdata-lotsizekey = 'Z3'.
      ls_plantdata-sloc_exprc = '6300'.   "Magazzino default per approvvigionamento esterno
      ls_plantdata-period_ind = 'M'.

*******************************************************************************
      IF ( lv_matnr(1) = '5' ).
*     ls_plantdata-backflush = '1'.
*        ls_plantdata-mrp_group = 'ZMTS'.

* SV BEGIN Ticket 6884
        CASE ls_clientdata-matl_group.
          WHEN '522' OR '523' OR '524'.
            ls_plantdata-iss_st_loc = '6331'.   "Magazzino produzione
            ls_plantdata-determ_grp = 'Z002'.   "Gruppo det. stock
          WHEN OTHERS.
            ls_plantdata-iss_st_loc = '6332'.   "Magazzino produzione
            ls_plantdata-determ_grp = 'Z001'.   "Gruppo det. stock
        ENDCASE.
* SV END Ticket 6884
        ls_plantdata-prodprof = 'Z00002'.  " provvisorio, da definire regola
        ls_plantdata-mrp_ctrler = '005'.
      ELSEIF ( lv_matnr(1) = '8' ).
*       ls_plantdata-backflush = '1'.
*        ls_plantdata-mrp_group = 'ZMTO'.
        ls_plantdata-iss_st_loc = '6312'.       "Magazzino produzione
        ls_plantdata-determ_grp = 'Z001'.       "Gruppo det. stock
        ls_plantdata-prodprof = 'Z00001'.
        ls_plantdata-mrp_ctrler = '001'.  " provvisorio, da definire regola
      ENDIF.   "IF ( is_data_mat-matnr(1) = '5' ).
*     Tonolli_G - Ticket 9421 - 15/12/2017 - Start Ins
      IF ls_plantdata01-iss_st_loc IS NOT INITIAL.
        ls_plantdata-iss_st_loc = ls_plantdata01-iss_st_loc.   "Magazzino produzione
      ENDIF.
*     Tonolli_G - Ticket 9421 - 15/12/2017 - End Ins
*******************************************************************************

      IF ( ls_plantdata-mrp_group = 'ZMTS' ).
        ls_plantdata-dep_req_id = '2'.
      ELSEIF ( ls_plantdata-mrp_group = 'ZMTO' ).
        ls_plantdata-dep_req_id = '1'.
      ENDIF.
      ls_plantdata-auto_p_ord = 'X'.
      ls_plantdata-sm_key = '000'.
*      ls_plantdata-comm_code    = is_data_mat-filler18.
*      ls_plantdata-profit_ctr = '639901'.
      zcl_bc_conversion_exit=>conversion_input( EXPORTING iv_field = '639901'
                                                CHANGING cv_field = ls_plantdata-profit_ctr ).
*   --------------------------------------------------------------------------------- PLANTDATA

      ls_plantdatax-plant      = '6301'.
      ls_plantdatax-abc_id     = abap_true.
      ls_plantdatax-availcheck = abap_true.
      ls_plantdatax-loadinggrp = abap_true.
      ls_plantdatax-countryori = abap_true.
      ls_plantdatax-regionorig = abap_true.
      ls_plantdatax-pur_group  = abap_true.
      ls_plantdatax-gr_pr_time = abap_true.
      ls_plantdatax-inhseprodt = abap_true.
      ls_plantdatax-plnd_delry = abap_true.
      ls_plantdatax-proc_type  = abap_true.
      ls_plantdatax-spproctype = abap_true.
      ls_plantdatax-mrp_type   = abap_true.
      ls_plantdatax-lotsizekey = abap_true.
      ls_plantdatax-sloc_exprc = abap_true.
      ls_plantdatax-period_ind = abap_true.
      ls_plantdatax-backflush  = abap_true.
      ls_plantdatax-mrp_group  = abap_true.
      ls_plantdatax-iss_st_loc = abap_true.
      ls_plantdatax-determ_grp = abap_true.
      ls_plantdatax-prodprof   = abap_true.
      ls_plantdatax-mrp_ctrler = abap_true.
      ls_plantdatax-dep_req_id = abap_true.
      ls_plantdatax-auto_p_ord = abap_true.
      ls_plantdatax-sm_key     = abap_true.
      ls_plantdatax-comm_code  = abap_true.

      ls_storagelocationdata-plant = '6301'.
      IF ( lv_matnr(1) = '5' ).
        ls_storagelocationdata-stge_loc = '6332'.
      ELSEIF ( lv_matnr(1) = '8' ).
        ls_storagelocationdata-stge_loc = '6312'.
      ENDIF.

      ls_storagelocationdatax-plant = '6301'.
      ls_storagelocationdatax-stge_loc = ls_storagelocationdata-stge_loc.

      ls_valuationdata-val_area = '6301'.
      IF ( lv_matnr(1) = '5' ).
        ls_valuationdata-val_class = 'MP02'.
      ELSEIF ( lv_matnr(1) = '8' ).
        ls_valuationdata-val_class = 'MP03'.
      ENDIF.
      ls_valuationdata-qty_struct  = abap_true.
      ls_valuationdata-orig_mat    = abap_true.
      ls_valuationdata-moving_pr   = '0.01'.  "DP-07_10_2016
      ls_valuationdata-price_unit  = '1'.  "SB. 20170124

      ls_valuationdatax-val_area   = '6301'.
      ls_valuationdatax-val_class  = abap_true.
      ls_valuationdatax-qty_struct = abap_true.
      ls_valuationdatax-orig_mat   = abap_true.
      ls_plantdatax-profit_ctr     = abap_true.
      ls_valuationdatax-moving_pr  = abap_true. "DP-07_10_2016
      ls_valuationdatax-price_unit = abap_true. "DP-07_10_2016

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata             = ls_headdata
          clientdata           = ls_clientdata
          clientdatax          = ls_clientdatax
          plantdata            = ls_plantdata
          plantdatax           = ls_plantdatax
*         FORECASTPARAMETERS   =
*         FORECASTPARAMETERSX  =
*         PLANNINGDATA         =
*         PLANNINGDATAX        =
          storagelocationdata  = ls_storagelocationdata
          storagelocationdatax = ls_storagelocationdatax
          valuationdata        = ls_valuationdata
          valuationdatax       = ls_valuationdatax
*         WAREHOUSENUMBERDATA  =
*         WAREHOUSENUMBERDATAX =
          salesdata            = ls_salesdata
          salesdatax           = ls_salesdatax
*         STORAGETYPEDATA      =
*         STORAGETYPEDATAX     =
*         FLAG_ONLINE          = ' '
*         FLAG_CAD_CALL        = ' '
*         NO_DEQUEUE           = ' '
*         NO_ROLLBACK_WORK     = ' '
        IMPORTING
          return               = ls_return
        TABLES
          materialdescription  = lt_materialdescription
          unitsofmeasure       = lt_unitsofmeasure
          unitsofmeasurex      = lt_unitsofmeasurex
*         INTERNATIONALARTNOS  =
          materiallongtext     = lt_materiallongtext
          taxclassifications   = lt_taxclassifications
          returnmessages       = lt_returnmessages
*         PRTDATA              =
*         PRTDATAX             =
*         EXTENSIONIN          =
*         EXTENSIONINX         =
*         NFMCHARGEWEIGHTS     =
*         NFMCHARGEWEIGHTSX    =
*         NFMSTRUCTURALWEIGHTS =
*         NFMSTRUCTURALWEIGHTSX       =
        .

      IF ( ls_return-type = 'E' OR ls_return-type = 'A' ).
        gv_errid = '020'.
        IF ( gv_msgid IS INITIAL ).
          RAISE EXCEPTION TYPE zcx_bc_exception.
        ELSE.
          RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER '000' WITH ls_return-message.
        ENDIF.
      ENDIF.

*      APPEND ls_pdm_am TO ct_pdm_am.
*    READ TABLE ct_pdm_am INTO DATA(ls_pdm_am_tmp) WITH KEY matnr = is_data_mat-matnr.
*    IF ( sy-subrc = 0 ).
*      ls_pdm_am_tmp-anamat_flg = ls_pdm_am-anamat_flg.
*      ls_pdm_am_tmp-anamat_flg = ls_pdm_am-anamat_flg.
*      MODIFY ct_pdm_am FROM ls_pdm_am_tmp INDEX sy-tabix.
*      RETURN.
*    ELSE.
*      APPEND ls_pdm_am TO ct_pdm_am.
*      RETURN.
*    ENDIF.
* identifico ZMM_MATERIAL-ZZMATERIAL dalla descrizione che si trova
* nel filler15
      DATA: va_zz_material_t TYPE zmm_material_t.
      IF ( gs_params_1-zzmaterial_descr IS NOT INITIAL ).
        SELECT SINGLE * FROM zmm_material_t INTO va_zz_material_t
           WHERE zzmaterial_descr = gs_params_1-zzmaterial_descr
             AND spras = sy-langu.
        IF sy-subrc = 0.
        ELSE.
          CLEAR va_zz_material_t-zzmaterial_descr.
        ENDIF.
      ENDIF.
      UPDATE mara SET zzmaterial  = va_zz_material_t-zzmaterial
                  WHERE matnr = lv_matnr.
      COMMIT WORK.
*DP-23_01_2017 Inizio
      IF ( ls_return-type NE 'E' AND ls_return-type NE 'A' ).
        CALL FUNCTION 'ZPP_SET_FLAG_MRP'
          EXPORTING
            i_matnr = lv_matnr
            i_berid = '6301'
            i_werks = '6301'
            i_gsaen = 'X'
* IMPORTING
*           E_ESITO =
*           E_DESCR_ESITO       =
          .
        COMMIT WORK AND WAIT.

        IF ( NOT gs_params_1-document IS INITIAL ).

          DATA: lt_documents    TYPE TABLE OF /dscsag/doc_change_ext,
                lt_objectlinks  TYPE TABLE OF bapi_doc_drad_keys,
                lt_documentdata TYPE TABLE OF /dscsag/doc_draw2_ext.

          lt_documents = VALUE #( ( documenttype    = gs_params_1-document-documenttype
                                    documentnumber  = gs_params_1-document-documentnumber
                                    documentversion = gs_params_1-document-documentversion
                                    documentpart    = gs_params_1-document-documentpart
                                    hostname        = 'DEFAULT' ) ).

          lt_objectlinks = VALUE #( ( objecttype = 'MARA'
                                      objectkey = rv_matnr
                                      documenttype    = gs_params_1-document-documenttype
                                      documentnumber  = gs_params_1-document-documentnumber
                                      documentversion = gs_params_1-document-documentversion
                                      documentpart    = gs_params_1-document-documentpart ) ).
*                                    tab_index = 1 ) ).

          lt_documentdata = VALUE #( ( documenttype    = gs_params_1-document-documenttype
                                       documentnumber  = gs_params_1-document-documentnumber
                                       documentversion = gs_params_1-document-documentversion
                                       documentpart    = gs_params_1-document-documentpart ) ).

          CALL FUNCTION '/DSCSAG/DOC_CHANGE_MULTI3'
            EXPORTING
*             STOP_ON_FIRST_ERROR        = ' '
*             COMMIT_ALL        = 'X'
*             COMMIT_AND_WAIT   = 'X'
*             PF_NO_UPDATE_TASK =
              iv_client_version = '4050'
*             INIT_CVAPI        = ' '
*             CLASS_CHANGENUMBER         =
*             CLASS_KEYDATE     = SY-DATUM
*             READ_LOG_MESSAGES = ' '
*         IMPORTING
*             VERSION_ID        =
*             RUNTIME           =
*             RETURN            =
            TABLES
              documents         = lt_documents
              documentdata      = lt_documentdata
*             documentdatax     = lt_documentdata
*             DOCUMENTDESCRIPTIONS       =
              objectlinks       = lt_objectlinks
*             DOCUMENTSTRUCTURE =
*             DOCUMENTFILES     =
*             LONGTEXTS         =
*             COMPONENTS        =
*             ET_MC_RETURNS     =
*             DOC_RETURNS       =
*             CLASSALLOCATIONS  =
*             CHARACTERISTICVALUES       =
*             EXTENSIONIN       =
*             ET_STACK          =
*             ET_ADD_RET        =
            .
        ENDIF.

      ENDIF.
*DP-23_01_2017 Fine
      gv_errid = '000'.

*    ELSE.
*      ls_pdm_am-anamat_flg = 'N'.
*      ls_pdm_am-anamat_log = 'Da non creare (vedi parametrizzazione)'(020).
    ENDIF.
*    READ TABLE ct_pdm_am INTO DATA(ls_pdm_am_tmp) WITH KEY matnr = is_data_mat-matnr.
*    IF ( sy-subrc = 0 ).
*      ls_pdm_am_tmp-anamat_flg = ls_pdm_am-anamat_flg.
*      ls_pdm_am_tmp-anamat_log = ls_pdm_am-anamat_log.
*      MODIFY ct_pdm_am FROM ls_pdm_am_tmp INDEX sy-tabix.
*      RETURN.
*    ELSE.
*      APPEND ls_pdm_am TO ct_pdm_am.
*      RETURN.
*    ENDIF.
  ENDMETHOD.

  METHOD get_material.
    CLEAR: rv_matnr.
    enqueue( ).
    DATA: ls_ztpp000 TYPE ztpp000.
    CLEAR: ls_ztpp000.
*    SELECT SINGLE zzds1 INTO rv_matnr FROM ztpp000 WHERE zzds2 = abap_true
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_ztpp000 FROM ztpp000 WHERE zzds2 = abap_true
                                                            AND   zzcd1 = 'ZMM_ECTR_MM01'
                                                            AND   zzcd2 = 'MATNR'
                                                            AND   zzcd3 = gs_params_1-tipologia
                                                            AND   zzcd4 = gs_params_1-ambito.
    IF ( ls_ztpp000-zzds1 > ls_ztpp000-zzcd6 ).
      TRY.
          dequeue( ).
        CATCH cx_root.
      ENDTRY.
      IF ( gv_msgid IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_bc_exception.
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER '011'.
      ENDIF.
    ENDIF.
    rv_matnr = ls_ztpp000-zzds1.
    CONDENSE rv_matnr.
    dequeue( rv_matnr ).
  ENDMETHOD.

  METHOD enqueue.
    DATA: ls_ztpp000 TYPE ztpp000.
    DATA(lv_enqueue) = abap_false.
    DO 20 TIMES.
      UPDATE ztpp000 SET zzds2 = abap_true
                         zzds3 = sy-datum
                         zzds4 = sy-uzeit
                         zzds5 = sy-uname
                     WHERE zzds2 = abap_false
                     AND   zzcd1 = 'ZMM_ECTR_MM01'
                     AND   zzcd2 = 'MATNR'
                     AND   zzcd3 = gs_params_1-tipologia
                     AND   zzcd4 = gs_params_1-ambito.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
        lv_enqueue = abap_true.
        EXIT.
      ELSE.
        WAIT UP TO '0.5' SECONDS.
      ENDIF.
    ENDDO.
    IF ( lv_enqueue = abap_false ).
      IF ( gv_msgid IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_bc_exception.
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER '010'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD dequeue.
    IF ( iv_matnr IS INITIAL ).
      UPDATE ztpp000 SET zzds2 = abap_false
                         zzds3 = sy-datum
                         zzds4 = sy-uzeit
                         zzds5 = sy-uname
                     WHERE zzds2 = abap_true
                     AND   zzcd1 = 'ZMM_ECTR_MM01'
                     AND   zzcd2 = 'MATNR'
                     AND   zzcd3 = gs_params_1-tipologia
                     AND   zzcd4 = gs_params_1-ambito.
      COMMIT WORK AND WAIT.
      IF ( gv_msgid IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_bc_exception.
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE ID gv_msgid NUMBER '010'.
      ENDIF.
    ELSE.
      DATA lv_matnr TYPE matnr.
      lv_matnr = iv_matnr + 1.
      CONDENSE lv_matnr.
      UPDATE ztpp000 SET zzds1 = lv_matnr
                         zzds2 = abap_false
                         zzds3 = sy-datum
                         zzds4 = sy-uzeit
                         zzds5 = sy-uname
                     WHERE zzds2 = abap_true
                     AND   zzcd1 = 'ZMM_ECTR_MM01'
                     AND   zzcd2 = 'MATNR'
                     AND   zzcd3 = gs_params_1-tipologia
                     AND   zzcd4 = gs_params_1-ambito.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.

  METHOD get_ztmm_ekgrp.
    DATA: lt_ztmm_ekgrp TYPE TABLE OF ztmm_ekgrp,
          lr_matnr      TYPE RANGE OF matnr,
          ls_matnr      LIKE LINE OF lr_matnr.
    REFRESH lt_ztmm_ekgrp.
    IF ( lt_ztmm_ekgrp[] IS INITIAL ).
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ztmm_ekgrp FROM ztmm_ekgrp ORDER BY sequenza.
    ENDIF.
    CLEAR es_ztmm_ekgrp.
    LOOP AT lt_ztmm_ekgrp INTO DATA(ls_ztmm_ekgrp).
      REFRESH: lr_matnr.
      ls_matnr-sign = 'I'.
      ls_matnr-option = 'CP'.
      ls_matnr-low = ls_ztmm_ekgrp-matnr.
      APPEND ls_matnr TO lr_matnr.
      IF ( iv_matnr IN lr_matnr ).
        es_ztmm_ekgrp = ls_ztmm_ekgrp.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF ( es_ztmm_ekgrp IS INITIAL ).
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF es_ztmm_ekgrp FROM ztmm_ekgrp
             WHERE matnr = space.
    ENDIF.
  ENDMETHOD.
ENDCLASS.               "lcl_z_mmm_ectr_mm01
