FUNCTION z_mmm_ectr_mm01.
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  IMPORTING
*"     VALUE(IV_TIPOLOGIA) TYPE  ZPP_ECTR_TIPO OPTIONAL
*"     VALUE(IV_AMBITO) TYPE  ZPP_ECTR_AMB OPTIONAL
*"     VALUE(IV_MAKTX) TYPE  MAKT-MAKTX OPTIONAL
*"     VALUE(IV_MAKTX2) TYPE  MAKT-MAKTX OPTIONAL
*"     VALUE(IV_MATKL) TYPE  MARA-MATKL OPTIONAL
*"     VALUE(IV_MEINS) TYPE  MARA-MEINS OPTIONAL
*"     VALUE(IV_BRGEW) TYPE  MARA-BRGEW OPTIONAL
*"     VALUE(IV_NTGEW) TYPE  MARA-NTGEW OPTIONAL
*"     VALUE(IV_GEWEI) TYPE  MARA-GEWEI DEFAULT 'g'
*"     VALUE(IV_VOLUM) TYPE  MARA-VOLUM OPTIONAL
*"     VALUE(IV_VOLEH) TYPE  MARA-VOLEH OPTIONAL
*"     VALUE(IV_LAENG) TYPE  MARA-LAENG OPTIONAL
*"     VALUE(IV_HOEHE) TYPE  MARA-HOEHE OPTIONAL
*"     VALUE(IV_BREIT) TYPE  MARA-BREIT OPTIONAL
*"     VALUE(IV_MEABM) TYPE  MARA-MEABM OPTIONAL
*"     VALUE(IV_ZZMATERIAL_DESCR) TYPE  ZMM_MATERIAL_T-ZZMATERIAL_DESCR
*"       OPTIONAL
*"     VALUE(IS_DOCUMENT) TYPE  BAPI_DOC_KEYS OPTIONAL
*"  EXPORTING
*"     VALUE(EV_MATNR) TYPE  MATNR
*"     VALUE(EV_ESITO) TYPE  ZDBC_ESITO
*"     VALUE(EV_DESCR_ESITO) TYPE  ZDBC_DESCR_ESITO
*"----------------------------------------------------------------------



  ev_esito = '999'.
  MESSAGE s999(zppp_ectr) INTO ev_descr_esito.


  TRY.
      DATA(lo_function) = NEW lcl_z_mmm_ectr_mm01( iv_msgid      = 'ZPPP_ECTR'
                                                   iv_tipologia  = iv_tipologia
                                                   iv_ambito     = iv_ambito
                                                   iv_maktx      = iv_maktx
                                                   iv_maktx2     = iv_maktx2
                                                   iv_matkl      = iv_matkl
                                                   iv_meins      = iv_meins
                                                   iv_brgew      = iv_brgew
                                                   iv_ntgew      = iv_ntgew
                                                   iv_gewei      = iv_gewei
                                                   iv_volum      = iv_volum
                                                   iv_voleh      = iv_voleh
                                                   iv_laeng      = iv_laeng
                                                   iv_hoehe      = iv_hoehe
                                                   iv_breit      = iv_breit
                                                   iv_meabm      = iv_meabm
                                                   iv_zzmaterial_descr = iv_zzmaterial_descr
                                                   is_document   = is_document ).


      " verifica che i parametri obbligatori siano valorizzati
      lo_function->is_initial( iv_value = iv_tipologia iv_msgno = '001' ).
      lo_function->is_initial( iv_value = iv_ambito iv_msgno = '002' ).
      lo_function->is_initial( iv_value = iv_maktx iv_msgno = '003' ).
      lo_function->is_initial( iv_value = iv_matkl iv_msgno = '004' ).
      lo_function->is_initial( iv_value = iv_meins iv_msgno = '005' ).

      lo_function->check_tipologia( ).

      lo_function->check_ambito( ).

      " verifica che i parametri esistano
      lo_function->is_exists( iv_table = 'T023' iv_msgno = '006' iv_msgv1 = iv_matkl
                              it_fields = VALUE zsbc_fields_t( ( name = 'matkl' value = iv_matkl ) ) ).
      IF ( NOT iv_meins IS INITIAL ).
        lo_function->is_exists( iv_table = 'T006' iv_msgno = '013' iv_msgv1 = iv_meins
                                it_fields = VALUE zsbc_fields_t( ( name = 'MSEHI' value = iv_meins ) ) ).
      ENDIF.
      IF ( NOT iv_gewei IS INITIAL ).
        lo_function->is_exists( iv_table = 'T006' iv_msgno = '014' iv_msgv1 = iv_gewei
                                it_fields = VALUE zsbc_fields_t( ( name = 'MSEHI' value = iv_gewei ) ) ).
      ENDIF.
      IF ( NOT iv_voleh IS INITIAL ).
        lo_function->is_exists( iv_table = 'T006' iv_msgno = '015' iv_msgv1 = iv_voleh
                                it_fields = VALUE zsbc_fields_t( ( name = 'MSEHI' value = iv_voleh ) ) ).
      ENDIF.
      IF ( NOT iv_zzmaterial_descr IS INITIAL ).
        lo_function->is_exists( iv_table = 'ZMM_MATERIAL_T' iv_msgno = '007' iv_msgv1 = iv_zzmaterial_descr iv_msgv2 = sy-langu
                                it_fields = VALUE zsbc_fields_t( ( name = 'spras' value = sy-langu )
                                                                 ( name = 'zzmaterial_descr' value = iv_zzmaterial_descr ) ) ).
      ENDIF.

      ev_matnr = lo_function->create_material( ).

    CATCH cx_root INTO DATA(lx_root).
      IF ( lo_function IS BOUND ).
        ev_esito = lo_function->get_error( ).
      ENDIF.
      ev_descr_esito = lx_root->get_text( ).
      RETURN.

  ENDTRY.

  ev_esito = lo_function->get_error( ).
  IF ( ev_esito = '000' ).
    CLEAR: ev_descr_esito.
  ENDIF.



ENDFUNCTION.
