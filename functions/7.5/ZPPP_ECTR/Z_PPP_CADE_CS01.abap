FUNCTION z_ppp_cade_cs01 .
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  IMPORTING
*"     VALUE(IV_ROOTNAME) TYPE  ZPP_ALBERO-ROOTNAME OPTIONAL
*"     VALUE(IV_WERKS) TYPE  WERKS_D OPTIONAL
*"  EXPORTING
*"     VALUE(EV_ESITO) TYPE  ZDBC_ESITO
*"     VALUE(EV_DESCR_ESITO) TYPE  ZDBC_DESCR_ESITO
*"  TABLES
*"      ZPP_BOM_IN STRUCTURE  ZPP_BOM_IN
*"      ZPP_BOM_OUT STRUCTURE  ZPP_BOM_OUT OPTIONAL
*"----------------------------------------------------------------------


  ev_esito = '999'.
  MESSAGE s999(zppp_ectr) INTO ev_descr_esito.

  TRY.
      DATA(lo_function) = NEW lcl_z_ppp_cade_cs01( iv_msgid      = 'ZPPP_ECTR'
                                                   iv_rootname   = iv_rootname
                                                   iv_werks      = iv_werks
                                                   it_bom_in     = zpp_bom_in[] ).


      " verifica che i parametri obbligatori siano valorizzati
      lo_function->is_initial( iv_value = iv_rootname iv_msgno = '030' ).
      lo_function->is_initial( iv_value = iv_werks iv_msgno = '021' ).

      " verifica che i parametri esistano
      lo_function->is_exists( iv_table = 'T001W' iv_msgno = '023' iv_msgv1 = iv_werks
                              it_fields = VALUE zsbc_fields_t( ( name = 'werks' value = iv_werks ) ) ).
      lo_function->is_exists( iv_table = 'MARA' iv_msgno = '028' iv_msgv1 = iv_rootname
                              it_fields = VALUE zsbc_fields_t( ( name = 'matnr' value = iv_rootname ) ) ).
      lo_function->is_exists( iv_table = 'MARC' iv_msgno = '029' iv_msgv1 = iv_rootname iv_msgv2 = iv_rootname
                              it_fields = VALUE zsbc_fields_t( ( name = 'werks' value = iv_werks )
                                                               ( name = 'matnr' value = iv_rootname ) ) ).

      " crea o modifica bom
      lo_function->maintain_bom( ).

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
