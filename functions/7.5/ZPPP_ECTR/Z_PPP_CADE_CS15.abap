FUNCTION z_ppp_cade_cs15 .
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  IMPORTING
*"     VALUE(IV_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(IV_MATNR) TYPE  MATNR OPTIONAL
*"  EXPORTING
*"     VALUE(EV_ESITO) TYPE  ZDBC_ESITO
*"     VALUE(EV_DESCR_ESITO) TYPE  ZDBC_DESCR_ESITO
*"  TABLES
*"      ZMM_CS15 STRUCTURE  ZMM_CS15
*"----------------------------------------------------------------------


  ev_esito = '999'.
  MESSAGE s999(zppp_ectr) INTO ev_descr_esito.

  TRY.
      DATA(lo_function) = NEW lcl_z_ppp_cade_cs15( iv_msgid      = 'ZPPP_ECTR'
                                                   iv_werks      = iv_werks
                                                   iv_matnr      = iv_matnr ).


      " verifica che i parametri obbligatori siano valorizzati
      lo_function->is_initial( iv_value = iv_werks iv_msgno = '021' ).
      lo_function->is_initial( iv_value = iv_matnr iv_msgno = '022' ).

      " verifica che i parametri esistano
      lo_function->is_exists( iv_table = 'T001W' iv_msgno = '023' iv_msgv1 = iv_werks
                              it_fields = VALUE zsbc_fields_t( ( name = 'werks' value = iv_werks ) ) ).
      lo_function->is_exists( iv_table = 'MARA' iv_msgno = '024' iv_msgv1 = iv_matnr
                              it_fields = VALUE zsbc_fields_t( ( name = 'matnr' value = iv_matnr ) ) ).
      lo_function->is_exists( iv_table = 'MARC' iv_msgno = '025' iv_msgv1 = iv_matnr iv_msgv2 = iv_werks
                              it_fields = VALUE zsbc_fields_t( ( name = 'werks' value = iv_werks )
                                                               ( name = 'matnr' value = iv_matnr ) ) ).

      zmm_cs15[] = lo_function->get_where_used( ).

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
