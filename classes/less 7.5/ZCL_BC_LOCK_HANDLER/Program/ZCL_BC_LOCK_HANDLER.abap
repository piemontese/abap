*&---------------------------------------------------------------------*
*& Report  ZCL_BC_LOCK_HANDLER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCL_BC_LOCK_HANDLER.

parameters: p_vbeln type vbeln obligatory.

START-OF-SELECTION.
  DATA: lo_lock TYPE REF TO zcl_bc_lock_handler,
        lv_garg     TYPE string,
        ls_lock     TYPE seqg3.

  lv_garg = sy-mandt && p_vbeln.

  CREATE OBJECT lo_lock
    EXPORTING
      iv_gname = 'VBAK'
      iv_time  = 10
      iv_garg  = lv_garg.

  IF ( lo_lock_old->wait_lock( ) = 0 ).
    message i398(00) with 'OdV' p_vbeln 'non bloccato'.
  else.
    ls_lock = lo_lock_old->get_lock( ).
    message w398(00) with 'Ordine bloccato da' ls_lock-guname.
  endif.
