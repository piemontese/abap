*&---------------------------------------------------------------------*
*& Report ZCL_WRFC_USER_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcl_wrfc_user_test.

SELECTION-SCREEN BEGIN OF BLOCK 000 WITH FRAME.
PARAMETERS: p_login  RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND usr,
            p_add    RADIOBUTTON GROUP rad,
            p_edit   RADIOBUTTON GROUP rad,
            p_chpwd  RADIOBUTTON GROUP rad,
            p_rstpwd RADIOBUTTON GROUP rad,
            p_user   TYPE zwrfc_user,
            p_type   TYPE zwrfc_user_type DEFAULT '2' MODIF ID typ,
            p_pwd    TYPE zwrfc_password MODIF ID pwd,
            p_newpwd TYPE zwrfc_password MODIF ID chp,
            p_repwd  TYPE zwrfc_password MODIF ID rep,
            p_first  TYPE zwrfc_first_name MODIF ID add,
            p_last   TYPE zwrfc_last_name MODIF ID add,
            p_email  TYPE zwrfc_email MODIF ID add,
            p_phone  TYPE ad_tlnmbr MODIF ID add,
            p_mobile TYPE ad_tlnmbr MODIF ID add.
SELECTION-SCREEN END OF BLOCK 000.

DATA(lv_clear_params) = abap_false.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF ( screen-group1 = 'ADD' ).
      CASE abap_true.
        WHEN p_add.
          screen-active = 1.
          screen-invisible = 0.
        WHEN p_login OR p_chpwd OR p_rstpwd.
          screen-active = 0.
          screen-invisible = 1.
      ENDCASE.
    ENDIF.
    IF ( screen-group1 = 'TYP' ).
      CASE abap_true.
        WHEN p_add.
          screen-active = 1.
          screen-invisible = 0.
        WHEN p_login OR p_edit OR p_chpwd OR p_rstpwd.
          screen-active = 0.
          screen-invisible = 1.
      ENDCASE.
    ENDIF.
    IF ( screen-group1 = 'PWD' ).
      CASE abap_true.
        WHEN p_login OR p_add OR p_edit OR p_chpwd.
          screen-active = 1.
          screen-invisible = 0.
        WHEN p_rstpwd.
          screen-active = 0.
          screen-invisible = 1.
      ENDCASE.
    ENDIF.
    IF ( screen-group1 = 'REP' ).
      CASE abap_true.
        WHEN p_login OR p_edit OR p_rstpwd.
          screen-active = 0.
          screen-invisible = 1.
        WHEN p_add OR p_chpwd.
          screen-active = 1.
          screen-invisible = 0.
      ENDCASE.
    ENDIF.
    IF ( screen-group1 = 'CHP' ).
      CASE abap_true.
        WHEN p_login OR p_add OR p_edit OR p_rstpwd.
          screen-active = 0.
          screen-invisible = 1.
        WHEN p_chpwd.
          screen-active = 1.
          screen-invisible = 0.
      ENDCASE.
    ENDIF.
    IF ( ( screen-group1 = 'PWD' OR screen-group1 = 'CHP' OR screen-group1 = 'REP' ) AND
         NOT screen-name CS 'TEXT' ).
      screen-invisible = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
  IF ( p_edit = abap_true ).
    IF ( NOT p_user IS INITIAL AND NOT p_pwd IS INITIAL ).
      TRY.
          DATA(ls_user) = zcl_wrfc_user=>get_info( iv_user     = CONV string( p_user )
                                                   iv_password = CONV string( p_pwd ) ).
          p_first = ls_user-first_name.
          p_last  = ls_user-last_name.
          p_email = ls_user-email.
          p_phone = ls_user-phone_number.
          p_mobile = ls_user-mobile_number.
        CATCH cx_root.
      ENDTRY.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  CASE abap_true.
    WHEN p_login.
      TRY.
          zcl_wrfc_user=>login( iv_user            = CONV string( p_user )
                                iv_password        = CONV string( p_pwd ) ).
          MESSAGE i398(00) WITH 'User successfully logged'.
          lv_clear_params = abap_true.
        CATCH cx_root INTO DATA(lx_root).
          MESSAGE i398(00) WITH CONV char50( lx_root->get_text( ) ).
          RETURN.
      ENDTRY.

    WHEN p_add.
      TRY.
          IF ( p_type = '1' ).
            SELECT SINGLE user_type INTO @DATA(lv_user_type) FROM zwrfc_users WHERE user_type = '1'.
            IF ( sy-subrc = 0 ).
              MESSAGE i398(00) WITH 'Administrator has created yet'.
              RETURN.
            ENDIF.
          ENDIF.
          zcl_wrfc_user=>add( iv_user            = CONV string( p_user )
                              iv_user_type       = CONV string( p_type )
                              iv_password        = CONV string( p_pwd )
                              iv_retype_password = CONV string( p_repwd )
                              iv_first_name      = CONV string( p_first )
                              iv_last_name       = CONV string( p_last )
                              iv_email           = CONV string( p_email )
                              iv_phone_number    = CONV string( p_phone )
                              iv_mobile_number   = CONV string( p_mobile ) ).
          MESSAGE i398(00) WITH 'User added'.
          lv_clear_params = abap_true.
        CATCH cx_root INTO lx_root.
          MESSAGE i398(00) WITH CONV char50( lx_root->get_text( ) ).
          RETURN.
      ENDTRY.

    WHEN p_edit.
      TRY.
          zcl_wrfc_user=>edit( iv_user            = CONV string( p_user )
                               iv_password        = CONV string( p_pwd )
                               iv_first_name      = CONV string( p_first )
                               iv_last_name       = CONV string( p_last )
                               iv_email           = CONV string( p_email )
                               iv_phone_number    = CONV string( p_phone )
                               iv_mobile_number   = CONV string( p_mobile ) ).
          MESSAGE i398(00) WITH 'User info updated'.
          lv_clear_params = abap_true.
        CATCH cx_root INTO lx_root.
          MESSAGE i398(00) WITH CONV char50( lx_root->get_text( ) ).
          RETURN.
      ENDTRY.

    WHEN p_chpwd.
      TRY.
          zcl_wrfc_user=>change_password( iv_user            = CONV string( p_user )
                                          iv_password        = CONV string( p_pwd )
                                          iv_new_password    = CONV string( p_newpwd )
                                          iv_renew_password  = CONV string( p_repwd ) ).
          MESSAGE i398(00) WITH 'Password changed'.
          lv_clear_params = abap_true.
        CATCH cx_root INTO lx_root.
          MESSAGE i398(00) WITH CONV char50( lx_root->get_text( ) ).
          RETURN.
      ENDTRY.

    WHEN p_rstpwd.
      TRY.
*          zcl_wrfc_user=>reset_password( iv_user = CONV string( p_user ) ).
          MESSAGE i398(00) WITH zcl_wrfc_user=>reset_password( iv_user = CONV string( p_user ) ).
          lv_clear_params = abap_true.
        CATCH cx_root INTO lx_root.
          MESSAGE i398(00) WITH CONV char50( lx_root->get_text( ) ).
          RETURN.
      ENDTRY.

  ENDCASE.

FORM clear_params.
  CLEAR: p_user,
         p_pwd,
         p_newpwd,
         p_repwd,
         p_first,
         p_last,
         p_email,
         p_phone,
         p_mobile.
ENDFORM.
