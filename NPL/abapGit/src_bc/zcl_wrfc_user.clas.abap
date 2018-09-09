class ZCL_WRFC_USER definition
  public
  final
  create public .

public section.

  constants C_PASSWORD_LENGTH type I value 8 ##NO_TEXT.
  constants C_PASSWORD_CAPITAL_LETTERS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants C_PASSWORD_NUMBERS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants C_PASSWORD_SPECIAL_CHARACTERS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.

  class-methods ADD
    importing
      !IV_USER type STRING
      !IV_PASSWORD type STRING
      !IV_RETYPE_PASSWORD type STRING
      !IV_USER_TYPE type STRING default '2'
      !IV_FIRST_NAME type STRING
      !IV_LAST_NAME type STRING
      !IV_EMAIL type STRING
      !IV_PHONE_NUMBER type STRING default SPACE
      !IV_MOBILE_NUMBER type STRING default SPACE
    raising
      resumable(ZCX_BC_EXCEPTION) .
  class-methods EDIT
    importing
      !IV_USER type STRING
      !IV_PASSWORD type STRING
      !IV_FIRST_NAME type STRING
      !IV_LAST_NAME type STRING
      !IV_EMAIL type STRING
      !IV_PHONE_NUMBER type STRING
      !IV_MOBILE_NUMBER type STRING .
  class-methods DELETE .
  class-methods ACTIVATE .
  class-methods DEACTIVATE .
  class-methods CHANGE_PASSWORD
    importing
      !IV_USER type STRING
      !IV_PASSWORD type STRING
      !IV_NEW_PASSWORD type STRING
      !IV_RENEW_PASSWORD type STRING
    raising
      resumable(ZCX_BC_EXCEPTION) .
  class-methods RESET_PASSWORD
    importing
      !IV_USER type STRING
    returning
      value(RV_PASSWORD) type STRING
    raising
      resumable(ZCX_BC_EXCEPTION) .
  class-methods LOGIN
    importing
      !IV_USER type STRING
      !IV_PASSWORD type STRING
    raising
      resumable(ZCX_BC_EXCEPTION) .
  class-methods GET_INFO
    importing
      !IV_USER type STRING
      !IV_PASSWORD type STRING
    returning
      value(RS_USER) type ZWRFC_USERS
    raising
      resumable(ZCX_BC_EXCEPTION) .
protected section.
private section.

  class-methods CHECK_PASSWORD
    importing
      !IV_PASSWORD type STRING
    raising
      resumable(ZCX_BC_EXCEPTION) .
ENDCLASS.



CLASS ZCL_WRFC_USER IMPLEMENTATION.


  method ACTIVATE.
  endmethod.


  METHOD add.
    zcl_bc_data_util=>is_initial( iv_value = iv_user
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '004' ).
    zcl_bc_data_util=>is_initial( iv_value = iv_first_name
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '005' ).
    zcl_bc_data_util=>is_initial( iv_value = iv_last_name
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '006' ).
    zcl_bc_data_util=>is_initial( iv_value = iv_email
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '007' ).
    zcl_bc_data_util=>is_equal( iv_value_1 = iv_password
                                iv_value_2 = iv_retype_password
                                iv_msgid = 'Z_WEBRFC'
                                 iv_msgno = '008' ).
    check_password( iv_password ).

    DATA: ls_users TYPE zwrfc_users.
    CLEAR: ls_users.
    ls_users-mandt         = sy-mandt.
    ls_users-usr           = iv_user.
    ls_users-password      = cl_http_utility=>encode_base64( iv_password ).
    ls_users-user_type     = iv_user_type.
    ls_users-active        = 1.
    ls_users-first_name    = iv_first_name.
    ls_users-last_name     = iv_last_name.
    ls_users-full_name     = iv_first_name && | | && iv_last_name.
    ls_users-phone_number  = iv_phone_number.
    ls_users-mobile_number = iv_mobile_number.
    ls_users-email         = iv_email.
    SELECT SINGLE usr INTO @DATA(lv_usr) FROM zwrfc_users WHERE usr = @iv_user.
    IF ( sy-subrc <> 0 ).
      INSERT zwrfc_users FROM ls_users.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s003(z_webrfc).
    ENDIF.
  ENDMETHOD.


  METHOD change_password.
    zcl_bc_data_util=>is_initial( iv_value = iv_user
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '004' ).
    zcl_bc_data_util=>is_equal( iv_value_1 = iv_new_password
                                iv_value_2 = iv_renew_password
                                iv_msgid = 'Z_WEBRFC'
                                iv_msgno = '008' ).
    check_password( iv_password ).

    DATA(lv_password) = cl_http_utility=>encode_base64( iv_password ).
    SELECT SINGLE zwrfc_users~* INTO @DATA(ls_users) FROM zwrfc_users WHERE usr      = @iv_user
                                                                      AND   password = @lv_password.
    IF ( sy-subrc = 0 ).
      ls_users-password = cl_http_utility=>encode_base64( iv_new_password ).
      UPDATE zwrfc_users FROM ls_users.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s013(z_webrfc).
    ENDIF.
  ENDMETHOD.


  METHOD check_password.
"    IF ( strlen( iv_password ) < c_password_length ).
    IF ( strlen( iv_password ) < c_password_length ).
      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s009(z_webrfc) WITH c_password_length.
    ENDIF.
    IF ( c_password_capital_letters = abap_true ).
      IF ( NOT iv_password CA 'ABCDEFGHIJKMNOPQRSTUVWXYZ' ).
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s012(z_webrfc).
      ENDIF.
    ENDIF.
    IF ( c_password_numbers = abap_true ).
      IF ( NOT iv_password CA '0123456789' ).
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s010(z_webrfc).
      ENDIF.
    ENDIF.
    IF ( c_password_special_characters = abap_true ).
      IF ( NOT iv_password CA '!$%&?_()=#@/*+' ).
        RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s011(z_webrfc).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method DEACTIVATE.
  endmethod.


  method DELETE.
  endmethod.


  METHOD edit.
    zcl_bc_data_util=>is_initial( iv_value = iv_user
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '004' ).
    zcl_bc_data_util=>is_initial( iv_value = iv_first_name
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '005' ).
    zcl_bc_data_util=>is_initial( iv_value = iv_last_name
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '006' ).
    zcl_bc_data_util=>is_initial( iv_value = iv_email
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '007' ).

    DATA(lv_password) = cl_http_utility=>encode_base64( iv_password ).
    SELECT SINGLE zwrfc_users~* INTO @DATA(ls_user) FROM zwrfc_users WHERE usr      = @iv_user
                                                                     AND   password = @lv_password.
    IF ( sy-subrc = 0 ).
      UPDATE zwrfc_users SET first_name    = @iv_first_name,
                             last_name     = @iv_last_name,
                             email         = @iv_email,
                             phone_number  = @iv_phone_number,
                             mobile_number = @iv_mobile_number
                         WHERE usr      = @iv_user
                         AND   password = @lv_password.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s003(z_webrfc).
    ENDIF.
  ENDMETHOD.


  METHOD get_info.
    CLEAR: rs_user.
    zcl_bc_data_util=>is_initial( iv_value = iv_user
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '004' ).

    DATA(lv_password) = cl_http_utility=>encode_base64( iv_password ).
    SELECT SINGLE zwrfc_users~* INTO @DATA(ls_users) FROM zwrfc_users WHERE usr      = @iv_user
                                                                      AND   password = @lv_password.
    IF ( sy-subrc = 0 ).
      rs_user = ls_users.
      CLEAR: rs_user-password.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s013(z_webrfc).
    ENDIF.
  ENDMETHOD.


  METHOD login.
    zcl_bc_data_util=>is_initial( iv_value = iv_user
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '004' ).

    DATA(lv_password) = cl_http_utility=>encode_base64( iv_password ).
    SELECT SINGLE zwrfc_users~* INTO @DATA(ls_users) FROM zwrfc_users WHERE usr      = @iv_user
                                                                      AND   password = @lv_password.
    IF ( sy-subrc <> 0 ).
      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s013(z_webrfc).
    ENDIF.
  ENDMETHOD.


  METHOD reset_password.
    CLEAR: rv_password.
    zcl_bc_data_util=>is_initial( iv_value = iv_user
                                  iv_msgid = 'Z_WEBRFC'
                                  iv_msgno = '004' ).

    SELECT SINGLE password INTO @rv_password FROM zwrfc_users WHERE usr = @iv_user.
    IF ( sy-subrc = 0 ).
      DATA: lv_new_pwd TYPE c LENGTH 100.
      CALL FUNCTION 'RSEC_GENERATE_PASSWORD'
        EXPORTING
*         ALPHABET      =
*         ALPHABET_LENGTH            = 0
*         FORCE_INIT    = ' '
          output_length = 10
*         DOWNWARDS_COMPATIBLE       = ' '
*         SECURITY_POLICY            = ' '
        IMPORTING
          output        = lv_new_pwd
        EXCEPTIONS
          some_error    = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      change_password( iv_user           = iv_user
                       iv_password       = cl_http_utility=>decode_base64( rv_password )
                       iv_new_password   = CONV string( lv_new_pwd )
                       iv_renew_password = CONV string( lv_new_pwd ) ).
      rv_password = lv_new_pwd.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s013(z_webrfc).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
