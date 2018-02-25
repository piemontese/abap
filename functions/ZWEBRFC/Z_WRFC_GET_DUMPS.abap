FUNCTION z_wrfc_get_dumps.
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  IMPORTING
*"     VALUE(IS_DATE) TYPE  MAWM_INT_RANGE_DATUM OPTIONAL
*"     VALUE(IS_TIME) TYPE  MCW_RANGEUZEIT OPTIONAL
*"     VALUE(IS_HOST) TYPE  SNAP_S_HOST OPTIONAL
*"     VALUE(IS_USER) TYPE  /SAPSRM/T_PDO_SEARCH_CREATEDBY OPTIONAL
*"  EXPORTING
*"     VALUE(ET_DUMPS) TYPE  RSDUMPTAB
*"----------------------------------------------------------------------

  DATA: lt_snap  TYPE TABLE OF snap,
        lt_dumps TYPE rsdumptab,
        ls_snap  TYPE snap_beg,
        ls_dumps TYPE rsdumpinfo,
        r_date   LIKE TABLE OF is_date,
        r_time   LIKE TABLE OF is_time,
        r_host   LIKE TABLE OF is_host,
        r_user   LIKE TABLE OF is_user.

  REFRESH: et_dumps.
  REFRESH: r_date, r_time, r_host, r_user.

  IF ( NOT is_date IS INITIAL ).
    APPEND is_date TO r_date.
  ENDIF.
  IF ( NOT is_time IS INITIAL ).
    APPEND is_time TO r_time.
  ENDIF.
  IF ( NOT is_host IS INITIAL ).
    APPEND is_host TO r_host.
  ENDIF.
  IF ( NOT is_user IS INITIAL ).
    APPEND is_user TO r_user.
  ENDIF.

  SELECT DISTINCT datum uzeit ahost uname mandt INTO CORRESPONDING FIELDS OF TABLE lt_snap FROM snap WHERE datum IN r_date
                                                                                                     AND   uzeit IN r_time
                                                                                                     AND   ahost IN r_host
                                                                                                     AND   uname IN r_user
                                                                                                     AND   mandt EQ sy-mandt.


  DATA: lv_datum TYPE datum.

  CLEAR: lv_datum.
  LOOP AT lt_snap INTO ls_snap.
    IF ( lv_datum <> ls_snap-datum ).
      REFRESH: lt_dumps.
      CALL FUNCTION 'RS_ST22_GET_DUMPS'
        EXPORTING
          p_day     = ls_snap-datum
        IMPORTING
          p_infotab = lt_dumps.

      DELETE lt_dumps WHERE NOT sydate IN r_date.
      DELETE lt_dumps WHERE NOT sytime IN r_time.
      DELETE lt_dumps WHERE NOT syhost IN r_host.
      DELETE lt_dumps WHERE NOT syuser IN r_user.

      APPEND LINES OF lt_dumps TO et_dumps.
    ENDIF.
    lv_datum = ls_snap-datum.
  ENDLOOP.


ENDFUNCTION.
