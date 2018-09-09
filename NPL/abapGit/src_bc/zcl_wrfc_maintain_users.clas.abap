class ZCL_WRFC_MAINTAIN_USERS definition
  public
  inheriting from ZCL_BC_PROGRAM_BASE_2
  create public .

public section.

  types TY_S_DATA type ZWRFC_USERS .
  types:
    ty_t_data TYPE TABLE OF ty_s_data .
  types:
    BEGIN OF ty_s_sel,
        s_date TYPE RANGE OF rsdumpinfo-sydate,
        s_time TYPE RANGE OF rsdumpinfo-sytime,
        s_host TYPE RANGE OF rsdumpinfo-syhost,
        s_user TYPE RANGE OF rsdumpinfo-syuser,
      END OF ty_s_sel .

  methods CONSTRUCTOR
    importing
      !IV_PROGRAM type SY-REPID
      !IV_LANGUAGE type SY-LANGU default SY-LANGU
      !IS_SEL type TY_S_SEL optional
      !IV_PFSTATUS type SYPFKEY optional
      !IV_EXTERNAL_CALL type ABAP_BOOL default ABAP_FALSE
    raising
      resumable(ZCX_BC_EXCEPTION) .
  methods INITIALIZATION
    raising
      resumable(ZCX_BC_EXCEPTION) .
  methods RUN
    importing
      value(IV_SHOW_ALV) type ABAP_BOOL default ABAP_TRUE
    raising
      resumable(ZCX_BC_EXCEPTION) .
  methods GET_SEL
    returning
      value(ES_SEL) type TY_S_SEL .
  methods ADD_USER
    importing
      !IV_SHOW_ALV type ABAP_BOOL default SPACE .
  methods REMOVE_USER
    importing
      !IV_SHOW_ALV type ABAP_BOOL default SPACE .
  methods EDIT_USER
    importing
      !IV_SHOW_ALV type ABAP_BOOL default SPACE .

  methods ALV_SHOW
    redefinition .
  methods MODIFY_SCREEN
    redefinition .
  methods SET_DATA
    redefinition .
protected section.

  methods AUTORITY_CHECK
    raising
      resumable(ZCX_BC_EXCEPTION) .
  methods CREATE_SELECTIONS
    raising
      resumable(ZCX_BC_EXCEPTION) .
private section.

  data GS_SEL type TY_S_SEL .
  data GT_DATA type TY_T_DATA .
ENDCLASS.



CLASS ZCL_WRFC_MAINTAIN_USERS IMPLEMENTATION.


  METHOD add_user.
    set_screen_dialog( ).
    set_pfstatus( iv_pfstatus = 'DIALOG' ).
    me->alv_show( ).
  ENDMETHOD.


  method ALV_SHOW.
    DATA : lo_selections    TYPE REF TO cl_salv_selections,
           lo_events        TYPE REF TO cl_salv_events_table,
           lo_event_handler TYPE REF TO lcl_handle_events,
           lo_sel           TYPE REF TO cl_salv_selections.

    lo_sel = go_alv->get_selections( ).
    lo_sel->set_selection_mode( if_salv_c_selection_mode=>multiple ).

    lo_events = go_alv->get_event( ).
    lo_event_handler = NEW lcl_handle_events( io_sender = me
                                              io_alv    = go_alv
                                              io_sel    = lo_sel ).

    SET HANDLER lo_event_handler->on_user_command FOR lo_events.

    super->alv_show( ).
  endmethod.


  method AUTORITY_CHECK.
*    AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
*       ID 'ACTVT' DUMMY
*       ID 'WERKS' FIELD gs_sel-p_werks.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE zcx_bc_exception MESSAGE s001(zmmm_modula) W
*    ENDIF.
  endmethod.


  METHOD constructor.
    super->constructor( iv_program       = iv_program
                        iv_language      = iv_language
                        iv_pfstatus      = iv_pfstatus
                        iv_external_call = iv_external_call ).
    create_selections( ).
  ENDMETHOD.


  METHOD CREATE_SELECTIONS.
    DATA: lref_sel TYPE REF TO data.
    CREATE DATA lref_sel TYPE ty_s_sel.
    set_selections( CHANGING cref_sel = lref_sel ).
    FIELD-SYMBOLS: <sel>    TYPE any.
    ASSIGN lref_sel->* TO <sel>.
    IF ( sy-subrc = 0 ).
      gs_sel = <sel>.
    ENDIF.
  ENDMETHOD.


  method EDIT_USER.
  endmethod.


  METHOD GET_SEL.
    es_sel = gs_sel.
  ENDMETHOD.


  METHOD INITIALIZATION.
  ENDMETHOD.


  METHOD MODIFY_SCREEN.
    super->modify_screen( ).
    create_selections( ).

  ENDMETHOD.


  method REMOVE_USER.
  endmethod.


  METHOD RUN.

    create_selections( ).
    initialization( ).
    autority_check( ).

    set_data( ).

    alv_create( EXPORTING iv_title = 'Dunps'(001)
                CHANGING ct_data = gt_data ).
    IF ( iv_show_alv = abap_true ).
      me->alv_show( ).
    ENDIF.

*    IF ( sy-batch = abap_true ).
**     Siamo in esecuzione Batch
*    ENDIF.

  ENDMETHOD.


  METHOD set_data.
    REFRESH: gt_data.

    SELECT zwrfc_users~* FROM zwrfc_users INTO TABLE @gt_data.

    " non cancellare nè spostare
    super->set_data( it_data = gt_data ).
  ENDMETHOD.
ENDCLASS.
