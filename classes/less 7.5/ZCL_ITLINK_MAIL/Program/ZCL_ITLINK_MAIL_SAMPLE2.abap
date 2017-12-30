*&---------------------------------------------------------------------*
*& Report  ZITLINK_CL_MAIL_SAMPLE1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zcl_itlink_mail_sample2.

PARAMETERS: p_img    AS CHECKBOX DEFAULT 'X',
            p_firma  AS CHECKBOX DEFAULT 'X',
            p_attach AS CHECKBOX DEFAULT 'X',
            p_closur AS CHECKBOX DEFAULT 'X'..

START-OF-SELECTION.
  PERFORM __main__.

*&---------------------------------------------------------------------*
*&      Form  __MAIN__
*&---------------------------------------------------------------------*
FORM __main__ .

  DATA: lo_mail           TYPE REF TO zcl_itlink_mail,
        lo_bcs_exception  TYPE REF TO cx_bcs,
        lt_binary_content TYPE solix_tab.

  TRY.
      " istanzia oggetto
      IF ( lo_mail IS INITIAL ).
        " crea mail senza immagine
        IF ( p_img IS INITIAL ).
          CREATE OBJECT lo_mail
            EXPORTING
              iv_signature = p_firma.
        ELSE.
          " crea mail con immagine
          CREATE OBJECT lo_mail
            EXPORTING
              iv_mime_url  = '/SAP/PUBLIC/novellini.jpg' " blank o non passato non mette immagine nel corpo della mail
              iv_signature = p_firma.
        ENDIF.
      ENDIF.

      " setta oggetto mail
      lo_mail->set_subject( 'Test invio mail con classe zcl_itlink_mail' ).

      " Aggiunge formattazioni nell'head
      lo_mail->add_html_head( '<style>' ).
      lo_mail->add_html_head( 'h1 { font-family: "Times New Roman", Times, Georgia, serif; }' ).
      lo_mail->add_html_head( 'body { font-family: "Times New Roman", Times, Georgia, serif; }' ).
      lo_mail->add_html_head( '</style>' ).

      " aggiunge righe al corpo della mail
      lo_mail->add_html_body( '<h1>riga 1</h1>' ).

      " setta lunghezza dell'immagine
      lo_mail->set_mime_width( '20%' ).

      " modifica mittente
      "lo_mail->set_sender( 'IT-PATMAS' ).

      " aggiunge destinatari
      lo_mail->add_recipient( 'pietro.piemontese@it-link.it' ).
      "lo_mail->add_recipient( 'onorio.garimberti@it-link.it' ).
      "lo_mail->add_recipient( 'massimiliano.patierno@it-link.it' ).

      IF ( NOT p_closur IS INITIAL ).
        " aggiunge testo di chiusura
        lo_mail->add_closure( '<h6>Testo di chiusura mail</h6>' ).
      ENDIF.

      IF ( NOT p_attach IS INITIAL ).
        DATA: lt_attachment TYPE lo_mail->ty_t_attachment,
              ls_attachment TYPE lo_mail->ty_s_attachment,
              lv_file       TYPE /isdfps/lm_de_accident_file.
        " aggiunge allegati
        lo_mail->add_attachment( iv_attachment_type    = 'PDF'
                                 iv_attachment_subject = 'Allegato'
*                                iv_att_content_text   =
                                 iv_att_content_hex    = lt_binary_content
                                 iv_file               = lv_file
                                 it_attachment         = lt_attachment
                               ).
      ENDIF.

      " invia mail
      lo_mail->send( ).

      " ------ gestione eccezioni ------------------------
    CATCH cx_bcs INTO lo_bcs_exception.
      WRITE: 'Error code: ', lo_bcs_exception->error_type..
      EXIT.

  ENDTRY.

  WRITE 'mail correttamente inviata'.

ENDFORM.                    " __MAIN__
