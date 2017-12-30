*&---------------------------------------------------------------------*
*& Report  ZITLINK_CL_MAIL_SAMPLE1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zcl_itlink_mail_sample.

PARAMETERS: p_img    AS CHECKBOX DEFAULT 'X',
            p_firma  AS CHECKBOX DEFAULT 'X',
            p_attach AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.
  PERFORM __main__.

*&---------------------------------------------------------------------*
*&      Form  __MAIN__
*&---------------------------------------------------------------------*
FORM __main__ .

  DATA: lo_mail           TYPE REF TO zcl_itlink_mail,
        lo_bcs_exception  TYPE REF TO cx_bcs,
        lt_binary_content TYPE solix_tab,
        lt_attachment     TYPE lo_mail->ty_t_attachment.

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

      " aggiunge righe al corpo della mail
      lo_mail->add_html_body( '<h1>riga 1</h1>' ).
      lo_mail->add_html_body( '<h2>riga 2</h2>' ).
      lo_mail->add_html_body( '<h3>riga 3</h3>' ).
      lo_mail->add_html_body( '<h4>riga 4</h4>' ).
      lo_mail->add_html_body( '<h5>riga 5</h5>' ).

      " setta lunghezza dell'immagine
      lo_mail->set_mime_width( '20%' ).

      " modifica mittente
      lo_mail->set_sender( 'IT-PATMAS' ).

      " aggiunge destinatari
      lo_mail->add_recipient( 'pietro.piemontese@it-link.it' ).
      lo_mail->add_recipient( 'onorio.garimberti@it-link.it' ).
      lo_mail->add_recipient( 'massimiliano.patierno@it-link.it' ).

      IF ( NOT p_attach IS INITIAL ).
        " aggiunge allegati
        lo_mail->add_attachment( iv_attachment_type    = 'PDF'
                                 iv_attachment_subject = 'Allegato'
*                                iv_att_content_text   =
                                 iv_att_content_hex    = lt_binary_content
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
