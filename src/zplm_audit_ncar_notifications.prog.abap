*&---------------------------------------------------------------------*
*& Report  ZPLM_AUDIT_NCAR_NOTIFICATIONS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zplm_audit_ncar_notifications.

DATA: lt_zaudit     TYPE TABLE OF zaudit_notif,
      ls_zaudit     TYPE zaudit_notif,
      lv_ftoday     TYPE facdate,
      lv_duedate    TYPE sy-datum,
      lv_sdate(10)  TYPE c,
      lv_fstarti    TYPE facdate,
      lv_fstartf    TYPE facdate,
      lv_fendi      TYPE facdate,
      lv_fendf      TYPE facdate,
      lv_lastemail  TYPE facdate,

      ls_auditaction    TYPE bapi_20370_d,
      ls_auditobject    TYPE bapi_20350_obj_d,
      lt_auditobject    LIKE TABLE OF ls_auditobject,
      lv_username       TYPE bapibname-bapibname,
      ls_smtp           TYPE bapiadsmtp,
      lt_smtp           LIKE TABLE OF ls_smtp,
      lt_return         TYPE TABLE OF bapiret2,
      lv_projectguid    TYPE cgpl_guid16,

      lt_pdf_data       TYPE text_line_tab,
      lt_pdf_attachment TYPE soli_tab,

      lt_gos_attach     TYPE TABLE OF srgbtbrel,
      ls_gos_attach     TYPE srgbtbrel,
      lv_gos_id         TYPE so_entryid,
      lv_gos_fname      TYPE sood-objdes,
      ls_gos_document   TYPE sofolenti1,
      lt_gos_data       TYPE TABLE OF solix,


      lr_project        TYPE REF TO cl_cgpl_project,
      ls_audit          TYPE bapi_20350_key,
      ls_bpemail        TYPE bapiadsmtp,
      lt_bpemail        LIKE TABLE OF ls_bpemail,
      lv_manager        TYPE sy-uname.

DATA:
      gv_sent_to_all   TYPE os_boolean,
      gv_email         TYPE adr6-smtp_addr,
      gv_subject       TYPE so_obj_des,
      gv_html          TYPE bcsy_text,
      gr_send_request  TYPE REF TO cl_bcs,
      gr_bcs_exception TYPE REF TO cx_bcs,
      gr_recipient     TYPE REF TO if_recipient_bcs,
      gr_sender        TYPE REF TO cl_sapuser_bcs,
      gr_document      TYPE REF TO cl_document_bcs.

CONSTANTS:  c_factorycalendar   TYPE scal-fcalid  VALUE 'UN',
            c_qapartner         TYPE c LENGTH 2 VALUE 'QA',

            c_important         TYPE c VALUE '1',
            c_subject           TYPE so_obj_des VALUE 'Audit Management Notification',
            c_html_header       TYPE string VALUE '<h1 style="color: #005bbb;">Quality Assurance Action Request</h1>',
            c_html_link         TYPE string
                                  VALUE '<p><a href="https://sm.spectraenergy.com/sap/bc/ui2/flp/FioriLaunchpad.html#ZOPS_SW-WOCOMPLETE">Link to Action - https://sm.spectraenergy.com/</a></p>'.

START-OF-SELECTION.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS:
    p_resp1 TYPE i DEFAULT 5,
    p_resp2 TYPE i DEFAULT 1,
    p_comp1 TYPE i DEFAULT 20,
    p_comp2 TYPE i DEFAULT 10.
  SELECTION-SCREEN END OF BLOCK b1.

END-OF-SELECTION.



  """"" Setup Variables """"""
  "Get todays Factory date
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      date                         = sy-datum
      factory_calendar_id          = c_factorycalendar
    IMPORTING
      factorydate                  = lv_ftoday
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



  """"" Send out Initial Email """""
  "Select where date is initial
  SELECT * FROM zaudit_notif
    INTO CORRESPONDING FIELDS OF TABLE lt_zaudit.

  LOOP AT lt_zaudit INTO ls_zaudit WHERE lastemail = 0.
    PERFORM sendemail USING ls_zaudit 'I'.
  ENDLOOP.



  """"" Send out Reminders """""
  LOOP AT lt_zaudit INTO ls_zaudit WHERE lastemail LT lv_ftoday.
    "Setup due dates
    lv_fstarti  = ls_zaudit-plannedstart - p_resp1.
    lv_fstartf  = ls_zaudit-plannedstart - p_resp2.
    lv_fendi    = ls_zaudit-plannedfinish - p_comp1.
    lv_fendf    = ls_zaudit-plannedfinish - p_comp2.

    "Save the last email date since it will change if we send an email out
    lv_lastemail = ls_zaudit-lastemail.

    "Reponse date
    IF ls_zaudit-response IS INITIAL.
      "5 days -> USM
      IF lv_lastemail < lv_fstarti AND lv_ftoday => lv_fstarti.
        PERFORM sendemail USING ls_zaudit 'RI'.
      ENDIF.
      "1 day -> USM+Manager
      IF lv_lastemail < lv_fstartf AND lv_ftoday => lv_fstartf.
        PERFORM sendemail USING ls_zaudit 'RF'.
      ENDIF.
    ENDIF.

    "Complete date
    "20 days -> USM
    IF lv_lastemail < lv_fendi AND lv_ftoday => lv_fendi.
      PERFORM sendemail USING ls_zaudit 'CI'.
    ENDIF.
    "10 days -> USM+Manager
    IF lv_lastemail < lv_fendf AND lv_ftoday => lv_fendf.
      PERFORM sendemail USING ls_zaudit 'CF'.
    ENDIF.
  ENDLOOP.



*&---------------------------------------------------------------------*
*&      Form  sendEmail
*&---------------------------------------------------------------------*
*       This will send the email out based on the Type variation
*----------------------------------------------------------------------*
*      -->IS_ZAUDIT  This is a record from ZAUDIT_NOTIF
*      -->IV_TYPE    'I' - Initial Email.
*                    'RI' - Response Initial
*                    'RF' - Response Final
*                    'CI' - Complete Initial
*                    'CF' - Complete Final
*----------------------------------------------------------------------*
FORM sendemail
USING is_zaudit   TYPE zaudit_notif
      iv_type     TYPE char2.

  CLEAR: ls_auditaction, ls_audit, lt_auditobject.

  "Get audit action details.
  CALL FUNCTION 'BAPI_BUS20370_GET_DETAIL'
    EXPORTING
      guid              = is_zaudit-action_guid
    IMPORTING
      auditcorrecaction = ls_auditaction.

  "Get Audit Question
  CALL FUNCTION 'BAPI_BUS20360_GET_DETAIL'
    EXPORTING
      guid  = ls_auditaction-parent_quest_guid
    IMPORTING
      audit = ls_audit.

  "Get Audit
  CALL FUNCTION 'BAPI_BUS20350_GET_DETAIL'
    EXPORTING
      guid           = ls_audit-guid
    TABLES
      auditedobjects = lt_auditobject.



  "Create Email class
  TRY.
      gr_send_request = cl_bcs=>create_persistent( ).
    CATCH cx_send_req_bcs .
  ENDTRY.

  "FROM
  TRY.
      gr_sender = cl_sapuser_bcs=>create( sy-uname ).
      CALL METHOD gr_send_request->set_sender
        EXPORTING
          i_sender = gr_sender.
    CATCH cx_send_req_bcs.
    CATCH cx_address_bcs .
  ENDTRY.
  "TO - Always USM
  "Save the USM to the table.
  is_zaudit-responsible = ls_auditaction-z_assignedto.

  "Lookup the email address
  lv_username = ls_auditaction-z_assignedto.
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username      = lv_username
      cache_results = 'X'
    TABLES
      return        = lt_return
      addsmtp       = lt_smtp.


  IF lt_return IS INITIAL.
    LOOP AT lt_smtp INTO ls_smtp.
      gv_email = ls_smtp-e_mail.
      TRY.
          gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
          CALL METHOD gr_send_request->add_recipient
            EXPORTING
              i_recipient = gr_recipient.
        CATCH cx_address_bcs.
        CATCH cx_send_req_bcs.

      ENDTRY.
    ENDLOOP.
  ENDIF.


  CASE iv_type.
    WHEN 'I'.
      LOOP AT lt_auditobject INTO ls_auditobject WHERE object_type = c_qapartner.
        "Lookup the email address
        IF ls_auditobject-value_text IS NOT INITIAL.
          gv_email = ls_auditobject-value_text.
          TRY.
              gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
              CALL METHOD gr_send_request->add_recipient
                EXPORTING
                  i_recipient = gr_recipient
                  i_copy      = abap_true.
            CATCH cx_address_bcs.
            CATCH cx_send_req_bcs.
          ENDTRY.
        ENDIF.
      ENDLOOP.
      "Email Body
      CLEAR gv_html.
      APPEND c_html_header TO gv_html.
      APPEND '<p><strong>Action: ' TO gv_html.
      APPEND ls_auditaction-external_id TO gv_html.
      APPEND '</strong></p>' TO gv_html.

      APPEND '<p>A non-conformance issue has been identified and a report has been generated for the following audit.' TO gv_html.
      APPEND '&nbspPlease use the link below to complete this action.</p>' TO gv_html.

      APPEND c_html_link TO gv_html.
      APPEND '<p>See attached PDF for a summary of this action.</p>' TO gv_html.
      TRY .
          gr_document = cl_document_bcs=>create_document(
                  i_type        = 'HTM'
                  i_text        = gv_html
                  i_importance  = c_important
                  i_subject     = c_subject ).
        CATCH cx_document_bcs .
      ENDTRY.
      "Include PDF of Action
      CALL METHOD cl_plm_audit_convert_services=>conversion_aud_input
        EXPORTING
          iv_input  = ls_auditaction-external_id
        RECEIVING
          rv_output = lv_projectguid.

      CALL METHOD cl_plm_audit_if_services=>get_cgpl_data_by_guid
        EXPORTING
          iv_guid        = lv_projectguid
          iv_change_mode = ' '
        IMPORTING
          er_project     = lr_project
        EXCEPTIONS
          not_found      = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CLEAR: lt_pdf_data, lt_pdf_attachment.

      CALL METHOD cl_plm_audit_services=>collect_data
        EXPORTING
          ir_object             = lr_project
          iv_convert_data_2_pdf = abap_true
          iv_application_type   = 'AUD'
        IMPORTING
          et_pdf_data           = lt_pdf_data
        EXCEPTIONS
          failed                = 1
          OTHERS                = 2.

      "Convert to BCS Format
      IF lt_pdf_data IS NOT INITIAL.

        CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
          EXPORTING
            line_width_src              = 134
            line_width_dst              = 255
          TABLES
            content_in                  = lt_pdf_data
            content_out                 = lt_pdf_attachment
          EXCEPTIONS
            err_line_width_src_too_long = 1
            err_line_width_dst_too_long = 2
            err_conv_failed             = 3
            OTHERS                      = 4.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
      ENDIF.

      IF lt_pdf_attachment IS NOT INITIAL.
        TRY.
            gr_document->add_attachment(
                          EXPORTING i_attachment_type = 'PDF'
                                    i_attachment_subject = 'Action Details'
                                    i_att_content_text = lt_pdf_attachment ).
          CATCH cx_document_bcs.
        ENDTRY.
      ENDIF.

      "Include GOS Attachments
      SELECT * FROM srgbtbrel
        INTO CORRESPONDING FIELDS OF TABLE lt_gos_attach
        WHERE reltype = 'ATTA'
          AND typeid_a = 'BUS20370'
          AND instid_a = ls_auditaction-guid.

      "Add all the attachments to the email
      LOOP AT lt_gos_attach INTO ls_gos_attach.
        CLEAR: lt_gos_data.
        lv_gos_id = ls_gos_attach-instid_b.
        CALL FUNCTION 'SO_DOCUMENT_READ_API1'
          EXPORTING
            document_id                = lv_gos_id
          IMPORTING
            document_data              = ls_gos_document
          TABLES
            contents_hex               = lt_gos_data
          EXCEPTIONS
            document_id_not_exist      = 1
            operation_no_authorization = 2
            x_error                    = 3
            OTHERS                     = 4.
        IF lt_gos_data IS NOT INITIAL.
          CONCATENATE ls_gos_document-obj_descr '.' ls_gos_document-obj_type INTO lv_gos_fname.
          gr_document->add_attachment(
                        EXPORTING i_attachment_type = 'EXT'
                                  i_attachment_subject = lv_gos_fname
                                  i_att_content_hex = lt_gos_data ).
        ENDIF.
      ENDLOOP.

      "Add document to send request
      TRY.
          CALL METHOD gr_send_request->set_document( gr_document ).
        CATCH cx_send_req_bcs .
      ENDTRY.
    WHEN 'RI'.
      "Get date from plannedstart
      CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
        EXPORTING
          factorydate                  = is_zaudit-plannedstart
          factory_calendar_id          = c_factorycalendar
        IMPORTING
          date                         = lv_duedate
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          factorydate_after_range      = 2
          factorydate_before_range     = 3
          factorydate_invalid          = 4
          factory_calendar_id_missing  = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      "Convert to yyyy/mm/dd
      CONCATENATE lv_duedate(4) lv_duedate+4(2) lv_duedate+6(2) INTO lv_sdate SEPARATED BY '/'.


      "Email Body
      CLEAR gv_html.
      APPEND c_html_header TO gv_html.
      APPEND '<h2 style="color: #005bbb; text-decoration: underline;">Response Warning</h2>' TO gv_html.

      APPEND '<p><strong>Action: ' TO gv_html.
      APPEND ls_auditaction-external_id TO gv_html.
      APPEND '</strong></p>' TO gv_html.

      APPEND '<p>One of your assigned actions has not been started and an initial response is required by <strong>' TO gv_html.
      APPEND lv_sdate TO gv_html.
      APPEND '</strong>.' TO gv_html.
      APPEND '&nbspPlease use the link below to start work on this action.</p>' TO gv_html.

      APPEND c_html_link TO gv_html.
      TRY.
          gr_document = cl_document_bcs=>create_document(
                          i_type        = 'HTM'
                          i_text        = gv_html
                          i_importance  = c_important
                          i_subject     = c_subject ).
        CATCH cx_document_bcs.
      ENDTRY.
      "Add document to send request
      TRY.
          CALL METHOD gr_send_request->set_document( gr_document ).
        CATCH cx_send_req_bcs.
      ENDTRY.

    WHEN 'RF'.
      "Get date from plannedstart
      CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
        EXPORTING
          factorydate                  = is_zaudit-plannedstart
          factory_calendar_id          = c_factorycalendar
        IMPORTING
          date                         = lv_duedate
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          factorydate_after_range      = 2
          factorydate_before_range     = 3
          factorydate_invalid          = 4
          factory_calendar_id_missing  = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      "Convert to yyyy/mm/dd
      CONCATENATE lv_duedate(4) lv_duedate+4(2) lv_duedate+6(2) INTO lv_sdate SEPARATED BY '/'.

      "USM Manager
      IF ls_auditaction-z_assignedto IS NOT INITIAL.
*        BOI by MPULAGAM Ticket ACR-4915
        IF ls_auditaction-actualstartdate is INITIAL.
*        EOI by MPULAGAM Ticket ACR-4915
          "Get manager from HR
          CALL FUNCTION 'ZHR_GET_MANAGER'
            EXPORTING
              imp_username     = ls_auditaction-z_assignedto
            IMPORTING
              exp_manager_user = lv_manager
            EXCEPTIONS
              nobody_found     = 1
              no_manager_found = 2
              no_data_supplied = 3
              OTHERS           = 4.
          IF sy-subrc = 0.
            "Get email from HR
            CALL FUNCTION 'ZWF_FIND_EMAIL'
              EXPORTING
                id    = lv_manager
              IMPORTING
                email = gv_email.
            IF sy-subrc = 0.
              TRY .
                  gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
                  CALL METHOD gr_send_request->add_recipient
                    EXPORTING
                      i_recipient = gr_recipient.
                CATCH CX_ADDRESS_BCS.
                CATCH CX_SEND_REQ_BCS.
              ENDTRY.
            ENDIF.
          ENDIF.
*        BOI by MPULAGAM Ticket ACR-4915
        ENDIF.
*        BOI by MPULAGAM Ticket ACR-4915
      ENDIF.


      "Email Body
      CLEAR gv_html.
      APPEND c_html_header TO gv_html.
      APPEND '<h2 style="color: #ff0000; text-decoration: underline;">Response Final Warning</h2>' TO gv_html.

      APPEND '<p><strong>Action: ' TO gv_html.
      APPEND ls_auditaction-external_id TO gv_html.
      APPEND '</strong></p>' TO gv_html.

      APPEND '<p>One of your assigned actions has not been started and an initial response is required by <span style="background-color: #ff0000;"><strong>' TO gv_html.
      APPEND lv_sdate TO gv_html.
      APPEND '</strong></span>.' TO gv_html.
      APPEND '&nbspYour manager has been copied on this email.' TO gv_html.
      APPEND '&nbspPlease use the link below to start work on this action.</p>' TO gv_html.

      APPEND c_html_link TO gv_html.

      TRY.
          gr_document = cl_document_bcs=>create_document(
                          i_type        = 'HTM'
                          i_text        = gv_html
                          i_importance  = c_important
                          i_subject     = c_subject ).
        CATCH cx_document_bcs.

      ENDTRY.
      "Add document to send request
      TRY.
          CALL METHOD gr_send_request->set_document( gr_document ).
        CATCH cx_send_req_bcs.

      ENDTRY.
    WHEN 'CI'.
      "Get date from plannedfinish
      CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
        EXPORTING
          factorydate                  = is_zaudit-plannedfinish
          factory_calendar_id          = c_factorycalendar
        IMPORTING
          date                         = lv_duedate
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          factorydate_after_range      = 2
          factorydate_before_range     = 3
          factorydate_invalid          = 4
          factory_calendar_id_missing  = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.
      IF sy-subrc <> 0.

      ENDIF.
      "Convert to yyyy/mm/dd
      CONCATENATE lv_duedate(4) lv_duedate+4(2) lv_duedate+6(2) INTO lv_sdate SEPARATED BY '/'.

      "Email Body
      CLEAR gv_html.
      APPEND c_html_header TO gv_html.
      APPEND '<h2 style="color: #005bbb; text-decoration: underline;">Completion Warning</h2>' TO gv_html.

      APPEND '<p><strong>Action: ' TO gv_html.
      APPEND ls_auditaction-external_id TO gv_html.
      APPEND '</strong></p>' TO gv_html.

      APPEND '<p>One of your assigned actions has not been completed and is due on <strong>' TO gv_html.
      APPEND lv_sdate TO gv_html.
      APPEND '</strong>.' TO gv_html.
      APPEND '&nbspPlease use the link below to complete this action.</p>' TO gv_html.

      APPEND c_html_link TO gv_html.
      TRY.
          gr_document = cl_document_bcs=>create_document(
                          i_type        = 'HTM'
                          i_text        = gv_html
                          i_importance  = c_important
                          i_subject     = c_subject ).
        CATCH cx_document_bcs.
      ENDTRY.
      "Add document to send request
      TRY.
          CALL METHOD gr_send_request->set_document( gr_document ).
        CATCH cx_send_req_bcs.
      ENDTRY.
    WHEN 'CF'.
      "Get date from plannedfinish
      CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
        EXPORTING
          factorydate                  = is_zaudit-plannedfinish
          factory_calendar_id          = c_factorycalendar
        IMPORTING
          date                         = lv_duedate
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          factorydate_after_range      = 2
          factorydate_before_range     = 3
          factorydate_invalid          = 4
          factory_calendar_id_missing  = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.
      IF sy-subrc <> 0.
      ENDIF.
      "Convert to yyyy/mm/dd
      CONCATENATE lv_duedate(4) lv_duedate+4(2) lv_duedate+6(2) INTO lv_sdate SEPARATED BY '/'.

      "USM Manager
      IF ls_auditaction-z_assignedto IS NOT INITIAL.
        IF sy-subrc = 0.
*        BOI by MPULAGAM Ticket ACR-4915
          IF ls_auditaction-actualstartdate is INITIAL.
*        EOI by MPULAGAM Ticket ACR-4915
            "Get manager from HR
            CALL FUNCTION 'ZHR_GET_MANAGER'
              EXPORTING
                imp_username     = ls_auditaction-z_assignedto
              IMPORTING
                exp_manager_user = lv_manager
              EXCEPTIONS
                nobody_found     = 1
                no_manager_found = 2
                no_data_supplied = 3
                OTHERS           = 4.
            IF sy-subrc = 0.
              "Get email from HR
              CALL FUNCTION 'ZWF_FIND_EMAIL'
                EXPORTING
                  id    = lv_manager
                IMPORTING
                  email = gv_email.
              IF sy-subrc = 0.
                TRY.
                    gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
                  CATCH cx_address_bcs.
                ENDTRY.
                TRY.
                    CALL METHOD gr_send_request->add_recipient
                      EXPORTING
                        i_recipient = gr_recipient.
                  CATCH cx_send_req_bcs.

                ENDTRY.
              ENDIF.
            ENDIF.
*          BOI by MPULAGAM Ticket ACR-4915
        ENDIF.
*        BOI by MPULAGAM Ticket ACR-4915
        ENDIF.
      ENDIF.
      "Email Body
      CLEAR gv_html.
      APPEND c_html_header TO gv_html.
      APPEND '<h2 style="color: #ff0000; text-decoration: underline;">Completion Final Warning!</h2>' TO gv_html.

      APPEND '<p><strong>Action: ' TO gv_html.
      APPEND ls_auditaction-external_id TO gv_html.
      APPEND '</strong></p>' TO gv_html.

      APPEND '<p>One of your assigned actions has not been completed and is due on <span style="background-color: #ff0000;"><strong>' TO gv_html.
      APPEND lv_sdate TO gv_html.
      APPEND '</strong></span>.' TO gv_html.
      APPEND '&nbspYour manager has been copied on this email.' TO gv_html.
      APPEND '&nbspPlease use the link below to complete this action.</p>' TO gv_html.

      APPEND c_html_link TO gv_html.

      TRY.
          gr_document = cl_document_bcs=>create_document(
                          i_type        = 'HTM'
                          i_text        = gv_html
                          i_importance  = c_important
                          i_subject     = c_subject ).
        CATCH cx_document_bcs.

      ENDTRY.
      "Add document to send request
      TRY.
          CALL METHOD gr_send_request->set_document( gr_document ).
        CATCH cx_send_req_bcs.
      ENDTRY.
  ENDCASE.



  "Send email
  TRY .
      gr_send_request->set_send_immediately( abap_true ).
      CALL METHOD gr_send_request->send(
        RECEIVING
          result = gv_sent_to_all ).
    CATCH cx_send_req_bcs.
  ENDTRY.
  IF gv_sent_to_all = abap_true.
    WRITE /: 'Email sent: ', iv_type.
    "Update zaudit_notif from this object
    is_zaudit-lastemail = lv_ftoday.
    UPDATE zaudit_notif FROM is_zaudit.
  ENDIF.

  "Send the email
  COMMIT WORK.
ENDFORM.                    "addit
