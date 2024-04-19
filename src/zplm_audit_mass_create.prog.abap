*&---------------------------------------------------------------------*
*& Report  ZPLM_AUDIT_MASS_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zplm_audit_mass_create.

TYPES:  BEGIN OF ty_data,
          template    TYPE bapi_20350_d-external_id,  "Required
          type        TYPE plmt_audit_type,           "Required
          trigger     TYPE plmt_audit_cause,          "Required
          grouping    TYPE plmt_grouping,
          district    TYPE z_district,
          division    TYPE z_division,
          description TYPE cgpl_text1,
          workforce   TYPE plmt_audit_object_value,
          manager     TYPE xubname,
          qapartner   TYPE cgpl_text1,
          auditor     TYPE cgpl_text1,
          jobid       TYPE z_jobid,
          contact     TYPE z_site_cont,
          meter       TYPE zbapi_20350_d-z_meterno,
          meterdesc   TYPE zbapi_20350_d-z_meterlocation,
          address     TYPE z_adrs1,
          town        TYPE z_town1,
          count,
        END OF ty_data.

DATA: gt_data       TYPE TABLE OF ty_data,
      gs_data       TYPE ty_data,
      gs_auditplan  TYPE bapi_20300_c,
      gv_error      TYPE xfeld,
      gv_wp_no      TYPE wpinfo-wp_no,
      gv_timestamp  TYPE timestampl,
      gt_msg        TYPE TABLE OF string,
      gv_msg        TYPE string.

CONSTANTS:  c_nrobject    TYPE inri-object    VALUE 'ZAUO',
            c_nrrangeap   TYPE inri-nrrangenr VALUE 'AP',
            c_nrrangeau   TYPE inri-nrrangenr VALUE 'AU'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_aplan TYPE bapi_bus20310_text-description OBLIGATORY ,
            p_start TYPE plmt_validfromdate OBLIGATORY DEFAULT sy-datum,
            p_end   TYPE plmt_validtodate,
            p_group TYPE plmt_grouping.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_lfile   TYPE  rfpdo-rfbifile DEFAULT 'H:\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',TXT File,*.txt'
      static    = 'X'
    CHANGING
      file_name = p_lfile.

START-OF-SELECTION.

  PERFORM validate_selection_data.
  PERFORM upload_local_file.
  IF gt_data[] IS NOT INITIAL AND gv_error IS INITIAL.
    "Get current WP for uniqueness later
    CALL FUNCTION 'TH_GET_OWN_WP_NO'
      IMPORTING
        wp_no = gv_wp_no.
    REPLACE FIRST OCCURRENCE OF space IN gv_wp_no WITH '0'.
    PERFORM process_data.
  ELSE.
    WRITE : / ' No data to process...'.
  ENDIF.
  PERFORM display_messages.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_data .

  DATA: lv_curline    TYPE string,

        lv_aplanguid  TYPE bapi_20350_c-guid,
        ls_auditplank TYPE bapi_20350_key,
        lt_aptext     TYPE STANDARD TABLE OF bapi_bus20310_text,


        ls_audit              TYPE bapi_20350_d,
        ls_newaudit           TYPE bapi_20350_c,

        lt_text               TYPE TABLE OF bapi_bus20310_text,
        lt_longtext           TYPE TABLE OF bapi_bus20350_long_text,
        lt_auditobject        TYPE TABLE OF bapi_20350_obj_d,
        lt_auditquestion      TYPE TABLE OF bapi_20360_d,

        lt_newtext            LIKE lt_text,
        lt_newlongtext        LIKE lt_longtext,
        lt_newauditobject     TYPE TABLE OF bapi_20350_obj_c,
        lt_newauditquestion   TYPE TABLE OF bapi_20360_c,

        ls_text               LIKE LINE OF lt_text,
        ls_longtext           LIKE LINE OF lt_longtext,
        ls_auditobject        LIKE LINE OF lt_auditobject,
        ls_auditquestion      LIKE LINE OF lt_auditquestion,

        ls_newauditobject     LIKE LINE OF lt_newauditobject,
        ls_newauditquestion   LIKE LINE OF lt_newauditquestion,

        lv_timestamp          TYPE timestampl,
        ls_return             TYPE bapiret2,
        lt_return             LIKE TABLE OF ls_return,

        lv_error              LIKE gv_error,
        lv_numaudits          TYPE i,
        lv_curaudit           TYPE i,
        lv_description        TYPE cgpl_text1.



  "Create the Audit Plan
  "Get the next number for the PLAN
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = c_nrrangeap
      object      = c_nrobject
    IMPORTING
      number      = gs_auditplan-external_id.

  "Add the AP infront of the number
  CONCATENATE 'AP' gs_auditplan-external_id INTO gs_auditplan-external_id.

  "Populate Fields
  gs_auditplan-validfromdate = p_start.
  gs_auditplan-validtodate = p_end.
  gs_auditplan-grouping = p_group.

  "Set the description text object
  ls_text-external_id   = gs_auditplan-external_id. "Same external id as main object
  ls_text-description   = p_aplan.
  APPEND ls_text TO lt_aptext.

  CALL FUNCTION 'BAPI_BUS20300_CREATE'
    EXPORTING
      auditplan = gs_auditplan
*     BELOW     = "Who's your Daddy
    IMPORTING
      guid      = ls_auditplank-guid
    TABLES
      texts     = lt_aptext.

  "Loop through all the audit entries
  LOOP AT gt_data INTO gs_data.
    lv_curline = sy-tabix.
    IF lv_curline = 1.
      CONTINUE.
    ENDIF.
    lv_error = abap_false.

    CLEAR:  ls_audit,
            lt_text,
            lt_longtext,
            lt_auditobject,
            lt_auditquestion,
            lt_return.

    "Validate Required entries in File
    IF gs_data-template IS INITIAL OR gs_data-type IS INITIAL OR gs_data-trigger IS INITIAL.
      CONCATENATE 'Line:' lv_curline '- Error:Missing required field (Template/Type/Trigger)' INTO gv_msg.
      APPEND gv_msg TO gt_msg.
      gv_error = abap_true.
      lv_error = abap_true.
      "Skip this line item.
      CONTINUE.
    ENDIF.

    gs_data-template = gs_data-template(14).

    "Read in the template
    CALL FUNCTION 'BAPI_BUS20350_GET_DETAIL'
      EXPORTING
        external_id       = gs_data-template
      IMPORTING
        audit             = ls_audit
      TABLES
        texts             = lt_text
        longtexts         = lt_longtext
        auditedobjects    = lt_auditobject
        auditquestresults = lt_auditquestion
        return            = lt_return.

    LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
      CONCATENATE 'Line:' lv_curline '- Error:' ls_return-message INTO gv_msg.
      APPEND gv_msg TO gt_msg.
      gv_error = abap_true.
      lv_error = abap_true.
    ENDLOOP.

    "Exit gt_data loop
    IF lv_error = abap_true.
      CONTINUE.
    ENDIF.

    "How many of these audits to we need?
    IF gs_data-count IS NOT INITIAL AND gs_data-count > 0.
      "Make sure it is a number
      IF gs_data-count CO '1234567890'.
        lv_numaudits = gs_data-count.
      ELSE.
        "Not a number
        CONCATENATE 'Line:' lv_curline '- Error:Number of Audits is non-numeric' INTO gv_msg.
        APPEND gv_msg TO gt_msg.
        gv_error = abap_true.
        lv_error = abap_true.
        "Skip this line
        CONTINUE.
      ENDIF.
    ELSE.
      lv_numaudits = 1.
    ENDIF.



    "Loop as many times as required to create the audits
    lv_curaudit = 0.
    WHILE lv_curaudit < lv_numaudits AND gv_error = abap_false.
      lv_curaudit = lv_curaudit + 1.

      "Clear all the data structures
      CLEAR:  lt_newtext,
              lt_newlongtext,
              "lt_newauditobject, "Do not clear this since it doesn't need to change each loop.
              lt_newauditquestion.


      IF lv_curaudit = 1.
        "First Loop

        "Copy from the template as a starting point
        MOVE-CORRESPONDING ls_audit TO ls_newaudit.

        "Get the next number for the AUDIT
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = c_nrrangeau
            object      = c_nrobject
          IMPORTING
            number      = ls_newaudit-external_id.

        "****Mandatory Fields
        CLEAR:  ls_newaudit-guid,
                ls_newaudit-actualstartdate,
                ls_newaudit-actualstarttime,
                ls_newaudit-actualfinishdate,
                ls_newaudit-actualfinishtime.
        CONCATENATE 'AU' ls_newaudit-external_id INTO ls_newaudit-external_id.
        ls_newaudit-audit_type = gs_data-type.
        ls_newaudit-cause = gs_data-trigger.
        ls_newaudit-planstartdate = p_start.
        ls_newaudit-planfinishdate = p_end.

        "****Description
        LOOP AT lt_text INTO ls_text WHERE external_id = ls_audit-external_id.
          CLEAR ls_text-ref_guid.
          ls_text-external_id = ls_newaudit-external_id.
          IF gs_data-description IS NOT INITIAL.
            ls_text-description = gs_data-description.
          ENDIF.
          "Save the description for next loop
          lv_description = ls_text-description.
          APPEND ls_text TO lt_newtext.
        ENDLOOP.

        "****Audit Object
        LOOP AT lt_auditobject INTO ls_auditobject.
          CLEAR ls_newauditobject.
          MOVE-CORRESPONDING ls_auditobject TO ls_newauditobject.
          CLEAR ls_newauditobject-guid.
          "Workforce
          IF gs_data-workforce IS NOT INITIAL AND ls_auditobject-object_type = '00'.
            ls_newauditobject-object_value = gs_data-workforce.
          ENDIF.
          "QAPartner
          IF gs_data-qapartner IS NOT INITIAL AND ls_auditobject-object_type = 'QA'.
            ls_newauditobject-object_value = gs_data-qapartner.
          ENDIF.
          APPEND ls_newauditobject TO lt_newauditobject.
        ENDLOOP.

        "****Optional Fields
        IF gs_data-district IS NOT INITIAL.
          ls_newaudit-z_district = gs_data-district.
        ENDIF.

        IF gs_data-division IS NOT INITIAL.
          ls_newaudit-z_division = gs_data-division.
        ENDIF.

        IF gs_data-manager IS NOT INITIAL.
          ls_newaudit-z_manager = gs_data-manager.
          TRANSLATE ls_newaudit-z_manager TO UPPER CASE.
        ENDIF.

        IF gs_data-auditor IS NOT INITIAL.
          ls_newaudit-z_auditor_planned = gs_data-auditor.
          TRANSLATE ls_newaudit-z_auditor_planned TO UPPER CASE.
        ENDIF.

        IF gs_data-jobid IS NOT INITIAL.
          ls_newaudit-z_jobid = gs_data-jobid.
        ENDIF.

        IF gs_data-contact IS NOT INITIAL.
          ls_newaudit-z_site_cont = gs_data-contact.
        ENDIF.

        IF gs_data-meter IS NOT INITIAL.
          ls_newaudit-z_meterno = gs_data-meter.
        ENDIF.

        IF gs_data-meterdesc IS NOT INITIAL.
          ls_newaudit-z_meterlocation = gs_data-meterdesc.
        ENDIF.

        IF gs_data-address IS NOT INITIAL.
          ls_newaudit-z_adress = gs_data-address.
        ENDIF.

        IF gs_data-town IS NOT INITIAL.
          ls_newaudit-z_town = gs_data-town.
        ENDIF.

      ELSE.
        "Subsequent loops
        "****New External ID
        "Get the next number for the AUDIT
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = c_nrrangeau
            object      = c_nrobject
          IMPORTING
            number      = ls_newaudit-external_id.
        CLEAR ls_newaudit-guid.

        CONCATENATE 'AU' ls_newaudit-external_id INTO ls_newaudit-external_id.

        "****Description
        LOOP AT lt_text INTO ls_text WHERE external_id = ls_audit-external_id.
          CLEAR ls_text-ref_guid.
          ls_text-external_id = ls_newaudit-external_id.
          "Convert current auditNumber to text
          ls_text-description = lv_curaudit.
          SHIFT ls_text-description LEFT DELETING LEADING ' '.
          CONCATENATE lv_description ls_text-description INTO ls_text-description SEPARATED BY space.
          APPEND ls_text TO lt_newtext.
        ENDLOOP.
      ENDIF.

      "****Long Text
      LOOP AT lt_longtext INTO ls_longtext WHERE external_id = ls_audit-external_id.
        CLEAR ls_longtext-ref_guid.
        ls_longtext-external_id = ls_newaudit-external_id.
        APPEND ls_longtext TO lt_longtext.
      ENDLOOP.

      "****Questions
      LOOP AT lt_auditquestion INTO ls_auditquestion.
        "Save the Current question/Clear guid
        CLEAR ls_newauditquestion.
        MOVE-CORRESPONDING ls_auditquestion TO ls_newauditquestion.
        CLEAR:  ls_newauditquestion-guid,
                ls_newauditquestion-assessment_rec,
                ls_newauditquestion-assessment_res,
                ls_newauditquestion-not_relevant,
                ls_newauditquestion-score_rec,
                ls_newauditquestion-score_res,
                ls_newauditquestion-score_max,
                ls_newauditquestion-corr_required.


        "Create new External_ID
        GET TIME STAMP FIELD lv_timestamp.
        IF lv_timestamp <= gv_timestamp.
          lv_timestamp = gv_timestamp + '0.0000001'.
        ENDIF.

        gv_timestamp = lv_timestamp.
        ls_newauditquestion-external_id = lv_timestamp.
        SHIFT ls_newauditquestion-external_id LEFT DELETING LEADING space.
        WRITE ls_newauditquestion-external_id+15(7) TO ls_newauditquestion-external_id+14.
        "Additional information to be sure that key is uniqeue 99,9999%
        WRITE gv_wp_no TO ls_newauditquestion-external_id+21.

        LOOP AT lt_text INTO ls_text WHERE external_id = ls_auditquestion-external_id.
          CLEAR ls_text-ref_guid.
          ls_text-external_id = ls_newauditquestion-external_id.
          APPEND ls_text TO lt_newtext.
        ENDLOOP.

        LOOP AT lt_longtext INTO ls_longtext WHERE external_id = ls_auditquestion-external_id.
          CLEAR ls_longtext-ref_guid.
          ls_longtext-external_id = ls_newauditquestion-external_id.
          APPEND ls_longtext TO lt_newlongtext.
        ENDLOOP.
        APPEND ls_newauditquestion TO lt_newauditquestion.
      ENDLOOP.

      "Create the audit as a child of the audit plan.
      CALL FUNCTION 'BAPI_BUS20350_CREATE'
        EXPORTING
          audit             = ls_newaudit
          auditplan         = ls_auditplank
        TABLES
          auditedobjects    = lt_newauditobject
          texts             = lt_newtext
          longtexts         = lt_newlongtext
          auditquestresults = lt_newauditquestion
          return            = lt_return.

      "Check for errors.
      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        CONCATENATE 'Line:' lv_curline '- Error:' ls_return-message INTO gv_msg.
        APPEND gv_msg TO gt_msg.
        gv_error = abap_true.
        lv_error = abap_true.
      ENDLOOP.
    ENDWHILE.
  ENDLOOP.
  IF gv_error = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.
ENDFORM.                    " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SELECTION_DATA
*&---------------------------------------------------------------------*
*       Validate Selection Screen values
*----------------------------------------------------------------------*
FORM validate_selection_data .

  IF p_end < p_start AND p_end IS NOT INITIAL.
    WRITE : / 'End Date cannot be before Start Date'.
    STOP.
  ENDIF.
ENDFORM.                    " VALIDATE_SELECTION_DATA
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_LOCAL_FILE
*&---------------------------------------------------------------------*
*       Read in the file
*----------------------------------------------------------------------*
FORM upload_local_file .
  DATA:   lv_filename TYPE string.

  lv_filename = p_lfile.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = lv_filename
      has_field_separator = 'X' "Tab Delimited
      filetype            = 'ASC'
    CHANGING
      data_tab            = gt_data
    EXCEPTIONS
      file_open_error     = 1
      file_read_error     = 2
      OTHERS              = 18.
  CASE sy-subrc.
    WHEN 1.
      CONCATENATE 'File Open error in file' p_lfile+2
            'in disk drive' p_lfile+0(2) INTO gv_msg
            SEPARATED BY space.
      APPEND gv_msg TO gt_msg.
      gv_error = abap_true.
    WHEN 2.
      CONCATENATE 'Read error in file' p_lfile+2
       'in disk drive' p_lfile+0(2) INTO gv_msg
       SEPARATED BY space.
      APPEND gv_msg TO gt_msg.
      gv_error = abap_true.
    WHEN 18.
      CONCATENATE 'Read error in file' p_lfile+2
       'in disk drive' p_lfile+0(2) INTO gv_msg
       SEPARATED BY space.
      APPEND gv_msg TO gt_msg.
      gv_error = abap_true.
  ENDCASE.
ENDFORM.                    " UPLOAD_LOCAL_FILE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*       Show messages
*----------------------------------------------------------------------*
FORM display_messages .
  LOOP AT gt_msg INTO gv_msg.
    WRITE : / gv_msg.
  ENDLOOP.

  IF gt_msg IS INITIAL.
    WRITE:  'Upload complete: Planned Audit ',
            gs_auditplan-external_id,
            ' created'.
  ENDIF.
ENDFORM.                    " DISPLAY_MESSAGES
