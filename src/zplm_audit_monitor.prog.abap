*&---------------------------------------------------------------------*
*& Report  PLM_AUDIT_MONITOR                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  This report is used to search for audit plans, audits,             *
*&  corrective actions, questionnaires, master questions and           *
*&  audit  questions/answers;                                          *
*&                                                                     *
*&  results are displayed in a table                                   *
*&---------------------------------------------------------------------*

REPORT  zplm_audit_monitor      .

TYPE-POOLS: rsds.

* general constants for audit management
INCLUDE plm_audit_obtypes.

************************************************************************
* selection screen definition                                          *
************************************************************************

DATA:
* radio button options
  gv_aud_objecttype   TYPE cgpl_object_type,
* text for object type
  gv_objecttext       TYPE cgpl_object_text,

* selection fields
  gv_searchfield      TYPE plmt_search_field,
  gv_grouping         TYPE plmt_grouping,
  gv_external_id      TYPE plmt_audit_object,
  gv_ext_position     TYPE plmt_ext_position,
  gv_planstart        TYPE plmt_planstartdate,
  gv_actualstart      TYPE plmt_actualstartdate,
  gv_planfinish       TYPE plmt_planfinishdate,
  gv_actualfinish     TYPE plmt_actualfinishdate,
  gv_validfrom        TYPE plmt_validfromdate,
  gv_validto          TYPE plmt_validtodate,
  gv_finaldate        TYPE plmt_audit_date_final,
  gv_changed_on       TYPE plmt_changed_on,
  gv_created_on       TYPE plmt_created_on,
  gv_changed_by       TYPE plmt_changed_by,
  gv_created_by       TYPE plmt_created_by, "LIKE USR02-BNAME,
  gv_partner_guid     TYPE bu_partner_guid,
  gv_name             LIKE usr02-bname, "TYPE SYUNAME,
  gv_audit_type       TYPE plmt_audit_type,
  gv_auth_group       LIKE plmc_auth-auth_group,
  gv_object_type      TYPE plmt_auditobjecttype,
  gv_object_value     TYPE plmt_audit_object_value,
  gv_status_partsig   TYPE c,

  gv_district         TYPE plmm_audit-z_district,     "ACR264
  gv_jobid            TYPE plmm_audit-z_jobid,        "ACR264
  gv_meter            TYPE plmm_audit-z_meterno,      "ACR264
  gv_sitecont         TYPE plmm_audit-z_site_cont,    "ACR264
  gv_add_sitecont     TYPE plmm_audit-z_add_sitecont, "ACR264
  gv_plannedaudit     TYPE plmm_audit-z_auditor_planned, "ACR264

* special fields for audited object search
  gv_object_fields_count  TYPE i,

* selection field ranges
  gt_searchfield_range    TYPE plmt_audit_ranges_searchfield,
  gt_grouping_range       TYPE plmt_audit_ranges_grouping,
  gt_external_id_range    TYPE plmt_audit_ranges_external_id,
  gt_auth_group_range     TYPE plmt_audit_ranges_auth_group,
  gt_created_on_range     TYPE plmt_audit_ranges_for_date,
  gt_changed_on_range     TYPE plmt_audit_ranges_for_date,
  gt_created_by_range     TYPE plmt_ranges_for_user,
  gt_changed_by_range     TYPE plmt_ranges_for_user,
  gt_actualstart_range    TYPE plmt_audit_ranges_for_date,
  gt_planstart_range      TYPE plmt_audit_ranges_for_date,
  gt_actualfinish_range   TYPE plmt_audit_ranges_for_date,
  gt_planfinish_range     TYPE plmt_audit_ranges_for_date,
  gt_ext_position_range   TYPE plmt_ranges_for_ext_position,
  gt_partner_guid_range   TYPE plmt_ranges_for_partner,
  gt_partner_range        TYPE plmt_ranges_for_partner,

  gt_district_range       TYPE z_district_range,
  gt_jobid_range          TYPE z_jobid_range,
  gt_meter_range          TYPE z_meter_range,
  gt_plannedaudit         TYPE z_auditor_p_range,
  gt_sitecont_range       TYPE z_site_cont_range,
  gt_add_sitecont_range   TYPE z_site_cont_range,

  gt_unplanned_item_range TYPE plmt_audit_ranges_flag,
  gt_corr_required_range  TYPE plmt_audit_ranges_flag,
  gt_txt_has_fact_range   TYPE plmt_audit_ranges_flag,
  gt_txt_has_posi_range   TYPE plmt_audit_ranges_flag,
  gt_txt_has_nega_range   TYPE plmt_audit_ranges_flag,
  gt_score_result_range   TYPE plmt_audit_ranges_dec10,
  gt_score_recorded_range TYPE plmt_audit_ranges_dec5,

  gt_audit_type_range       TYPE plmt_audit_ranges_audit_type,
  gt_object_type_range      TYPE plmt_audit_ranges_audit_type,
  gt_object_value_range     TYPE plmt_ranges_for_audited_object,
  gt_audited_obj_complex    TYPE plmt_audited_object_search_tab,

  gt_unplanned_range        TYPE plmt_audit_ranges_flag,
  gt_score_res_range        TYPE plmt_audit_ranges_dec10,
  gt_next_audit_oblig_range TYPE plmt_audit_ranges_flag,

  gt_quest_ref_guid_range   TYPE plmt_audit_ranges_guid,

  gt_proc_status_range      TYPE plmt_audit_ranges_proc_status,

  gv_dynprofield            TYPE help_info-dynprofld.

DATA gs_help_infos    TYPE help_info.
DATA gt_excludefun    TYPE STANDARD TABLE OF syucomm.
DATA gt_helplines     TYPE STANDARD TABLE OF tline.
DATA gs_tline         TYPE  tline.
DATA lv_acc_mode      TYPE abap_bool.

* form routines:
INCLUDE plm_audit_monitor_forms.

*selection-screen skip 1.

************************************************************************
* block 0: audit object type definition
************************************************************************

SELECTION-SCREEN: BEGIN OF BLOCK auditobject
                  WITH FRAME TITLE text-001.

* auditplan
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       _auditpl RADIOBUTTON GROUP obj
                         MODIF ID 001
                         USER-COMMAND ent1.
SELECTION-SCREEN: COMMENT 4(35) text-002 FOR FIELD _auditpl
                                         MODIF ID 001.
* questionnaire header
SELECTION-SCREEN POSITION 41.
PARAMETERS:       _quest_h RADIOBUTTON GROUP obj
                  MODIF ID 002.
SELECTION-SCREEN: COMMENT 42(35) text-020 FOR FIELD _quest_h
                                         MODIF ID 002,

                  END OF LINE.

* audit
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       _audit RADIOBUTTON GROUP obj
                  DEFAULT 'X' MODIF ID 003.
SELECTION-SCREEN: COMMENT 4(35) text-003 FOR FIELD _audit
                                          MODIF ID 003.
* master question
SELECTION-SCREEN POSITION 41.
PARAMETERS:       _quest_i RADIOBUTTON GROUP obj
                  MODIF ID 004.
SELECTION-SCREEN: COMMENT 42(35) text-022 FOR FIELD _quest_i
                                         MODIF ID 004,
                  END OF LINE.

* corrective action
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       _action RADIOBUTTON GROUP obj
                  MODIF ID 005.
SELECTION-SCREEN: COMMENT 4(35) text-004 FOR FIELD _action
                                         MODIF ID 005.
* audit question with result
SELECTION-SCREEN POSITION 41.
PARAMETERS:       _aud_res RADIOBUTTON GROUP obj
                  MODIF ID 006.
SELECTION-SCREEN: COMMENT 42(35) text-021 FOR FIELD _aud_res
                                         MODIF ID 006,
                  END OF LINE.

*SELECTION-SCREEN SKIP 1.  "new line

************************************************************************
* 1) special search fields
************************************************************************

** external position number
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT (29) text-029 FOR FIELD _ext_pos
*                                         MODIF ID 101.
*SELECTION-SCREEN POSITION 30.
*SELECT-OPTIONS:   _ext_pos FOR gv_ext_position NO INTERVALS
*                                         MODIF ID 101.
*SELECTION-SCREEN: END OF LINE.

* external id
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (29) text-019 FOR FIELD _ext_id
                                         MODIF ID 102.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:       _ext_id FOR gv_external_id  NO INTERVALS
                                         MODIF ID 102.
SELECTION-SCREEN: END OF LINE.

** search field
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT (29) text-017 FOR FIELD _sfield
*                                         MODIF ID 103.
*SELECTION-SCREEN POSITION 30.
*SELECT-OPTIONS:       _sfield FOR gv_searchfield NO INTERVALS
*                                         MODIF ID 103.
*SELECTION-SCREEN: END OF LINE.

* grouping
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-018 FOR FIELD _group
                                         MODIF ID 104,
                    POSITION 30.
SELECT-OPTIONS:       _group FOR gv_grouping NO INTERVALS
                                         MODIF ID 104.
SELECTION-SCREEN: END OF LINE.

* District
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (8) text-042 FOR FIELD _distric
                                         MODIF ID 115.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:   _distric FOR gv_district NO INTERVALS
                                         MODIF ID 115.
SELECTION-SCREEN: END OF LINE.


SELECTION-SCREEN SKIP 1.  "new line

* Job ID
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (6) text-044 FOR FIELD _jobid
                                        MODIF ID 117.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:   _jobid FOR gv_jobid    NO INTERVALS
                                         MODIF ID 117.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (6) text-045 FOR FIELD _meter
                                        MODIF ID 118.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:   _meter FOR gv_meter    NO INTERVALS
                                         MODIF ID 118.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: END OF BLOCK auditobject.

************************************************************************
* 2) block: status                                                     *
************************************************************************

SELECTION-SCREEN: BEGIN OF BLOCK status
                  WITH FRAME TITLE text-005.

SELECTION-SCREEN: BEGIN OF LINE.

* in preparation
PARAMETERS:       _prep RADIOBUTTON GROUP stat
                                         MODIF ID 201.
SELECTION-SCREEN: COMMENT 5(15) text-023 FOR FIELD _prep
                                         MODIF ID 201.

* released
PARAMETERS:       _open RADIOBUTTON GROUP stat
                  DEFAULT 'X'            MODIF ID 202 .
SELECTION-SCREEN: COMMENT 23(15) text-007 FOR FIELD _open
                                         MODIF ID 202.

* closed/finished
PARAMETERS:       _closed RADIOBUTTON GROUP stat
                                         MODIF ID 203.
SELECTION-SCREEN: COMMENT 41(15) text-008 FOR FIELD _closed
                                         MODIF ID 203.

* all
PARAMETERS:       _sall RADIOBUTTON GROUP stat
                                         MODIF ID 204.
SELECTION-SCREEN: COMMENT 59(15) text-009 FOR FIELD _sall
                                         MODIF ID 204.

* partially signed
PARAMETERS:       _partsig RADIOBUTTON GROUP stat
                                         MODIF ID 205.
SELECTION-SCREEN: COMMENT 77(20) text-040 FOR FIELD _partsig
                                         MODIF ID 205,

                  END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.

PARAMETERS:       _awork RADIOBUTTON GROUP stus DEFAULT 'X' MODIF ID 211. "Auditor Work
SELECTION-SCREEN: COMMENT 5(20) text-a01 FOR FIELD _awork MODIF ID 211.
PARAMETERS:       _aldout RADIOBUTTON GROUP stus MODIF ID 212.  "Lead Auditor Outstanding
SELECTION-SCREEN: COMMENT 28(20) text-a02 FOR FIELD _aldout MODIF ID 212.
PARAMETERS:       _aldcomp RADIOBUTTON GROUP stus MODIF ID 213.  "Lead Auditor Complete
SELECTION-SCREEN: COMMENT 51(20) text-a03 FOR FIELD _aldcomp MODIF ID 213.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       _ancomp RADIOBUTTON GROUP stus MODIF ID 214.   "Not Complete
SELECTION-SCREEN: COMMENT 5(20) text-a04 FOR FIELD _ancomp MODIF ID 214.
PARAMETERS:       _acomp RADIOBUTTON GROUP stus MODIF ID 215.    "Complete
SELECTION-SCREEN: COMMENT 28(20) text-a05 FOR FIELD _acomp MODIF ID 215.
PARAMETERS:       _aall RADIOBUTTON GROUP stus MODIF ID 216.
SELECTION-SCREEN: COMMENT 51(20) text-a06 FOR FIELD _aall MODIF ID 216.

SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: END OF BLOCK status.

************************************************************************
* 3) block person                                                      *
************************************************************************

SELECTION-SCREEN: BEGIN OF BLOCK person
                  WITH FRAME TITLE text-013.

** me
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS:       _me RADIOBUTTON GROUP pers
*                                         MODIF ID 301
*                  USER-COMMAND ent1.
*SELECTION-SCREEN: COMMENT 5(10) text-024 FOR FIELD _me
*                                         MODIF ID 301.
** user
*SELECTION-SCREEN POSITION 16.
*PARAMETERS:       _name RADIOBUTTON GROUP pers
*                  DEFAULT 'X'            MODIF ID 302 .
*SELECTION-SCREEN: COMMENT 17(15) text-025 FOR FIELD _name
*                                         MODIF ID 302.
*
** partner
*SELECTION-SCREEN POSITION 52.
*PARAMETERS:       _bupa RADIOBUTTON GROUP pers
*                                         MODIF ID 303.
*SELECTION-SCREEN: COMMENT 53(15) text-026 FOR FIELD _bupa
*                                         MODIF ID 303.
*SELECTION-SCREEN: END OF LINE.
*
** User
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN POSITION 16.
*SELECTION-SCREEN: COMMENT (16) text-025 FOR FIELD name
*                                         MODIF ID 302.
*PARAMETERS:       name TYPE syuname      MATCHCODE OBJECT user_addr
*                                         MODIF ID 302.
** Partner
*SELECTION-SCREEN POSITION 52.
*SELECTION-SCREEN: COMMENT (15) text-026 FOR FIELD partner
*                                         MODIF ID 303.
*PARAMETERS:       partner TYPE bu_partner
*                                         MODIF ID 303.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN SKIP 1.  "new line
*
** Partner 2
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT (29) text-026 FOR FIELD partner2
*                                         MODIF ID 306.
*SELECTION-SCREEN POSITION 33.
*PARAMETERS:       partner2 TYPE bu_partner
*                                         MODIF ID 306.
*SELECTION-SCREEN: END OF LINE.

* changed by
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (30) text-014 FOR FIELD _chng_by
                                         MODIF ID 304.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:   _chng_by FOR gv_changed_by NO INTERVALS
                  MATCHCODE OBJECT user_addr
                                         MODIF ID 304.
SELECTION-SCREEN: END OF LINE.

* created by
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (30) text-016 FOR FIELD _crea_by
                                         MODIF ID 305,
                    POSITION 30.
SELECT-OPTIONS:     _crea_by FOR gv_created_by NO INTERVALS
                    MATCHCODE OBJECT user_addr
                                         MODIF ID 305.
SELECTION-SCREEN: END OF LINE.

* Planned Auditor
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (30) text-049 FOR FIELD _audplan
                                         MODIF ID 150.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:   _audplan FOR gv_plannedaudit NO INTERVALS
                                         MODIF ID 150.
SELECTION-SCREEN: END OF LINE.


* Employee/Foreman
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (30) text-047 FOR FIELD _sitecon
                                         MODIF ID 150.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:   _sitecon FOR gv_sitecont NO INTERVALS
                                         MODIF ID 150.
SELECTION-SCREEN: END OF LINE.

*additional site Contact
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (30) text-048 FOR FIELD _addsite
                                         MODIF ID 151.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:   _addsite FOR gv_add_sitecont NO INTERVALS
                                         MODIF ID 151.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: END OF BLOCK person.

************************************************************************
* 0b) simple or complex view
************************************************************************

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS _extview AS CHECKBOX USER-COMMAND ent1
                                         MODIF ID 00c.
SELECTION-SCREEN COMMENT 10(70) text-032 FOR FIELD _extview
                                         MODIF ID 00c.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.  "new line

**AUTH_GROUP
*SELECTION-SCREEN: BEGIN OF LINE,
*                    COMMENT (29) text-033 FOR FIELD _authgrp
*                                         MODIF ID 5p1,
*                    POSITION 30.
*SELECT-OPTIONS:     _authgrp FOR gv_auth_group NO INTERVALS
*                                         MODIF ID 5p1.
*SELECTION-SCREEN: END OF LINE.
*
**dummy line for spacing
*SELECTION-SCREEN COMMENT 1(1) text-038 MODIF ID 5pc.

************************************************************************
* 4) block: date                                                       *
************************************************************************

SELECTION-SCREEN: BEGIN OF BLOCK date
                  WITH FRAME TITLE text-006 .

* created
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-015 FOR FIELD _datecr
                                         MODIF ID 401,
                    POSITION 30.
SELECT-OPTIONS:     _datecr FOR gv_created_on NO-EXTENSION
                                         MODIF ID 401.
SELECTION-SCREEN: END OF LINE.

* changed on
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-012 FOR FIELD _datech
                                         MODIF ID 402,
                    POSITION 30.
SELECT-OPTIONS:       _datech FOR gv_changed_on NO-EXTENSION
                                         MODIF ID 402.
SELECTION-SCREEN: END OF LINE.

* planstart
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-010 FOR FIELD _datepl
                                         MODIF ID 403,
                    POSITION 30.
SELECT-OPTIONS:     _datepl FOR gv_planstart NO-EXTENSION
                                         MODIF ID 403.
SELECTION-SCREEN: END OF LINE.

* actualstart
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-011 FOR FIELD _dateac
                                         MODIF ID 404,
                    POSITION 30.
SELECT-OPTIONS:     _dateac FOR gv_actualstart NO-EXTENSION
                                         MODIF ID 404.
SELECTION-SCREEN: END OF LINE.

* planfinish
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-030 FOR FIELD _datplf
                                         MODIF ID 405,
                    POSITION 30.
SELECT-OPTIONS:     _datplf FOR gv_planfinish NO-EXTENSION
                                         MODIF ID 405.
SELECTION-SCREEN: END OF LINE.

* actualfinish
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-031 FOR FIELD _datacf
                                         MODIF ID 406,
                    POSITION 30.
SELECT-OPTIONS:     _datacf FOR gv_actualfinish NO-EXTENSION
                                         MODIF ID 406.
SELECTION-SCREEN: END OF LINE.

* valid from date
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-034 FOR FIELD _valdate
                                         MODIF ID 407,
                    POSITION 30.
SELECT-OPTIONS:     _valdate FOR gv_validfrom NO-EXTENSION
                                         MODIF ID 407.
SELECTION-SCREEN: END OF LINE.

* valid to date
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-035 FOR FIELD _todate
                                         MODIF ID 408,
                    POSITION 30.
SELECT-OPTIONS:     _todate FOR gv_validto NO-EXTENSION
                                         MODIF ID 408.
SELECTION-SCREEN: END OF LINE.

* final date
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) text-036 FOR FIELD _findate
                                         MODIF ID 409,
                    POSITION 30.
SELECT-OPTIONS:     _findate FOR gv_finaldate NO-EXTENSION
                                         MODIF ID 409.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: END OF BLOCK date.

***********************************************************************
* 5) special report fields
***********************************************************************

SELECTION-SCREEN SKIP 1.  "new line

** question has remark
*PARAMETERS _q_fact LIKE plmt_quest_res_ui-txt_has_fact AS CHECKBOX
*                                                  MODIF ID 5c1.

* corrective action required
PARAMETERS _qcorreq LIKE plmt_quest_res_ui-corr_required AS CHECKBOX
                                                  MODIF ID 5c1.

** question has positive remark
*PARAMETERS _q_posi LIKE plmt_quest_res_ui-txt_has_posi AS CHECKBOX
*                                                  MODIF ID 5c1.
*
** question has negative remark
*PARAMETERS _q_nega LIKE plmt_quest_res_ui-txt_has_nega AS CHECKBOX
*                                                  MODIF ID 5c1.

* unplanned question in audit
PARAMETERS _qunplnd LIKE plmt_quest_res_ui-unplanned_item AS CHECKBOX
                                                  MODIF ID 5c1.

* (not yet implemented in SAP standard:)
*SCORE_RESULT
*SCORE_RECORDED
*SCORE_RES
*QUEST_REF_GUID

**********************************************************************
* 6) audited object (value) (AUDITED_OBJ_COMPLEX)
**********************************************************************

*AUDIT_TYPE
PARAMETERS _a_typ   LIKE plmt_audit_ui-audit_type
            VISIBLE LENGTH 40 AS LISTBOX USER-COMMAND ent1 MODIF ID 5p2.

* audited object: OBJECT_TYPE
*PARAMETERS _AOBTYP LIKE PLMT_AUDIT_OBJECT_UI-OBJECT_TYPE
*                                                  MODIF ID 5P2.
** audited object (value)
*SELECTION-SCREEN: BEGIN OF LINE,
*                    COMMENT (29) TEXT-027 FOR FIELD _AOBVAL1
*                                         MODIF ID 5P2,
*                    POSITION 30.
*SELECT-OPTIONS:     _AOBVAL1 FOR GV_OBJECT_VALUE NO INTERVALS
*                                         MODIF ID 5P2.

SELECTION-SCREEN: BEGIN OF BLOCK audited_object
                  WITH FRAME TITLE text-027.

* field 1
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_1
                    FOR FIELD _objva_1   MODIF ID v01,
                    POSITION 30.
SELECT-OPTIONS:     _objva_1 FOR gv_object_value NO INTERVALS
                                         MODIF ID v01.
SELECTION-SCREEN: END OF LINE.

* field 2
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_2
                    FOR FIELD _objva_2   MODIF ID v02,
                    POSITION 30.
SELECT-OPTIONS:     _objva_2 FOR gv_object_value NO INTERVALS
                                         MODIF ID v02.
SELECTION-SCREEN: END OF LINE.

* field 3
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_3
                    FOR FIELD _objva_3   MODIF ID v03,
                    POSITION 30.
SELECT-OPTIONS:     _objva_3 FOR gv_object_value NO INTERVALS
                                         MODIF ID v03.
SELECTION-SCREEN: END OF LINE.

* field 4
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_4
                    FOR FIELD _objva_4   MODIF ID v04,
                    POSITION 30.
SELECT-OPTIONS:     _objva_4 FOR gv_object_value NO INTERVALS
                                         MODIF ID v04.
SELECTION-SCREEN: END OF LINE.

* field 5
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_5
                    FOR FIELD _objva_5   MODIF ID v05,
                    POSITION 30.
SELECT-OPTIONS:     _objva_5 FOR gv_object_value NO INTERVALS
                                         MODIF ID v05.
SELECTION-SCREEN: END OF LINE.

* field 6
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_6
                    FOR FIELD _objva_6   MODIF ID v06,
                    POSITION 30.
SELECT-OPTIONS:     _objva_6 FOR gv_object_value NO INTERVALS
                                         MODIF ID v06.
SELECTION-SCREEN: END OF LINE.

* field 7
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_7
                    FOR FIELD _objva_7   MODIF ID v07,
                    POSITION 30.
SELECT-OPTIONS:     _objva_7 FOR gv_object_value NO INTERVALS
                                         MODIF ID v07.
SELECTION-SCREEN: END OF LINE.

* field 8
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_8
                    FOR FIELD _objva_8   MODIF ID v08,
                    POSITION 30.
SELECT-OPTIONS:     _objva_8 FOR gv_object_value NO INTERVALS
                                         MODIF ID v08.
SELECTION-SCREEN: END OF LINE.

* field 9
SELECTION-SCREEN: BEGIN OF LINE,
                    COMMENT (29) _gtxt_9
                    FOR FIELD _objva_9   MODIF ID v09,
                    POSITION 30.
SELECT-OPTIONS:     _objva_9 FOR gv_object_value NO INTERVALS
                                         MODIF ID v09.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: END OF BLOCK audited_object.


SELECTION-SCREEN SKIP 1.  "new line

* unplanned audit
PARAMETERS _aunplnd LIKE plmt_audit_ui-unplanned AS CHECKBOX
                                                  MODIF ID 5p2.

** next audit obligatory
*PARAMETERS _audnext LIKE plmt_audit_ui-next_audit_oblig AS CHECKBOX
*                                                  MODIF ID 5p2.

***********************************************************************
* question statistics
***********************************************************************

SELECTION-SCREEN SKIP 1.  "new line

SELECTION-SCREEN SKIP 1.  "new line

SELECTION-SCREEN COMMENT 1(79) text-101           MODIF ID 5r1.
PARAMETERS _questh                                MODIF ID 5r1
                LIKE plmt_quest_h_ui-external_id.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*

  PERFORM audit_object_determine CHANGING gv_aud_objecttype .

  PERFORM audited_object_initialize
                            TABLES   _objva_1
                                     _objva_2
                                     _objva_3
                                     _objva_4
                                     _objva_5
                                     _objva_6
                                     _objva_7
                                     _objva_8
                                     _objva_9
                            USING    gv_aud_objecttype
                                     _a_typ
                            CHANGING _gtxt_1
                                     _gtxt_2
                                     _gtxt_3
                                     _gtxt_4
                                     _gtxt_5
                                     _gtxt_6
                                     _gtxt_7
                                     _gtxt_8
                                     _gtxt_9
                                    gv_audit_type
                                    gv_object_fields_count.

  PERFORM initialize_and_loop_at_screen
    USING
      gv_aud_objecttype
      _extview
      gv_object_fields_count.

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
*-- default values

*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
* check, if accessibility mode is on
  CALL FUNCTION 'GET_ACCESSIBILITY_MODE'
    IMPORTING
      accessibility     = lv_acc_mode
    EXCEPTIONS
      its_not_available = 1.

  IF lv_acc_mode = abap_true.
    IF _extview IS NOT INITIAL.
      MESSAGE s099(plm_audit).
    ENDIF.
  ENDIF.

  PERFORM audit_object_determine CHANGING gv_aud_objecttype .

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON RADIOBUTTON GROUP obj.
*----------------------------------------------------------------------*

  PERFORM audit_object_determine CHANGING gv_aud_objecttype .

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _ext_id-low.
*----------------------------------------------------------------------*

  PERFORM audit_object_determine CHANGING gv_aud_objecttype.
  gv_dynprofield = '_EXT_ID-LOW'.
  PERFORM on_f4_for_external_id USING gv_aud_objecttype
                                      gv_dynprofield.

*----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR _sfield-low.
**----------------------------------------------------------------------*
*
*  PERFORM audit_object_determine CHANGING gv_aud_objecttype.
*  gv_dynprofield = '_SFIELD-LOW'.
*  PERFORM on_f4_for_search_field USING gv_aud_objecttype
*                                       gv_dynprofield.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _group-low.
*----------------------------------------------------------------------*

  PERFORM audit_object_determine CHANGING gv_aud_objecttype.
  gv_dynprofield = '_GROUP-LOW'.
  PERFORM on_f4_for_grouping USING gv_aud_objecttype
                                   gv_dynprofield.

**----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR _audplan-low.
**----------------------------------------------------------------------*
*
*  PERFORM audit_object_determine CHANGING gv_aud_objecttype.
*  gv_dynprofield = '_AUDPLAN-LOW'.
*  PERFORM on_f4_for_planned_auditor USING gv_aud_objecttype
*                                   gv_dynprofield.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_1-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            1
                                   CHANGING _objva_1.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_2-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            2
                                   CHANGING _objva_2.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_3-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            3
                                   CHANGING _objva_3.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_4-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            4
                                   CHANGING _objva_4.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_5-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            5
                                   CHANGING _objva_5.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_6-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            6
                                   CHANGING _objva_6.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_7-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            7
                                   CHANGING _objva_7.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_8-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            8
                                   CHANGING _objva_8.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR _objva_9-low.
*----------------------------------------------------------------------*

  PERFORM on_f4_for_audited_object USING    gv_audit_type
                                            9
                                   CHANGING _objva_9.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR _prep.
*----------------------------------------------------------------------*

  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
    EXPORTING
      doklangu         = sy-langu
      called_for_tab   = 'PLMM_AUDIT'
      called_for_field = 'PROC_STATUS'
    EXCEPTIONS
      object_not_found = 0
      OTHERS           = 0.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR _open.
*----------------------------------------------------------------------*

  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
    EXPORTING
      doklangu         = sy-langu
      called_for_tab   = 'PLMM_AUDIT'
      called_for_field = 'PROC_STATUS'
    EXCEPTIONS
      object_not_found = 0
      OTHERS           = 0.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR _closed.
*----------------------------------------------------------------------*

  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
    EXPORTING
      doklangu         = sy-langu
      called_for_tab   = 'PLMM_AUDIT'
      called_for_field = 'PROC_STATUS'
    EXCEPTIONS
      object_not_found = 0
      OTHERS           = 0.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR _sall.
*----------------------------------------------------------------------*

  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
    EXPORTING
      doklangu         = sy-langu
      called_for_tab   = 'PLMM_AUDIT'
      called_for_field = 'PROC_STATUS'
    EXCEPTIONS
      object_not_found = 0
      OTHERS           = 0.

**----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON HELP-REQUEST FOR _me.
**----------------------------------------------------------------------*
*  CLEAR gs_tline.
*  REFRESH gt_helplines.
*
*  MOVE 'U1'    TO gs_tline-tdformat.
*  MOVE '&USE&' TO gs_tline-tdline.
*  APPEND gs_tline to gt_helplines.
*  MOVE 'AS'    TO gs_tline-tdformat.
*  MOVE text-_me TO gs_tline-tdline.
*  APPEND gs_tline TO gt_helplines.
*
*  MOVE sy-repid  TO gs_help_infos-program.
*  MOVE sy-dynnr  TO gs_help_infos-dynpro.
*  MOVE sy-repid  TO gs_help_infos-report.
*  move sy-repid  to gs_help_infos-DYNPPROG.
*  MOVE '_ME' TO gs_help_infos-dynprofld.
*  CALL FUNCTION 'HELP_DOCULINES_SHOW'
*    EXPORTING
*      help_infos = gs_help_infos
*    TABLES
*      excludefun = gt_excludefun
*      helplines  = gt_helplines.
**----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON HELP-REQUEST FOR _name.
**----------------------------------------------------------------------*
*  CLEAR gs_tline.
*  REFRESH gt_helplines.
*
*  MOVE 'U1'    TO gs_tline-tdformat.
*  MOVE '&USE&' TO gs_tline-tdline.
*  APPEND gs_tline to gt_helplines.
*  MOVE 'AS'    TO gs_tline-tdformat.
*  MOVE text-_na TO gs_tline-tdline.
*  APPEND gs_tline TO gt_helplines.
*
*  MOVE sy-repid  TO gs_help_infos-program.
*  MOVE sy-dynnr  TO gs_help_infos-dynpro.
*  MOVE sy-repid  TO gs_help_infos-report.
*  move sy-repid  to gs_help_infos-DYNPPROG.
*  MOVE '_NAME' TO gs_help_infos-dynprofld.
*  CALL FUNCTION 'HELP_DOCULINES_SHOW'
*    EXPORTING
*      help_infos = gs_help_infos
*    TABLES
*      excludefun = gt_excludefun
*      helplines  = gt_helplines.

**----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON HELP-REQUEST FOR _bupa.
**----------------------------------------------------------------------*
*  CLEAR gs_tline.
*  REFRESH gt_helplines.
*
*  MOVE 'U1'    TO gs_tline-tdformat.
*  MOVE '&USE&' TO gs_tline-tdline.
*  APPEND gs_tline to gt_helplines.
*  MOVE 'AS'    TO gs_tline-tdformat.
*  MOVE text-_bp TO gs_tline-tdline.
*  APPEND gs_tline TO gt_helplines.
*
*  MOVE sy-repid  TO gs_help_infos-program.
*  MOVE sy-dynnr  TO gs_help_infos-dynpro.
*  MOVE sy-repid  TO gs_help_infos-report.
*  move sy-repid  to gs_help_infos-DYNPPROG.
*  MOVE '_BUPA' TO gs_help_infos-dynprofld.
*  CALL FUNCTION 'HELP_DOCULINES_SHOW'
*    EXPORTING
*      help_infos = gs_help_infos
*    TABLES
*      excludefun = gt_excludefun
*      helplines  = gt_helplines.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

*  IF NOT partner2 IS INITIAL.
*    MOVE partner2 TO partner.
*  ENDIF.

* buffer when special status for audits was selected
  IF _partsig IS NOT INITIAL.
    gv_status_partsig = _partsig.
  ENDIF.

* append selection criteria from screen
  PERFORM ranges_fill_from_screen USING    gv_aud_objecttype
                                           _extview
                               CHANGING gt_searchfield_range
                                        gt_grouping_range
                                        gt_external_id_range
                                        gt_auth_group_range
                                        gt_created_on_range
                                        gt_changed_on_range
                                        gt_created_by_range
                                        gt_changed_by_range
                                        gt_actualstart_range
                                        gt_planstart_range
                                        gt_actualfinish_range
                                        gt_planfinish_range
                                        gt_proc_status_range
                                        gt_ext_position_range
                                        gt_partner_range
                                        gv_name
                                        gv_audit_type
                                        gt_audit_type_range
                                        gt_object_type_range
                                        gt_object_value_range
                                        gt_audited_obj_complex
                                        gt_unplanned_item_range
                                        gt_corr_required_range
                                        gt_txt_has_fact_range
                                        gt_txt_has_posi_range
                                        gt_txt_has_nega_range
                                        gt_score_result_range
                                        gt_score_recorded_range
                                        gt_unplanned_range
                                        gt_score_res_range
                                        gt_next_audit_oblig_range
                                        gt_quest_ref_guid_range
                                        gt_jobid_range          "ACR264
                                        gt_district_range       "ACR264
                                        gt_add_sitecont_range   "ACR264
                                        gt_meter_range          "ACR264
                                        gt_plannedaudit         "ACR264
                                        gt_sitecont_range.      "ACR264
  PERFORM get_objecttext USING gv_aud_objecttype
                               gv_objecttext.


  IF NOT _extview IS INITIAL            AND  "extended view
     NOT _questh  IS INITIAL            AND  "questionnaire given
    gv_aud_objecttype = co_obtyp-audit.      "audit is marked

***********************************************************************
*   special report: audit question statistics
***********************************************************************

    PERFORM get_audit_question_statistics USING _questh.

  ELSE.  " gv_aud_objecttype = co_obtyp-audit and

***********************************************************************
*   load and display data as list
***********************************************************************

    CALL FUNCTION 'ZPLM_AUDIT_MONITOR_PROCESS'
      EXPORTING
        iv_aud_objecttype      = gv_aud_objecttype
        iv_objecttext          = gv_objecttext
* person
        iv_name                = gv_name
        "iv_partner             = partner
        it_partner_range       = gt_partner_guid_range
        it_created_by_range    = gt_created_by_range
        it_changed_by_range    = gt_changed_by_range
* main search criteria
        it_external_id_range   = gt_external_id_range
        it_searchfield_range   = gt_searchfield_range
        it_grouping_range      = gt_grouping_range
        it_auth_group_range    = gt_auth_group_range
* date
        it_planstart_range     = gt_planstart_range
        it_actualstart_range   = gt_actualstart_range
        it_planfinish_range    = gt_planfinish_range
        it_actualfinish_range  = gt_actualfinish_range
        it_created_on_range    = gt_created_on_range
        it_changed_on_range    = gt_changed_on_range
* status
        it_proc_status_range   = gt_proc_status_range
* questionnaire external position
        it_ext_position_range  = gt_ext_position_range
* audit specific search
*       IV_AUDIT_TYPE             = GV_AUDIT_TYPE
*       IV_OBJECT_TYPE            = GV_OBJECT_TYPE
*       IV_OBJECT_VALUE           = GV_OBJECT_VALUE
        it_audit_type_range       = gt_audit_type_range
        it_object_type_range      = gt_object_type_range
        it_object_value_range     = gt_object_value_range
        it_audited_obj_complex    = gt_audited_obj_complex
        it_unplanned_range        = gt_unplanned_range
        it_score_res_range        = gt_score_res_range
        it_next_audit_oblig_range = gt_next_audit_oblig_range
* audit question specific fields
        it_unplanned_item         = gt_unplanned_item_range
        it_corr_required          = gt_corr_required_range
        it_txt_has_fact           = gt_txt_has_fact_range
        it_txt_has_posi           = gt_txt_has_posi_range
        it_txt_has_nega           = gt_txt_has_nega_range
        it_score_result           = gt_score_result_range
        it_score_recorded         = gt_score_recorded_range
        it_quest_ref_guid         = gt_quest_ref_guid_range
        it_job_id                 = gt_jobid_range
        it_site_cont              = gt_add_sitecont_range
        it_add_site               = gt_sitecont_range
        it_district               = gt_district_range
        it_meter_range            = gt_meter_range
        it_plannedaudit           = gt_plannedaudit
        IV_AUDITWORK              = _awork
        IV_LEADPROGRESS           = _aldout
        IV_LEADCOMPLETE           = _aldcomp
        IV_NOTCOMPLETE            = _ancomp
        IV_COMPLETE               = _acomp
      EXCEPTIONS
        no_success                = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.  " gv_aud_objecttype = co_obtyp-audit and

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*





************************************************************************
* report-specific forms                                                *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  AUDIT_OBJECT_DETERMINE
*&---------------------------------------------------------------------*
*       determine audit object by radiobuttons
*----------------------------------------------------------------------*
FORM audit_object_determine CHANGING
                              p_objecttype   TYPE cgpl_object_type.

  DATA lv_objtyp_old TYPE cgpl_object_type.
  DATA lv_objtyp_new TYPE cgpl_object_type.
  DATA lv_acc_mode   TYPE abap_bool.

  lv_objtyp_old = p_objecttype.

  IF     NOT _audit   IS INITIAL.
    p_objecttype = co_obtyp-audit.
  ELSEIF NOT _auditpl IS INITIAL.
    p_objecttype = co_obtyp-plan.
  ELSEIF NOT _action  IS INITIAL.
    p_objecttype = co_obtyp-corr_act.
  ELSEIF NOT _quest_h IS INITIAL.
    p_objecttype = co_obtyp-quest_h.
  ELSEIF NOT _quest_i IS INITIAL.
    p_objecttype = co_obtyp-quest_i.
  ELSEIF NOT _aud_res IS INITIAL.
    p_objecttype = co_obtyp-auditquest.
  ELSE.
    MESSAGE a002(00).
  ENDIF.

  lv_objtyp_new = p_objecttype.

  CALL FUNCTION 'GET_ACCESSIBILITY_MODE'
    IMPORTING
      accessibility     = lv_acc_mode
    EXCEPTIONS
      its_not_available = 1.

  IF lv_acc_mode = abap_true.
    IF lv_objtyp_old IS NOT INITIAL.
      IF NOT lv_objtyp_old = lv_objtyp_new.
        MESSAGE s099(plm_audit).
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " AUDIT_OBJECT_DETERMINE

*&---------------------------------------------------------------------*
*&      Form  RANGES_FILL_FROM_SCREEN
*&---------------------------------------------------------------------*
*       fill ranges as selection criteria from user input
*----------------------------------------------------------------------*
FORM ranges_fill_from_screen
       USING
         iv_aud_objecttype         TYPE cgpl_object_type
         iv_complex_mode           TYPE char1
       CHANGING
         et_searchfield_range      TYPE plmt_audit_ranges_searchfield
         et_grouping_range         TYPE plmt_audit_ranges_grouping
         et_external_id_range      TYPE plmt_audit_ranges_external_id
         et_auth_group_range       TYPE plmt_audit_ranges_auth_group
         et_created_on_range       TYPE plmt_audit_ranges_for_date
         et_changed_on_range       TYPE plmt_audit_ranges_for_date
         et_created_by_range       TYPE plmt_ranges_for_user
         et_changed_by_range       TYPE plmt_ranges_for_user
         et_actualstart_range      TYPE plmt_audit_ranges_for_date
         et_planstart_range        TYPE plmt_audit_ranges_for_date
         et_actualfinish_range     TYPE plmt_audit_ranges_for_date
         et_planfinish_range       TYPE plmt_audit_ranges_for_date
         et_proc_status_range      TYPE plmt_audit_ranges_proc_status
         et_ext_position_range     TYPE plmt_ranges_for_ext_position
         et_partner_range          TYPE plmt_ranges_for_partner
         cv_name                   TYPE sy-uname
         ev_audit_type             TYPE plmt_audit_type
         et_audit_type_range       TYPE plmt_audit_ranges_audit_type
         et_object_type_range      TYPE plmt_audit_ranges_audit_type
         et_object_value_range     TYPE plmt_ranges_for_audited_object
         et_audited_obj_complex    TYPE plmt_audited_object_search_tab
         et_unplanned_item_range   TYPE plmt_audit_ranges_flag
         et_corr_required_range    TYPE plmt_audit_ranges_flag
         et_txt_has_fact_range     TYPE plmt_audit_ranges_flag
         et_txt_has_posi_range     TYPE plmt_audit_ranges_flag
         et_txt_has_nega_range     TYPE plmt_audit_ranges_flag
         et_score_result_range     TYPE plmt_audit_ranges_dec10
         et_score_recorded_range   TYPE plmt_audit_ranges_dec5
         et_unplanned_range        TYPE plmt_audit_ranges_flag
         et_score_res_range        TYPE plmt_audit_ranges_dec10
         et_next_audit_oblig_range TYPE plmt_audit_ranges_flag
         et_quest_ref_guid_range   TYPE plmt_audit_ranges_guid
         et_jobid_range            TYPE z_jobid_range         "ACR264
         et_district_range         TYPE z_district_range      "ACR264
         et_add_sitecont_range     TYPE z_site_cont_range     "ACR264
         et_meter_range            TYPE z_meter_range         "ACR264
         et_plannedaudit           TYPE z_auditor_p_range     "ACR264
         et_sitecont_range         TYPE z_site_cont_range.    "ACR264

  DATA:
    ls_flag_range             TYPE plmt_audit_char1_ranges_wa.

  CLEAR: et_planstart_range,
         et_actualstart_range,
         et_planfinish_range,
         et_actualfinish_range,
         et_created_on_range,
         et_changed_on_range,
         et_created_by_range,
         et_changed_by_range,
         et_searchfield_range,
         et_grouping_range,
         et_external_id_range,
         et_auth_group_range,
         et_proc_status_range,
         et_ext_position_range,
         et_partner_range,
*        cv_name,
         ev_audit_type,
         et_audit_type_range,
         et_object_type_range,
         et_object_value_range,
         et_audited_obj_complex,
         et_unplanned_item_range,
         et_corr_required_range,
         et_txt_has_fact_range,
         et_txt_has_posi_range,
         et_txt_has_nega_range,
         et_score_result_range,
         et_score_recorded_range,
         et_unplanned_range,
         et_score_res_range,
         et_next_audit_oblig_range,
         et_quest_ref_guid_range,
         et_jobid_range,              "ACR264
         et_district_range,           "ACR264
         et_add_sitecont_range,       "ACR264
         et_meter_range,              "ACR264
         et_plannedaudit,             "ACR264
         et_sitecont_range.           "ACR264


  IF NOT iv_complex_mode IS INITIAL.
* Detailed search is active

*   general CGPL fields (date)
    et_created_on_range   = _datecr[].
    et_changed_on_range   = _datech[].

    CASE iv_aud_objecttype.
      WHEN co_obtyp-audit.
        et_planstart_range    = _datepl[].
        et_actualstart_range  = _dateac[].
        et_planfinish_range   = _datplf[].
        et_actualfinish_range = _datacf[].
      WHEN co_obtyp-plan
        OR co_obtyp-quest_h.
        et_planstart_range    = _valdate[].
        et_planfinish_range   = _todate[].
      WHEN co_obtyp-quest_i
        OR co_obtyp-auditquest.
      WHEN co_obtyp-corr_act.
        et_planstart_range    = _datepl[].
        et_actualstart_range  = _dateac[].
        et_actualfinish_range = _datacf[].
        et_planfinish_range   = _findate[].
    ENDCASE.

  ENDIF.  " NOT IV_COMPLEX_MODE IS INITIAL.

* general CGPL fields (person)
  et_created_by_range   = _crea_by[].
  et_changed_by_range   = _chng_by[].

* general search fields
  "et_searchfield_range[]  = _sfield[] .
  et_grouping_range[]     = _group[].
  et_external_id_range[]  = _ext_id[].
  "et_ext_position_range[] = _ext_pos[].

* Audit custom fields                            "ACR264
  IF iv_aud_objecttype = co_obtyp-audit.
    et_jobid_range[]        = _jobid[].
    et_district_range[]     = _distric[].
    et_add_sitecont_range[] = _addsite[].
    et_meter_range[]        = _meter[].
    et_plannedaudit[]       = _audplan[].
    et_sitecont_range[]     = _sitecon[].
  ENDIF.

** authority group
*  IF NOT iv_complex_mode IS INITIAL.
**   Detailed search is active
*    et_auth_group_range[]   = _authgrp[].
*  ENDIF.

***********************************************************************
* flags for special reports
***********************************************************************

  IF NOT iv_complex_mode IS INITIAL.
*   Detailed search is active

* unplanned question in audit
    IF NOT _qunplnd IS INITIAL.
      CLEAR ls_flag_range.
      ls_flag_range-sign   = 'I'.
      ls_flag_range-option = 'EQ'.
      ls_flag_range-low    = _qunplnd.
      APPEND ls_flag_range TO et_unplanned_item_range.
    ENDIF.

* corrective action required
    IF NOT _qcorreq IS INITIAL.
      CLEAR ls_flag_range.
      ls_flag_range-sign   = 'I'.
      ls_flag_range-option = 'EQ'.
      ls_flag_range-low    = _qcorreq.
      APPEND ls_flag_range TO et_corr_required_range.
    ENDIF.

** question has remark
*    IF NOT _q_fact IS INITIAL.
*      CLEAR ls_flag_range.
*      ls_flag_range-sign   = 'I'.
*      ls_flag_range-option = 'EQ'.
*      ls_flag_range-low    = _q_fact.
*      APPEND ls_flag_range TO et_txt_has_fact_range.
*    ENDIF.

** question has positive remark
*    IF NOT _q_posi IS INITIAL.
*      CLEAR ls_flag_range.
*      ls_flag_range-sign   = 'I'.
*      ls_flag_range-option = 'EQ'.
*      ls_flag_range-low    = _q_posi.
*      APPEND ls_flag_range TO et_txt_has_posi_range.
*    ENDIF.
*
** question has negative remark
*    IF NOT _q_nega IS INITIAL.
*      CLEAR ls_flag_range.
*      ls_flag_range-sign   = 'I'.
*      ls_flag_range-option = 'EQ'.
*      ls_flag_range-low    = _q_nega.
*      APPEND ls_flag_range TO et_txt_has_nega_range.
*    ENDIF.

* unplanned audit
    IF NOT _aunplnd IS INITIAL.
      CLEAR ls_flag_range.
      ls_flag_range-sign   = 'I'.
      ls_flag_range-option = 'EQ'.
      ls_flag_range-low    = _aunplnd.
      APPEND ls_flag_range TO et_unplanned_range.
    ENDIF.

** next audit obligatory
*    IF NOT _audnext IS INITIAL.
*      CLEAR ls_flag_range.
*      ls_flag_range-sign   = 'I'.
*      ls_flag_range-option = 'EQ'.
*      ls_flag_range-low    = _audnext.
*      APPEND ls_flag_range TO et_next_audit_oblig_range.
*    ENDIF.

  ENDIF."NOT IV_COMPLEX_MODE IS INITIAL

** Name
*  IF     NOT _me IS INITIAL.
*    cv_name = sy-uname.
*  ELSEIF NOT _bupa IS INITIAL.
*    CLEAR cv_name.
*  ELSEIF NOT _name IS INITIAL.
*    cv_name = name.
*  ENDIF.

* status flag
  IF _sall IS INITIAL AND iv_aud_objecttype NE co_obtyp-audit.
    CALL METHOD cl_plm_audit_status_management=>proc_status_range_fill
      EXPORTING
        iv_select_only_in_preparation = _prep
        iv_select_only_usable_objects = _open
        iv_select_only_completed      = _closed
        iv_select_only_partial_signed = _partsig
      IMPORTING
        et_proc_status_range          = et_proc_status_range.
  ELSE.
    CLEAR: et_proc_status_range.
  ENDIF.

**********************************************************************
* audited object (OBJECT_VALUE)
**********************************************************************

  DATA:
    ls_audit_obj            TYPE plmv_audit_obj,
    lt_audit_obj            TYPE plmt_audit_obj_tab,
    ls_complex_range_wa     TYPE plmt_audited_object_search_wa.

  IF NOT iv_complex_mode IS INITIAL.
*   Detailed search is active

*   get selection screen input value for audit type
    ev_audit_type = _a_typ.

*   ET_AUDITED_OBJ_COMPLEX
    IF iv_aud_objecttype = co_obtyp-audit.
      IF NOT ev_audit_type IS INITIAL.
*       try to get information about possible audited objects

*       read audited object customizing
        CALL FUNCTION 'PLM_AUD_AUDIT_OBJ_TYPE_READ_M'
          EXPORTING
            i_audit_type      = ev_audit_type
*           I_LANGU           = SY-LANGU
          IMPORTING
            et_plmt_audit_obj = lt_audit_obj
          EXCEPTIONS
            no_audit_type     = 1
            no_object_type    = 2
            no_text           = 3.
        IF sy-subrc = 0 OR
           sy-subrc = 3.
          ls_complex_range_wa-audit_type = ev_audit_type.
*       field 1
          IF NOT _objva_1[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 1 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_1[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 2
          IF NOT _objva_2[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 2 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_2[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 3
          IF NOT _objva_3[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 3 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_3[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 4
          IF NOT _objva_4[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 4 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_4[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 5
          IF NOT _objva_5[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 5 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_5[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 6
          IF NOT _objva_6[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 6 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_6[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 7
          IF NOT _objva_7[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 7 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_7[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 8
          IF NOT _objva_8[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 8 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_8[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
*       field 9
          IF NOT _objva_9[] IS INITIAL.
            READ TABLE lt_audit_obj INDEX 9 INTO ls_audit_obj.
            IF sy-subrc IS INITIAL.
              ls_complex_range_wa-object_type = ls_audit_obj-object_type.
              ls_complex_range_wa-value_range = _objva_9[].
              APPEND ls_complex_range_wa TO et_audited_obj_complex.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.
    ENDIF.

*   simple case: no complex table for audited object
    IF et_audited_obj_complex[] IS INITIAL.

*     AUDIT_TYPE
      DATA ls_audit_type_range TYPE plmt_audit_type_ranges_wa.
      IF _a_typ IS INITIAL.
        CLEAR: ev_audit_type,
               et_audit_type_range.
      ELSE.
        ev_audit_type = _a_typ.
        ls_audit_type_range-sign    = 'I'.
        ls_audit_type_range-option  = 'EQ'.
        ls_audit_type_range-low     = _a_typ.
        APPEND ls_audit_type_range TO et_audit_type_range.
      ENDIF.

** OBJECT_TYPE
*  DATA LS_OBJECT_TYPE_RANGE TYPE PLMT_AUDIT_TYPE_RANGES_WA.
*  IF _AOBTYP IS INITIAL.
*    CLEAR: ET_OBJECT_TYPE_RANGE.
*  ELSE.
*    LS_OBJECT_TYPE_RANGE-SIGN    = 'I'.
*    LS_OBJECT_TYPE_RANGE-OPTION  = 'EQ'.
*    LS_OBJECT_TYPE_RANGE-LOW     = _AOBTYP.
*    APPEND LS_OBJECT_TYPE_RANGE TO ET_OBJECT_TYPE_RANGE.
*  ENDIF.

**   OBJECT_VALUE
*    ET_OBJECT_VALUE_RANGE[] = _AOBVAL1[].

    ENDIF.  " ET_AUDITED_OBJ_COMPLEX[] IS INITIAL
  ENDIF."not IV_COMPLEX_MODE is initial

ENDFORM.                    " RANGES_FILL_FROM_SCREEN

*----------------------------------------------------------------------*
*       Form  GET_OBJECTTEXT                                           *
*----------------------------------------------------------------------*
*       Get text for object type                                       *
*----------------------------------------------------------------------*
FORM get_objecttext USING iv_objecttype TYPE cgpl_object_type
                          ev_objecttext TYPE cgpl_object_text.

  CLEAR: ev_objecttext.

  CASE iv_objecttype.
    WHEN co_obtyp-quest_h.
      ev_objecttext = text-020.
    WHEN co_obtyp-quest_i.
      ev_objecttext = text-022.
    WHEN co_obtyp-plan.
      ev_objecttext = text-002.
    WHEN co_obtyp-audit.
      ev_objecttext = text-003.
    WHEN co_obtyp-auditquest.
      ev_objecttext = text-021.
    WHEN co_obtyp-corr_act.
      ev_objecttext = text-004.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

ENDFORM.                    " GET_OBJECTTEXT

*&---------------------------------------------------------------------*
*&      Form  initialize_and_loop_at_screen
*&---------------------------------------------------------------------*
*       PBO : initialize variables and perform field modification
*----------------------------------------------------------------------*
FORM initialize_and_loop_at_screen
   USING
     iv_objecttype           TYPE cgpl_object_type
     iv_complex_mode         TYPE char1
     iv_obj_fields_count     TYPE i. "no of audited object input fields

  DATA lv_dig_sig_on TYPE xfeld.

  CLASS cl_plm_roles_assignment DEFINITION LOAD.

  CALL FUNCTION 'PLM_AUD_DS_SOFTSWITCH'
    IMPORTING
      ev_settings_available = lv_dig_sig_on.

** radiobutton me -> USERNAME
*  IF NOT _me IS INITIAL.
*    cv_name = sy-uname.
*  ENDIF.

  LOOP AT SCREEN.

    CASE screen-group1(1).
      WHEN '0'.
* control elements
        IF screen-group1 = '00C'.
          screen-intensified = '1'.
        ENDIF.
      WHEN '1'.
        IF screen-group1 = '101'.
*         external position is only valid in questionnaires
          IF iv_objecttype NE co_obtyp-quest_h AND
             iv_objecttype NE co_obtyp-quest_i AND
             iv_objecttype NE co_obtyp-auditquest .
            screen-input     = '0'.
            screen-output    = '0'.
            screen-invisible = '1'.
            IF screen-name = '%F029023_1000'.
              screen-invisible = '0'.
            ENDIF.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
        CASE screen-group1+1(1). "ACR264
          WHEN '1' OR '5'.
            "11X is in the main area
            "15X is in the contact
            IF iv_objecttype NE co_obtyp-audit OR iv_complex_mode IS NOT INITIAL. "Complex mode is disabled since the function cannot take custom fields
              screen-input     = '0'.
              screen-output    = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN.
            ENDIF.
        ENDCASE.
      WHEN '2'.
********************************************************************
* status block
********************************************************************
        IF screen-name = '_PARTSIG'.
          IF lv_dig_sig_on IS INITIAL.
            screen-invisible = '1'.
            MODIFY SCREEN.
          ELSE.
*         "initialize"
*         and "loop at screen"

*           buffer when special status for audits was selected
            IF _partsig IS NOT INITIAL.
              gv_status_partsig = _partsig.
            ENDIF.

            IF iv_objecttype <> co_obtyp-audit.
              screen-invisible = '1'.
              MODIFY SCREEN.
            ELSE.
*           switching back to audit selection
*           --> need to check out buffered partial signature status
*               in order to manually adapt the radiobutton in the status group
*             This manipulation is neccessary because the generated handling of the
*             radiobutton group causes a "too many radiobuttons" dump
*             because it does not handle the invisible status _partsig properly
              IF gv_status_partsig IS NOT INITIAL.
*             status partial signature was set before
                CLEAR:  _prep, _open, _closed, _sall.
                _partsig =  gv_status_partsig.
              ELSE.
                CLEAR: _partsig.
              ENDIF. " initializing radiobutton group
            ENDIF.   " depending on object type
          ENDIF.     " whether digital signatures are switched on or not
        ENDIF.       " only when processing the partial signature field

        CASE screen-group1+1(1). "ACR264
          WHEN '0'. "Standard 20x
            IF iv_objecttype EQ co_obtyp-audit AND iv_complex_mode IS INITIAL.
              screen-input     = '0'.
              screen-output    = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN.
            ENDIF.
          WHEN '1'. "Custom Audit 21x
            IF iv_objecttype NE co_obtyp-audit OR iv_complex_mode IS NOT INITIAL.
              screen-input     = '0'.
              screen-output    = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN.
            ENDIF.
        ENDCASE.



      WHEN '3'.

********************************************************************
* partner block
********************************************************************

*        IF screen-name = 'NAME'.
*          IF NOT _me IS INITIAL.
*            screen-input = '0'.
*            MODIFY SCREEN.
*          ENDIF.
*          IF NOT _bupa IS INITIAL.
*            screen-input = '0'.
*            MODIFY SCREEN.
*          ENDIF.
*
*        ELSEIF screen-name = 'PARTNER'.
*          IF NOT _me IS INITIAL.
*            screen-input = '0'.
*            MODIFY SCREEN.
*          ENDIF.
*          IF NOT _name IS INITIAL.
*            screen-input = '0'.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDIF.
*        IF cl_plm_roles_assignment=>hr_is_active = 'X'.
*          IF screen-name = '_BUPA' OR
*            screen-name = '_ME'   OR
*            screen-name = '_NAME' OR
*            screen-group1 = '302' OR
*            screen-group1 = '303'.
*            screen-input     = '0'.
*            screen-output    = '0'.
*            screen-invisible = '1'.
*            MODIFY SCREEN.
*          ELSEIF screen-name = 'PARTNER'.
*            screen-input = '1'.
*            MODIFY SCREEN.
*          ENDIF.
*        ELSE.
        IF screen-group1 = '306'.
          screen-input     = '0'.
          screen-output    = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
*        ENDIF.

      WHEN '4'.

********************************************************************
* date block
********************************************************************

        IF iv_complex_mode IS INITIAL.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN. CONTINUE.
        ENDIF.

        CASE iv_objecttype.
          WHEN co_obtyp-audit.
            IF screen-group1 = '407' OR
               screen-group1 = '408' OR
               screen-group1 = '409'.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
          WHEN co_obtyp-plan
            OR co_obtyp-quest_h.
            IF screen-group1 NE '401' AND
               screen-group1 NE '402' AND
               screen-group1 NE '407' AND
               screen-group1 NE '408'.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
          WHEN co_obtyp-corr_act.
            IF screen-group1 = '405' OR
               screen-group1 = '407' OR
               screen-group1 = '408'.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
          WHEN co_obtyp-quest_i
            OR co_obtyp-auditquest.
*           no dates for questions
            IF screen-group1 NE '401' AND
               screen-group1 NE '402'.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
        ENDCASE.

        CASE screen-name.
          WHEN '_DATEPL-LOW'
            OR '_DATEPL-HIGH'
            OR '_DATEAC-LOW'
            OR '_DATEAC-HIGH'
            OR '_DATECH-LOW'
            OR '_DATECH-HIGH'
            OR '_DATECR-LOW'
            OR '_DATECR-HIGH'
            OR '_DATACF-LOW'
            OR '_DATACF-HIGH'
            OR '_DATPLF-LOW'
            OR '_DATPLF-HIGH'
            OR '_FINDATE-LOW'
            OR '_FINDATE-HIGH'.
            screen-value_help = '1'.
            MODIFY SCREEN.

        ENDCASE.  " screen-name

      WHEN '5'.

********************************************************************
* special report fields
********************************************************************

        IF iv_complex_mode IS INITIAL.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN. CONTINUE.
        ENDIF.
        CASE screen-group1.
          WHEN '5P1'.
*           authority group
            IF iv_objecttype NE co_obtyp-audit    AND
               iv_objecttype NE co_obtyp-plan     AND
               iv_objecttype NE co_obtyp-corr_act AND
               iv_objecttype NE co_obtyp-quest_h.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
          WHEN '5PC'.
*          dummy spacing line: see above
            screen-input  = '0'.
            screen-output = '0'.
            MODIFY SCREEN.
            IF iv_objecttype NE co_obtyp-audit    AND
               iv_objecttype NE co_obtyp-plan     AND
               iv_objecttype NE co_obtyp-corr_act AND
               iv_objecttype NE co_obtyp-quest_h.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
          WHEN '5C1'.
*           audit question attributes
            IF iv_objecttype NE co_obtyp-auditquest.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
          WHEN '5P2'.
*           audit attributes
            IF iv_objecttype NE co_obtyp-audit.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
          WHEN '5R1'.
*           audit answer statistic report
            IF iv_objecttype NE co_obtyp-audit.
              screen-input = '0'.
              screen-invisible = '1'.
              MODIFY SCREEN. CONTINUE.
            ENDIF.
        ENDCASE.

      WHEN 'V'.

***********************************************************************
* audited object block
***********************************************************************

        IF iv_complex_mode IS INITIAL.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN. CONTINUE.
        ELSEIF iv_objecttype NE co_obtyp-audit.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN. CONTINUE.
        ELSE.
*         fields are relevant for audit
          IF iv_obj_fields_count IS INITIAL.
*           no audit type given yet
            screen-input = '0'.
            screen-invisible = '1'.
            MODIFY SCREEN. CONTINUE.
          ELSE.
            CASE screen-group1.
              WHEN 'V01'.
                IF iv_obj_fields_count LT 1.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V02'.
                IF iv_obj_fields_count LT 2.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V03'.
                IF iv_obj_fields_count LT 3.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V04'.
                IF iv_obj_fields_count LT 4.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V05'.
                IF iv_obj_fields_count LT 5.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V06'.
                IF iv_obj_fields_count LT 6.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V07'.
                IF iv_obj_fields_count LT 7.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V08'.
                IF iv_obj_fields_count LT 8.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
              WHEN 'V09'.
                IF iv_obj_fields_count LT 9.
                  screen-input = '0'.
                  screen-invisible = '1'.
                  MODIFY SCREEN. CONTINUE.
                ENDIF.
            ENDCASE.   "SCREEN-GROUP1
          ENDIF.
        ENDIF.

      WHEN ' '.

      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " initialize_and_loop_at_screen

*&---------------------------------------------------------------------*
*&      Form  GET_AUDIT_QUESTION_STATISTICS
*&---------------------------------------------------------------------*
*   1)    find audits for given selection criteria
*   2)    get questions of given master questionnaire
*   3)    count number of certain question answers in audits
*   4)    display accumulated results for questions
*----------------------------------------------------------------------*
FORM get_audit_question_statistics
  USING
    iv_external_id     TYPE plmt_quest_id.
*   and global audit selection criteria

  DATA:
    lt_audit_guid  TYPE STANDARD TABLE OF plmt_auditobject_guid_str,
    lv_lines       TYPE i.

**********************************************************************
* 1) find audits involved
**********************************************************************

  "TODO ADD DISTRICT and Planned Auditor
  CALL FUNCTION 'PLM_AUDIT_OBJECTS_FETCH'
    EXPORTING
      iv_aud_objecttype         = co_obtyp-audit
      iv_name                   = gv_name
                                  "iv_partner                = partner
      it_partner_range          = gt_partner_range
      it_planstart_range        = gt_planstart_range
      it_actualstart_range      = gt_actualstart_range
      it_planfinish_range       = gt_planfinish_range
      it_actualfinish_range     = gt_actualfinish_range
      it_changed_on_range       = gt_changed_on_range
      it_created_on_range       = gt_created_on_range
      it_search_field_range     = gt_searchfield_range
      it_created_by_range       = gt_created_by_range
      it_changed_by_range       = gt_changed_by_range
      it_proc_status_range      = gt_proc_status_range
      it_grouping_range         = gt_grouping_range
      it_external_id_range      = gt_external_id_range
      it_auth_group_range       = gt_auth_group_range
      iv_audit_type             = gv_audit_type
      iv_object_type            = gv_object_type
      iv_object_value           = gv_object_value
      it_audit_type_range       = gt_audit_type_range
      it_object_type_range      = gt_object_type_range
      it_object_value_range     = gt_object_value_range
      it_audited_obj_complex    = gt_audited_obj_complex
      it_unplanned_range        = gt_unplanned_range
      it_score_res_range        = gt_score_res_range
      it_next_audit_oblig_range = gt_next_audit_oblig_range
    TABLES
      e_project_guid_tab        = lt_audit_guid
    EXCEPTIONS
      wrong_input               = 1
      nothing_found             = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    DESCRIBE TABLE lt_audit_guid LINES lv_lines.
    MESSAGE s013(plm_audit_db) WITH lv_lines .
  ENDIF.

* get questions, prepare statistics and display data
  PERFORM get_quest_res_statistics TABLES lt_audit_guid[]
                                   USING  iv_external_id.

ENDFORM.                    " GET_AUDIT_QUESTION_STATISTICS

AT SELECTION-SCREEN ON HELP-REQUEST FOR _partsig.
*----------------------------------------------------------------------*

  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
    EXPORTING
      doklangu         = sy-langu
      called_for_tab   = 'PLMM_AUDIT'
      called_for_field = 'PROC_STATUS'
    EXCEPTIONS
      object_not_found = 0
      OTHERS           = 0.

*----------------------------------------------------------------------*
