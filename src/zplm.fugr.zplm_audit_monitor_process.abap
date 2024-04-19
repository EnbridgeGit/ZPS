FUNCTION zplm_audit_monitor_process.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_AUD_OBJECTTYPE) TYPE  CGPL_OBJECT_TYPE
*"     VALUE(IV_NAME) TYPE  SYUNAME OPTIONAL
*"     VALUE(IV_PARTNER) TYPE  BU_PARTNER OPTIONAL
*"     REFERENCE(IT_PARTNER_RANGE) TYPE  PLMT_RANGES_FOR_PARTNER
*"       OPTIONAL
*"     REFERENCE(IT_EXTERNAL_ID_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_EXTERNAL_ID OPTIONAL
*"     REFERENCE(IT_SEARCHFIELD_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_SEARCHFIELD OPTIONAL
*"     REFERENCE(IT_GROUPING_RANGE) TYPE  PLMT_AUDIT_RANGES_GROUPING
*"       OPTIONAL
*"     REFERENCE(IT_PLANSTART_RANGE) TYPE  PLMT_AUDIT_RANGES_FOR_DATE
*"       OPTIONAL
*"     REFERENCE(IT_ACTUALSTART_RANGE) TYPE  PLMT_AUDIT_RANGES_FOR_DATE
*"       OPTIONAL
*"     REFERENCE(IT_CREATED_ON_RANGE) TYPE  PLMT_AUDIT_RANGES_FOR_DATE
*"       OPTIONAL
*"     REFERENCE(IT_CREATED_BY_RANGE) TYPE  PLMT_RANGES_FOR_USER
*"       OPTIONAL
*"     REFERENCE(IT_CHANGED_ON_RANGE) TYPE  PLMT_AUDIT_RANGES_FOR_DATE
*"       OPTIONAL
*"     REFERENCE(IT_CHANGED_BY_RANGE) TYPE  PLMT_RANGES_FOR_USER
*"       OPTIONAL
*"     REFERENCE(IT_PROC_STATUS_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_PROC_STATUS OPTIONAL
*"     REFERENCE(IT_EXT_POSITION_RANGE) TYPE
*"        PLMT_RANGES_FOR_EXT_POSITION OPTIONAL
*"     REFERENCE(IV_AUDIT_TYPE) TYPE  PLMT_AUDIT_TYPE OPTIONAL
*"     REFERENCE(IV_OBJECT_TYPE) TYPE  PLMT_AUDITOBJECTTYPE OPTIONAL
*"     REFERENCE(IV_OBJECT_VALUE) TYPE  PLMT_AUDIT_OBJECT_VALUE
*"       OPTIONAL
*"     REFERENCE(IT_AUDIT_TYPE_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_AUDIT_TYPE OPTIONAL
*"     REFERENCE(IT_OBJECT_VALUE_RANGE) TYPE
*"        PLMT_RANGES_FOR_AUDITED_OBJECT OPTIONAL
*"     REFERENCE(IT_OBJECT_TYPE_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_AUDIT_TYPE OPTIONAL
*"     REFERENCE(IT_AUDITED_OBJ_COMPLEX) TYPE
*"        PLMT_AUDITED_OBJECT_SEARCH_TAB OPTIONAL
*"     REFERENCE(IV_OBJECTTEXT) TYPE  CGPL_OBJECT_TEXT OPTIONAL
*"     REFERENCE(IT_PLANFINISH_RANGE) TYPE  PLMT_AUDIT_RANGES_FOR_DATE
*"       OPTIONAL
*"     REFERENCE(IT_ACTUALFINISH_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_FOR_DATE OPTIONAL
*"     REFERENCE(IT_UNPLANNED_ITEM) TYPE  PLMT_AUDIT_RANGES_FLAG
*"       OPTIONAL
*"     REFERENCE(IT_CORR_REQUIRED) TYPE  PLMT_AUDIT_RANGES_FLAG
*"       OPTIONAL
*"     REFERENCE(IT_TXT_HAS_FACT) TYPE  PLMT_AUDIT_RANGES_FLAG OPTIONAL
*"     REFERENCE(IT_TXT_HAS_POSI) TYPE  PLMT_AUDIT_RANGES_FLAG OPTIONAL
*"     REFERENCE(IT_TXT_HAS_NEGA) TYPE  PLMT_AUDIT_RANGES_FLAG OPTIONAL
*"     REFERENCE(IT_SCORE_RESULT) TYPE  PLMT_AUDIT_RANGES_DEC10
*"       OPTIONAL
*"     REFERENCE(IT_SCORE_RECORDED) TYPE  PLMT_AUDIT_RANGES_DEC5
*"       OPTIONAL
*"     REFERENCE(IT_QUEST_REF_GUID) TYPE  PLMT_AUDIT_RANGES_GUID
*"       OPTIONAL
*"     REFERENCE(IT_AUTH_GROUP_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_AUTH_GROUP OPTIONAL
*"     REFERENCE(IT_UNPLANNED_RANGE) TYPE  PLMT_AUDIT_RANGES_FLAG
*"       OPTIONAL
*"     REFERENCE(IT_SCORE_RES_RANGE) TYPE  PLMT_AUDIT_RANGES_DEC10
*"       OPTIONAL
*"     REFERENCE(IT_NEXT_AUDIT_OBLIG_RANGE) TYPE
*"        PLMT_AUDIT_RANGES_FLAG OPTIONAL
*"     REFERENCE(IT_LISTITYPE_RANGE) TYPE  PLMT_RANGES_FOR_LISTITYPE
*"       OPTIONAL
*"     REFERENCE(IT_REFERENCE_RANGE) TYPE  PLMT_RANGES_FOR_REFERENCE
*"       OPTIONAL
*"     REFERENCE(IT_ACTIONTYPE_RANGE) TYPE  PLMT_RANGES_FOR_ACTIONTYPE
*"       OPTIONAL
*"     REFERENCE(IT_JOB_ID) TYPE  Z_JOBID_RANGE OPTIONAL
*"     REFERENCE(IT_SITE_CONT) TYPE  Z_SITE_CONT_RANGE OPTIONAL
*"     REFERENCE(IT_ADD_SITE) TYPE  Z_ADD_SITE_RANGE OPTIONAL
*"     REFERENCE(IT_DISTRICT) TYPE  Z_DISTRICT_RANGE OPTIONAL
*"     REFERENCE(IT_METER_RANGE) TYPE  Z_METER_RANGE OPTIONAL
*"     REFERENCE(IT_PLANNEDAUDIT) TYPE  Z_AUDITOR_P_RANGE OPTIONAL
*"     REFERENCE(IV_AUDITWORK) TYPE  XFELD OPTIONAL
*"     REFERENCE(IV_LEADPROGRESS) TYPE  XFELD OPTIONAL
*"     REFERENCE(IV_LEADCOMPLETE) TYPE  XFELD OPTIONAL
*"     REFERENCE(IV_NOTCOMPLETE) TYPE  XFELD OPTIONAL
*"     REFERENCE(IV_COMPLETE) TYPE  XFELD OPTIONAL
*"  EXCEPTIONS
*"      NO_SUCCESS
*"----------------------------------------------------------------------

  CONSTANTS:
    lc_proj_objecttype  TYPE cgpl_object_type VALUE co_obtyp-audit.

  DATA:
    lt_project_guid     TYPE STANDARD TABLE OF plmt_auditobject_guid_str,
    lt_task_guid        TYPE STANDARD TABLE OF plmt_auditobject_guid_str,
    lv_tabname          TYPE tabname,
    lt_data             TYPE REF TO data.

  FIELD-SYMBOLS:
    <fs_tab> TYPE STANDARD TABLE.

  FIELD-SYMBOLS: <ls_audit> TYPE plmt_audit_alv,
                 <ls_coract> TYPE plmt_audit_act_alv.
  DATA: "ls_Audit TYPE plmt_audit_alv,
        ls_plmm_audit TYPE plmm_audit,
        ls_plmm_audit_act TYPE plmm_audit_act.
**********************************************************************
*/ this function module is used in reports for PLM auditmanagement
*/ it selects and displays objects for given selection criteria
**********************************************************************

*/ find object keys on database
  CALL FUNCTION 'PLM_AUDIT_OBJECTS_FETCH'
       EXPORTING
         iv_aud_objecttype          = iv_aud_objecttype
         iv_proj_objecttype         = lc_proj_objecttype
         iv_name                    = iv_name
         iv_partner                 = iv_partner
         it_partner_range           = it_partner_range
         it_planstart_range         = it_planstart_range
         it_actualstart_range       = it_actualstart_range
         it_planfinish_range        = it_planfinish_range
         it_actualfinish_range      = it_actualfinish_range
         it_changed_on_range        = it_changed_on_range
         it_created_on_range        = it_created_on_range
         it_search_field_range      = it_searchfield_range
         it_created_by_range        = it_created_by_range
         it_changed_by_range        = it_changed_by_range
         it_proc_status_range       = it_proc_status_range
         it_grouping_range          = it_grouping_range
         it_external_id_range       = it_external_id_range
         it_auth_group_range        = it_auth_group_range
* questionnaire related criteria
         it_ext_position_range      = it_ext_position_range
         it_quest_ref_guid_range    = it_quest_ref_guid
* audit specific criteria
         iv_audit_type              = iv_audit_type
         iv_object_type             = iv_object_type
         iv_object_value            = iv_object_value
         it_audit_type_range        = it_audit_type_range
         it_object_type_range       = it_object_type_range
         it_object_value_range      = it_object_value_range
         it_audited_obj_complex     = it_audited_obj_complex
         it_unplanned_range         = it_unplanned_range
         it_score_res_range         = it_score_res_range
         it_next_audit_oblig_range  = it_next_audit_oblig_range
* audit question specific criteria
         it_listitype_range         = it_listitype_range
         it_unplanned_item_range    = it_unplanned_item
         it_corr_required_range     = it_corr_required
         it_txt_has_fact_range      = it_txt_has_fact
         it_txt_has_posi_range      = it_txt_has_posi
         it_txt_has_nega_range      = it_txt_has_nega
         it_score_result_range      = it_score_result
         it_score_recorded_range    = it_score_recorded
         it_reference_range         = it_reference_range
* action specific criteria
         it_actiontype_range        = it_actiontype_range
       TABLES
         e_project_guid_tab         = lt_project_guid
         e_task_guid_tab            = lt_task_guid
       EXCEPTIONS
*        WRONG_INPUT                = 1
         nothing_found              = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING no_success .
  ENDIF.

  CASE iv_aud_objecttype.
    WHEN co_obtyp-plan.
      CREATE DATA lt_data TYPE STANDARD TABLE OF plmt_auditplan_alv.
    WHEN co_obtyp-audit.
      CREATE DATA lt_data TYPE STANDARD TABLE OF plmt_audit_alv.
    WHEN co_obtyp-corr_act.
      CREATE DATA lt_data TYPE STANDARD TABLE OF plmt_audit_act_alv.
    WHEN co_obtyp-quest_h.
      CREATE DATA lt_data TYPE STANDARD TABLE OF plmt_quest_h_alv.
    WHEN co_obtyp-quest_i.
      CREATE DATA lt_data TYPE STANDARD TABLE OF plmt_quest_i_alv.
    WHEN co_obtyp-auditquest.
      CREATE DATA lt_data TYPE STANDARD TABLE OF plmt_quest_res_alv.
  ENDCASE.
  ASSIGN lt_data->* TO <fs_tab> .

*/ load objects with attributes
  CALL FUNCTION 'PLM_AUDIT_OBJECTS_LOAD'
    EXPORTING
      i_aud_objecttype  = iv_aud_objecttype
    IMPORTING
      ev_data_structure = lv_tabname
    TABLES
      it_project_guid   = lt_project_guid
      it_task_guid      = lt_task_guid
      et_data           = <fs_tab>
    EXCEPTIONS
      wrong_input       = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING no_success .
    EXIT.
  ENDIF.

*  "----begin custom fields ACR264
  "Remove custom status's
  IF iv_aud_objecttype EQ co_obtyp-audit.
    IF iv_auditwork IS NOT INITIAL.
      DELETE <fs_tab> where ('s_status NS ''AM00'' AND s_status NS ''AM01'' AND s_status NS ''AM02'''). "Remove if not 00/01/02
      DELETE <fs_tab> where ('s_status CS ''AM02'' AND s_status CS ''AM17'' AND s_status CS ''AM18'''). "Remove exactly 02+17+18
      DELETE <fs_tab> where ('s_status CS ''AM02'' AND s_status CS ''AM17'' AND s_status CS ''AM19'''). "Remove exactly 02+17+19
    ELSEIF iv_leadprogress IS NOT INITIAL.
      DELETE <fs_tab> where ('s_status NS ''AM02'' OR s_status NS ''AM17'' OR s_status NS ''AM18'''). "Remove if not exactly 02+17+18
    ELSEIF iv_leadcomplete IS NOT INITIAL.
      DELETE <fs_tab> where ('s_status NS ''AM02'' OR s_status NS ''AM17'' OR s_status NS ''AM19'''). "Remove if not exactly 02+17+19
    ELSEIF iv_notcomplete IS NOT INITIAL.
      DELETE <fs_tab> where ('s_status CS ''AM03'' OR s_status CS ''AM20'''). "Remove if it contains 03/20
    ELSEIF iv_complete IS NOT INITIAL.
      DELETE <fs_tab> where ('s_status NS ''AM03'' AND s_status NS ''AM20'''). "Remove if not 03/20
    ELSE.
      "Only other object is "ALL"
    ENDIF.
  ENDIF.

  "Audit
  IF iv_aud_objecttype = co_obtyp-audit.
    LOOP AT <fs_tab> ASSIGNING <ls_audit>.
      CLEAR ls_plmm_audit.
      SELECT SINGLE * FROM plmm_audit INTO ls_plmm_audit
        WHERE guid = <ls_audit>-guid.
      CHECK sy-subrc = 0.

      <ls_audit>-z_add_sitecont     = ls_plmm_audit-z_add_sitecont.
      <ls_audit>-z_adress           = ls_plmm_audit-z_adress.
      <ls_audit>-z_auditor          = ls_plmm_audit-z_auditor.
      <ls_audit>-z_auditor_planned  = ls_plmm_audit-z_auditor_planned.
      <ls_audit>-z_district         = ls_plmm_audit-z_district.
      <ls_audit>-z_division         = ls_plmm_audit-z_division.
      <ls_audit>-z_jobid            = ls_plmm_audit-z_jobid.
      <ls_audit>-z_manager          = ls_plmm_audit-z_manager.
      <ls_audit>-z_meterlocation    = ls_plmm_audit-z_meterlocation.
      <ls_audit>-z_meterno          = ls_plmm_audit-z_meterno.
      <ls_audit>-z_site_cont        = ls_plmm_audit-z_site_cont.
      <ls_audit>-z_town             = ls_plmm_audit-z_town.

      IF <ls_audit>-z_district NOT IN it_district OR
         <ls_audit>-z_auditor_planned NOT IN it_plannedaudit OR
         <ls_audit>-z_meterno NOT IN it_meter_range OR
         <ls_audit>-z_jobid NOT IN it_job_id OR
         <ls_audit>-z_site_cont NOT IN it_site_cont OR
         <ls_audit>-z_add_sitecont NOT IN it_add_site." OR
        DELETE <fs_tab> INDEX syst-tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.
  "Corrective Action
  IF iv_aud_objecttype = co_obtyp-corr_act.
    LOOP AT <fs_tab> ASSIGNING <ls_coract>.
      CLEAR ls_plmm_audit_act.
      SELECT SINGLE * FROM plmm_audit_act INTO ls_plmm_audit_act
        WHERE guid = <ls_coract>-guid.
      CHECK sy-subrc = 0.
      <ls_coract>-z_assignedto  = ls_plmm_audit_act-z_assignedto.
      <ls_coract>-z_leadauditor = ls_plmm_audit_act-z_leadauditor.
      <ls_coract>-z_rootcause   = ls_plmm_audit_act-z_rootcause.
    ENDLOOP.
  ENDIF.
*  "-----end of custom fields
  CHECK NOT <fs_tab> IS INITIAL.

*/Display data - use PLM version of ALV Grid
  CALL FUNCTION 'PLM_AUDIT_MONITOR_ALV_DISPLAY'
    EXPORTING
      iv_object_type = iv_aud_objecttype
      it_data        = <fs_tab>
      iv_structure   = lv_tabname
      iv_object_text = iv_objecttext.
ENDFUNCTION.
