class ZCL_PLM_AUO_CUSTOM_FIELDS definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_PLM_AUO_CUSTOM_FIELDS
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_PLM_AUO_CUSTOM_FIELDS .
*"* protected components of class ZCL_PLM_AUO_CUSTOM_FIELDS
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_PLM_AUO_CUSTOM_FIELDS
*"* do not include other source files here!!!
private section.

  data MS_PLMT_AUDIT_UI type PLMT_AUDIT_UI .
  data MR_AUDIT type ref to CL_CGPL_PROJECT .
ENDCLASS.



CLASS ZCL_PLM_AUO_CUSTOM_FIELDS IMPLEMENTATION.


METHOD if_ex_plm_auo_custom_fields~check.
  DATA: lv_username LIKE sy-uname,
        ls_return   LIKE LINE OF et_return.

  IF is_attributes-z_manager IS NOT INITIAL.
    SELECT SINGLE bname FROM usr02 INTO lv_username WHERE bname = is_attributes-z_manager.
    IF sy-subrc <> 0.
      ls_return-type = 'E'.
      ls_return-id = 'ZFI01'.
      ls_return-number = '000'.
      ls_return-message_v1 = 'Manager does not exists'.
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

  IF is_attributes-z_auditor_planned IS NOT INITIAL.
    SELECT SINGLE bname FROM usr02 INTO lv_username WHERE bname = is_attributes-z_auditor_planned.
    IF sy-subrc <> 0.
      ls_return-type = 'E'.
      ls_return-id = 'ZFI01'.
      ls_return-number = '000'.
      ls_return-message_v1 = 'Planned Auditor does not exists'.
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

ENDMETHOD.


method IF_EX_PLM_AUO_CUSTOM_FIELDS~GET_SCREEN_DATA.
    MOVE MS_PLMT_AUDIT_UI TO es_plmt_audit_ui.
    ER_AUDIT = MR_AUDIT.
endmethod.


METHOD if_ex_plm_auo_custom_fields~get_ui_structure.
  cs_external_structure-z_district = is_attributes-z_district.
  cs_external_structure-z_division = is_attributes-z_division.
  cs_external_structure-z_manager = is_attributes-z_manager.
  cs_external_structure-z_town = is_attributes-z_town.
  cs_external_structure-z_adress = is_attributes-z_adress.
  cs_external_structure-z_jobid = is_attributes-z_jobid.
  cs_external_structure-z_site_cont = is_attributes-z_site_cont.
  cs_external_structure-z_add_sitecont = is_attributes-z_add_sitecont.
  cs_external_structure-z_auditor	= is_attributes-z_auditor.
  cs_external_structure-z_auditor_planned = is_attributes-z_auditor_planned.
ENDMETHOD.


METHOD if_ex_plm_auo_custom_fields~import_ui_structure_data.
  cs_attributes-z_district = is_external_structure-z_district.
  cs_attributes-z_division = is_external_structure-z_division.
  cs_attributes-z_manager = is_external_structure-z_manager.
  cs_attributes-z_town = is_external_structure-z_town.
  cs_attributes-z_adress = is_external_structure-z_adress.
  cs_attributes-z_jobid = is_external_structure-z_jobid.
  cs_attributes-z_site_cont = is_external_structure-z_site_cont.
  cs_attributes-z_add_sitecont = is_external_structure-z_add_sitecont.
  "cs_attributes-z_usm_code = is_external_structure-z_usm_code.
  cs_attributes-z_auditor	= is_external_structure-z_auditor.
  cs_attributes-z_auditor_planned = is_external_structure-z_auditor_planned.
ENDMETHOD.


method IF_EX_PLM_AUO_CUSTOM_FIELDS~SET_CUSTOMER_TABSTRIP_NAME.
endmethod.


method IF_EX_PLM_AUO_CUSTOM_FIELDS~SET_SCREEN_DATA.
    MOVE is_plmt_audit_ui TO MS_PLMT_AUDIT_UI .
    mr_audit = ir_audit.
endmethod.
ENDCLASS.
