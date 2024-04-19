class ZCL_PLM_COR_CUSTOM_FIELDS definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_PLM_COR_CUSTOM_FIELDS
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_PLM_COR_CUSTOM_FIELDS .
*"* protected components of class ZCL_PLM_COR_CUSTOM_FIELDS
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_PLM_COR_CUSTOM_FIELDS
*"* do not include other source files here!!!
private section.

  data MS_PLMT_AUDIT_ACT_UI type PLMT_AUDIT_ACT_UI .
  data MR_ACTION type ref to CL_CGPL_PROJECT .
ENDCLASS.



CLASS ZCL_PLM_COR_CUSTOM_FIELDS IMPLEMENTATION.


METHOD if_ex_plm_cor_custom_fields~check.
  DATA: lv_username LIKE sy-uname,
        ls_return   LIKE LINE OF et_return.

  IF is_attributes-z_assignedto IS NOT INITIAL.
    SELECT SINGLE bname FROM usr02 INTO lv_username WHERE bname = is_attributes-z_assignedto.
    IF sy-subrc <> 0.
      ls_return-type = 'E'.
      ls_return-id = 'ZFI01'.
      ls_return-number = '000'.
      ls_return-message_v1 = 'Assigned To person does not exists'.
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.
ENDMETHOD.


method IF_EX_PLM_COR_CUSTOM_FIELDS~GET_SCREEN_DATA.
  MOVE ms_plmt_audit_act_ui TO es_plmt_audit_act_ui .
  ER_ACTION = MR_ACTION.
endmethod.


METHOD if_ex_plm_cor_custom_fields~get_ui_structure.
  cs_external_structure-z_assignedto = is_attributes-z_assignedto.
ENDMETHOD.


METHOD if_ex_plm_cor_custom_fields~import_ui_structure_data.
  cs_attributes-z_assignedto = is_external_structure-z_assignedto.
ENDMETHOD.


method IF_EX_PLM_COR_CUSTOM_FIELDS~SET_CUSTOMER_TABSTRIP_NAME.
endmethod.


method IF_EX_PLM_COR_CUSTOM_FIELDS~SET_SCREEN_DATA.
  MOVE is_plmt_audit_act_ui TO ms_plmt_audit_act_ui .
  mr_action = ir_action.
endmethod.
ENDCLASS.
