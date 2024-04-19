class ZCL_IM_PLM_AUDIT_QUN_UPDAT definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_QUN_UPDAT
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_QUN_UPDATE .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_QUN_UPDAT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_QUN_UPDAT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_QUN_UPDAT IMPLEMENTATION.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CHANGE_HEADER_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CHANGE_HEADER_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CHANGE_HEADER_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CHANGE_ITEM_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CHANGE_ITEM_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CHANGE_ITEM_IN_UPDATE.
endmethod.


METHOD if_ex_plm_audit_qun_update~create_header_at_save.
  DATA: ls_auditquestion TYPE plmt_quest_h_ui.
  CALL METHOD ir_header_attr_assignment->get_ui_structure
    IMPORTING
      es_external_structure = ls_auditquestion.

  "Default Values
  IF ls_auditquestion-assessm_profil IS INITIAL.
    ls_auditquestion-assessm_profil = 'AM10'.
  ENDIF.
  IF ls_auditquestion-audit_calculate IS INITIAL.
    ls_auditquestion-audit_calculate = '52'.
  ENDIF.
  IF ls_auditquestion-hierarchy_profil IS INITIAL.
    ls_auditquestion-hierarchy_profil = '10'.
  ENDIF.

  CALL METHOD ir_header_attr_assignment->import_ui_structure_data
    EXPORTING
      is_external_structure = ls_auditquestion.
ENDMETHOD.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CREATE_HEADER_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CREATE_HEADER_IN_UPDATE.
endmethod.


METHOD if_ex_plm_audit_qun_update~create_item_at_save.
  DATA: ls_auditquestion TYPE plmt_quest_i_ui.
  CALL METHOD ir_item_attr_assignment->get_ui_structure
    IMPORTING
      es_external_structure = ls_auditquestion.

  "Default Values
  IF ls_auditquestion-min_result IS INITIAL.
    ls_auditquestion-min_result = '100'.
  ENDIF.


  CALL METHOD ir_item_attr_assignment->import_ui_structure_data
    EXPORTING
      is_external_structure = ls_auditquestion.
ENDMETHOD.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CREATE_ITEM_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~CREATE_ITEM_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~DELETE_HEADER_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~DELETE_HEADER_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~DELETE_HEADER_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~DELETE_ITEM_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~DELETE_ITEM_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_QUN_UPDATE~DELETE_ITEM_IN_UPDATE.
endmethod.
ENDCLASS.
