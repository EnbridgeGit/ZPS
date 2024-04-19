*&---------------------------------------------------------------------*
*& Module Pool       ZPLM_AUDIT_ENHANCEMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM  zplm_audit_enhancement.

TYPES:
  BEGIN OF vrm_value,
    key(40) TYPE c,
    text(80) TYPE c,
  END OF vrm_value.

DATA: ls_values     TYPE vrm_value,
      lt_values     LIKE TABLE OF ls_values,
      ls_district   TYPE zplm_district,
      ls_division   TYPE zplm_division,
      lv_oldistrict TYPE zplm_district-dist_code,
      ls_address    TYPE bapiaddr3,
      lv_username   TYPE sy-uname,
      lt_return     TYPE TABLE OF bapiret2.

TABLES: plmt_audit_ui.
DATA:   manager_name          TYPE c LENGTH 40,
        auditor_name          TYPE c LENGTH 40.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  DATA: lv_edit_mode TYPE char01,
        lr_badi TYPE REF TO badi_plm_auo_custom_fields,
        ir_audit TYPE REF TO cl_cgpl_project.

  TRY.
      GET BADI lr_badi.
      CALL BADI lr_badi->get_screen_data
        IMPORTING
          es_plmt_audit_ui = plmt_audit_ui
          er_audit         = ir_audit.
      "get the transaction mode (edit or display)
      CALL METHOD ir_audit->get_edit_mode
        IMPORTING
          ex_edit_mode = lv_edit_mode.

      "Display only, if not edit mode OR if Signature is provided
      IF lv_edit_mode IS INITIAL OR plmt_audit_ui-s_status CS 'AM02' OR plmt_audit_ui-s_status CS 'AM03'.
        LOOP AT SCREEN.
          screen-input = 0.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.
    CATCH cx_badi_not_implemented.                      "#EC NO_HANDLER
  ENDTRY.

  lv_oldistrict = plmt_audit_ui-z_district.

  "Populate the Drop Down Values for District
  CLEAR: lt_values, ls_values.
  SELECT * INTO ls_district FROM zplm_district.
    ls_values-key = ls_district-dist_code.
    ls_values-text = ls_district-dist_name.
    APPEND ls_values TO lt_values.
  ENDSELECT.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'PLMT_AUDIT_UI-Z_district'
      values = lt_values.

  "Populate the Division
  IF plmt_audit_ui-z_district IS NOT INITIAL.
    CLEAR: lt_values, ls_values.
    SELECT * INTO ls_division FROM zplm_division
      WHERE district = plmt_audit_ui-z_district.
      ls_values-key = ls_division-division.
      ls_values-text = ls_division-division_name.
      APPEND ls_values TO lt_values.
    ENDSELECT.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'PLMT_AUDIT_UI-Z_division'
        values = lt_values.
  ENDIF.

  "Populate Manager Name:
  IF plmt_audit_ui-z_manager IS NOT INITIAL.
    lv_username = plmt_audit_ui-z_manager.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = lv_username
        cache_results = 'X'
      IMPORTING
        address       = ls_address
      TABLES
        return        = lt_return.
    IF lt_return IS INITIAL.
      manager_name = ls_address-fullname.
    ELSE.
      manager_name = ''.
    ENDIF.
  ELSE.
    manager_name = ''.
  ENDIF.

  "Populate Planned Auditor:
  IF plmt_audit_ui-z_auditor_planned IS NOT INITIAL.
    lv_username = plmt_audit_ui-z_auditor_planned.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = lv_username
        cache_results = 'X'
      IMPORTING
        address       = ls_address
      TABLES
        return        = lt_return.
    IF lt_return IS INITIAL.
      auditor_name = ls_address-fullname.
    ELSE.
      auditor_name = ''.
    ENDIF.
  ELSE.
    auditor_name = ''.
  ENDIF.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
*  "Clear the Selected value of District
  IF lv_oldistrict <> plmt_audit_ui-z_district.
    CLEAR plmt_audit_ui-z_division.
  ENDIF.

  TRY.
*      GET BADI lr_badi.
      CALL BADI lr_badi->set_screen_data
        EXPORTING
          is_plmt_audit_ui = plmt_audit_ui.
    CATCH cx_badi_not_implemented.                      "#EC NO_HANDLER
  ENDTRY.

ENDMODULE.                 " USER_COMMAND_9001  INPUT
