*&---------------------------------------------------------------------*
*& Module Pool       ZPLM_AUDIT_ACTION_ENHANCEMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM  zplm_audit_action_enhancement.

TABLES:   zplm_actions.

TYPES:  BEGIN OF ty_action_ui,
          mandt     TYPE zplm_actions-mandt,
          guid      TYPE zplm_actions-guid,
          indx      TYPE zplm_actions-indx,
          code      TYPE zplm_actions-code,
          details   TYPE zplm_actions-details,
          long_desc TYPE zplm_actions_t-long_desc,
        END OF ty_action_ui.

DATA:     lt_action_ui      TYPE ty_action_ui OCCURS 0,
          ls_action_ui      TYPE ty_action_ui, "work area
          ls_plm_actions    TYPE zplm_actions,
          ls_plm_actions_t  TYPE zplm_actions_t,
          lv_changes        TYPE char1.

DATA:     gv_action_ui_copied,
          gv_guid        TYPE zplm_actions-guid,
          gv_textline    TYPE i.

CONTROLS: action_table_ui TYPE TABLEVIEW USING SCREEN 9001.

DATA: g_editor TYPE REF TO cl_gui_textedit,
      g_editor_container TYPE REF TO cl_gui_custom_container.

CONSTANTS: gc_mycontainer(30) TYPE c VALUE 'TEXTEDITOR_UI'.

DATA:     ls_columns LIKE LINE OF action_table_ui-cols.

DATA:       ir_action     TYPE REF TO cl_cgpl_project,
            ls_address    TYPE bapiaddr3,
            lv_username   TYPE sy-uname,
            lv_topindx    TYPE zplm_actions-indx,
            lv_lines      TYPE i,
            lt_return     TYPE TABLE OF bapiret2.

TABLES: plmt_audit_act_ui.
DATA:   assignedto_name TYPE c LENGTH 40.



*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  DATA: lv_edit_mode TYPE char01,
        lr_badi TYPE REF TO badi_plm_cor_custom_fields.

  IF g_editor IS INITIAL.
    CREATE OBJECT g_editor_container
      EXPORTING
        container_name              = gc_mycontainer
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT g_editor
      EXPORTING
        parent                     = g_editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
  ENDIF.

  "When GUID changes
  TRY.
      GET BADI lr_badi.
      CALL BADI lr_badi->get_screen_data
        IMPORTING
          es_plmt_audit_act_ui = plmt_audit_act_ui
          er_action            = ir_action.
    CATCH cx_badi_not_implemented.                      "#EC NO_HANDLER
  ENDTRY.

  "get the transaction mode (edit or display)
  CALL METHOD ir_action->get_edit_mode
    IMPORTING
      ex_edit_mode = lv_edit_mode.


  IF lv_edit_mode IS INITIAL OR plmt_audit_act_ui-s_status CS 'AM03' OR plmt_audit_act_ui-s_status CS 'AM10'.
    LOOP AT SCREEN.
      IF screen-name CS 'DISPLAY_TEXT_BTN'.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
    LOOP AT action_table_ui-cols INTO ls_columns.
      ls_columns-screen-input = 0.
      MODIFY action_table_ui-cols FROM ls_columns.
    ENDLOOP.
    CALL METHOD g_editor->set_readonly_mode
      EXPORTING
        readonly_mode = 1.    " read-only mode; eq 0: OFF ; ne 0: ON
  ELSE.
*    LOOP AT SCREEN.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ENDLOOP.
    LOOP AT action_table_ui-cols INTO ls_columns.
      ls_columns-screen-input = 1.
      MODIFY action_table_ui-cols FROM ls_columns.
    ENDLOOP.
    CALL METHOD g_editor->set_readonly_mode
      EXPORTING
        readonly_mode = 0.    " read-only mode; eq 0: OFF ; ne 0: ON
  ENDIF.



  "Populate assigned to Name:
  IF plmt_audit_act_ui-z_assignedto IS NOT INITIAL.
    lv_username = plmt_audit_act_ui-z_assignedto.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = lv_username
        cache_results = 'X'
      IMPORTING
        address       = ls_address
      TABLES
        return        = lt_return.
    IF lt_return IS INITIAL.
      assignedto_name = ls_address-fullname.
    ELSE.
      assignedto_name = ''.
    ENDIF.
  ELSE.
    assignedto_name = ''.
  ENDIF.


  "When GUID changes
  IF plmt_audit_act_ui-guid <> gv_guid.
    clear lt_action_ui.
    gv_guid = plmt_audit_act_ui-guid.
    CLEAR gv_textline.
    CALL METHOD g_editor->delete_text.
    SELECT * FROM zplm_actions
       INTO CORRESPONDING FIELDS
       OF TABLE lt_action_ui
       WHERE guid = plmt_audit_act_ui-guid.

    SORT lt_action_ui ASCENDING BY indx.

    LOOP AT lt_action_ui INTO ls_action_ui.
      SELECT SINGLE long_desc FROM zplm_actions_t
        INTO ls_action_ui-long_desc
        WHERE guid = ls_action_ui-guid
          AND indx = ls_action_ui-indx.
      MODIFY lt_action_ui FROM ls_action_ui.
    ENDLOOP.
    "action_table_ui-top_line = 1.
  ENDIF.

*  "Add a dummy entry:
*  LOOP AT lt_action_ui INTO ls_action_ui
*    WHERE guid IS INITIAL.
*  ENDLOOP.
*
*  IF sy-subrc = 4.
*    "No entry found, add a dummy
*    CLEAR ls_action_ui.
*    APPEND ls_action_ui TO lt_action_ui.
*  ENDIF.
*
*
  DESCRIBE TABLE lt_action_ui LINES lv_lines.
  action_table_ui-lines    = lv_lines.
  action_table_ui-fixed_cols = 3.



  CALL METHOD g_editor->set_toolbar_mode
    EXPORTING
      toolbar_mode = 0.    " visibility of toolbar; eq 0: OFF ; ne 0: ON

  IF gv_textline = 0.
    CALL METHOD g_editor->set_visible
      EXPORTING
        visible = abap_false.
  ENDIF.
ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  DATA: ls_text TYPE char256,
        lt_text LIKE TABLE OF ls_text.

  IF gv_textline <> 0.
    CALL METHOD g_editor->get_text_as_stream
      EXPORTING
        only_when_modified = 1    " get text only when modified
      IMPORTING
        text               = lt_text.    " text as stream with carrige retruns and linefeeds

    IF lt_text IS NOT INITIAL.
      READ TABLE lt_action_ui INTO ls_action_ui INDEX gv_textline.
      CLEAR ls_action_ui-long_desc.
      LOOP AT lt_text INTO ls_text.
        CONCATENATE ls_action_ui-long_desc ls_text INTO ls_action_ui-long_desc.
      ENDLOOP.
      MODIFY lt_action_ui FROM ls_action_ui INDEX gv_textline.
    ENDIF.
  ENDIF.

  DATA okcode LIKE sy-ucomm.
  CALL 'DYNP_OKCODE_GET' ID 'FCODE' FIELD okcode.
  IF okcode(1) EQ '='.
    SHIFT okcode.
  ENDIF.

  IF okcode = 'DISTEXT'.
    data: lv_line type i.
    get cursor line lv_line.
    gv_textline = action_table_ui-current_line + lv_line - 1.
    READ TABLE lt_action_ui INTO ls_action_ui INDEX gv_textline.

    CALL METHOD g_editor->set_textstream
      EXPORTING
        text = ls_action_ui-long_desc.

    CALL METHOD g_editor->set_visible
      EXPORTING
        visible = abap_true.
  ENDIF.

  "If new
  IF okcode = 'ZNEW'.
    SELECT  indx FROM zplm_actions
  INTO lv_topindx WHERE guid = gv_guid ORDER BY indx DESCENDING.
      EXIT.
    ENDSELECT.

    lv_topindx = lv_topindx + 1.
    CLEAR: ls_plm_actions, ls_plm_actions_t.
    ls_plm_actions-guid = gv_guid.
    ls_plm_actions-indx = lv_topindx.
    MOVE-CORRESPONDING ls_plm_actions TO ls_plm_actions_t.

    INSERT INTO zplm_actions VALUES ls_plm_actions.
    INSERT INTO zplm_actions_t VALUES ls_plm_actions_t.
    CLEAR gv_guid. "Force refresh
  ENDIF.

  "If save
  IF okcode = 'SAVE'.
    lv_changes = abap_true.


    LOOP AT lt_action_ui INTO ls_action_ui.
      "Update item.
      CLEAR ls_plm_actions.
      MOVE-CORRESPONDING ls_action_ui TO ls_plm_actions.
      MOVE-CORRESPONDING ls_action_ui TO ls_plm_actions_t.
      MODIFY zplm_actions FROM ls_plm_actions.
      MODIFY zplm_actions_t FROM ls_plm_actions_t.
    ENDLOOP.
    CLEAR gv_textline.
  ENDIF.


  TRY.
      "Make a change to the action to trigger save logic.
      GET BADI lr_badi.
      CALL BADI lr_badi->set_screen_data
        EXPORTING
          is_plmt_audit_act_ui = plmt_audit_act_ui
          ir_action            = ir_action.
    CATCH cx_badi_not_implemented.                      "#EC NO_HANDLER
  ENDTRY.

ENDMODULE.                 " USER_COMMAND_9001  INPUT


*----------------------------------------------------------------------*
*  MODULE action_table_ui_move OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE action_table_ui_move OUTPUT.
  MOVE-CORRESPONDING ls_action_ui TO zplm_actions.
*  LOOP AT SCREEN.
*    IF screen-name CS 'ZPLM_ACTIONS-DETAILS' AND zplm_actions-code IS INITIAL.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ELSEIF screen-name CS 'DISPLAY_TEXT_BTN' AND zplm_actions-code IS INITIAL.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDMODULE.                    "ACTION_TABLE_UI_MOVE OUTPUT


*----------------------------------------------------------------------*
*  MODULE action_table_ui_modify INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE action_table_ui_modify INPUT.
  MOVE-CORRESPONDING zplm_actions TO ls_action_ui.
  ls_action_ui-guid = gv_guid.
  MODIFY lt_action_ui
    FROM ls_action_ui
    INDEX action_table_ui-current_line.
ENDMODULE.                    "ACTION_TABLE_UI_MODIFY INPUT
