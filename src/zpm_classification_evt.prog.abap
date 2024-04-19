*&---------------------------------------------------------------------*
*&  Include           ZPM_CLASSIFICATION_EVT
*&---------------------------------------------------------------------*

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_pth = ''.
      IF screen-group1 = 'M1'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM get_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_variant_inputhelp USING p_layout.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM create_table.

  IF p_alv IS NOT INITIAL.
    PERFORM display_alv.
  ENDIF.

  IF p_pth IS NOT INITIAL.
    PERFORM write_appl_server.
  ENDIF.
