*&---------------------------------------------------------------------*
*&  Include           Z_PS_AM_DISPLAY_SCR
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   Z_PS_AM_DISPLAY                               *
* Program Include    :   Z_PS_AM_DISPLAY_SCR                           *
* Author             :                                                 *
* Date               :   April,13, 2018                                *
* Technical Contact  :   Ashok kumar madasu                            *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :                                                 *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*=======================================================================
* AT SELECTION-SCREEN
*=======================================================================
SELECTION-SCREEN BEGIN OF BLOCK s4 WITH FRAME TITLE text-t05.

PARAMETERS:r_ANLKL RADIOBUTTON GROUP RAD DEFAULT 'X' USER-COMMAND cmd.
SELECT-OPTIONS:s_ANLKL FOR ANKA-ANLKL MODIF ID DA.

PARAMETERS:r_AFASL RADIOBUTTON GROUP RAD.
SELECT-OPTIONS: s_AFASL FOR T090NAZ-AFASL MODIF ID DD.
SELECTION-SCREEN END OF BLOCK s4.
*=======================================================================
* AT SELECTION-SCREEN OUTPUT
*=======================================================================
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF r_ANLKL = 'X'.
      IF screen-group1 = 'DA'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'DD'.
        screen-active = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'DA'.
        screen-active = 0.
      ENDIF.
      IF screen-group1 = 'DD'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
