*&---------------------------------------------------------------------*
*&  Include           Z_PS_AM_DISPLAY_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   Z_PS_AM_DISPLAY                               *
* Program Include    :   Z_PS_AM_DISPLAY_F01                           *
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
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DATA .

  CLEAR:gt_T090NAT[],gt_T090NAZ[],gt_T090NS[],gt_T090NA[].
  IF R_ANLKL EQ 'X'.
    SELECT ANLKL XLOEV XSPEA INTO TABLE GT_ANKA FROM ANKA WHERE ANLKL IN S_ANLKL.
    IF SY-SUBRC IS INITIAL.
      DELETE GT_ANKA WHERE XSPEA EQ 'X'.
      DELETE GT_ANKA WHERE XLOEV EQ 'X'.
    ENDIF.
    IF GT_ANKA[] IS NOT INITIAL.
      SELECT ANLKL AFASL FROM ANKB INTO TABLE GT_ANKB FOR ALL ENTRIES IN GT_ANKA[]
                                   WHERE ANLKL = GT_ANKA-ANLKL
                                   AND AFAPL eq GC_CAD.
      IF SY-SUBRC IS INITIAL.
        SORT GT_ANKB BY AFASL.
        DELETE ADJACENT DUPLICATES FROM GT_ANKB COMPARING AFASL.
        SELECT AFAPL AFASL XAKTIV from T090NA INTO TABLE gt_T090NA
          FOR ALL ENTRIES IN GT_ANKB
                            where  AFAPL eq GC_CAD
                           and   AFASL = GT_ANKB-AFASL
                           and  XAKTIV EQ GC_FLAG.

        if sy-subrc is INITIAL and gt_T090NA[] IS NOT INITIAL.
          SELECT AFAPL AFASL AFATXT from T090NAT
                                    into table gt_T090NAT
                                    FOR ALL ENTRIES IN gt_T090NA
                                    where SPRAS eq sy-langu
                                    and   AFAPL eq GC_CAD
                                    and   AFASL eq gt_T090NA-AFASL.
          IF sy-subrc is INITIAL.
            sort gt_T090NAT by AFAPL AFASL.
          ENDIF.
          SELECT AFAPL AFASL METSTU from T090NAZ
                                    into table gt_T090NAZ
                                    where AFAPL eq GC_CAD
                                    and   AFASL in s_AFASL.
          IF sy-subrc is INITIAL.
            sort gt_T090NAZ by AFAPL AFASL.
            select AFAPL METSTU AFPROZ from T090NS
                                       into table gt_T090NS
                                       FOR ALL ENTRIES IN gt_T090NAZ
                                       where AFAPL eq GC_CAD
                                       and METSTU = gt_T090NAZ-METSTU.
            if sy-subrc is INITIAL.
              sort gt_T090NS by AFAPL METSTU.
            endif.
          ENDIF.
        else.
          WRITE:'No data found'.
        ENDIF.
      ELSE.
        WRITE:'No data found'.
      ENDIF.
    ELSE.
      WRITE:'No data found'.
    ENDIF.
  ELSE.
    SELECT AFAPL AFASL XAKTIV from T090NA INTO TABLE gt_T090NA
                                where  AFAPL eq GC_CAD
                               and   AFASL in s_AFASL
                               and  XAKTIV EQ GC_FLAG.

    if sy-subrc is INITIAL and gt_T090NA[] IS NOT INITIAL.
      SELECT AFAPL AFASL AFATXT from T090NAT
                                into table gt_T090NAT
                                FOR ALL ENTRIES IN gt_T090NA
                                where SPRAS eq sy-langu
                                and   AFAPL eq GC_CAD
                                and   AFASL eq gt_T090NA-AFASL.
      IF sy-subrc is INITIAL.
        sort gt_T090NAT by AFAPL AFASL.
      ENDIF.
      SELECT AFAPL AFASL METSTU from T090NAZ
                                into table gt_T090NAZ
                                where AFAPL eq GC_CAD
                                and   AFASL in s_AFASL.
      IF sy-subrc is INITIAL.
        sort gt_T090NAZ by AFAPL AFASL.
        select AFAPL METSTU AFPROZ from T090NS
                                   into table gt_T090NS
                                   FOR ALL ENTRIES IN gt_T090NAZ
                                   where AFAPL eq GC_CAD
                                   and METSTU = gt_T090NAZ-METSTU.
        if sy-subrc is INITIAL.
          sort gt_T090NS by AFAPL METSTU.
        endif.
      ENDIF.
    else.
      WRITE:'No data found'.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_DATA.

  CLEAR: GT_FINAL[].
  IF R_ANLKL EQ 'X'.
    LOOP AT GT_ANKB INTO gs_ankb.
      READ TABLE gt_T090NAT INTO gs_T090NAT with key AFASL = gs_ankb-afasl.
      IF sy-subrc is INITIAL.
        gs_final-ANLKL     = gs_ankb-ANLKL.
        gs_final-AFASL     = gs_T090NAT-AFASL.
        gs_final-AFATXT    = gs_T090NAT-AFATXT.
      ENDIF.
      READ TABLE gt_T090NAZ into gs_T090NAZ with key AFAPL = gs_T090NAT-AFAPL
                                                      AFASL = gs_T090NAT-AFASL BINARY SEARCH.
      LOOP AT gt_T090Ns into gs_T090Ns where METSTU = gs_T090NAZ-METSTU.
        gs_final-AFPROZ = gs_T090Ns-AFPROZ.
        APPEND gs_final to gt_final.
        clear:gs_T090Ns.
      ENDLOOP.
      clear:gs_T090NAT,gs_T090NAZ,gs_final,GS_ankb.
    ENDLOOP.
  ELSE.
    LOOP AT gt_T090NAT INTO gs_T090NAT.

      gs_final-AFASL     = gs_T090NAT-AFASL.
      gs_final-AFATXT    = gs_T090NAT-AFATXT.
      READ TABLE gt_T090NAZ into gs_T090NAZ with key AFAPL = gs_T090NAT-AFAPL
                                                     AFASL = gs_T090NAT-AFASL BINARY SEARCH.
      LOOP AT gt_T090Ns into gs_T090Ns where METSTU = gs_T090NAZ-METSTU.
        gs_final-AFPROZ = gs_T090Ns-AFPROZ.
        APPEND gs_final to gt_final.
        clear:gs_T090Ns.
      ENDLOOP.
      clear:gs_T090NAT,gs_T090NAZ,gs_final.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DISPLAY .
  PERFORM f_BUILD_FIELDCATALOG .
  if gt_final[] is not INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_INTERFACE_CHECK           = ' '
*       I_BYPASSING_BUFFER          = ' '
*       I_BUFFER_ACTIVE             = ' '
        I_CALLBACK_PROGRAM          = sy-repid
*       I_CALLBACK_PF_STATUS_SET    = ' '
*       I_CALLBACK_USER_COMMAND     = ' '
*       I_CALLBACK_TOP_OF_PAGE      = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*       I_CALLBACK_HTML_END_OF_LIST = ' '
*       I_STRUCTURE_NAME            =
*       I_BACKGROUND_ID             = ' '
*       I_GRID_TITLE                =
*       I_GRID_SETTINGS             =
*       IS_LAYOUT                   =
        IT_FIELDCAT                 = gt_FIELDCATALOG
*       IT_EXCLUDING                =
*       IT_SPECIAL_GROUPS           =
*       IT_SORT                     =
*       IT_FILTER                   =
*       IS_SEL_HIDE                 =
*       I_DEFAULT                   = 'X'
*       I_SAVE                      = ' '
*       IS_VARIANT                  =
*       IT_EVENTS                   =
*       IT_EVENT_EXIT               =
*       IS_PRINT                    =
*       IS_REPREP_ID                =
*       I_SCREEN_START_COLUMN       = 0
*       I_SCREEN_START_LINE         = 0
*       I_SCREEN_END_COLUMN         = 0
*       I_SCREEN_END_LINE           = 0
*       I_HTML_HEIGHT_TOP           = 0
*       I_HTML_HEIGHT_END           = 0
*       IT_ALV_GRAPHICS             =
*       IT_HYPERLINK                =
      TABLES
        T_OUTTAB                    = gt_final[]
      EXCEPTIONS
        PROGRAM_ERROR               = 1
        OTHERS                      = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  else.
    WRITE: 'No data found.'.
  endif.
ENDFORM.                    " F_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUILD_FIELDCATALOG .
  Perform field_cat using 'ANLKL' 'Asset Class' .
  Perform field_cat using 'AFASL' 'Depreciation Key' .
  Perform field_cat using 'AFPROZ' 'Depreciation Rate' .
  Perform field_cat using 'AFATXT' 'Depreciation Key Description' .
ENDFORM.                    " F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0167   text
*      -->P_0168   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING    p_field p_label.
  if R_ANLKL EQ ' ' and p_field eq 'ANLKL'.
    gs_fieldcatalog-NO_OUT = 'X'.
  ENDIF.
  gs_fieldcatalog-fieldname   = p_field.
  gs_fieldcatalog-seltext_m   = p_label.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear  gs_fieldcatalog.

ENDFORM.                    " FIELD_CAT*&---------------------------------------------------------------------*
*&  Include           Z_PS_AM_DISPLAY_F01
*&---------------------------------------------------------------------*
