REPORT ZPWBR002 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 255
                MESSAGE-ID ZP.
************************************************************************
*  Author:   Dorothy Bialkowska
*  Brief Description:
*     - The purpose of this program is to produce a CAPITAL EXPENDITURES
*       (by PROJECT REFERENCE NUMBER) report for selected projects.
************************************************************************
* 97/10/06 md7140 #225 Actuals should not in CO $ not Transaction $
*                      Changed WTG001 to WKG001
************************************************************************

TABLES:   BPJA, COSP, JEST, PROJ, PRPS, TCJ1T, TCN7T, T001T.

data:     value       like     cosp-wkg001,
          PLAN        LIKE     VALUE,
          COST        LIKE     VALUE,
          BUDGET      LIKE     BPJA-WTJHR,
          APPROVAL    LIKE     BUDGET.

DATA:    BEGIN OF BIG_TAB OCCURS 100000,
               WBS(4)             TYPE     C,
               STEP               LIKE     PRPS-STUFE,
               CENTER             LIKE     PRPS-FKSTL,
               PROJNR             LIKE     PRPS-POSKI,
               OBJNR              LIKE     PRPS-OBJNR,
               TITLE              LIKE     PRPS-POST1,
               plan               like     cosp-wkg001,
               cost               like     cosp-wkg001,
               budget             like     cosp-wkg001,
               approval           like     cosp-wkg001,
               FLAG1(1),
         END OF BIG_TAB.

DATA:    BEGIN OF NEAT_TAB OCCURS 100000.
        INCLUDE STRUCTURE BIG_TAB.
DATA:    END OF NEAT_TAB.

DATA:     LEN           TYPE I,
          COUNTER       TYPE I,
          FLAG(2)             ,
          START         TYPE I,
          SYMBOL(4)     TYPE C,
          STRING        TYPE I,
          TEMPNME       LIKE PRPS-POST1,
          tot1          like cosp-wkg001,
          TOT2          LIKE TOT1,
          TOT3          LIKE TOT1,
          TOT4          LIKE TOT1,
          GRAND1        LIKE TOT1,
          GRAND2        LIKE TOT1,
          GRAND3        LIKE TOT1,
          GRAND4        LIKE TOT1,
          TEMP          LIKE TOT1.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS: P_CCODE LIKE T001T-BUKRS DEFAULT 'UGL',
            P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS S_PROJ FOR PROJ-PSPID.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 6(26) TEXT-013.
PARAMETER P_VERS LIKE TKVST-VERSI DEFAULT '0'.
SELECTION-SCREEN END OF LINE .
SELECTION-SCREEN END OF BLOCK BOX.

* End of selection screen
*-----------------------------------------------------------------------

START-OF-SELECTION.
  SELECT * FROM PROJ
     WHERE PSPID IN S_PROJ
     AND VBUKR = P_CCODE
     AND VKOKR = '10'.
    IF SY-SUBRC EQ 0.
      COUNTER = 1.
    ENDIF.
    SELECT * FROM PRPS
      WHERE PSPHI = PROJ-PSPNR
      AND PBUKR = P_CCODE
      AND PKOKR = '10'
      ORDER BY PSPHI PSPNR.
      IF COUNTER EQ 1.
        PERFORM SHOW_NAME.
        MOVE PRPS-POST1 TO TEMPNME.
        COUNTER = COUNTER + 1.
        PERFORM GET_TYPE.
        PERFORM GET_PRIORITY.
      ELSE.
        PERFORM GET_FISCAL_DATA.
        PERFORM GET_BUDGET.
        SELECT SINGLE * FROM JEST
           WHERE OBJNR = PRPS-OBJNR
           AND STAT = 'I0063'.
        IF SY-SUBRC EQ 0.
          APPROVAL = BUDGET.
        ENDIF.

        LEN = STRLEN( PRPS-POSID ).
        CHECK LEN GE 4.
        START = LEN - 4.
        SYMBOL = PRPS-POSID+START(4).

        PERFORM BUILD_TABLE.
      ENDIF.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      PERFORM DISPLAY_TABLE.
      WRITE: /102 TEXT-011 ,130 TOT1, 164 TOT2, 198 TOT3, 232 TOT4.
      WRITE: 1 SY-VLINE, 119 SY-VLINE, 153 SY-VLINE, 187 SY-VLINE,
             221 SY-VLINE, 255 SY-VLINE.
      WRITE:/ SY-ULINE.

      GRAND1 = GRAND1 + TOT1.
      GRAND2 = GRAND2 + TOT2.
      GRAND3 = GRAND3 + TOT3.
      GRAND4 = GRAND4 + TOT4.

      CLEAR: TOT1, TOT2, TOT3, TOT4.

      SKIP 1.
      FORMAT INTENSIFIED OFF COLOR 5 ON.
      WRITE : /1(255) TEXT-035, 26 TEMPNME.
      FORMAT INTENSIFIED ON COLOR 5 OFF.
    ENDIF.
    SKIP 3.
  ENDSELECT.
  ULINE.
  FORMAT COLOR 7 ON.
  WRITE: /96 TEXT-023, 130 GRAND1, 164 GRAND2, 198 GRAND3, 232 GRAND4.
  ULINE.
*-----------------------------------------------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-RPT, SY-REPID, 120 TEXT-001 COLOR COL_GROUP,
       220 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS.
* write: / text-002, sy-datum, 240 text-003, sy-pagno.
* write: / sy-uzeit under sy-datum, sy-repid under text-003.
* write: /105(150) text-001.
  ULINE.
  FORMAT INTENSIFIED ON.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************

*-----------------------------------------------------------------------
*     FORM GET_TYPE
*-----------------------------------------------------------------------
*   Description:
*   - Every project has a specific project type associated with it.
*     This subroutine finds out and displayes project type.
*-----------------------------------------------------------------------
FORM GET_TYPE.
  SELECT SINGLE * FROM TCJ1T
     WHERE LANGU = SY-LANGU
     AND PRART = PRPS-PRART.

  TRANSLATE TCJ1T-PRATX TO UPPER CASE.
  FORMAT INTENSIFIED OFF COLOR 4 ON.
  WRITE: /49(207) TEXT-034, 57 TEXT-028, 67 TCJ1T-PRATX.
  FORMAT INTENSIFIED ON COLOR 4 OFF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_PRIORITY
*-----------------------------------------------------------------------
*   Description:
*   - A project can have different priority within the same project
*     type.  This subruotine dispalyes the name of priority for project
*     type.
*-----------------------------------------------------------------------
FORM GET_PRIORITY.
  SELECT SINGLE * FROM TCN7T
     WHERE LANGU = SY-LANGU
     AND NPRIO = PRPS-PSPRI.
  FORMAT INTENSIFIED OFF COLOR 4 ON.
  WRITE: /49(207) TEXT-034, 57 TEXT-022, 67 TCN7T-KTEXT.
  FORMAT INTENSIFIED ON COLOR 4 OFF.
  SKIP 1.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_FISCAL_DATA
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine calculates YEAR-TO-DAY value of the approved plan
*     for a selected plan version and YEAR-TO-DAY value of the
*     actual cost.
*-----------------------------------------------------------------------
FORM GET_FISCAL_DATA.
  SELECT * FROM COSP
     WHERE OBJNR = PRPS-OBJNR
     AND  ( WRTTP = '01' OR WRTTP = '04' )
     AND GJAHR = P_FYEAR
     ORDER BY OBJNR.

    IF COSP-WRTTP = '01'.
      CHECK COSP-VERSN = P_VERS.
      PERFORM CALC_VALUE.
      PLAN = PLAN + VALUE.
      CLEAR VALUE.
    ENDIF.
    IF COSP-WRTTP = '04'.
      PERFORM CALC_VALUE.
      COST = COST + VALUE.
      CLEAR VALUE.
    ENDIF.

  ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM CALC_VALUE
*-----------------------------------------------------------------------
*   Description:
*   - Because YEAR-TO-DATE value has to be calculated from the beginig
*     of the year up to the current month, this procedure is used to do
*     so.
*-----------------------------------------------------------------------
FORM CALC_VALUE.
  IF SY-DATUM+4(2) = '01'.
    value = cosp-wkg001.
  ENDIF.
  IF SY-DATUM+4(2) = '02'.
    value = cosp-wkg001 + cosp-wkg002.
  ENDIF.
  IF SY-DATUM+4(2) = '03'.
    value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003.
  ENDIF.
  IF SY-DATUM+4(2) = '04'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004.
  ENDIF.
  IF SY-DATUM+4(2) = '05'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005.
  ENDIF.
  IF SY-DATUM+4(2) = '06'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005 + cosp-wkg006.
  ENDIF.
  IF SY-DATUM+4(2) = '07'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005 + cosp-wkg006 + cosp-wkg007.
  ENDIF.
  IF SY-DATUM+4(2) = '08'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005 + cosp-wkg006 + cosp-wkg007 + cosp-wkg008.
  ENDIF.
  IF SY-DATUM+4(2) = '09'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005 + cosp-wkg006 + cosp-wkg007 + cosp-wkg008 +
            cosp-wkg009.
  ENDIF.
  IF SY-DATUM+4(2) = '10'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005 + cosp-wkg006 + cosp-wkg007 + cosp-wkg008 +
            cosp-wkg009 + cosp-wkg010.
  ENDIF.
  IF SY-DATUM+4(2) = '11'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005 + cosp-wkg006 + cosp-wkg007 + cosp-wkg008 +
            cosp-wkg009 + cosp-wkg010 + cosp-wkg011.
  ENDIF.
  IF SY-DATUM+4(2) = '12'.
    VALUE =
            cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004 +
            cosp-wkg005 + cosp-wkg006 + cosp-wkg007 + cosp-wkg008 +
            cosp-wkg009 + cosp-wkg010 + cosp-wkg011 + cosp-wkg012.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_BUDGET
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine is used to calculate total budget for WBS element.
*-----------------------------------------------------------------------
FORM GET_BUDGET.
  SELECT * FROM BPJA
     WHERE OBJNR = PRPS-OBJNR
     AND WRTTP = '41'
     AND GJAHR = P_FYEAR
     ORDER BY OBJNR.
    BUDGET = BUDGET + BPJA-WTJHV.
  ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BUILD_TABLE
*-----------------------------------------------------------------------
*   Description:
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE.
  MOVE SYMBOL         TO BIG_TAB-WBS.
  MOVE PRPS-FKSTL     TO BIG_TAB-CENTER.
  MOVE PRPS-OBJNR     TO BIG_TAB-OBJNR.
  MOVE PRPS-POSKI     TO BIG_TAB-PROJNR.
  MOVE PRPS-POST1     TO BIG_TAB-TITLE.
  MOVE PLAN           TO BIG_TAB-PLAN.
  MOVE COST           TO BIG_TAB-COST.
  MOVE BUDGET         TO BIG_TAB-BUDGET.
  MOVE APPROVAL       TO BIG_TAB-APPROVAL.
  MOVE PRPS-STUFE     TO BIG_TAB-STEP.

  APPEND BIG_TAB.
  CLEAR BIG_TAB.

  CLEAR: PLAN, COST, VALUE, BUDGET, APPROVAL.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displayes data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.
  DATA:     L_TITLE     LIKE    PRPS-POST1,
            L_NUMBER    LIKE    PRPS-POSKI,
            L_CENTER    LIKE    PRPS-FKSTL.

  PERFORM PRINT_HEADINGS.
  SORT BIG_TAB BY WBS STEP CENTER.


  FORMAT  COLOR 2 ON INTENSIFIED OFF.
  LOOP AT BIG_TAB.
    MOVE BIG_TAB-TITLE TO L_TITLE.
    MOVE BIG_TAB-PROJNR TO L_NUMBER.
    MOVE BIG_TAB-CENTER TO L_CENTER.

    AT END OF WBS.
      SUM.
      TOT1 = TOT1 + BIG_TAB-PLAN.
      TOT2 = TOT2 + BIG_TAB-COST.
      TOT3 = TOT3 + BIG_TAB-BUDGET.
      TOT4 = TOT4 + BIG_TAB-APPROVAL.

      MOVE BIG_TAB-WBS          TO NEAT_TAB-WBS.

      MOVE L_TITLE              TO NEAT_TAB-TITLE.
      MOVE L_NUMBER             TO NEAT_TAB-PROJNR.
      MOVE L_CENTER             TO NEAT_TAB-CENTER.
      MOVE BIG_TAB-PLAN         TO NEAT_TAB-PLAN.
      MOVE BIG_TAB-COST         TO NEAT_TAB-COST.
      MOVE BIG_TAB-BUDGET       TO NEAT_TAB-BUDGET.
      MOVE BIG_TAB-APPROVAL     TO NEAT_TAB-APPROVAL.
      APPEND NEAT_TAB.
      CLEAR NEAT_TAB.
    ENDAT.

    MOVE BIG_TAB-STEP TO STRING.
    MOVE BIG_TAB-WBS+0(2) TO FLAG.

    AT END OF WBS+0(2).
      SUM.
      MOVE BIG_TAB-PLAN TO TEMP.

      IF STRING EQ 5.
        READ TABLE BIG_TAB WITH KEY STEP = 4 WBS+0(2) = FLAG.
        IF SY-SUBRC EQ 0.
          MOVE TEMP TO BIG_TAB-PLAN.
          MODIFY BIG_TAB INDEX SY-TABIX.
          MOVE 'X'  TO BIG_TAB-FLAG1.
          MODIFY BIG_TAB INDEX SY-TABIX.
          MOVE BIG_TAB TO NEAT_TAB.
          MODIFY NEAT_TAB INDEX SY-TABIX.
        ENDIF.
      ENDIF.

      IF STRING EQ 4.
        READ TABLE BIG_TAB WITH KEY STEP = 3 WBS+0(2) = FLAG.
        IF SY-SUBRC EQ 0.
          MOVE TEMP TO BIG_TAB-PLAN.
          MODIFY BIG_TAB INDEX SY-TABIX.
          MOVE 'X'  TO BIG_TAB-FLAG1.
          MODIFY BIG_TAB INDEX SY-TABIX.
          MOVE BIG_TAB TO NEAT_TAB.
          MODIFY NEAT_TAB INDEX SY-TABIX.
        ENDIF.
      ENDIF.

      IF STRING EQ 3.
        READ TABLE BIG_TAB WITH KEY STEP = 2 WBS+0(2) = FLAG.
        IF SY-SUBRC EQ 0.
          MOVE TEMP TO BIG_TAB-PLAN.
          MODIFY BIG_TAB INDEX SY-TABIX.
          MOVE 'X'  TO BIG_TAB-FLAG1.
          MODIFY BIG_TAB INDEX SY-TABIX.
          MOVE BIG_TAB TO NEAT_TAB.
          MODIFY NEAT_TAB INDEX SY-TABIX.
        ENDIF.
      ENDIF.
    ENDAT.
  ENDLOOP.

  LOOP AT NEAT_TAB.
    IF ( NEAT_TAB-PLAN NE 0 ) OR ( NEAT_TAB-COST NE 0 ) OR
       ( NEAT_TAB-BUDGET NE 0 ) OR ( NEAT_TAB-APPROVAL NE 0 ).
      PERFORM PRINT_VERT.
      WRITE: 6 NEAT_TAB-WBS, 19 NEAT_TAB-TITLE,
             77 NEAT_TAB-PROJNR, 105 NEAT_TAB-CENTER, 130
             NEAT_TAB-PLAN, 164 NEAT_TAB-COST, 198 NEAT_TAB-BUDGET,
             232  NEAT_TAB-APPROVAL.
    ENDIF.
  ENDLOOP.

  CLEAR: BIG_TAB, NEAT_TAB.
  REFRESH: BIG_TAB, NEAT_TAB.
  WRITE: /1(255) SY-ULINE.
  FORMAT COLOR 2 OFF INTENSIFIED OFF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM SHOW_NAME
*-----------------------------------------------------------------------
*   Description:
*   - The name of the project is printed.
*-----------------------------------------------------------------------
FORM SHOW_NAME.
  FORMAT INTENSIFIED OFF COLOR 5 ON.
  WRITE : /1(255) TEXT-034 , 11(12) PRPS-POSKI, 24 PRPS-POST1.
  FORMAT INTENSIFIED ON COLOR 5 OFF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_HEADINGS
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
  FORMAT COLOR 2 ON.
  WRITE: /1(255) SY-ULINE.
  PERFORM PRINT_VERT.
  WRITE: 3 TEXT-005, 32 TEXT-006, 77 TEXT-014, 103 TEXT-008, 135
         TEXT-007, 165 TEXT-009, 198 TEXT-010, 234 TEXT-018.
  PERFORM PRINT_VERT.
  WRITE: 130 TEXT-016, 231 TEXT-019.
  WRITE: /1(255) SY-ULINE.
  FORMAT COLOR 2 OFF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_VERT
*-----------------------------------------------------------------------
*   Description:
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE: /1 SY-VLINE, 16 SY-VLINE, 71 SY-VLINE, 97 SY-VLINE, 119
          SY-VLINE, 153 SY-VLINE, 187 SY-VLINE, 221 SY-VLINE,
          255 SY-VLINE.
ENDFORM.
