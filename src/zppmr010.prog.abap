REPORT ZPPMR010 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        February 1998
*  Description:
*     - The purpose of this program is to produce Summary of Capital
*       Expenditures Report by Project Type, Priority Code and
*       Div/dept. - selection is by WBS element
************************************************************************
*2014/09/15 gymana SDP73514 Change selection screen to allow multiple
*                           company codes.
* 02/01/10 mokhan #939 Changes to print report for companies that deal
*                      in currencies other than Canadian dollar.
*                      for example: EMPIRE State.
* 98/05/05 md7140 #--- Projects - planning only - must be picked up
* 98/04/22 md7140 #--- Pick up budget at lowest level
* 98/03/23 md7140 #--- Created program - used ZPPMR007 for start
*                      Eliminate TOTAL COMMITMENT & REMAINING PLAN col.
***** ***** ***** ***** ****** ***** ***** ***** ***** ***** ***** *****
*  modified by Nancy Gilligan, OmniLogic Oct 21, 1998    D30K905350    *
*   (marked with OMNING)                                               *
*    - added version numbers to headers for planned values             *
*    - changed budget and actuals to select only version '000'         *
*----------------------------------------------------------------------*
* 2010/07/07 btboundy - TR843 Include plan amount from COSS table.
************************************************************************

TABLES: BPJA, COSP, COSS, CSKU, JEST, PROJ, PRPS, T001, T247, TCJ1T,
        TCN7T.

DATA:
   BEGIN OF WA  OCCURS 0,
      PRART         LIKE PRPS-PRART,          "Project Type
      PSPRI         LIKE PRPS-PSPRI,          "Priority Type
      POSID         LIKE PRPS-POSID,          "Project Number
      OBJNR         LIKE PRPS-OBJNR,          "Object Number
      PSPHI         LIKE PRPS-PSPHI,
      PLAKZ         LIKE PRPS-PLAKZ,          "Planning element
      STUFE         LIKE PRPS-STUFE,          "Level Number
      VERNR         LIKE PRPS-VERNR,          "Divison (person respons)
      POST1         LIKE PRPS-POST1,          "WBS Description
   END OF WA.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       POSID(4)      TYPE C,                    "WBS Element
       PRART         LIKE PRPS-PRART,           "Project Type
       DISTGRP(1)    TYPE C,                    "WA for subtotalling
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       VERNR         LIKE PRPS-VERNR,           "Div/dept
       PROJECT       LIKE PRPS-POSID,           "Project (7 digits)
       PSPHI         LIKE PRPS-PSPHI,           "Internal Project No.
       OBJNR         LIKE PRPS-OBJNR,           "Object Number
       WBSPOST1      LIKE PRPS-POST1,           "WBS Description
       POST1         LIKE PROJ-POST1,           "Project Description
       verna          like proj-verna,          "vernr Name
       actual(8)     type p decimals 2,           "Actual Amount
       PLAN(8)     TYPE P DECIMALS 2,           "Plan Amount
       variance(8) type p decimals 2,           "Variance (actual-plan)
       LOCKED(8)   TYPE P DECIMALS 2,           "Locked budget
*      commit(8)   type p decimals 2,           "Total Committed
       ANNPLAN(8)  TYPE P DECIMALS 2,           "Annual Plan
*      remplan(8)  type p decimals 2,           "Remaining Plan
*      uncommit(8) type p decimals 2,           "Uncommited Budget
   END OF BIG_TABLE.


data:   value         like cosp-wkg001,
        ANNVALUE      LIKE COSP-WKG001,
        actual          like cosp-wkg001,
        plan          like cosp-wkg001,
*       commit        like cosp-wkg001,
        annplan       like cosp-wkg001,
*       remplan       like cosp-wkg001,
        variance type p decimals 2,              "Variance (actual-plan)
        POST1         LIKE PRPS-POST1,           "Temp - description
        POST2         LIKE PROJ-POST1,           "Temp - description
        LOCKED        LIKE BPJA-WTJHV,
*       uncommit      like bpja-wtjhv,
        VERNA         LIKE PROJ-VERNA,
        P_BEGNME(9),
        P_ENDNME(9),
        BEGMTH(2)     TYPE C,
        ENDMTH(2)     TYPE C.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: S_CCODE FOR T001-BUKRS  DEFAULT 'UGL',     "SDP73514
                SDIV FOR PRPS-VERNR.                       "Division
PARAMETERS:     P_VERS  LIKE COSP-VERSN DEFAULT '0',       "Plan version
                P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS: SMTH FOR SY-DATUM+4(2) OBLIGATORY          "Months
                      DEFAULT SY-DATUM+4(2),
                 SWBS    FOR PRPS-POSID+7(4),             "WBS elements
                 SPRART FOR PRPS-PRART,                   "Project Type
                 SPSPRI FOR PRPS-PSPRI.                   "Priority Type
SELECTION-SCREEN END OF BLOCK BOX.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-036.
PARAMETER: P_CHKWBS AS CHECKBOX,
           P_CHKPRJ AS CHECKBOX,
           P_CHKDET AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX2.
*------------------------  AT SELECTION-SCREEN  ------------------------

AT SELECTION-SCREEN.
  IF ( P_CHKWBS IS INITIAL ) AND
     ( P_CHKPRJ IS INITIAL ) AND
     ( P_CHKDET IS INITIAL ).
    MESSAGE E100 WITH 'Must select at least one report'.
  ENDIF.

AT SELECTION-SCREEN ON S_CCODE.                          "SDP73514
  SELECT SINGLE * FROM T001                        "Get Company Name
    WHERE BUKRS IN S_CCODE.                               "SDP73514

AT SELECTION-SCREEN ON SMTH.
  SELECT SINGLE * FROM T247                        "Get Month Name(Begin)
    WHERE SPRAS = 'E'
    AND MNR = SMTH+3(2).
  P_BEGNME = T247-LTX.

  P_ENDNME = SPACE.
  IF SMTH+5(2) <> '00'.
    SELECT SINGLE * FROM T247                      "Get Month Name(End)
       WHERE SPRAS = 'E'
         AND MNR = SMTH+5(2).
    P_ENDNME = T247-LTX.
  ENDIF.
  BEGMTH = SMTH+3(2).
  ENDMTH = SMTH+5(2).
  IF ENDMTH = '00'.
    ENDMTH = BEGMTH.
  ENDIF.
*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.
  SELECT PRART PSPRI POSID OBJNR PSPHI PLAKZ STUFE VERNR POST1
    INTO WA FROM PRPS
    WHERE PBUKR IN S_CCODE              "Company Code         SDP73514
      AND PKOKR = '10'                 "Controlling area
      AND VERNR IN SDIV                "Division
      AND LOEVM <> 'X'                 "Not flagged deleted
      AND PRART IN SPRART              "Project type
      AND PSPRI IN SPSPRI              "Priority type
      AND ( BELKZ = 'X'                "Lowest level - actuals
         OR PLAKZ = 'X' ).             "Lowest level - planning 98/05/06
    IF WA-POSID+7(4) IN SWBS.               "WBS selection
      IF WA-POSID+5(2) CO '1234567890'.    "Eliminates templates
        PERFORM GET_FISCAL_DATA.
      ENDIF.
    ENDIF.
  ENDSELECT.

  IF SY-SUBRC EQ 0.
    PERFORM DISPLAY_TABLE.
  ENDIF.


*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: /1 TEXT-RPT, SY-REPID,                          "Report Id
         80 T001-BUTXT,                                  "Company Name
        140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,  "Client
        63 TEXT-003,                                     "Report Title
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.            "Page Number
  WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS,              "Version
        75 TEXT-004, P_FYEAR.                            "Fiscal Year
  WRITE: / TEXT-037.                                 "Divisions Requested
  IF SDIV+3(8) = 0.
    WRITE: 11 TEXT-038.
  ELSEIF SDIV+11(8) = 0.
    WRITE: 11 SDIV+3(8).
  ELSE.
    WRITE: 11 SDIV+1(2), SDIV+3(8), SDIV+11(8).    "End divisions
  ENDIF.
  IF P_ENDNME = SPACE.                                   "Time Frame
    WRITE: 80 P_BEGNME.
  ELSE.
    WRITE: 70 P_BEGNME, TEXT-024, P_ENDNME, TEXT-025.
  ENDIF.
  FORMAT INTENSIFIED ON.
  PERFORM PRINT_HEADINGS.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

  SORT BIG_TABLE BY POSID PRART DISTGRP PSPRI VERNR PROJECT.
  IF P_CHKWBS = 'X'.                "Summarized at wbs level
    LOOP AT BIG_TABLE.
      MOVE BIG_TABLE-WBSPOST1 TO POST1.

      AT END OF POSID.                              "WBS element
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 2 BIG_TABLE-POSID, POST1.
        PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT LAST.                                     "Company TOTAL
        SUM.
        ULINE.
        PERFORM PRINT_VERT.
        WRITE: 2 TEXT-029.
        PERFORM AMOUNT_TOTALS.
        ULINE.
        WRITE: /.
        WRITE: /80 TEXT-028.
      ENDAT.
    ENDLOOP.
  ENDIF.

  IF P_CHKPRJ = 'X'.                "Summarized at project type
    NEW-PAGE.
    LOOP AT BIG_TABLE.
      MOVE BIG_TABLE-WBSPOST1 TO POST1.
      MOVE BIG_TABLE-VERNA    TO VERNA.

      AT NEW POSID.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        WRITE: 2 BIG_TABLE-POSID, POST1.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT NEW PRART.
        FORMAT COLOR COL_POSITIVE.
        PERFORM GET_PROJECT_TYPE.
        PERFORM PRINT_VERT.
        WRITE: 7 TCJ1T-PRATX.
        FORMAT COLOR COL_POSITIVE.
      ENDAT.

      AT NEW PSPRI.
        FORMAT COLOR COL_HEADING.
        PERFORM GET_PRIORITY_TYPE.
        PERFORM PRINT_VERT.
        WRITE: 11 TCN7T-KTEXT.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF VERNR.
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 13 VERNA.
        PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF PSPRI.
        SUM.
        FORMAT COLOR COL_HEADING.
        PERFORM GET_PRIORITY_TYPE.
        PERFORM PRINT_VERT.
        WRITE: 11 TCN7T-KTEXT.
        PERFORM AMOUNT_TOTALS.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF DISTGRP.
        IF BIG_TABLE-DISTGRP <> 'Z'.
          SUM.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
          PERFORM PRINT_VERT.
          IF BIG_TABLE-DISTGRP = '1'.
            WRITE: 9 TEXT-033.           "TOTAL NEW BUSINESS
          ELSEIF BIG_TABLE-DISTGRP = '2'. "TOTAL ADDITIONS
            WRITE: 9 TEXT-035.
          ELSEIF BIG_TABLE-DISTGRP = '3'. "TOTAL REPLACEMENTS
            WRITE: 9 TEXT-034.
          ENDIF.
          PERFORM AMOUNT_TOTALS.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
        ENDIF.
      ENDAT.

      AT END OF PRART.             "Summarized at project level
        SUM.
        PERFORM GET_PROJECT_TYPE.
        FORMAT COLOR COL_POSITIVE.
        PERFORM PRINT_VERT.
        WRITE: 7 TCJ1T-PRATX(35).
        FORMAT COLOR COL_BACKGROUND.
        PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF POSID.             "Summarized at WBS element
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 81(89) SY-ULINE.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        WRITE: 2 BIG_TABLE-POSID, POST1.
        PERFORM AMOUNT_TOTALS.
        FORMAT COLOR COL_BACKGROUND.
        ULINE.
      ENDAT.

      AT LAST.                                     "Company TOTAL
        SUM.
        ULINE.
        PERFORM PRINT_VERT.
        WRITE: 2 TEXT-029.
        PERFORM AMOUNT_TOTALS.
        ULINE.
        WRITE: /.
        WRITE: /80 TEXT-028.
      ENDAT.
    ENDLOOP.
  ENDIF.

  IF P_CHKDET = 'X'.                "Summarized at project type
    NEW-PAGE.
    LOOP AT BIG_TABLE.
      MOVE BIG_TABLE-WBSPOST1 TO POST1.
      MOVE BIG_TABLE-POST1    TO POST2.
      MOVE BIG_TABLE-VERNA    TO VERNA.

      AT NEW POSID.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        WRITE: 2 BIG_TABLE-POSID, POST1.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT NEW PRART.
        FORMAT COLOR COL_POSITIVE.
        PERFORM GET_PROJECT_TYPE.
        PERFORM PRINT_VERT.
        WRITE: 7 TCJ1T-PRATX(35).
        FORMAT COLOR COL_POSITIVE.
      ENDAT.

      AT NEW PSPRI.
        FORMAT COLOR COL_HEADING.
        PERFORM GET_PRIORITY_TYPE.
        PERFORM PRINT_VERT.
        WRITE: 11 TCN7T-KTEXT.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF PROJECT.

        SUM.
*        move big_table-actual to big_table-commit.
*        if big_table-locked > big_table-commit.
*           move big_table-locked to big_table-commit.
*        endif.
*        big_table-remplan = big_table-plan - big_table-commit.
        PERFORM PRINT_VERT.
        WRITE: 15 BIG_TABLE-PROJECT(10), POST2.
        PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF VERNR.
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 13 VERNA.
        PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF PSPRI.
        SUM.
        FORMAT COLOR COL_HEADING.
        PERFORM GET_PRIORITY_TYPE.
        PERFORM PRINT_VERT.
        WRITE: 11 TCN7T-KTEXT.
        PERFORM AMOUNT_TOTALS.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF DISTGRP.
        IF BIG_TABLE-DISTGRP <> 'Z'.
          SUM.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
          PERFORM PRINT_VERT.
          IF BIG_TABLE-DISTGRP = '1'.
            WRITE: 9 TEXT-033.           "TOTAL NEW BUSINESS
          ELSEIF BIG_TABLE-DISTGRP = '2'. "TOTAL ADDITIONS
            WRITE: 9 TEXT-035.
          ELSEIF BIG_TABLE-DISTGRP = '3'. "TOTAL REPLACEMENTS
            WRITE: 9 TEXT-034.
          ENDIF.
          PERFORM AMOUNT_TOTALS.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
        ENDIF.
      ENDAT.

      AT END OF PRART.             "Summarized at project level
        SUM.
        PERFORM GET_PROJECT_TYPE.
        FORMAT COLOR COL_POSITIVE.
        PERFORM PRINT_VERT.
        WRITE: 7 TCJ1T-PRATX(35).
        FORMAT COLOR COL_BACKGROUND.
        PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF POSID.             "Summarized at WBS element
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 81(89) SY-ULINE.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        WRITE: 2 BIG_TABLE-POSID, POST1.
        PERFORM AMOUNT_TOTALS.
        FORMAT COLOR COL_BACKGROUND.
        ULINE.
      ENDAT.

      AT LAST.                                     "Company TOTAL
        SUM.
        ULINE.
        PERFORM PRINT_VERT.
        WRITE: 2 TEXT-029.
        PERFORM AMOUNT_TOTALS.
        ULINE.
        WRITE: /.
        WRITE: /80 TEXT-028.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "DISPLAY_TABLE

*------------------------  AMOUNT_TOTALS  ------------------------------
FORM AMOUNT_TOTALS.
  WRITE: T001-WAERS           UNDER TEXT-041,   "Currency  Issue 939
         BIG_TABLE-PLAN       UNDER TEXT-008,   "YTD Plan
         BIG_TABLE-ACTUAL     UNDER TEXT-010,   "Actuals
         BIG_TABLE-VARIANCE   UNDER TEXT-011,   "Variance(actual-plan
         BIG_TABLE-LOCKED     UNDER TEXT-013,   "Appr Budget Comt
*        big_table-commit     under text-015,   "Total Commitment
         BIG_TABLE-ANNPLAN    UNDER TEXT-016.   "Annual Plan
*        big_table-remplan    under text-017.   "Remaining Plan
ENDFORM.                    "AMOUNT_TOTALS
*----------------------- PRINT_HEADINGS -------------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
  FORMAT COLOR 2 ON.
  WRITE: /1(170) SY-ULINE.
  PERFORM PRINT_VERT.
*   WRITE:  81 TEXT-008, 117 TEXT-011,                  "Issue 939
  WRITE:  72 TEXT-041, 81 TEXT-008, 117 TEXT-011,           "Issue 939
         135 TEXT-013, 153 TEXT-016.
*          99 text-013, 117 text-015, 135 text-016, 153 text-017.
  PERFORM PRINT_VERT.
  WRITE: 7 TEXT-005, 18 TEXT-007,
         TEXT-009 UNDER TEXT-008, 99 TEXT-010,
         TEXT-012 UNDER TEXT-011, TEXT-014 UNDER TEXT-013,
         TEXT-009 UNDER TEXT-016.
*         text-032 under text-015, text-009 under text-016,
*         text-009 under text-017.
  PERFORM PRINT_VERT.
* write in versions numbers:                                      OMNING
  WRITE: TEXT-040 UNDER TEXT-008, P_VERS,           "ytd plan
         TEXT-040 UNDER TEXT-010, '0',              "actuals
         TEXT-040 UNDER TEXT-016, P_VERS.           "annual plan
  ULINE.
  PERFORM PRINT_VERT.
  FORMAT COLOR 2 OFF.
ENDFORM.                    "PRINT_HEADINGS

*--------------------------  PRINT_VERT  -------------------------------
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
* write:  /1 sy-vline, 44 sy-vline, 62 sy-vline, 80 sy-vline,
* WRITE:  /1 SY-VLINE,  80 SY-VLINE,                        "Issue 939
  WRITE:  /1 SY-VLINE,  70 SY-VLINE,  80 SY-VLINE,          "Issue 939

          98 SY-VLINE, 116 SY-VLINE, 134 SY-VLINE, 152 SY-VLINE,
         170 SY-VLINE.
ENDFORM.                    "PRINT_VERT

*&---------------------------------------------------------------------*
*&      Form  heading_vernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form heading_vernr.
  PERFORM PRINT_VERT.
  FORMAT COLOR COL_POSITIVE.
  WRITE: 2 TEXT-023, VERNA.
  FORMAT COLOR COL_BACKGROUND.
ENDFORM.                    "heading_vernr

*&---------------------------------------------------------------------*
*&      Form  HEADING_PROJECT_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HEADING_PROJECT_TYPE.
  SELECT SINGLE * FROM TCJ1T
      WHERE LANGU = 'E' AND
            PRART = BIG_TABLE-PRART.
  PERFORM PRINT_VERT.
  FORMAT COLOR COL_TOTAL.
  WRITE: 4 TCJ1T-PRATX.
  FORMAT COLOR COL_BACKGROUND.
ENDFORM.                    "HEADING_PROJECT_TYPE

*&---------------------------------------------------------------------*
*&      Form  GET_PROJECT_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_PROJECT_TYPE.
  SELECT SINGLE * FROM TCJ1T
      WHERE LANGU = 'E'
        AND PRART = BIG_TABLE-PRART.
ENDFORM.                    "GET_PROJECT_TYPE

*&---------------------------------------------------------------------*
*&      Form  GET_PRIORITY_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_PRIORITY_TYPE.
  SELECT SINGLE * FROM TCN7T
    WHERE LANGU = 'E'
      AND NPRIO = BIG_TABLE-PSPRI.
ENDFORM.                    "GET_PRIORITY_TYPE

*&---------------------------------------------------------------------*
*&      Form  HEADING_PRIORITY_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HEADING_PRIORITY_TYPE.
  SELECT SINGLE * FROM TCN7T
    WHERE LANGU = 'E'
      AND NPRIO = BIG_TABLE-PSPRI.
  FORMAT COLOR COL_POSITIVE.
  IF SY-SUBRC <> 0.
    PERFORM PRINT_VERT.
    WRITE: 6 TEXT-021, BIG_TABLE-PSPRI, TEXT-022.
  ELSE.
    PERFORM PRINT_VERT.
    WRITE: 4 TCN7T-KTEXT.
  ENDIF.
  FORMAT COLOR COL_BACKGROUND.
ENDFORM.                    "HEADING_PRIORITY_TYPE

*&---------------------------------------------------------------------*
*&      Form  BIG_TABLE_DUMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BIG_TABLE_DUMP.
  WRITE: /2 BIG_TABLE-POSID,
              111 BIG_TABLE-PLAN,            "YTD Plan
              129 BIG_TABLE-ACTUAL,          "Actuals
              147 BIG_TABLE-VARIANCE,        "Variance(actual - plan
              165 BIG_TABLE-LOCKED,          "Appr Budget Comt
              183 BIG_TABLE-VARIANCE,        "Total Commitment
*                183 big_table-commit,          "Total Commitment
              201 BIG_TABLE-ANNPLAN.         "Annual Plan
*                237 big_table-uncommit.        "Uncommitted Budget
ENDFORM.                    "BIG_TABLE_DUMP
*&---------------------------------------------------------------------*
*&      Form  GET_FISCAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_FISCAL_DATA.
*write: / wa-prart, wa-pspri, wa-posid, wa-objnr, wa-psphi, wa-plakz.
* COST TOTALS - External Postings
  SELECT * FROM COSP
           WHERE OBJNR = WA-OBJNR          "Matching objects
             AND GJAHR = P_FYEAR           "Fiscal Year selected
             AND ( VERSN = P_VERS            "Version
                 OR VERSN = '000' )                             "omning
             AND WRTTP IN ('01','04')      "Record with actuals & plans
             AND BEKNZ IN ('S','H','L').    "Debit/Credit Indicator

    IF T001-WAERS = 'CAD'.                     "Canadian dollar   Issue 939
      ADD COSP-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.   "Issue 939
      ADD COSP-WKG001 FROM 1 TO 12 GIVING ANNVALUE.         "Issue 939
    ELSE.                                                   "Issue 939
      ADD COSP-WOG001 FROM BEGMTH TO ENDMTH GIVING VALUE.   "Issue 939
      ADD COSP-WOG001 FROM 1 TO 12 GIVING ANNVALUE.         "Issue 939
    ENDIF.                                                  "Issue 939

*    ADD COSP-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.      "Issue 939
*    ADD COSP-WKG001 FROM 1 TO 12 GIVING ANNVALUE.            "Issue 939

    CASE COSP-WRTTP.
      WHEN '01'.
        IF COSP-VERSN = P_VERS.                           "omning
          PLAN = PLAN + VALUE.
          ANNPLAN = ANNPLAN + ANNVALUE.
        ENDIF.                                            "omning
        CLEAR: VALUE, ANNVALUE.
      WHEN '04'.
        IF COSP-VERSN = '000'.                            "omning
          actual = actual + value.
        ENDIF.                                            "omning
        CLEAR VALUE.
    ENDCASE.

  ENDSELECT.


* COST TOTALS - Internal Postings
  SELECT * FROM COSS
           WHERE OBJNR = WA-OBJNR          "Matching objects
             AND GJAHR = P_FYEAR           "Fiscal Year selected
             AND ( VERSN = P_VERS            "Version
                 OR VERSN = '000' )                        "omning
*             AND WRTTP IN ('04').         "Record with actuals             TR843
             AND WRTTP IN ('01','04').     "Record with actuals and Plan    TR843
*             and beknz in ('S','H','L').   "Debit/Credit Indicator
    IF T001-WAERS = 'CAD'.                   "Canadian dollar   Issue 939
      ADD COSS-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.   "Issue 939
    ELSE.                                                   "Issue 939
      ADD COSS-WOG001 FROM BEGMTH TO ENDMTH GIVING VALUE.   "Issue 939
    ENDIF.                                                  "Issue 939

*    ADD COSS-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.      "Issue 939

    CASE COSS-WRTTP.
      WHEN '01'.                          "                                TR843
        IF COSS-VERSN = P_VERS.           "omning                          TR843
          PLAN = PLAN + VALUE.            "                                TR843
          ANNPLAN = ANNPLAN + ANNVALUE.   "                                TR843
        ENDIF.                            "omning                          TR843
        CLEAR: VALUE, ANNVALUE.           "                                TR843
      WHEN '04'.
        IF COSS-VERSN = '000'.                            "omning
          actual = actual + value.
        ENDIF.                                            "omning
        CLEAR VALUE.
    ENDCASE.
  ENDSELECT.
* End of COST TOTALS - Internal Postings

*  if  wa-stufe = '1'.
  PERFORM GET_BUDGET_DATA.
*  endif.

  IF PLAN <> 0 OR           "Eliminate lines with all zeros
     ACTUAL <> 0 OR
     LOCKED <> 0 OR
     ANNPLAN <> 0.
    PERFORM BUILD_TABLE.
  ENDIF.
  CLEAR: PLAN, ACTUAL, VARIANCE, LOCKED, ANNPLAN, ANNVALUE.
ENDFORM.                    "GET_FISCAL_DATA

*-----------------------------------------------------------------------
*     FORM GET_BUDGET_DATA
*-----------------------------------------------------------------------
*   Description:
*   - The routine is used to calculate the budgeted values
*   check bjpa-wrttp for budget(41) vs plan(01)
*   check jest-stat  for status:  I0001 ==> created
*                                 I0002 ==> released?
*                                 I0060 ==> planned
*                                 I0063 ==> locked
*                                 I0067 ==> budgeted
*                                 I0076 ==> deleted
*-----------------------------------------------------------------------
FORM GET_BUDGET_DATA.
  SELECT * FROM BPJA                 "Budget Info
      WHERE OBJNR = WA-OBJNR
        AND WRTTP IN ('01', '41')
        AND GJAHR = P_FYEAR
        AND VERSN = '000'.                               "omning
    SELECT * FROM JEST
       WHERE OBJNR = WA-OBJNR
       AND INACT <> 'X'
       AND STAT IN ('I0060','I0067','I0076')        "md7140 97/10/06
       ORDER BY STAT.
      CASE BPJA-WRTTP.
*             when '01'.
*               case jest-stat.
*               when 'I0060'.
*                  annplan = bpja-wtjhr.                  "Planned
*               endcase.
        WHEN '41'.
          CASE JEST-STAT.
            WHEN 'I0067'.
              LOCKED = BPJA-WTJHR.         "Unlocked 97/10/06
            WHEN 'I0076'.
              CLEAR: ANNPLAN, LOCKED.      "Deleted
          ENDCASE.
      ENDCASE.
    ENDSELECT.

  ENDSELECT.
ENDFORM.                    "GET_BUDGET_DATA
*-----------------------------------------------------------------------
*     FORM BUILD_TABLE
*-----------------------------------------------------------------------
*   Description:
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE.
  CLEAR: BIG_TABLE-PLAN,    BIG_TABLE-ACTUAL, BIG_TABLE-VARIANCE,
         BIG_TABLE-LOCKED,  BIG_TABLE-ANNPLAN.
*          big_table-uncommit.
  MOVE 'Z'               TO BIG_TABLE-DISTGRP.
  IF WA-PRART BETWEEN '01' AND '05'.
    IF WA-PSPRI CO  '1234'.
      MOVE '1'         TO BIG_TABLE-DISTGRP.
    ELSEIF WA-PSPRI CO '58'.
      MOVE '2'         TO BIG_TABLE-DISTGRP.
    ELSEIF WA-PSPRI CO '79CDE'.
      MOVE '3'         TO BIG_TABLE-DISTGRP.
    ENDIF.
  ENDIF.
  MOVE WA-POST1          TO BIG_TABLE-WBSPOST1.
  MOVE WA-POSID+7(4)     TO BIG_TABLE-POSID.
  MOVE WA-POSID(7)       TO BIG_TABLE-PROJECT.
  move wa-posid(2)       to big_table-vernr.
  MOVE WA-PRART          TO BIG_TABLE-PRART.
  MOVE WA-PSPRI          TO BIG_TABLE-PSPRI.
  MOVE WA-PSPHI          TO BIG_TABLE-PSPHI.
  SELECT SINGLE * FROM PROJ
         WHERE PSPNR = WA-PSPHI.
  MOVE PROJ-POST1        TO BIG_TABLE-POST1.
  MOVE PROJ-VERNA        TO BIG_TABLE-VERNA.
  MOVE WA-OBJNR          TO BIG_TABLE-OBJNR.
  MOVE PLAN              TO BIG_TABLE-PLAN.
  MOVE ACTUAL            TO BIG_TABLE-ACTUAL.
  variance = plan - actual.
  MOVE VARIANCE          TO BIG_TABLE-VARIANCE.

  MOVE LOCKED           TO BIG_TABLE-LOCKED.

  MOVE ANNPLAN           TO BIG_TABLE-ANNPLAN.
*   big_table-uncommit = uncommit.

*   if  big_table-plan <> 0 or           "Eliminate lines with all zeros
*       big_table-actual <> 0 or
*       big_table-locked <> 0 or
*       big_table-annplan <> 0.
  APPEND BIG_TABLE.
*   endif.
ENDFORM.                    "BUILD_TABLE
