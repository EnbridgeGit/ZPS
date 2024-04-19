REPORT ZPPMR001 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 255
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        January 1997
*  Description:
*     - The purpose of this program is to produce Summary of Capital
*       Expenditures Report by Division, Project Type & Priority Type
************************************************************************
* 02/01/03 mokhan #939 Changes to print report for companies that deal
*                      in currencies other than Canadian dollar.
*                      for example: EMPIRE State.
* 99/03/16 md7140 #--- Remove all zero rows
* 99/01/26 md7140 #624 Change calculation of commitment column
* 98/10/21 omning #--- Nancy Gilligan
*              - added version numbers to headers for planned values
*              - changed budget and actuals to select only version '000'
* 98/05/01 md7140 #--- Annual plan should be from COSP table, not BPJA
* 98/03/23 md7140 #--- Project can have a character in p 5 nn-nn-cnn
* 98/03/10 md7140 Added TOTAL NEW BUSINESS & TOTAL REPLACEMENT
*                 summary.
* 97/10/06 md7140 Change wkg001 to wkg001 to pick up CO dollars
*                 Changed CALC_VALUE & CALC_VALUE_COSS to ADD statement
* 97/09/30 md7140 Eliminate all zero line & Uncommitted budget
* 97/06/03 md7140 Transport:D30K....... - Fix DIV/DEPT Name
* 97/04/03 md7140 retransport due to production problems
* 97/03/21 MD7140 Multi division selection, Company total
*                 Deleted projects
*                 Added COSS table - Cost Totals - Internal Postings
*----------------------------------------------------------------------
* 2010/07/07 btboundy - TR843 Include plan amount from COSS table.
*
************************************************************************

TABLES: BPJA, COSP, COSS, CSKU, JEST, PROJ, PRPS, T001, T247, TCJ1T,
        TCN7T,
        COBRA.                     "Settlement Rule for Order Settlement

DATA:
   BEGIN OF WA  OCCURS 0,
      PRART         LIKE PRPS-PRART,          "Project Type
      PSPRI         LIKE PRPS-PSPRI,          "Priority Type
      POSID         LIKE PRPS-POSID,          "Project Number
      OBJNR         LIKE PRPS-OBJNR,          "Object Number
      PSPHI         LIKE PRPS-PSPHI,
      PLAKZ         LIKE PRPS-PLAKZ,          "Planning element
      STUFE         LIKE PRPS-STUFE,          "Level Number
      PROJACTUAL(8)   TYPE P DECIMALS 2,           "Actual Amount
      PROJLOCKED(8)   TYPE P DECIMALS 2,           "Locked budget
      PRIORCOMMIT(8)  TYPE P DECIMALS 2,
      PRTYPCOMMIT(8)  TYPE P DECIMALS 2,
      DEPTCOMMIT(8)   TYPE P DECIMALS 2,
      COMCOMMIT(8)    TYPE P DECIMALS 2,
      DISTCOMMIT(8)    TYPE P DECIMALS 2,                   "99/02/05
      REMPLAN(8)      TYPE P DECIMALS 2,           "Remaining Plan
      PRIORREMPLAN(8) TYPE P DECIMALS 2,
      PRTYPREMPLAN(8) TYPE P DECIMALS 2,
      DEPTREMPLAN(8)  TYPE P DECIMALS 2,
      COMREMPLAN(8)   TYPE P DECIMALS 2,
      DISTREMPLAN(8)   TYPE P DECIMALS 2,                   "99/02/05
      ANNPLAN(8)      TYPE P DECIMALS 2,
   END OF WA.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       DEPT(2),                                 "Div/Dept
       PRART         LIKE PRPS-PRART,           "Project Type
       DISTGRP(1)    TYPE C,                    "Distribution Group
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       PROJECT       LIKE PRPS-POSID,           "Project (7 digits)
       PSPHI         LIKE PRPS-PSPHI,           "Internal Project No.
*      posid         like prps-posid,           "WBS Element
*      objnr         like prps-objnr,           "Object Number
       POST1         LIKE PROJ-POST1,           "Project Description
       VERNA          LIKE PROJ-VERNA,          "Dept Name
       actual(8)     type p decimals 2,           "Actual Amount
       PLAN(8)     TYPE P DECIMALS 2,           "Plan Amount
       variance(8) type p decimals 2,           "Variance (actual-plan)
       LOCKED(8)   TYPE P DECIMALS 2,           "Locked budget
       COMMIT(8)   TYPE P DECIMALS 2,           "Total Committed
       ANNPLAN(8)  TYPE P DECIMALS 2,           "Annual Plan
       UNCOMMIT(8) TYPE P DECIMALS 2,           "Uncommited Budget
       BZDAT       TYPE I,          "Asset Value Date Entered 0/1 #624
   END OF BIG_TABLE.


data:   value         like cosp-wkg001,
        ANNVALUE      LIKE COSP-WKG001,
        begvalue      like cosp-wkg001,
        endvalue      like cosp-wkg001,
        actual          like cosp-wkg001,
        plan          like cosp-wkg001,
        commit        like cosp-wkg001,
        annplan       like cosp-wkg001,
        remplan       like cosp-wkg001,
        variance type p decimals 2,              "Variance (actual-plan)
        POST1         LIKE PROJ-POST1,
        P_MONTH(2),
        P_BEGNME(9),
        P_ENDNME(9),
        A_BEGMTH(2)     TYPE C,
        A_ENDMTH(2)     TYPE C,
        LOCKED        LIKE BPJA-WTJHV,
        UNCOMMIT      LIKE BPJA-WTJHV,
        VERNA         LIKE PROJ-VERNA,
        BZDAT(1)      TYPE C.                               "#624
*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS: P_CCODE LIKE T001T-BUKRS DEFAULT 'UGL',
            P_VERS  LIKE COSP-VERSN DEFAULT '0',  "Plan version
            P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
            P_BEGMTH(2)         DEFAULT SY-DATUM+4(2),
            P_ENDMTH(2).
SELECT-OPTIONS: S_PSPID  FOR PROJ-PSPID,                    "#624
                S_VERNR  FOR PROJ-VERNR.                    "#624
SELECTION-SCREEN END OF BLOCK BOX.

*-----------------------  END of SELECTION SCREEN-----------------------

A_BEGMTH = P_BEGMTH.
IF  P_ENDMTH = SPACE.
  A_ENDMTH = P_BEGMTH - 0.
ELSE.
  A_ENDMTH = P_ENDMTH - 0.
ENDIF.

START-OF-SELECTION.

  SELECT SINGLE * FROM T001                    "Get Company Name
       WHERE BUKRS = P_CCODE.


  SELECT * FROM PROJ                                        "#624
      WHERE PSPID IN S_PSPID
        AND VERNR IN S_VERNR.

    SELECT PRART PSPRI POSID OBJNR PSPHI PLAKZ STUFE
      INTO WA FROM PRPS
      WHERE PBUKR = P_CCODE        "Company Code
        AND PKOKR = '10'           "Controlling area
        AND PSPHI = PROJ-PSPNR                              "#624
        AND LOEVM <> 'X'.
      IF WA-POSID+5(2) CO '1234567890'.    "Eliminates templates 98/03/23
        PERFORM GET_FISCAL_DATA.
      ENDIF.
    ENDSELECT.                       "PRPS
  ENDSELECT.                        "PROJ

*----------------------------------------------------------------------*

  SELECT SINGLE * FROM T247                   "Get Month Name(Begin)
      WHERE SPRAS = 'E'
        AND MNR = P_BEGMTH.
  P_BEGNME = T247-LTX.
  P_ENDNME = SPACE.
  IF P_ENDMTH <> SPACE.
    SELECT SINGLE * FROM T247                "Get Month Name(End)
           WHERE SPRAS = 'E' AND
                 MNR = P_ENDMTH.
    P_ENDNME = T247-LTX.
  ENDIF.

  IF SY-SUBRC EQ 0.
    PERFORM REMOVE_ZERO_ROWS.
    PERFORM DISPLAY_TABLE.
    WRITE: / TEXT-043 UNDER TEXT-006.
  ENDIF.


*-----------------------------------------------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: /1 TEXT-RPT, SY-REPID,                          "Report Id
        106 T001-BUTXT,                                  "Company Name
        200 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,  "Client
       100 TEXT-003,                                     "Report Title
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.            "Page Number
  WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS,              "Version
       104 TEXT-004, P_FYEAR.                            "Fiscal Year
  IF P_ENDMTH = SPACE.                                   "Time Frame
    WRITE: /110 P_BEGNME.
  ELSE.
    WRITE: /100 P_BEGNME, TEXT-024, P_ENDNME, TEXT-025.
  ENDIF.
  ULINE.
  FORMAT INTENSIFIED ON.
  PERFORM PRINT_HEADINGS.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*------------------------- REMOVE_ZERO_ROWS ----------------------------
* Routine to remove rows with all zeros
*-----------------------------------------------------------------------
FORM REMOVE_ZERO_ROWS.                                      "99/03/16
  LOOP AT BIG_TABLE.
    DELETE BIG_TABLE WHERE PLAN    = 0
                       AND ACTUAL  = 0
                       AND LOCKED  = 0
                       AND ANNPLAN = 0.
  ENDLOOP.
ENDFORM.                    "REMOVE_ZERO_ROWS
*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

  BZDAT = 'N'.
  SORT BIG_TABLE BY DEPT PRART DISTGRP PSPRI PROJECT.
  LOOP AT BIG_TABLE.
    MOVE BIG_TABLE-POST1   TO POST1.
    MOVE BIG_TABLE-VERNA   TO VERNA.
    ADD BIG_TABLE-ACTUAL   TO WA-PROJACTUAL.
    ADD BIG_TABLE-LOCKED   TO WA-PROJLOCKED.
    ADD BIG_TABLE-ANNPLAN  TO WA-ANNPLAN.
    IF BIG_TABLE-BZDAT <> '0'.                              "#624
      BZDAT = 'Y'.                                          "#624
    ENDIF.                                                  "#624
*  perform big_table_dump.                "Dump for problem solving
    AT NEW DEPT.                            "Change in Div/Dept
      NEW-PAGE.
      PERFORM HEADING_DEPT.
    ENDAT.

    AT NEW PRART.                           "Change in Project Type
      PERFORM HEADING_PROJECT_TYPE.
    ENDAT.

    AT NEW PSPRI.                           "Change in Priority Type
      PERFORM HEADING_PRIORITY_TYPE.
    ENDAT.



    AT END OF PROJECT.                      "PROJECT TOTALS
      IF BZDAT = 'Y'.                         "if activation date     #624
      ELSE.
        IF WA-PROJACTUAL = 0.
          WA-PROJACTUAL = WA-PROJLOCKED.
        ELSEIF WA-PROJLOCKED = 0.
*        continue
        ELSEIF WA-PROJLOCKED > WA-PROJACTUAL.
          WA-PROJACTUAL = WA-PROJLOCKED.
        ENDIF.
      ENDIF.

      WA-REMPLAN = WA-ANNPLAN - WA-PROJACTUAL.
      SUM.
*     if big_table-plan   <> 0 or big_table-actual  <> 0 or
*        big_table-locked <> 0 or big_table-annplan <> 0.
      PERFORM PRINT_VERT.
      WRITE: 54 BIG_TABLE-PROJECT,        "Proj#
             70 POST1,                    "Project Desc
            111 BIG_TABLE-PLAN,           "YTD Plan
            129 BIG_TABLE-ACTUAL,         "Actuals
            147 BIG_TABLE-VARIANCE,       "Variance(actual-plan
            165 BIG_TABLE-LOCKED,         "Appr Budget Comt
            183 WA-PROJACTUAL,            "Total Commitment
            201 BIG_TABLE-ANNPLAN,        "Annual Plan
            219 WA-REMPLAN,               "Remaining Plan
            239 BZDAT,                    "I/S/D
            244 T001-WAERS.               "Currency   Issue 939
*     endif.
      BZDAT = 'N'.

*                 237 big_table-uncommit.       "Uncommitted Budget
      ADD WA-PROJACTUAL       TO WA-PRIORCOMMIT.
      ADD WA-REMPLAN          TO WA-PRIORREMPLAN.
      CLEAR: WA-PROJLOCKED, WA-PROJACTUAL, WA-REMPLAN, WA-ANNPLAN.
    ENDAT.
    AT END OF PSPRI.
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 54(202) SY-ULINE.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
      WRITE: 40 TEXT-020, TCN7T-KTEXT,       "Total Priority
            111 BIG_TABLE-PLAN,              "YTD Plan
            129 BIG_TABLE-ACTUAL,            "Actuals
            147 BIG_TABLE-VARIANCE,          "Variance(actual-plan
            165 BIG_TABLE-LOCKED,            "Appr Budget Comt
            183 WA-PRIORCOMMIT,              "Total Commitment
            201 BIG_TABLE-ANNPLAN,           "Annual Plan
            219 WA-PRIORREMPLAN,             "Remaining Plan
*                 237 big_table-uncommit.          "Uncommitted Budget
            244 T001-WAERS.                  "Currency   Issue 939
      FORMAT COLOR COL_BACKGROUND.
      PERFORM PRINT_VERT.
*          add wa-priorcommit       to wa-prtypcommit.       "99/02/05
*          add wa-priorremplan      to wa-prtypremplan.
      ADD WA-PRIORCOMMIT       TO WA-DISTCOMMIT.
      ADD WA-PRIORREMPLAN      TO WA-DISTREMPLAN.           "99/02/05
      CLEAR: WA-PRIORCOMMIT, WA-PRIORREMPLAN.
    ENDAT.
    AT END OF DISTGRP.
      IF BIG_TABLE-DISTGRP <> 'Z'.
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 40(215) SY-ULINE.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_TOTAL.
        IF BIG_TABLE-DISTGRP = '1'.
          WRITE: 25 TEXT-033.
        ELSEIF BIG_TABLE-DISTGRP = '2'.
          WRITE: 25 TEXT-035.
        ELSEIF BIG_TABLE-DISTGRP = '3'.
          WRITE: 25 TEXT-034.
        ENDIF.
        WRITE: 111 BIG_TABLE-PLAN,              "YTD Plan
              129 BIG_TABLE-ACTUAL,            "Actuals
              147 BIG_TABLE-VARIANCE,          "Variance(actual-plan
              165 BIG_TABLE-LOCKED,            "Appr Budget Comt
              183 WA-DISTCOMMIT,              "Total Commitment
              201 BIG_TABLE-ANNPLAN,           "Annual Plan
              219 WA-DISTREMPLAN,             "Remaining Plan
*                 237 big_table-uncommit.          "Uncommitted Budget
              244 T001-WAERS.                 "Currency   Issue 939
        FORMAT COLOR COL_BACKGROUND.
        PERFORM PRINT_VERT.
      ENDIF.
      ADD WA-DISTCOMMIT TO WA-PRTYPCOMMIT.
      ADD WA-DISTREMPLAN TO WA-PRTYPREMPLAN.
      CLEAR: WA-DISTCOMMIT, WA-DISTREMPLAN.
*         else.
*           add wa-distcommit to wa-prtypcommit.
*           add wa-distremplan to wa-prtypremplan.
    ENDAT.
    AT END OF PRART.
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 40(215) SY-ULINE.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_TOTAL.
      WRITE: 20 TEXT-020, TCJ1T-PRATX,       "Total Project Type
            111 BIG_TABLE-PLAN,              "YTD Plan
            129 BIG_TABLE-ACTUAL,            "Actuals
            147 BIG_TABLE-VARIANCE,          "Variance(actual-plan
            165 BIG_TABLE-LOCKED,            "Appr Budget Comt
            183 WA-PRTYPCOMMIT,              "Total Commitment
            201 BIG_TABLE-ANNPLAN,           "Annual Plan
            219 WA-PRTYPREMPLAN,             "Remaining Plan
*                 237 big_table-uncommit.          "Uncommitted Budget
            244 T001-WAERS.                  "Currency   Issue 939
      FORMAT COLOR COL_BACKGROUND.
      PERFORM PRINT_VERT.
      ADD WA-PRTYPCOMMIT       TO WA-DEPTCOMMIT.
      ADD WA-PRTYPREMPLAN      TO WA-DEPTREMPLAN.
      CLEAR: WA-PRTYPCOMMIT, WA-PRTYPREMPLAN.
    ENDAT.
    AT END OF DEPT.                              "Div/Dept Total
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 20(235) SY-ULINE.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_POSITIVE.
      WRITE: 2 TEXT-020, TEXT-023,
            111 BIG_TABLE-PLAN,              "YTD Plan
            129 BIG_TABLE-ACTUAL,            "Actuals
            147 BIG_TABLE-VARIANCE,          "Variance(actual-plan
            165 BIG_TABLE-LOCKED,            "Appr Budget Comt
            183 WA-DEPTCOMMIT,               "Total Commitment
            201 BIG_TABLE-ANNPLAN,           "Annual Plan
            219 WA-DEPTREMPLAN,              "Remaining Plan
*                 237 big_table-uncommit.          "Uncommitted Budget
            244 T001-WAERS.                  "Currency   Issue 939
      FORMAT COLOR COL_BACKGROUND.
      WRITE: /1(255) SY-ULINE.
      ADD WA-DEPTCOMMIT       TO WA-COMCOMMIT.
      ADD WA-DEPTREMPLAN      TO WA-COMREMPLAN.
      CLEAR:  WA-DEPTCOMMIT, WA-DEPTREMPLAN.
    ENDAT.
    AT LAST.
      NEW-PAGE.
      SUM.
      PERFORM PRINT_VERT.
      WRITE: TEXT-029,
            111 BIG_TABLE-PLAN,              "YTD Plan
            129 BIG_TABLE-ACTUAL,            "Actuals
            147 BIG_TABLE-VARIANCE,          "Variance(actual-plan
            165 BIG_TABLE-LOCKED,            "Appr Budget Comt
            183 WA-COMCOMMIT,                "Total Commitment
            201 BIG_TABLE-ANNPLAN,           "Annual Plan
            219 WA-COMREMPLAN,               "Remaining Plan
*                 237 big_table-uncommit.          "Uncommitted Budget
            244 T001-WAERS.                  "Currency   Issue 939
      WRITE: /1(255) SY-ULINE.
      CLEAR:  WA-DEPTCOMMIT, WA-DEPTREMPLAN.
      WRITE: /.
      WRITE: /105 TEXT-028.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "DISPLAY_TABLE

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
  WRITE:  69 TEXT-006, 118 TEXT-008,               150 TEXT-011,
         168 TEXT-013, 188 TEXT-015, 207 TEXT-016, 223 TEXT-017,
         237 TEXT-041, 243 TEXT-042.                        "Issue 939
*          237 TEXT-041.                             "Issue 939
  PERFORM PRINT_VERT.
  WRITE:  54 TEXT-005,
          67 TEXT-007, 118 TEXT-009, 135 TEXT-010, 150 TEXT-012,
         168 TEXT-014, 186 TEXT-032, 208 TEXT-009, 225 TEXT-009.
*         243 text-019.
  PERFORM PRINT_VERT.
* write in versions numbers:                                      OMNING
  WRITE: TEXT-040 UNDER TEXT-008, P_VERS,           "ytd plan
         TEXT-040 UNDER TEXT-010, '0',              "actuals
         TEXT-040 UNDER TEXT-016, P_VERS,           "annual plan
         TEXT-040 UNDER TEXT-017, P_VERS.           "remaining plan
  PERFORM PRINT_VERT.
  WRITE:  54(202) SY-ULINE.
  FORMAT COLOR 2 OFF.
ENDFORM.                    "PRINT_HEADINGS

*-----------------------------------------------------------------------
*     FORM PRINT_VERT
*-----------------------------------------------------------------------
*   Description:
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:  /1 SY-VLINE, 110 SY-VLINE, 128 SY-VLINE, 146 SY-VLINE,
         164 SY-VLINE, 182 SY-VLINE, 200 SY-VLINE, 218 SY-VLINE,
         236 SY-VLINE, 242 SY-VLINE.
*        236 sy-vline, 254 sy-vline.
ENDFORM.                    "PRINT_VERT

*&---------------------------------------------------------------------*
*&      Form  HEADING_DEPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HEADING_DEPT.
  PERFORM PRINT_VERT.
  FORMAT COLOR COL_POSITIVE.
  WRITE: 2 TEXT-023, VERNA.
  FORMAT COLOR COL_BACKGROUND.
ENDFORM.                    "HEADING_DEPT

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
  WRITE: 20 TCJ1T-PRATX.
  FORMAT COLOR COL_BACKGROUND.
ENDFORM.                    "HEADING_PROJECT_TYPE

*&---------------------------------------------------------------------*
*&      Form  HEADING_PRIORITY_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HEADING_PRIORITY_TYPE.
  SELECT SINGLE * FROM TCN7T
         WHERE LANGU = 'E' AND
               NPRIO = BIG_TABLE-PSPRI.
  FORMAT COLOR COL_GROUP.
  IF  SY-SUBRC <> 0.
    PERFORM PRINT_VERT.
    WRITE: 40 TEXT-021, BIG_TABLE-PSPRI, TEXT-022.
  ELSE.
    PERFORM PRINT_VERT.
    WRITE: 40 TCN7T-KTEXT.
  ENDIF.
  FORMAT COLOR COL_BACKGROUND.
ENDFORM.                    "HEADING_PRIORITY_TYPE

*&---------------------------------------------------------------------*
*&      Form  BIG_TABLE_DUMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BIG_TABLE_DUMP.
*     write: /2 big_table-posid,
  WRITE: /111 BIG_TABLE-PLAN,            "YTD Plan
              129 BIG_TABLE-ACTUAL,          "Actuals
              147 BIG_TABLE-VARIANCE,        "Variance(actual - plan
              165 BIG_TABLE-LOCKED,          "Appr Budget Comt
              183 BIG_TABLE-VARIANCE,        "Total Commitment
              183 BIG_TABLE-COMMIT,          "Total Commitment
              201 BIG_TABLE-ANNPLAN,         "Annual Plan
              219 WA-REMPLAN,                "Remaining Plan
              237 BIG_TABLE-UNCOMMIT.        "Uncommitted Budget
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
     WHERE OBJNR = WA-OBJNR                 "Matching objects
       AND GJAHR = P_FYEAR                  "Fiscal Year selected
       AND ( VERSN = P_VERS                 "Version
          OR VERSN = '000' )                "omning
       AND WRTTP IN ('01','04')             "Record with actuals & plans
       AND BEKNZ IN ('S','H','L').          "Debit/Credit Indicator
    IF T001-WAERS = 'CAD'.                     "Canadian dollar   Issue 939
      ADD COSP-WKG001 FROM A_BEGMTH TO A_ENDMTH GIVING VALUE.
      ADD COSP-WKG001 FROM 1 TO 12 GIVING ANNVALUE.         "98/05/01
    ELSE.                                                   "Issue 939
      ADD COSP-WOG001 FROM A_BEGMTH TO A_ENDMTH GIVING VALUE. "Issue 939
      ADD COSP-WOG001 FROM 1 TO 12 GIVING ANNVALUE.         "Issue 939
    ENDIF.                                                  "Issue 939

    CASE COSP-WRTTP.
      WHEN '01'.
        IF COSP-VERSN = P_VERS.                                "omning
          PLAN = PLAN + VALUE.
          ANNPLAN = ANNPLAN + ANNVALUE.                     "98/05/01
        ENDIF.                                                 "omning
        CLEAR: VALUE, ANNVALUE.
      WHEN '04'.
        IF COSP-VERSN = '000'.                                 "omning
          ACTUAL = ACTUAL + VALUE.
        ENDIF.                                                 "omning
        CLEAR VALUE.
    ENDCASE.
  ENDSELECT.                                                      "COSP


* COST TOTALS - Internal Postings
  SELECT * FROM COSS
    WHERE OBJNR = WA-OBJNR                          "Matching objects
      AND GJAHR = P_FYEAR                           "Fiscal Year selected
      AND ( VERSN = P_VERS                          "Version
         OR VERSN = '000' )                         "omning
*   AND WRTTP IN ('04').                            "Record with actuals            TR843
     AND WRTTP IN ('01', '04').                     "Record with actuals and Plan   TR843

    IF T001-WAERS = 'CAD'.                     "Canadian dollar   Issue 939
      ADD COSS-WKG001 FROM A_BEGMTH TO A_ENDMTH GIVING VALUE.
    ELSE.                                                   "Issue 939
      ADD COSS-WOG001 FROM A_BEGMTH TO A_ENDMTH GIVING VALUE. "Issue 939
    ENDIF.                                                  "Issue 939

    CASE COSS-WRTTP.
      WHEN '01'.                                    "                               TR843
        IF COSS-VERSN = P_VERS.                     "omning                         TR843
          PLAN = PLAN + VALUE.                      "                               TR843
          ANNPLAN = ANNPLAN + ANNVALUE.             "98/05/01                       TR843
        ENDIF.                                      "omning                         TR843
        CLEAR: VALUE, ANNVALUE.                     "                               TR843
      WHEN '04'.
        IF COSS-VERSN = '000'.                                     "omning
          ACTUAL = ACTUAL + VALUE.
        ENDIF.                                                     "omning
        CLEAR VALUE.
    ENDCASE.
  ENDSELECT.                                                      "COSS
* End of COST TOTALS - Internal Postings

  IF  WA-STUFE = '1'.
    PERFORM GET_BUDGET_DATA.
  ENDIF.

  PERFORM BUILD_TABLE.
  CLEAR: PLAN, ACTUAL, VARIANCE, LOCKED, COMMIT, ANNPLAN, REMPLAN,
         UNCOMMIT.
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
        AND WRTTP IN ('41')
*          and wrttp in ('01', '41')
        AND GJAHR = P_FYEAR
        AND VERSN = '000'.                               "omning
    SELECT * FROM JEST
       WHERE OBJNR = WA-OBJNR
       AND INACT <> 'X'
       AND STAT IN ('I0060','I0067','I0076')        "md7140 97/10/06
*          and stat in ('I0060','I0063','I0067','I0076')
       ORDER BY STAT.
      CASE BPJA-WRTTP.
*             when '01'.
*               case jest-stat.
*               when 'I0060'.
*                  annplan = bpja-wtjhr.                  "Planned
*               endcase.
        WHEN '41'.
          CASE JEST-STAT.
*               when 'I0063'.
*                  locked = bpja-wtjhr.                    "Locked
            WHEN 'I0067'.
              LOCKED = BPJA-WTJHR.         "Unlocked 97/10/06
            WHEN 'I0076'.
              CLEAR: ANNPLAN, LOCKED, UNCOMMIT.       "Deleted
          ENDCASE.
      ENDCASE.
    ENDSELECT.

  ENDSELECT.
ENDFORM.                    "GET_BUDGET_DATA
*-----------------------------------------------------------------------
FORM BUILD_TABLE.
  CLEAR: BIG_TABLE-PLAN,     BIG_TABLE-ACTUAL, BIG_TABLE-VARIANCE,
         BIG_TABLE-LOCKED,   BIG_TABLE-COMMIT, BIG_TABLE-ANNPLAN,
         BIG_TABLE-UNCOMMIT, BIG_TABLE-BZDAT.
*    move wa-posid          to big_table-posid.
  MOVE WA-POSID(7)       TO BIG_TABLE-PROJECT.
  MOVE WA-POSID(2)       TO BIG_TABLE-DEPT.
  MOVE 'Z'               TO BIG_TABLE-DISTGRP.
  IF WA-PRART BETWEEN '01' AND  '05'.
    IF WA-PSPRI CO '1234'.
      MOVE '1'         TO BIG_TABLE-DISTGRP. "TOTAL NEW BUSINESS
    ELSEIF WA-PSPRI CO '58'.
      MOVE '2'         TO BIG_TABLE-DISTGRP. "TOTAL ADDITIONS
    ELSEIF WA-PSPRI CO '79CDE'.
      MOVE '3'         TO BIG_TABLE-DISTGRP. "TOTAL REPLACEMENT
    ENDIF.
  ENDIF.
  MOVE WA-PRART          TO BIG_TABLE-PRART.
  MOVE WA-PSPRI          TO BIG_TABLE-PSPRI.
  MOVE WA-PSPHI          TO BIG_TABLE-PSPHI.
*    select single * from proj
*           where pspnr = wa-psphi.
  MOVE PROJ-POST1        TO BIG_TABLE-POST1.
  MOVE PROJ-VERNA        TO BIG_TABLE-VERNA.
*    move wa-objnr          to big_table-objnr.
  MOVE PLAN              TO BIG_TABLE-PLAN.
  MOVE ACTUAL            TO BIG_TABLE-ACTUAL.
  variance = plan - actual.
  MOVE VARIANCE          TO BIG_TABLE-VARIANCE.

  MOVE LOCKED           TO BIG_TABLE-LOCKED.

  MOVE ANNPLAN           TO BIG_TABLE-ANNPLAN.
  BIG_TABLE-UNCOMMIT = UNCOMMIT.

  BIG_TABLE-BZDAT = 0.
  SELECT SINGLE * FROM COBRA                                "#624
     WHERE OBJNR = WA-OBJNR                                      "
       AND BZDAT <> '00000000'.                                  "
  IF SY-SUBRC = '0'.                                             "
    BIG_TABLE-BZDAT = 1.                                        "
  ENDIF.                                                    "#624

*   if  big_table-plan <> 0 or           "Eliminate lines with all zeros
*       big_table-actual <> 0 or
*       big_table-locked <> 0 or
*       big_table-annplan <> 0
*       big_table-bzdat = 'Y'.
  COLLECT BIG_TABLE.
*       append big_table.
*   endif.
ENDFORM.                    "BUILD_TABLE
