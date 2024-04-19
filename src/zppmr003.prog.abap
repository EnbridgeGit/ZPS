REPORT ZPPMR003 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 147

                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        August 1997
*  Requestor:   Murray Caughy
*  Description:
*     - The purpose of this program is to produce a report of projects
*       to be Audited  These projects are not on the Interest During
*       Construction Report but due to the percentage complete should
*       have their status changed to "TECHICALLY COMPLETE"
************************************************************************
* 13/03/28 Mohammad Khan SDP44925 Remove Major Proj column from report.
* 07/11/05 Mohammad Khan TR537 Changes for CLSD status projects.
* 07/08/29 Mohammad Khan TR314 Add check box to optionally include
*          closed projects.
* 07/04/30 Mohammad Khan TR67 Get the dollars from CO currency instead
*          of using trans currency. i.e., use WKG00x instead of WKG00x.
* 02/11/11 Mohammad Khan #964 This program is consuming more than 20
*          hours and it needs a tune up to decrease it's run time.
*          a) Check the project status and if it's "closed" then do not
*             process these projects. Please note that closed stus is
*             for the project and not for WBS element that means use
*             objnr from PROJ table not from PRPS table.
*          b) For characteristic fields load data once from AUSP table
*             instead of using FM "CLFM_SELECT_AUSP".
* 99/08/10 mdemeest #607 Add comment to screen
* 98/12/09 NANCY GILLIGAN, OMNILOGIC - D30K906581
*          - ADDED IN DEFAULT PROJECT #'S IF USER DOES NOT SPECIFY.
* 98/03/23 md7140 #--- Change in template definiton
* 97/11/18 md7140 #nnn TECO projects with blank auditors should print
* 97/08/29 md7140 increase font size by eliminating Station, Auditor &
*                 Settlement Receiver columns per M. Caughy
* 2018/08/24 AKMADASU Power plan project chnages
************************************************************************
TABLES:
        BPJA,                          "Budget Info
        PROJ,                          "Project Definition
        COBRA,                         "Asset Valuation Date
        COBRB,                         "Settlement Rule Order
        COSP,                          "External Postings
        COSS,                          "Internal Postings
        JEST,                          "Status of WBS Element
        PRPS,                          "WBS Element Master Data
        TJ02T,                "System Status Texts (ie TECO, REL, etc)
        T001.                          "Company Code Table

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_AUDITOR   LIKE CABN-ATINN,       "Auditor Char
       G_ATINN_ACT_LGTH  LIKE CABN-ATINN,       "Actual Length Char
       G_ATINN_BUD_LGTH  LIKE CABN-ATINN,       "Budget Length Char
       G_ATINN_MAJ_PROJ  LIKE CABN-ATINN.       "Major Proj SDP39220
*      G_ATINN_STATION   LIKE CABN-ATINN, "Station Char ISSUE log 964
*      G_ATINN_TEMP      LIKE CABN-ATINN.              "ISSUE log 964

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.
*-----------------------------------------------------------------------
DATA: BEGIN OF COSP_TABLE.
        INCLUDE STRUCTURE COSP.
DATA: END OF COSP_TABLE.
*-----------------------------------------------------------------------
DATA: BEGIN OF COSS_TABLE.
        INCLUDE STRUCTURE COSS.
DATA: END OF COSS_TABLE.

DATA: STATUS         LIKE JEST-STAT,   "Status
      STATUSDESC     LIKE TJ02T-TXT04, "Status Description
      BZDAT          LIKE COBRA-BZDAT, "Valuation Date
      BUD_LGTH       LIKE AUSP-ATFLV,  "Budget Length
      ACT_LGTH       LIKE AUSP-ATFLV,  "Actual Length
      AUDITOR        LIKE AUSP-ATWRT,  "Auditor
      MAJ_PROJ       TYPE C,           "Maj Proj SDP39220
      BUDGET         LIKE COSP-WKG001, "Budget Total
      ACTUAL         LIKE COSP-WKG001, "Actual Total
      ACTCOSS        LIKE COSP-WKG001, "Actual Total
      ACTCOSP        LIKE COSP-WKG001, "Actual Total
      FLAG(3)        TYPE C  VALUE 'yes',      "NO TECO STATUS
      FLAG1(3)       TYPE C  VALUE 'no ',      "REL/PREL STATUS

      PERPROJ        TYPE I,           "Project Totals
      ACTPROJ        LIKE COSP-WKG001,
      BUDPROJ        LIKE COSP-WKG001,
      VARPROJ        LIKE COSP-WKG001,

      PERDIV         TYPE I,           "Division Totals
      ACTDIV         LIKE COSP-WKG001,
      BUDDIV         LIKE COSP-WKG001,
      VARDIV         LIKE COSP-WKG001,
      CNTDIV         TYPE I,           "No.of projects in div

      TMPBUDG        LIKE COSP-WKG001,
      TMPACT         LIKE COSP-WKG001,
*     tmppcnt        like cosp-WKG001.
      TMPPCNT    TYPE I.

DATA:                                  "Info from PRPS
  BEGIN OF SAVE_TABLE OCCURS 10000,
      POSID         LIKE PRPS-POSID,   "Project Number
      POST1         LIKE PRPS-POST1,   "Description
      PROJDESC      LIKE PROJ-POST1,   "Description
      STATUS        LIKE JEST-STAT,    "Status
      BZDAT         LIKE COBRA-BZDAT,  "Asset Valuation Date
      BUDLGTH       LIKE AUSP-ATFLV,   "Budget Length
      ACTLGTH       LIKE AUSP-ATFLV,   "Actual Length
      MPROJECT      TYPE C,            "Maj Proj SDP39220
      BUDGET        LIKE COSP-WKG001,  "Budget Total
      ACTUAL        LIKE COSP-WKG001,  "Actual Total
      EPROG         like PROJ-EPROG,   " Est In-Srv Date NARIGELA
      USR08         LIKE PRPS-USR08,  " added by akmadasu
      USR09         LIKE PRPS-USR09,   " added by akmadasu
  END OF SAVE_TABLE.

DATA:                                  "Report Data
  BEGIN OF BIG_TABLE OCCURS 10000,
      DEPT(2),                         "Div/Dept
      POSIDSRT      LIKE PROJ-PSPID,   "Project Number
      POSID         LIKE PRPS-POSID,   "Project Number
      PROJDESC      LIKE PROJ-POST1,   "Description
      BZDAT         LIKE COBRA-BZDAT,  "Asset Valuation Date
      POST1         LIKE PRPS-POST1,   "Description
      STATUS        LIKE JEST-STAT,    "Status
      BUDLGTH(3)    TYPE P DECIMALS 0, "Budget Length
      ACTLGTH(3)    TYPE P DECIMALS 0, "Actual Length
      MPROJECT      TYPE C,            "Maj Proj SDP39220
      BUDGET        LIKE COSP-WKG001,  "Budget Total
      ACTUAL        LIKE COSP-WKG001,  "Actual Total
      VARIANCE      LIKE COSP-WKG001,  "Budget - Actual
      EPROG         like PROJ-EPROG, " Est In-Srv Date NARIGELA
      USR08         LIKE PRPS-USR08,  " AKMADASU
      usr09         like prps-usr09,  " added by akmadasu
  END OF BIG_TABLE.
**--START OF CHANGES BY AKMADASU
TYPES:                                  "Report Data
BEGIN OF TY_BIG_TABLE_T,
    DEPT(2),                         "Div/Dept
    POSIDSRT      LIKE PROJ-PSPID,   "Project Number
    POSID         LIKE PRPS-POSID,   "Project Number
    PROJDESC      LIKE PROJ-POST1,   "Description
    BZDAT         LIKE COBRA-BZDAT,  "Asset Valuation Date
    POST1         LIKE PRPS-POST1,   "Description
    STATUS        LIKE JEST-STAT,    "Status
    BUDLGTH(3)    TYPE P DECIMALS 0, "Budget Length
    ACTLGTH(3)    TYPE P DECIMALS 0, "Actual Length
    MPROJECT      TYPE C,            "Maj Proj SDP39220
    BUDGET        LIKE COSP-WKG001,  "Budget Total
    ACTUAL        LIKE COSP-WKG001,  "Actual Total
    VARIANCE      LIKE COSP-WKG001,  "Budget - Actual
    EPROG         like PROJ-EPROG, " Est In-Srv Date NARIGELA
    USR08         LIKE PRPS-USR08,  " AKMADASU
    usr09         like prps-usr09, " akmadasu
   END OF TY_BIG_TABLE_T.

DATA:BIG_TABLE_T TYPE TABLE OF TY_BIG_TABLE_T,
      BIG_TABLE_Z TYPE TABLE OF TY_BIG_TABLE_T,
      BIG_TABLE_S TYPE  TY_BIG_TABLE_T.
**-- END OF CHANGES BY AKMADASU

DATA:                                                   "Issue log 964
  BEGIN OF NEW_ITAB OCCURS 0,                           "Issue log 964
      PSPID         LIKE PROJ-PSPID,   "Project Number  "Issue log 964
      PSPNR         LIKE PROJ-PSPNR,   "Project Number  "Issue log 964
      POST1         LIKE PROJ-POST1,   "Description     "Issue log 964
      EPROG         LIKE PROJ-EPROG,   " Est In-Srv Date " NARIGELA
  END OF NEW_ITAB.                                      "Issue log 964

*----------------------  SELECTION SCREEN  -----------------------------

SELECTION-SCREEN   BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(70) TEXT-030.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(70) TEXT-032.
SELECTION-SCREEN END OF LINE.

PARAMETER:         PBUKRS  LIKE PRPS-PBUKR MEMORY ID BUK.
SELECT-OPTIONS:    SVERNR  FOR PRPS-VERNR.          "Responsible Persons
PARAMETER:         PVERS   LIKE COSP-VERSN DEFAULT '00'.        "Version
SELECT-OPTIONS:    SPSPID  FOR PROJ-PSPID,                      "Project
                   SPCNT   FOR TMPPCNT.                         "Percent
*Start of SDP39220 changes
SELECTION-SCREEN SKIP.
SELECTION-SCREEN   BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-033.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:    F_YEAR  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN COMMENT 3(15) TEXT-041.
SELECTION-SCREEN COMMENT 20(12) TEXT-042.
*PARAMETER:         PGJAHR  LIKE COSP-GJAHR DEFAULT SY-DATUM(4). "FiscYr
SELECT-OPTIONS:     PGJAHR  FOR COSP-GJAHR  NO-EXTENSION         "FiscYr
                                            NO INTERVALS.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
PARAMETERS:    TO_DATE RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN   END OF BLOCK BOX1.
*End of SDP39220 changes
SELECTION-SCREEN SKIP.
PARAMETERS: P_CLOSE  AS CHECKBOX.           "To include closed projects
SELECTION-SCREEN   END OF BLOCK BOX.

*-----------------------------------------------------------------------


*  START OF SELECTION
START-OF-SELECTION.
  MOVE 'AUDITOR'        TO CHARIC.     "Characteristics Required
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_AUDITOR.

  MOVE 'ACTUAL_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_ACT_LGTH.

  MOVE 'BUDGET_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_BUD_LGTH.

  MOVE 'MAJORPROJECT'   TO CHARIC.                          "SDP39220
  PERFORM GET_ATINN.                                        "SDP39220
  MOVE G_ATINN          TO G_ATINN_MAJ_PROJ.                "SDP39220

  PERFORM BUILD_AUSP.                                   "ISSUE log 964

  SELECT SINGLE * FROM T001            "Company Code Description
    WHERE BUKRS = PBUKRS.
*----------------------------------------------------------------------*
*     THIS PARAGRAPH WILL REPLACE THE NEXT PARAGRAPH      new one      *
*----------------------------------------------------------------------*

  IF P_CLOSE = 'X'.
    SELECT PSPID  PSPNR  POST1 EPROG                            "Issue log 964
      INTO TABLE NEW_ITAB                                 "Issue log 964
      FROM   PROJ                                         "Issue log 964
       WHERE VBUKR  =  PBUKRS                             "Issue log 964
         AND PSPID  IN SPSPID                             "Issue log 964
         AND VERNR  IN SVERNR.                            "Issue log 964
  ELSE.                                                    "Issue log 964
    SELECT PSPID  PSPNR  POST1 EPROG                             "Issue log 964
      INTO TABLE NEW_ITAB                                  "Issue log 964
      FROM   PROJ                                          "Issue log 964
       WHERE VBUKR  =  PBUKRS                              "Issue log 964
         AND PSPID  IN SPSPID                              "Issue log 964
         AND VERNR  IN SVERNR                              "Issue log 964
         AND NOT EXISTS ( SELECT *                         "Issue log 964
                            FROM JEST                      "Issue log 964
                           WHERE OBJNR = PROJ~OBJNR        "Issue log 964
                             AND JEST~STAT  = 'I0046'      "Issue log 964
                             AND JEST~INACT  <> 'X' ).     "Issue log 964
  ENDIF.
**--start of changes by akmadasu
  TYPES:BEGIN OF gty_prps,
        pspnr type PS_POSNR,
        posid type PS_POSID,
        usr08 type USR08PRPS,
        usr09 type usr09prps,
        END OF gty_prps.
        data:gt_prps TYPE TABLE OF gty_prps,
              gs_prps type gty_prps.
  if new_itab[] is not INITIAL.
  select pspnr Posid usr08 usr09 into TABLE gt_prps from prps FOR ALL ENTRIES IN new_itab
                                                  where posid = new_itab-pspid.
    IF sy-subrc is INITIAL.
  sort gt_prps by posid.
    ENDIF.
  endif.
**-- end of chnages by akmadasu
  LOOP AT NEW_ITAB.                                        "Issue log 964
    CLEAR: TMPBUDG, TMPACT, TMPPCNT.
    FLAG = 'yes'.                      "==> NO TECO
    FLAG1 = 'no '.                     "==> NO REL/PREL
    IF NEW_ITAB-PSPID+5(2) CO '1234567890'."No templates "Issue log 964
      IF SPSPID IS INITIAL.   " only look at non blankets          omning
        IF NEW_ITAB-PSPID+4(3) > '199'.      "001-199 excluded blanket
          PERFORM CONTINUE_PROCESSING.                             "omning
        ENDIF.                           "End of 001-199 exclusion
      ELSE.       "it's not initial - include what is specified    omning
        PERFORM CONTINUE_PROCESSING.                             "omning
      ENDIF.                             "End of project selection
    ENDIF.                             "End of Template
  ENDLOOP.

  PERFORM DISPLAY_TABLE.

  WRITE: / TEXT-003 UNDER TEXT-001.    "End of Report

*----------------------------------------------------------------------*
*     note : ** means already commented     old one - Issue log 964    *
*----------------------------------------------------------------------*

*  SELECT * FROM PROJ
*       WHERE VBUKR = PBUKRS            "Company Code
*         AND PSPID IN SPSPID           "Project
*         AND VERNR IN SVERNR.          "Division
*    CLEAR: TMPBUDG, TMPACT, TMPPCNT.
*    FLAG = 'yes'.                      "==> NO TECO
*    FLAG1 = 'no '.                     "==> NO REL/PREL
**   if proj-pspid+4(3) co '1234567890'."No templates
*    IF PROJ-PSPID+5(2) CO '1234567890'."No templates          98/03/23
*
**  modified as per Joe McConkey - Jan 7/99:        "omning
**  - If user enters something in project selection, then include all
**  projects specified. If the user leaves the project selection blank
**    then include all projects in that fiscal period except blankets.
*
*     IF SPSPID IS INITIAL.   " only look at non blankets omning
*      IF PROJ-PSPID+4(3) > '199'.      "001-199 excluded blanket
*        PERFORM CONTINUE_PROCESSING.      "omning
*      ENDIF.                           "End of 001-199 exclusion
*     ELSE.     "it's not initial-include what is specified omning
*        PERFORM CONTINUE_PROCESSING.          "omning
*     ENDIF.                             "End of project selection
*    ENDIF.                             "End of Template
*  ENDSELECT.
*
*  PERFORM DISPLAY_TABLE.
*
*  WRITE: / TEXT-003 UNDER TEXT-001.    "End of Report


************************************************************************
*                     Subroutines in main program                      *
************************************************************************

*------------------------  PRINT_VERT  ---------------------------------
*  Print vertical lines to separate one column from another
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:   1 SY-VLINE, 17 SY-VLINE, 58 SY-VLINE,  63 SY-VLINE,
          74 SY-VLINE, 81 SY-VLINE, 88 SY-VLINE,
         100 SY-VLINE, 112 SY-VLINE,
         124 SY-VLINE, 132 SY-VLINE, 147 SY-VLINE. " NARIGELA added 150
ENDFORM.

*------------------------  DISPLAY_TABLE  ------------------------------
*   This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

  SORT BIG_TABLE BY DEPT POSIDSRT POSID.
*  loop at big_table.                 "To see information in big_table
*   perform big_table_dump.
*  endloop.

  LOOP AT BIG_TABLE.
    AT NEW DEPT.
      NEW-PAGE.
    ENDAT.
**-- start of changes by akmadasu
    IF big_table-usr09 is not INITIAL.
    continue.
    ENDIF.
**-- END OF CHANGES BY AKMADASU
    CLEAR STATUSDESC.
    SELECT SINGLE * FROM TJ02T
        WHERE ISTAT = BIG_TABLE-STATUS
         AND SPRAS = SY-LANGU.
    IF  SY-SUBRC = '0'.
      STATUSDESC = TJ02T-TXT04.
    ENDIF.
    WRITE:  / BIG_TABLE-POSID    UNDER TEXT-004,      "Project Number
              BIG_TABLE-POST1    UNDER TEXT-005,      "Description
              "BIG_TABLE-BZDAT    UNDER TEXT-007,      "Valuation Date  " akmadasu
              BIG_TABLE-EPROG    UNDER TEXT-050,       "Est In-Srv Date " akmadasu
              STATUSDESC         UNDER TEXT-006,      "Status
              BIG_TABLE-BUDLGTH  UNDER TEXT-013,      "Budget Length
              BIG_TABLE-ACTLGTH  UNDER TEXT-011,      "Actual Length
              BIG_TABLE-USR08    UNDER TEXT-059,      "In-Srv date " NARIGELA
*              BIG_TABLE-MPROJECT UNDER TEXT-035,      "MajProj SDP44925
         (11) big_table-budget under text-017 decimals 0,   "Budget
         (11) big_table-actual under text-020 decimals 0.   "Actual##
    IF BIG_TABLE-VARIANCE < 0.         "Red for Negatives
      write: (11) big_table-variance under text-021 decimals 0
                             COLOR COL_NEGATIVE INVERSE ON.
    ELSE.
      write: (11) big_table-variance under text-021 decimals 0.
    ENDIF.
*    ENDIF. " ADDED BY AKMADASU
    PERFORM PRINT_VERT.

    ACTPROJ = ACTPROJ + BIG_TABLE-ACTUAL.           "Proj Actual Total
    BUDPROJ = BUDPROJ + BIG_TABLE-BUDGET.           "Proj Budget Total
    VARPROJ = VARPROJ + BIG_TABLE-VARIANCE.         "Proj Variance Total
*    IF LV_PROCESS ='X'. " ADDED BY AKMADASU
    AT END OF POSIDSRT.
      CNTDIV = CNTDIV + 1.
      IF  BUDPROJ = 0.                 "Calculate % complete
        PERPROJ = 100.
      ELSE.
        PERPROJ = ACTPROJ / BUDPROJ * 100.
      ENDIF.

      WRITE:  / TEXT-026 UNDER TEXT-005 COLOR COL_TOTAL,
           (11) actproj under text-020 color col_total decimals 0, "##
           (11) budproj under text-017 color col_total decimals 0,
           (11) varproj under text-021 color col_total decimals 0.
      IF PERPROJ > 90.
        WRITE: (5) PERPROJ UNDER TEXT-023 COLOR COL_POSITIVE.
      ELSE.
        WRITE: (5) PERPROJ UNDER TEXT-023 COLOR COL_TOTAL.
      ENDIF.
      PERFORM PRINT_VERT.              "Print project totals

      ACTDIV = ACTDIV + ACTPROJ.       "Div Actual Total
      BUDDIV = BUDDIV + BUDPROJ.       "Div Budget Total
      VARDIV = VARDIV + VARPROJ.       "Div Variance Total


      CLEAR: PERPROJ, ACTPROJ, BUDPROJ, VARPROJ.

      ULINE.

    ENDAT.

    AT END OF DEPT.
      IF  BUDDIV = 0.                  "Calculate % complete
        PERDIV = 100.
      ELSE.
        PERDIV = ACTDIV / BUDDIV * 100.
      ENDIF.

      WRITE:  / TEXT-028 UNDER TEXT-005,
                CNTDIV COLOR COL_TOTAL USING EDIT MASK '____',
                TEXT-027 UNDER TEXT-004 COLOR COL_TOTAL,
           (11) actdiv under text-020 decimals 0 color col_total,   "##
           (11) buddiv under text-017 decimals 0 color col_total,
           (11) vardiv under text-021 decimals 0 color col_total.
      IF PERDIV > 90.
        WRITE: (5) PERDIV UNDER TEXT-023 COLOR COL_POSITIVE.
      ELSE.
        WRITE: (5) PERDIV UNDER TEXT-023 COLOR COL_TOTAL.
      ENDIF.
      PERFORM PRINT_VERT.              "Print Division totals



      CLEAR: PERDIV, ACTDIV, BUDDIV, VARDIV, CNTDIV.

      ULINE.
    ENDAT.
*    ENDIF." ADDED BY AKMADASU
  ENDLOOP.

ENDFORM.


*--------------------  PROCESS_TABLES  ---------------------------------
*   This routine selects all PRPS row which require Interest
*   Calculation (PRPS-ZSCHM = 'CTRA/UN')
*-----------------------------------------------------------------------
FORM PROCESS_TABLES.
  REFRESH SAVE_TABLE.
  SELECT * FROM PRPS
    WHERE PSPHI = NEW_ITAB-PSPNR.
*    WHERE PSPHI = PROJ-PSPNR.
   CLEAR: ACTUAL.
    OBJECT = PRPS-OBJNR.
    PERFORM FIND_CHARACTERISTIC.

    PERFORM GET_STATUS.
    PERFORM ASSET_VALUATION_DATE.
    PERFORM COSP_ACTUAL.
    PERFORM COSS_ACTUAL.

    CLEAR: BUDGET.
    IF  PRPS-BELKZ = 'X'.               "Get budget at lowest level only
      PERFORM BPJA-BUDGET.             "Roll up occurs on higher levels
    ENDIF.

    OBJECT = PRPS-OBJNR.

    PERFORM BUILD_SAVE_TABLE.
  ENDSELECT.

  IF TMPBUDG = 0.
    MOVE 100 TO TMPPCNT.
  ELSE.
    COMPUTE TMPPCNT = TMPACT / TMPBUDG * 100.
  ENDIF.

*write: / 'flag=', flag, 'flag1=', flag1, '%=', tmppcnt, proj-pspid.
* if flag = 'yes'.
  IF  FLAG1 = 'yes'.
    IF TMPPCNT IN SPCNT.
      PERFORM BUILD_BIG_TABLE.
    ENDIF.
  ENDIF.
* endif.

ENDFORM.

*--------------------  ASSET VALUATION DATE ----------------------------
FORM ASSET_VALUATION_DATE.
  CLEAR BZDAT.
  SELECT SINGLE * FROM COBRA           "Valuation Date
    WHERE OBJNR = PRPS-OBJNR.
  IF SY-SUBRC = '0'.
    BZDAT = COBRA-BZDAT.
    IF COBRA-BZDAT CA '123456789'.
      FLAG = 'no'.
    ENDIF.
  ENDIF.
ENDFORM.


*--------------------  COSP_ACTUAL  ------------------------------------
FORM COSP_ACTUAL.
  CLEAR ACTCOSP.
  SELECT * FROM COSP INTO COSP_TABLE
    WHERE OBJNR = PRPS-OBJNR
*      AND GJAHR = PGJAHR                   "SDP39220
      AND GJAHR IN PGJAHR                                   "SDP39220
      AND VERSN = PVERS
      AND WRTTP = '04'
      AND BEKNZ IN ('S', 'H', 'L').
    ADD COSP_TABLE-WKG001 FROM 1 TO 12 GIVING ACTCOSP.
    ACTUAL = ACTUAL + ACTCOSP.
  ENDSELECT.
ENDFORM.

*--------------------  COSS_ACTUAL  ------------------------------------
FORM COSS_ACTUAL.
  CLEAR ACTCOSS.
  SELECT * FROM COSS INTO COSS_TABLE
    WHERE OBJNR = PRPS-OBJNR
*      AND GJAHR = PGJAHR                   "SDP39220
      AND GJAHR IN PGJAHR                                   "SDP39220
      AND VERSN = PVERS
      AND WRTTP = '04'
      AND BEKNZ IN ('S', 'H', 'L').
    ADD COSS_TABLE-WKG001 FROM 1 TO 12 GIVING ACTCOSS.
    ACTUAL = ACTUAL + ACTCOSS.
  ENDSELECT.
ENDFORM.

*--------------------  BPJA_BUDGET  ------------------------------------
FORM BPJA-BUDGET.
  SELECT * FROM BPJA
    WHERE OBJNR = PRPS-OBJNR
*      AND GJAHR = PGJAHR                   "SDP39220
      AND GJAHR IN PGJAHR                                   "SDP39220
      AND VERSN = PVERS
      AND WRTTP = '41'.
    BUDGET = BPJA-WLJHR.
  ENDSELECT.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM BUILD_BIG_TABLE                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_BIG_TABLE.
  LOOP AT SAVE_TABLE.
*
    IF SAVE_TABLE-STATUS <> 'I0076'.
      CLEAR BIG_TABLE.
      MOVE SAVE_TABLE-POSID(7) TO BIG_TABLE-POSIDSRT.   "Project
      MOVE SAVE_TABLE-POSID(2) TO BIG_TABLE-DEPT.       "Department
      MOVE SAVE_TABLE-POSID    TO BIG_TABLE-POSID.      "Project
      MOVE SAVE_TABLE-POST1    TO BIG_TABLE-POST1.      "Description
      MOVE SAVE_TABLE-BZDAT    TO BIG_TABLE-BZDAT.      "Valuation Date
      MOVE SAVE_TABLE-STATUS   TO BIG_TABLE-STATUS.     "Status
      MOVE SAVE_TABLE-BUDLGTH  TO BIG_TABLE-BUDLGTH.    "Budget Length
      MOVE SAVE_TABLE-ACTLGTH  TO BIG_TABLE-ACTLGTH.    "Actual Length
      MOVE SAVE_TABLE-MPROJECT TO BIG_TABLE-MPROJECT. "Majproj SDP39220
      MOVE SAVE_TABLE-BUDGET   TO BIG_TABLE-BUDGET.     "Budget
      MOVE SAVE_TABLE-ACTUAL   TO BIG_TABLE-ACTUAL.     "Actual
      MOVE SAVE_TABLE-EPROG   TO BIG_TABLE-EPROG.     "Actual
      MOVE SAVE_TABLE-usr08   TO BIG_TABLE-usr08.     "Actual
      MOVE SAVE_TABLE-usr09   TO BIG_TABLE-usr09.     "Actual

      BIG_TABLE-VARIANCE = BIG_TABLE-BUDGET - BIG_TABLE-ACTUAL.
      APPEND BIG_TABLE.
    ENDIF.
  ENDLOOP.
ENDFORM.

*------------------------  GET_STATUS  ---------------------------------
*  Only RELEASED (I0002) or TECHNICALLY COMPLETE STATUS are acceptable
*-----------------------------------------------------------------------
FORM GET_STATUS.
  CLEAR STATUS.
  SELECT  * FROM JEST
    WHERE OBJNR = PRPS-OBJNR
      AND INACT <> 'X'                 "Ignore Inactives
      ORDER BY STAT.
    CASE JEST-STAT.
      WHEN 'I0002'.                    "Status Released
        MOVE JEST-STAT TO STATUS.
        MOVE 'yes'     TO FLAG1.
      WHEN 'I0042'.                    "Status Partially Released
        MOVE JEST-STAT TO STATUS.
        MOVE 'yes'     TO FLAG1.
      WHEN 'I0045'.                    "Status Teco Complete
        MOVE JEST-STAT TO STATUS.
        MOVE 'yes'     TO FLAG1.
      WHEN 'I0046'.                    "Status Closed   "Issue log 964
        MOVE JEST-STAT TO STATUS.
        MOVE 'yes'     TO FLAG1.
      WHEN 'I0076'.                    "Status Flag Deleted
        MOVE JEST-STAT TO STATUS.
    ENDCASE.
  ENDSELECT.
ENDFORM.
*---------------------  BUILD_SAVE_TABLE  ------------------------------
*   Description:
*   Store all info in SAVE_TABLE.
*-----------------------------------------------------------------------
FORM BUILD_SAVE_TABLE.
**-- start of chnages by akmadasu
  READ TABLE gt_prps into gs_prps with key posid = new_itab-pspid.
  IF sy-subrc is INITIAL.
  MOVE gs_PRPS-USR08      TO SAVE_TABLE-USR08.
  MOVE gs_PRPS-USR09      TO SAVE_TABLE-USR09.
  ENDIF.
**-- end of changes by akmadasu
  MOVE PRPS-POSID      TO SAVE_TABLE-POSID.     "Project
  MOVE PRPS-POST1      TO SAVE_TABLE-POST1.     "Description
  MOVE NEW_ITAB-POST1      TO SAVE_TABLE-PROJDESC.  "Project Description
*  MOVE PROJ-POST1      TO SAVE_TABLE-PROJDESC.  "Project Description
  MOVE NEW_ITAB-EPROG  TO SAVE_TABLE-EPROG.  " Est In-Srv Date NARIGELA

  MOVE BZDAT           TO SAVE_TABLE-BZDAT.     "Valuation Date
  MOVE STATUS          TO SAVE_TABLE-STATUS.    "Status
  MOVE BUD_LGTH        TO SAVE_TABLE-BUDLGTH.   "Budget Length
  MOVE ACT_LGTH        TO SAVE_TABLE-ACTLGTH.   "Actual Length
  MOVE MAJ_PROJ        TO SAVE_TABLE-MPROJECT.  "MajProj SDP39220
  MOVE BUDGET          TO SAVE_TABLE-BUDGET.    "Budget
  MOVE ACTUAL          TO SAVE_TABLE-ACTUAL.    "Actual
  TMPBUDG = TMPBUDG + BUDGET.
  TMPACT  = TMPACT  + ACTUAL.
  APPEND SAVE_TABLE.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM BIG_TABLE_DUMP
*-----------------------------------------------------------------------
*   Description:
*   To view contents loop thru BIG_TABLE
*-----------------------------------------------------------------------
FORM BIG_TABLE_DUMP.
  WRITE: /1 BIG_TABLE-DEPT, 20 BIG_TABLE-POSID,
         60 '*', BIG_TABLE-POST1.
ENDFORM.

*-----------------------  GET_ATINN  -----------------------------------
* Routine used to get the internal character number for project control
*-----------------------------------------------------------------------
FORM GET_ATINN.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
    EXPORTING
      CLASS_TYPE                  = '014'
      FEATURE_NEUTRAL_NAME        = CHARIC
    IMPORTING
      FEATURE_ID                  = G_ATINN
    EXCEPTIONS
      INVALID_CLASS_TYPE          = 1
      MISSING_FEATURE_INFORMATION = 2
      NO_FEATURE_FOUND            = 3
      NO_FEATURE_VALID            = 4
      NO_LANGUAGE                 = 5
      OTHERS                      = 6.
  IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTICS OF', CHARIC.
  ENDIF.
ENDFORM.

*------------------------ BUILD_AUSP-----------------------------------*
*              NEW PARAGRAPH FOR ISSUE LOG 964                         *
*----------------------------------------------------------------------*

FORM BUILD_AUSP.
  REFRESH CHAR_TAB.
  SELECT * FROM AUSP INTO TABLE CHAR_TAB
         WHERE ( ATINN = G_ATINN_BUD_LGTH OR
                 ATINN = G_ATINN_ACT_LGTH OR
                 ATINN = G_ATINN_MAJ_PROJ OR                "SDP39220
                 ATINN = G_ATINN_AUDITOR ) AND
                 MAFID = 'O'          AND
                 KLART = '014'.
  SORT CHAR_TAB BY OBJEK ATINN.
ENDFORM.

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine used to get the value of the project control number *
*-----------------------------------------------------------------------
*----------------------------------------------------------------------*
*   THIS PARAGRAPH WILL REPLACE THE NEXT ONE FOR ISSUE LOG 964    NEW  *
*----------------------------------------------------------------------*
FORM FIND_CHARACTERISTIC.

* Character  values in "ATWRT', numeric values in"ATFLV".
  CLEAR: BUD_LGTH, ACT_LGTH, MAJ_PROJ, AUDITOR.

**  budget length
  READ TABLE CHAR_TAB WITH KEY OBJEK = OBJECT
                               ATINN = G_ATINN_BUD_LGTH  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATFLV TO BUD_LGTH.
  ENDIF.

**  actual length
  READ TABLE CHAR_TAB WITH KEY OBJEK = OBJECT
                               ATINN = G_ATINN_ACT_LGTH  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATFLV TO ACT_LGTH.
  ENDIF.

**  auditor
  READ TABLE CHAR_TAB WITH KEY OBJEK = OBJECT
                               ATINN = G_ATINN_AUDITOR  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATWRT TO AUDITOR.
  ENDIF.

*Start of SDP39220 changes
**  major proj
  READ TABLE CHAR_TAB WITH KEY OBJEK = OBJECT
                               ATINN = G_ATINN_MAJ_PROJ  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATWRT TO MAJ_PROJ.
  ENDIF.
*End of SDP39220 changes
  CLEAR: OBJECT.

ENDFORM.
*----------------------------------------------------------------------*
*                                                ISSUE LOG 964    OLD  *
*----------------------------------------------------------------------*
*FORM FIND_CHARACTERISTIC.
*  REFRESH CHAR_TAB.
*  CALL FUNCTION 'CLFM_SELECT_AUSP'
*       EXPORTING
*            MAFID     = 'O'
*            CLASSTYPE = '014'
*            OBJECT    = OBJECT
*       TABLES
*            EXP_AUSP  = CHAR_TAB
*       EXCEPTIONS
*            NO_VALUES = 1
*            OTHERS    = 2.
**loop at char_tab.
**write: / char_tab-atinn, char_tab-atflv, char_tab-atwrt.
**endloop.
** Character  values in "ATWRT', numeric values in"ATFLV".
*  CLEAR: BUD_LGTH, ACT_LGTH, AUDITOR.
*  IF SY-SUBRC EQ 0.
*    SORT CHAR_TAB BY ATINN.
**  budget length
*   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_BUD_LGTH BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      MOVE CHAR_TAB-ATFLV TO BUD_LGTH.
*    ENDIF.
**  actual length
*   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_ACT_LGTH BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      MOVE CHAR_TAB-ATFLV TO ACT_LGTH.
*    ENDIF.
**  auditor
*    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_AUDITOR BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      MOVE CHAR_TAB-ATWRT TO AUDITOR.
*    ENDIF.
*  ENDIF.
*  CLEAR: OBJECT.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONTINUE_PROCESSING
*&---------------------------------------------------------------------*
FORM CONTINUE_PROCESSING.
*  Process only those projects where the auditor is blank at the
*  highest level (stufe = '1')
  SELECT SINGLE * FROM PRPS
     WHERE PSPHI = NEW_ITAB-PSPNR
*           WHERE PSPHI = PROJ-PSPNR
       AND STUFE = '1'.

  OBJECT = PRPS-OBJNR.
  PERFORM FIND_CHARACTERISTIC.

  IF AUDITOR = SPACE.            "Auditor test
    PERFORM PROCESS_TABLES.
  ENDIF.                         "End of Auditor test

ENDFORM.                    " CONTINUE_PROCESSING

*---------------------  TOP-OF-PAGE  -----------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-RPT, SY-REPID, 55 TEXT-001,                  "Title
           105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  IF F_YEAR = 'X'.                                          "SDP39220
    WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
           TEXT-002 UNDER TEXT-001, PGJAHR-LOW,                "Fiscal Year
             TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  ELSE.                                                     "SDP39220
    WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, "SDP39220
             TEXT-043 UNDER TEXT-001,                       "SDP30220
             TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "SDP30220
  ENDIF.                                                    "SDP30220
  WRITE: / TEXT-VRS UNDER TEXT-RPT, PVERS,
           59 T001-BUTXT.  "Company
*           T001-BUTXT UNDER TEXT-001.  "Company
  ULINE.
  FORMAT INTENSIFIED ON.

*write: /2 text-004,  28 text-005,  69 text-006,  76 text-007,
*       87 text-013,  96 text-011, 105 text-017,
*      124 text-019, 143 text-021, 162 text-023.
  DATA: TEXT100(8).

  IF F_YEAR = 'X'.
    MOVE TEXT-019 TO TEXT100.
  ELSE.
    MOVE TEXT-017 TO TEXT100.
  ENDIF.                                                    "SDP39220
  WRITE: /2 TEXT-004,  18 TEXT-005,  59 TEXT-006,  64 TEXT-050, "TEXT-007,
         75 TEXT-013,  82 TEXT-011,  89 TEXT-017,
        102 TEXT100, 113 TEXT-021, 125 TEXT-023, 133 TEXT-059." NARIGELA Added text50 Est In-Srv Date
*ELSE.  "Start of SDP39220 changes
*  WRITE: /2 TEXT-004,  18 TEXT-005,  59 TEXT-006,  64 TEXT-007,
*         75 TEXT-013,  82 TEXT-011,  89 TEXT-017,
*        101 TEXT-017, 113 TEXT-021, 125 TEXT-023, 133 TEXT-034.
*ENDIF. "End of SDP39220 changes
  PERFORM PRINT_VERT.

  WRITE: / TEXT-008 UNDER TEXT-007,      "Asset Valuation Date
             TEXT-012 UNDER TEXT-011,    "Actual Length
             TEXT-012 UNDER TEXT-013,    "Budget Length
             TEXT-018 UNDER TEXT-017,    "Total Budget
             TEXT-020 UNDER TEXT100,    "Ytd Actual $
             TEXT-022 UNDER TEXT-021,    "Over/Under
            TEXT-025 UNDER TEXT-023.     "Percentage Complete

  PERFORM PRINT_VERT.

  WRITE: / TEXT-009 UNDER TEXT-007,      "Asset Valuation Date
             TEXT-024 UNDER TEXT-021.    "Over/Under Budget
  PERFORM PRINT_VERT.

  ULINE.
