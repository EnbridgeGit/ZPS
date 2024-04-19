REPORT ZPSRR001 NO STANDARD PAGE HEADING LINE-COUNT 64 LINE-SIZE 145.
********************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                          *
* Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)      *
*       Date: March 12, 1997                                       *
* Request ID:                                                      *
*                                                                  *
* The following program will generate a Project Summary Report     *
* sorted by division within project type.  This program will       *
* break on the project types and will calculate all wbs-elements   *
* total related to their project number.  A grand total is         *
* displayed for the total plan.                                    *
********************************************************************
* 10/04/16 mokhan #813 Costing sheet (Secondary cost element) changes.
* 09/10/06 mokhan #663 Apply the Authorization check for Market Hub
*                        Companies.
* 98/03/23 md7140 #--- Change template definition. Add Company selection
* 97/10/31 md7140 #269 Eliminate templates & add in division selection
* 97/11/05 md7140 #269 Do not add projects with $0.00 to maintab.
*                      $0 projects sometimes cause Divisional totals
*                      to disappear.
********************************************************************
TABLES: COSP,  "CO Object:  Cost Totals - External Postings
        COSS,  "Secondary cost                             "TR813
        PROJ,  "Project Definition
        PRPS,  "WBS (Work Breakdown Structure) Element Master Data
        TCJ1T. "Project Types
*                                              "TR813 Start change
*DATA:  PLAN     LIKE COSP-KSTAR VALUE '0000480199',  "Plan Cost Element
*       IDC      LIKE COSP-KSTAR VALUE '0000324102',  "IDC  Cost Element
*       PROCEEDS LIKE COSP-KSTAR VALUE '0000430025',  "Proc Cost Element
*                                              "TR813 End change

DATA:   FIRST(2) TYPE N VALUE '01',                   "Month periods
        LAST(2)  TYPE N VALUE '12',                   "Month Periods
        PL_AMT   LIKE COSP-WKG001,                    "Calculation field
        IDC_AMT  LIKE COSP-WKG001,
        PROC_AMT LIKE COSP-WKG001,
        CST_AMT  LIKE COSP-WKG001,        "TR813
        TOTAL    LIKE COSP-WKG001,
        S_PL     LIKE COSP-WKG001,
        S_IDC    LIKE COSP-WKG001,
        S_PROC   LIKE COSP-WKG001,
        S_CST    LIKE COSP-WKG001,        "TR813
        S_TOT    LIKE COSP-WKG001,
        TOT_PL   LIKE COSP-WKG001,
        TOT_IDC  LIKE COSP-WKG001,
        TOT_CST  LIKE COSP-WKG001,        "TR813
        TOT_PROC LIKE COSP-WKG001,
        SUB_TOT  LIKE COSP-WKG001,
        GR_PL    LIKE COSP-WKG001,
        GR_IDC   LIKE COSP-WKG001,
        GR_CST   LIKE COSP-WKG001,        "TR813
        GR_PROC  LIKE COSP-WKG001,
        GR_TOT   LIKE COSP-WKG001.

* Table for control-break processing.
DATA:  BEGIN OF MAINTAB OCCURS 10000,
         PRATX  LIKE TCJ1T-PRATX,                     "Type - Descriptio
         DIV    LIKE PRPS-POSID,                      "Division
         WBS    LIKE PRPS-POSID,                      "WBS element
         PRART  LIKE TCJ1T-PRART,                     "Project Type
         KSTAR  LIKE COSP-KSTAR,                      "Cost Element
         GJAHR  LIKE COSP-GJAHR,                      "Fiscal Year

* Monthly values set a side for Planning cost elements
         PLAN01 LIKE COSP-WKG001,                     "Jan
         PLAN02 LIKE COSP-WKG002,                     "Feb
         PLAN03 LIKE COSP-WKG003,                     "Mar
         PLAN04 LIKE COSP-WKG004,                     "Apr
         PLAN05 LIKE COSP-WKG005,                     "May
         PLAN06 LIKE COSP-WKG006,                     "Jun
         PLAN07 LIKE COSP-WKG007,                     "Jul
         PLAN08 LIKE COSP-WKG008,                     "Aug
         PLAN09 LIKE COSP-WKG009,                     "Sep
         PLAN10 LIKE COSP-WKG010,                     "Oct
         PLAN11 LIKE COSP-WKG011,                     "Nov
         PLAN12 LIKE COSP-WKG012,                     "Dec
*START of TR813 changes
* Monthly values set a side for costing sheet cost elements
         CSHEET01 LIKE COSS-WKG001,                   "Jan
         CSHEET02 LIKE COSS-WKG001,                   "Feb
         CSHEET03 LIKE COSS-WKG001,                   "Mar
         CSHEET04 LIKE COSS-WKG001,                   "Apr
         CSHEET05 LIKE COSS-WKG001,                   "May
         CSHEET06 LIKE COSS-WKG001,                   "Jun
         CSHEET07 LIKE COSS-WKG001,                   "Jul
         CSHEET08 LIKE COSS-WKG001,                   "Aug
         CSHEET09 LIKE COSS-WKG001,                   "Sep
         CSHEET10 LIKE COSS-WKG001,                   "Oct
         CSHEET11 LIKE COSS-WKG001,                   "Nov
         CSHEET12 LIKE COSS-WKG001,                   "Dec
*END of TR813 changes
* Monthly values set a side for IDC cost elements
         IDC001 LIKE COSP-WKG001,
         IDC002 LIKE COSP-WKG002,
         IDC003 LIKE COSP-WKG003,
         IDC004 LIKE COSP-WKG004,
         IDC005 LIKE COSP-WKG005,
         IDC006 LIKE COSP-WKG006,
         IDC007 LIKE COSP-WKG007,
         IDC008 LIKE COSP-WKG008,
         IDC009 LIKE COSP-WKG009,
         IDC010 LIKE COSP-WKG010,
         IDC011 LIKE COSP-WKG011,
         IDC012 LIKE COSP-WKG012,

* Monthly values set a side for Proceeds cost elements
         PROC01 LIKE COSP-WKG001,
         PROC02 LIKE COSP-WKG002,
         PROC03 LIKE COSP-WKG003,
         PROC04 LIKE COSP-WKG004,
         PROC05 LIKE COSP-WKG005,
         PROC06 LIKE COSP-WKG006,
         PROC07 LIKE COSP-WKG007,
         PROC08 LIKE COSP-WKG008,
         PROC09 LIKE COSP-WKG009,
         PROC10 LIKE COSP-WKG010,
         PROC11 LIKE COSP-WKG011,
         PROC12 LIKE COSP-WKG012,
   END OF MAINTAB,

* Table used for storing the projects description.
   BEGIN OF SUBTAB OCCURS 1000,
         PSPID  LIKE PROJ-PSPID,
         PSPNR  LIKE PRPS-PSPNR,
         POST1  LIKE PROJ-POST1,
   END OF SUBTAB.

RANGES: S_BUKRS FOR T001T-BUKRS,     "TR663
        S_KOSTL FOR PROJ-KOSTL,      "TR663
        S_CCGRP FOR RGSB4-SETNR,     "TR663
        S_AUFNR FOR BSEG-AUFNR,      "TR663
        S_PSPNR FOR PROJ-PSPNR,      "TR663
        S_PSPID FOR PROJ-PSPID,      "TR663
        S_NPLNR FOR BSEG-NPLNR,      "TR663
        S_WERKS FOR PRPS-WERKS,      "TR663
        S_EKORG FOR EKKO-EKORG.      "TR663

SELECTION-SCREEN BEGIN OF BLOCK BEGIN WITH FRAME.

PARAMETERS: P_VBUKR LIKE PROJ-VBUKR MODIF ID ABC OBLIGATORY   "Company
                                    DEFAULT 'UGL'.
SELECT-OPTIONS:
            S_VERNR FOR PROJ-VERNR MODIF ID ABC,              "Division
            S_PROJ FOR PROJ-PSPID MODIF ID ABC,               "Projects
            S_TYPE FOR PRPS-PRART MODIF ID ABC                "Type
              DEFAULT '01'.
PARAMETERS: P_VERSN LIKE TKA09-VERSN OBLIGATORY DEFAULT '000' "Version
                    MODIF ID ABC,
            P_YEAR  LIKE COSP-GJAHR DEFAULT SY-DATUM(4)       "Year
                    OBLIGATORY MODIF ID ABC.
*START of TR813 changes
*SELECTION-SCREEN END OF BLOCK BEGIN.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-106.
SELECT-OPTIONS:
 PL_KSTAR FOR COSP-KSTAR DEFAULT '0000480199',                "Planing
 CS_KSTAR FOR COSP-KSTAR DEFAULT '0000830000' TO '0000830099'. "Csheet
PARAMETERS:
 ID_KSTAR  LIKE COSP-KSTAR DEFAULT '0000324102',                 "I.D.C
 PR_KSTAR  LIKE COSP-KSTAR DEFAULT '0000430025'.              "Proceeds
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN END OF BLOCK BEGIN.

* The following will highlight the screen's output for certain texts. *
*AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    CHECK SCREEN-GROUP1 = 'ABC'.
*    SCREEN-INTENSIFIED = '1'.
*    MODIFY SCREEN.
*  ENDLOOP.
*END of TR813 changes

************************************************************************
*Authorization Ckeck   "TR663
S_BUKRS-SIGN   = 'I'.
S_BUKRS-OPTION = 'EQ'.
S_BUKRS-LOW    = P_VBUKR.
APPEND S_BUKRS.
CLEAR  S_BUKRS.

INCLUDE ZFICO_AUTH_CHECK_INCLUDE.       "TR663
INCLUDE ZNONFICO_AUTH_CHECK_INCLUDE.    "TR663

START-OF-SELECTION.
 PERFORM FICO_AUTHORIZATION_CHECK.      "TR663
 PERFORM NONFICO_AUTHORIZATION_CHECK.   "TR663

 PERFORM INITIALIZE.
 PERFORM GET_PROJ.
*perform get_data.
 PERFORM WRITE_DATA.
 PERFORM FINAL.
END-OF-SELECTION.

* This routine initializes the internal tables.
FORM INITIALIZE.
 REFRESH: MAINTAB, SUBTAB.
 CLEAR:   MAINTAB, SUBTAB.
ENDFORM.

* This routine gathers the project description.
FORM GET_PROJ.
  SELECT * FROM PROJ WHERE PSPID IN S_PROJ
                       AND VERNR IN S_VERNR
                       AND VBUKR = P_VBUKR.           "Company
*   if proj-pspid+4(3) co '0123456789'.          "Template Elimination
    IF PROJ-PSPID+5(2) CO '0123456789'.   "Template Elimination 98/03/23
       SELECT * FROM PRPS
          WHERE PSPHI = PROJ-PSPNR
            AND PRART IN S_TYPE.
         CLEAR: SUBTAB.
         MOVE: PRPS-PSPNR TO SUBTAB-PSPNR,
               PROJ-PSPID TO SUBTAB-PSPID,
               PROJ-POST1 TO SUBTAB-POST1.
         APPEND SUBTAB.
         PERFORM GET_DATA.
       ENDSELECT.                               "End of PRPS
    ENDIF.                                      "End of Template Elim.
  ENDSELECT.                                    "End of PROJ
* select * from prps where posid in s_proj
*                      and prart in s_type.
*  if prps-posid+4(3) co '0123456789'.          "#269 - Template elimint
*     select * from proj where pspid = prps-posid
*                          and vernr in s_vernr.                 "#269
*     clear: subtab.
*     move: prps-pspnr to subtab-pspnr,
*           proj-pspid to subtab-pspid,
*           proj-post1 to subtab-post1.
*     append subtab.
*     endselect.                                "End of PROJ selection
*  endif.                                       "End of Template
*endselect.
ENDFORM.

* This routine gathers the cost element, values, version and projects.
FORM GET_DATA.
*  select * from prps where posid in s_proj
*    and prart in s_type.
    SELECT * FROM COSP WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_YEAR AND VERSN = P_VERSN AND WRTTP = '01'.
*START of TR813 changes
*      CHECK COSP-KSTAR = PLAN OR COSP-KSTAR = IDC OR
*            COSP-KSTAR = PROCEEDS.
*     CHECK COSP-KSTAR = PL_KSTAR  OR  COSP-KSTAR = ID_KSTAR OR  "TR813
      CHECK COSP-KSTAR IN PL_KSTAR  OR  COSP-KSTAR = ID_KSTAR OR "TR813
            COSP-KSTAR = PR_KSTAR.
        SELECT SINGLE * FROM TCJ1T WHERE PRART = PRPS-PRART
*END of TR813 changes
                                     AND LANGU = SY-LANGU.
*          and langu = 'E'.
          CLEAR: MAINTAB.
          MOVE: TCJ1T-PRART TO MAINTAB-PRART,
                TCJ1T-PRATX TO MAINTAB-PRATX,
                PRPS-POSID  TO MAINTAB-WBS.
                MAINTAB-WBS+7 = '0000'.    "Turns WBS into projects
          MOVE: PRPS-POSID  TO MAINTAB-DIV.
                MAINTAB-DIV+2 = '000000000'. "Prepares for break on div
          PERFORM POP_PLAN.
          PERFORM POP_IDC.
          PERFORM POP_PROC.
    ENDSELECT.
*  endselect.

*Start of TR813 changes
    SELECT * FROM COSS WHERE OBJNR = PRPS-OBJNR AND GJAHR = P_YEAR
       AND VERSN = P_VERSN AND WRTTP = '01' AND KSTAR IN CS_KSTAR.
        SELECT SINGLE * FROM TCJ1T WHERE PRART = PRPS-PRART
                                     AND LANGU = SY-LANGU.
*          and langu = 'E'.
          CLEAR: MAINTAB.
          MOVE: TCJ1T-PRART TO MAINTAB-PRART,
                TCJ1T-PRATX TO MAINTAB-PRATX,
                PRPS-POSID  TO MAINTAB-WBS.
                MAINTAB-WBS+7 = '0000'.    "Turns WBS into projects
          MOVE: PRPS-POSID  TO MAINTAB-DIV.
                MAINTAB-DIV+2 = '000000000'. "Prepares for break on div
          PERFORM POP_COSTING_SHEET.
    ENDSELECT.

*End of TR813 changes
ENDFORM.

* This routine process the data and writes the output.
FORM WRITE_DATA.
 SORT MAINTAB BY PRATX WBS KSTAR.
 LOOP AT MAINTAB.
  AT NEW PRATX.                        "Control break - Project type
     ULINE: /1.
     FORMAT COLOR COL_NEGATIVE ON.
     WRITE: /1 SY-VLINE, 2 TEXT-007, 13 MAINTAB-PRATX(22), 54 SY-VLINE.
     FORMAT COLOR COL_NEGATIVE OFF.
     ULINE: /1(54).
     FORMAT COLOR COL_GROUP ON.
     FORMAT INTENSIFIED OFF.
*START of TR813 changes
*     WRITE: /1 TEXT-008, 13 TEXT-009, 56 TEXT-010, 80 TEXT-011,
*          100 TEXT-012, 125 TEXT-013.
     WRITE: /1 TEXT-008, 13 TEXT-009, 56 TEXT-010, 78 TEXT-021,
                         100 TEXT-011, 119 TEXT-012, 139 TEXT-013.
*END of TR813 changes
     FORMAT INTENSIFIED ON.
     FORMAT COLOR COL_GROUP OFF.
     SKIP 1.
  ENDAT.
  AT NEW WBS.                           "Control break - WBS/project
     SUM.
     ADD MAINTAB-PLAN01   FROM FIRST TO LAST GIVING PL_AMT.
     ADD MAINTAB-IDC001   FROM FIRST TO LAST GIVING IDC_AMT.
     ADD MAINTAB-PROC01   FROM FIRST TO LAST GIVING PROC_AMT.
     ADD MAINTAB-CSHEET01 FROM FIRST TO LAST GIVING CST_AMT.  "TR813
*     TOTAL     = PL_AMT + IDC_AMT + PROC_AMT.                "TR813
     TOTAL     = PL_AMT   + IDC_AMT + PROC_AMT + CST_AMT.     "TR813
     TOT_PL    = TOT_PL   + PL_AMT.
     TOT_IDC   = TOT_IDC  + IDC_AMT.
     TOT_PROC  = TOT_PROC + PROC_AMT.
     TOT_CST   = TOT_CST  + CST_AMT.                          "TR813
     S_TOT     = S_TOT    + TOTAL.
     S_PL      = S_PL     + PL_AMT.
     S_IDC     = S_IDC    + IDC_AMT.
     S_PROC    = S_PROC   + PROC_AMT.
     S_CST     = S_CST    + CST_AMT.                          "TR813
     IF TOTAL  = 0.
        CONTINUE.
     ENDIF.
     READ TABLE SUBTAB WITH KEY PSPID = MAINTAB-WBS.
     IF SUBTAB-PSPID NE MAINTAB-WBS.
        CLEAR: SUBTAB-POST1.
     ENDIF.
     FORMAT COLOR 4 ON INTENSIFIED OFF.
     WRITE: /2 MAINTAB-WBS, 13 SUBTAB-POST1.
     FORMAT COLOR 4 OFF INTENSIFIED ON.
     FORMAT COLOR 2 ON INTENSIFIED OFF.
*START of TR813 changes
*     WRITE: 44 PL_AMT DECIMALS 0, 66 IDC_AMT DECIMALS 0,
*            88 PROC_AMT DECIMALS 0, 110 TOTAL DECIMALS 0.
     WRITE: 44 PL_AMT DECIMALS 0, 65 CST_AMT DECIMALS 0,
            86 IDC_AMT DECIMALS 0,
            107 PROC_AMT DECIMALS 0, 128 TOTAL DECIMALS 0.
*END of TR813 changes
     FORMAT COLOR 2 OFF INTENSIFIED ON.
     CLEAR: TOTAL, PL_AMT, IDC_AMT, PROC_AMT, CST_AMT.
  ENDAT.
  AT END OF DIV.
     FORMAT COLOR COL_TOTAL ON INTENSIFIED OFF.
     WRITE: /4 TEXT-015, MAINTAB-DIV(2), TEXT-016,
     44 S_PL DECIMALS 0, 65 S_CST DECIMALS 0,
     86 S_IDC DECIMALS 0,
     107 S_PROC DECIMALS 0, 128 S_TOT DECIMALS 0.
     FORMAT COLOR COL_NEGATIVE OFF.
     CLEAR: S_PL, S_IDC, S_PROC, S_CST, S_TOT.
     FORMAT COLOR COL_TOTAL OFF INTENSIFIED ON.
  ENDAT.

  AT END OF PRATX.                        "Control break - project type
     RESERVE 6 LINES.
     SKIP 1.
     WRITE: /1 SY-ULINE.
*     SUB_TOT = TOT_PL + TOT_IDC + TOT_PROC.
     SUB_TOT = TOT_PL + TOT_IDC + TOT_PROC + TOT_CST.
     GR_PL   = GR_PL + TOT_PL.
     GR_IDC  = GR_IDC + TOT_IDC.
     GR_PROC = GR_PROC + TOT_PROC.
     GR_CST  = GR_CST  + TOT_CST.                      "TR813
     GR_TOT  = GR_TOT + SUB_TOT.
     FORMAT COLOR COL_NEGATIVE ON.
     WRITE: /4 TEXT-013, 13 MAINTAB-PRATX, 43 SY-VLINE,
     44 TOT_PL DECIMALS 0, 65 TOT_CST DECIMALS 0,
     86 TOT_IDC DECIMALS 0,
     107 TOT_PROC DECIMALS 0, 128 SUB_TOT DECIMALS 0.
     FORMAT COLOR COL_NEGATIVE OFF.
     CLEAR: TOT_PL, TOT_IDC, TOT_PROC, TOT_CST, SUB_TOT.
  ENDAT.
 ENDLOOP.
ENDFORM.

* This routine writes the final/Grand totals.
FORM FINAL.
 RESERVE 6 LINES.
 WRITE: /43 SY-VLINE, 44 SY-ULINE.
 FORMAT COLOR COL_TOTAL ON.
 WRITE: /1 TEXT-017, 43 SY-VLINE.
 WRITE: 44 GR_PL DECIMALS 0, 65 GR_CST DECIMALS 0,
        86 GR_IDC DECIMALS 0,
        107 GR_PROC DECIMALS 0, 128 GR_TOT DECIMALS 0.
 FORMAT COLOR COL_TOTAL OFF.
 WRITE: /43 SY-VLINE, 44 SY-ULINE.
ENDFORM.

* This routine populates the maintab table with the planning values.
FORM POP_PLAN.
* IF COSP-KSTAR = PL_KSTAR.            "Planning    "TR813
 IF COSP-KSTAR IN PL_KSTAR.            "Planning    "TR813
    CLEAR TOTAL.
    ADD COSP-WKG001 FROM 1 TO 12 GIVING TOTAL.
    IF TOTAL NE 0.
       MOVE: COSP-KSTAR  TO MAINTAB-KSTAR,
             COSP-GJAHR  TO MAINTAB-GJAHR,
             COSP-WKG001 TO MAINTAB-PLAN01,
             COSP-WKG002 TO MAINTAB-PLAN02,
             COSP-WKG003 TO MAINTAB-PLAN03,
             COSP-WKG004 TO MAINTAB-PLAN04,
             COSP-WKG005 TO MAINTAB-PLAN05,
             COSP-WKG006 TO MAINTAB-PLAN06,
             COSP-WKG007 TO MAINTAB-PLAN07,
             COSP-WKG008 TO MAINTAB-PLAN08,
             COSP-WKG009 TO MAINTAB-PLAN09,
             COSP-WKG010 TO MAINTAB-PLAN10,
             COSP-WKG011 TO MAINTAB-PLAN11,
             COSP-WKG012 TO MAINTAB-PLAN12.
       APPEND MAINTAB.
     ENDIF.
 ENDIF.
ENDFORM.

* This routine populates the maintab table with the I.D.C. values.
FORM POP_IDC.
 IF COSP-KSTAR = ID_KSTAR.                             "I.D.C.
    CLEAR TOTAL.
    ADD COSP-WKG001 FROM 1 TO 12 GIVING TOTAL.
    IF TOTAL NE 0.
       MOVE: COSP-KSTAR  TO MAINTAB-KSTAR,
             COSP-GJAHR  TO MAINTAB-GJAHR,
             COSP-WKG001 TO MAINTAB-IDC001,
             COSP-WKG002 TO MAINTAB-IDC002,
             COSP-WKG003 TO MAINTAB-IDC003,
             COSP-WKG004 TO MAINTAB-IDC004,
             COSP-WKG005 TO MAINTAB-IDC005,
             COSP-WKG006 TO MAINTAB-IDC006,
             COSP-WKG007 TO MAINTAB-IDC007,
             COSP-WKG008 TO MAINTAB-IDC008,
             COSP-WKG009 TO MAINTAB-IDC009,
             COSP-WKG010 TO MAINTAB-IDC010,
             COSP-WKG011 TO MAINTAB-IDC011,
             COSP-WKG012 TO MAINTAB-IDC012.
        APPEND MAINTAB.
    ENDIF.
 ENDIF.
ENDFORM.

* This routine populates the maintab table with the proceeds values.
FORM POP_PROC.
 IF COSP-KSTAR = PR_KSTAR.                         "Proceeds
    CLEAR TOTAL.
    ADD COSP-WKG001 FROM 1 TO 12 GIVING TOTAL.
    IF TOTAL NE 0.
       MOVE: COSP-WKG001 TO MAINTAB-PROC01,
             COSP-WKG002 TO MAINTAB-PROC02,
             COSP-WKG003 TO MAINTAB-PROC03,
             COSP-WKG004 TO MAINTAB-PROC04,
             COSP-WKG005 TO MAINTAB-PROC05,
             COSP-WKG006 TO MAINTAB-PROC06,
             COSP-WKG007 TO MAINTAB-PROC07,
             COSP-WKG008 TO MAINTAB-PROC08,
             COSP-WKG009 TO MAINTAB-PROC09,
             COSP-WKG010 TO MAINTAB-PROC10,
             COSP-WKG011 TO MAINTAB-PROC11,
             COSP-WKG012 TO MAINTAB-PROC12.
       APPEND MAINTAB.
    ENDIF.
 ENDIF.
ENDFORM.

*START of TR813 changes
FORM POP_COSTING_SHEET.
    CLEAR TOTAL.
    ADD COSS-WKG001 FROM 1 TO 12 GIVING TOTAL.
    IF TOTAL NE 0.
       MOVE: COSS-KSTAR  TO MAINTAB-KSTAR,
             COSS-GJAHR  TO MAINTAB-GJAHR,
             COSS-WKG001 TO MAINTAB-CSHEET01,
             COSS-WKG002 TO MAINTAB-CSHEET02,
             COSS-WKG003 TO MAINTAB-CSHEET03,
             COSS-WKG004 TO MAINTAB-CSHEET04,
             COSS-WKG005 TO MAINTAB-CSHEET05,
             COSS-WKG006 TO MAINTAB-CSHEET06,
             COSS-WKG007 TO MAINTAB-CSHEET07,
             COSS-WKG008 TO MAINTAB-CSHEET08,
             COSS-WKG009 TO MAINTAB-CSHEET09,
             COSS-WKG010 TO MAINTAB-CSHEET10,
             COSS-WKG011 TO MAINTAB-CSHEET11,
             COSS-WKG012 TO MAINTAB-CSHEET12.
       APPEND MAINTAB.
     ENDIF.
ENDFORM.
*END of TR813 changes

* This event is used to display the main headings.
TOP-OF-PAGE.
 FORMAT COLOR COL_NORMAL ON.
 WRITE: /1 SY-ULINE.
 WRITE: /2 TEXT-001,12 SY-REPID, SY-SYSID,
        57 TEXT-002,113 TEXT-003,129 SY-PAGNO.
 WRITE: /1 TEXT-004,12 SY-UZEIT, 63 TEXT-005, 112 TEXT-006,122 SY-DATUM.
 WRITE: /59 TEXT-014, 67 P_VERSN, 70 ',', 72 P_YEAR(4).
 WRITE: /1 TEXT-018, P_VBUKR.
 ULINE: /1.
 FORMAT COLOR COL_NORMAL OFF.
