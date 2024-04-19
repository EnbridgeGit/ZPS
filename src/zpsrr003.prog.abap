REPORT ZPSRR003 NO STANDARD PAGE HEADING LINE-COUNT 64 LINE-SIZE 132.
********************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                          *
* Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)      *
*       Date: March 17, 1997                                       *
* Request ID:                                                      *
*                                                                  *
* The following program will generate a Project Summary Report     *
* sorted by the responsible cost centres.  This program will       *
* break on cost centres and will calculate all wbs-elements        *
* total related to their project number.  A grand total is         *
* displayed for the total plan.                                    *
* CHANGES                                                          *
* 98/03/24 md7140 #--- Added company code                          *
********************************************************************
TABLES: COSP,  "CO Object:  Cost Totals - External Postings
        CSKT,  "Cost Centre Texts
        PROJ,  "Project Definition
        PRPS.  "WBS (Work Breakdown Structure) Element Master Data

DATA:   PLAN     LIKE COSP-KSTAR VALUE '0000480199',  "Plan Cost Element
        IDC      LIKE COSP-KSTAR VALUE '0000324102',  "IDC  Cost Element
        PROCEEDS LIKE COSP-KSTAR VALUE '0000430025',  "Proc Cost Element
        FIRST(2) TYPE N VALUE '01',                   "Month periods
        LAST(2)  TYPE N VALUE '12',                   "Month Periods
        TMPDATE  LIKE SY-DATUM,                       "Temporary Date
        TMPVAR(12) TYPE C,                            "Temporary Variab1
        TMPVAR2  LIKE TMPVAR,                         "Temporary Variab2
        PL_AMT   LIKE COSP-WKG001,                    "Calculation field
        IDC_AMT  LIKE COSP-WKG001,
        PROC_AMT LIKE COSP-WKG001,
        TOTAL    LIKE COSP-WKG001,
        TOT_PL   LIKE COSP-WKG001,
        TOT_IDC  LIKE COSP-WKG001,
        TOT_PROC LIKE COSP-WKG001,
        SUB_TOT  LIKE COSP-WKG001,
        GR_PL    LIKE COSP-WKG001,
        GR_IDC   LIKE COSP-WKG001,
        GR_PROC  LIKE COSP-WKG001,
        GR_TOT   LIKE COSP-WKG001.

* Table for control-break processing.
DATA:  BEGIN OF MAINTAB OCCURS 10000,
         FKSTL  LIKE PRPS-FKSTL,                      "Resp. Cost Centre
         WBS    LIKE PRPS-POSID,                      "WBS element
         KSTAR  LIKE COSP-KSTAR,                      "Cost Element
         GJAHR  LIKE COSP-GJAHR,                      "Fiscal Year
         POST1  LIKE PROJ-POST1,
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
         FKSTL  LIKE PRPS-FKSTL,
   END OF SUBTAB.

SELECTION-SCREEN BEGIN OF BLOCK BEGIN WITH FRAME.

PARAMETERS: P_VBUKR LIKE PROJ-VBUKR OBLIGATORY MODIF ID ABC    "Company
                                   DEFAULT 'UGL'.
SELECT-OPTIONS:
            S_PROJ FOR PRPS-POSID MODIF ID ABC,               "Projects
            S_RCC  FOR PRPS-FKSTL MODIF ID ABC. "Responsible Cost Centre
PARAMETERS: P_VERSN LIKE TKA09-VERSN OBLIGATORY DEFAULT '000' "Version
                    MODIF ID ABC,
            P_YEAR  LIKE COSP-GJAHR DEFAULT SY-DATUM(4)       "Year
                    OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK BEGIN.

* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.
************************************************************************

START-OF-SELECTION.
 PERFORM INITIALIZE.
 PERFORM GET_PROJ.
 PERFORM GET_DATA.
 PERFORM WRITE_DATA.
 PERFORM FINAL.
END-OF-SELECTION.

* This routine initializes the internal tables and temp date variable.
FORM INITIALIZE.
 REFRESH: MAINTAB, SUBTAB.
 CLEAR:   MAINTAB, SUBTAB.
 WRITE: '99991231' TO TMPDATE.
ENDFORM.

* This routine gathers the project description.
FORM GET_PROJ.
 SELECT * FROM PRPS WHERE POSID IN S_PROJ
   AND FKSTL IN S_RCC.
   SELECT * FROM PROJ WHERE PSPID = PRPS-POSID
                        AND VBUKR = P_VBUKR.                 "98/03/24
     CLEAR: SUBTAB.
     MOVE: PRPS-PSPNR TO SUBTAB-PSPNR,
           PROJ-PSPID TO SUBTAB-PSPID,
           PROJ-POST1 TO SUBTAB-POST1,
           PRPS-FKSTL TO SUBTAB-FKSTL.
     APPEND SUBTAB.
   ENDSELECT.
 ENDSELECT.
ENDFORM.

* This routine gathers the cost element, values, version and projects.
FORM GET_DATA.
  SELECT * FROM PRPS WHERE POSID IN S_PROJ.
    SELECT * FROM COSP WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_YEAR AND VERSN = P_VERSN AND WRTTP = '01'.
      CHECK COSP-KSTAR = PLAN OR COSP-KSTAR = IDC OR
            COSP-KSTAR = PROCEEDS.
          CLEAR: MAINTAB.
          MOVE: PRPS-POSID  TO MAINTAB-WBS.
                MAINTAB-WBS+7 = '0000'.    "Turns WBS into projects
                SY-TABIX = 0.
                READ TABLE SUBTAB WITH KEY PSPID = MAINTAB-WBS.
                IF SY-TABIX > 0.
                   MOVE SUBTAB-FKSTL TO MAINTAB-FKSTL.
                ELSE. CONTINUE.
                ENDIF.
                MOVE SUBTAB-POST1 TO MAINTAB-POST1.
                IF SUBTAB-PSPID NE MAINTAB-WBS.
                   CLEAR: MAINTAB-POST1.
                ENDIF.
          PERFORM POP_PLAN.
          PERFORM POP_IDC.
          PERFORM POP_PROC.
    ENDSELECT.
  ENDSELECT.
ENDFORM.

* This routine process the data and writes the output.
FORM WRITE_DATA.
 SORT MAINTAB BY FKSTL WBS KSTAR.
 LOOP AT MAINTAB.
  SELECT SINGLE * FROM CSKT WHERE SPRAS = 'E'
         AND KOKRS = '10' AND KOSTL = MAINTAB-FKSTL
         AND DATBI = TMPDATE.
  AT NEW FKSTL.                        "Control break - Resp. Cost Ctr
     ULINE: /1.
     FORMAT COLOR COL_NEGATIVE ON.
     IF MAINTAB-FKSTL = ''.
        TMPVAR        = TEXT-015.
     ELSE.
        WRITE: MAINTAB-FKSTL TO TMPVAR.
     ENDIF.
     WRITE: /1 SY-VLINE, 2 TEXT-007, 13 TMPVAR, CSKT-KTEXT,
            54 SY-VLINE.
     FORMAT COLOR COL_NEGATIVE OFF.
     ULINE: /1(54).
     FORMAT COLOR COL_GROUP ON.
     FORMAT INTENSIFIED OFF.
     WRITE: /1 TEXT-008, 13 TEXT-009, 56 TEXT-010, 80 TEXT-011,
          100 TEXT-012, 125 TEXT-013.
     FORMAT INTENSIFIED ON.
     FORMAT COLOR COL_GROUP OFF.
     SKIP 1.
  ENDAT.
  AT NEW WBS.                           "Control break - WBS/project
     SUM.
     ADD MAINTAB-PLAN01 FROM FIRST TO LAST GIVING PL_AMT.
     ADD MAINTAB-IDC001 FROM FIRST TO LAST GIVING IDC_AMT.
     ADD MAINTAB-PROC01 FROM FIRST TO LAST GIVING PROC_AMT.
     TOTAL     = PL_AMT   + IDC_AMT + PROC_AMT.
     TOT_PL    = TOT_PL   + PL_AMT.
     TOT_IDC   = TOT_IDC  + IDC_AMT.
     TOT_PROC  = TOT_PROC + PROC_AMT.
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
     WRITE: 44 PL_AMT DECIMALS 0, 66 IDC_AMT DECIMALS 0,
            88 PROC_AMT DECIMALS 0, 110 TOTAL DECIMALS 0.
     FORMAT COLOR 2 OFF INTENSIFIED ON.
     CLEAR: TOTAL, PL_AMT, IDC_AMT, PROC_AMT.
  ENDAT.
  AT END OF FKSTL.                        "Control break - Resp. Cost Ct
     IF MAINTAB-FKSTL = ''.
        TMPVAR2       = TEXT-015.
     ELSE.
        WRITE: MAINTAB-FKSTL TO TMPVAR2.
     ENDIF.
     RESERVE 6 LINES.
     SKIP 1.
     WRITE: /1 SY-ULINE.
     SUB_TOT = TOT_PL + TOT_IDC + TOT_PROC.
     GR_PL   = GR_PL + TOT_PL.
     GR_IDC  = GR_IDC + TOT_IDC.
     GR_PROC = GR_PROC + TOT_PROC.
     GR_TOT  = GR_TOT + SUB_TOT.
     FORMAT COLOR COL_NEGATIVE ON.
     WRITE: /4 TEXT-013, 13 TMPVAR2, 43 SY-VLINE,
     44 TOT_PL DECIMALS 0, 66 TOT_IDC DECIMALS 0,
     88 TOT_PROC DECIMALS 0, 110 SUB_TOT DECIMALS 0.
     FORMAT COLOR COL_NEGATIVE OFF.
     CLEAR: TOT_PL, TOT_IDC, TOT_PROC, SUB_TOT.
  ENDAT.
 ENDLOOP.
ENDFORM.

* This routine writes the final/Grand totals.
FORM FINAL.
 RESERVE 6 LINES.
 WRITE: /43 SY-VLINE, 44 SY-ULINE.
 FORMAT COLOR COL_TOTAL ON.
 WRITE: /1 TEXT-016, 43 SY-VLINE.
 WRITE: 44 GR_PL DECIMALS 0, 66 GR_IDC DECIMALS 0,
        88 GR_PROC DECIMALS 0, 110 GR_TOT DECIMALS 0.
 FORMAT COLOR COL_TOTAL OFF.
 WRITE: /43 SY-VLINE, 44 SY-ULINE.
ENDFORM.

* This routine populates the maintab table with the planning values.
FORM POP_PLAN.
 IF COSP-KSTAR = PLAN.                          "Planning
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
ENDFORM.

* This routine populates the maintab table with the I.D.C. values.
FORM POP_IDC.
 IF COSP-KSTAR = IDC.                             "I.D.C.
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
ENDFORM.

* This routine populates the maintab table with the proceeds values.
FORM POP_PROC.
 IF COSP-KSTAR = PROCEEDS.                         "Proceeds
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
ENDFORM.

* This event is used to display the main headings.
TOP-OF-PAGE.
 FORMAT COLOR COL_NORMAL ON.
 WRITE: /1 SY-ULINE.
 WRITE: /2 TEXT-001,12 SY-REPID, SY-SYSID,
        57 TEXT-002,113 TEXT-003,129 SY-PAGNO.
 WRITE: /1 TEXT-004,12 SY-UZEIT, 59 TEXT-005, 112 TEXT-006,122 SY-DATUM.
 WRITE: /59 TEXT-014, 67 P_VERSN(1), 68 ',', 70 P_YEAR(4).
 WRITE: /1 TEXT-017, P_VBUKR.
 ULINE: /1.
 FORMAT COLOR COL_NORMAL OFF.
