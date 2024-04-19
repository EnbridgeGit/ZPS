REPORT ZPSRR004 NO STANDARD PAGE HEADING LINE-COUNT 64 LINE-SIZE 132.
********************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                          *
* Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)      *
*       Date: March 19, 1997                                       *
* Request ID:                                                      *
*                                                                  *
* The following program will generate a Project Summary Report     *
* by project control number within project type.  This program will*
* break on the project types and will calculate all wbs-elements   *
* total related to their project number.  A grand total is         *
* displayed for the total plan.                                    *
********************************************************************
* 98/03/23 md7140 #--- Change in template definition. Added
*                      company selection.
* 97/08/12 md7140 changed PROJECTCONTROLNUMBER to PROJECT_CNTR_NUMBER
* 97/04/02 md7140 expand maintab-atflv from 3 to 6 digits          *
********************************************************************
TABLES: AUSB,  "Table plant status
        CABN,  "Characteristics
        COSP,  "CO Object:  Cost Totals - External Postings
        PROJ,  "Project Definition
        PRPS,  "WBS (Work Breakdown Structure) Element Master Data
        TCJ1T, "Project Types
        AUSP.

CONSTANTS: BLANKVAR(132) TYPE C VALUE ''.    "Writes blanks

DATA:  FLAGPRT(3)        TYPE C VALUE 'no'.

DATA:   PLAN     LIKE COSP-KSTAR VALUE '0000480199',  "Plan Cost Element
        IDC      LIKE COSP-KSTAR VALUE '0000324102',  "IDC  Cost Element
        PROCEEDS LIKE COSP-KSTAR VALUE '0000430025',  "Proc Cost Element
        FIRST(2) TYPE N VALUE '01',                   "Month periods
        LAST(2)  TYPE N VALUE '12',                   "Month Periods
        PL_AMT   LIKE COSP-WKG001,                    "Calculation field
        IDC_AMT  LIKE COSP-WKG001,
        PROC_AMT LIKE COSP-WKG001,
        TOTAL    LIKE COSP-WKG001,
        S_PL     LIKE COSP-WKG001,
        S_IDC    LIKE COSP-WKG001,
        S_PROC   LIKE COSP-WKG001,
        S_TOT    LIKE COSP-WKG001,
        TOT_PL   LIKE COSP-WKG001,
        TOT_IDC  LIKE COSP-WKG001,
        TOT_PROC LIKE COSP-WKG001,
        SUB_TOT  LIKE COSP-WKG001,

        GR_PL    LIKE COSP-WKG001,
        GR_IDC   LIKE COSP-WKG001,
        GR_PROC  LIKE COSP-WKG001,
        GR_TOT   LIKE COSP-WKG001,
        ATWRT  LIKE AUSP-ATWRT,                      "Project Control #
        POST1    LIKE PROJ-POST1.

* Table for control-break processing.
DATA:  BEGIN OF MAINTAB OCCURS 10000,
         PRATX  LIKE TCJ1T-PRATX,                     "Type - Descriptio
         ATWRT  LIKE AUSP-ATWRT,                      "Project Control #
*        atflv(6) type n,
         WBS    LIKE PRPS-POSID,                      "WBS element
         POST1  LIKE PROJ-POST1,
         PRART  LIKE TCJ1T-PRART,                     "Project Type
         KSTAR  LIKE COSP-KSTAR,                      "Cost Element
         GJAHR  LIKE COSP-GJAHR,                      "Fiscal Year
         PL_AMT  LIKE COSP-WKG001,                "Total plans
         IDC_AMT LIKE COSP-WKG001,                "Total IDC
         PROC_AMT LIKE COSP-WKG001,               "Total Proceeds
   END OF MAINTAB,

* Table used for storing the projects description.
   BEGIN OF SUBTAB OCCURS 1000,
         PSPID  LIKE PROJ-PSPID,
         PSPNR  LIKE PRPS-PSPNR,
         POST1  LIKE PROJ-POST1,
         ATINN  LIKE CABN-ATINN,
         ATFLV  LIKE AUSP-ATFLV,
         ATWRT  LIKE AUSP-ATWRT,
   END OF SUBTAB.

   DATA: BEGIN OF CHAR_TAB OCCURS 20.
         INCLUDE STRUCTURE AUSP.
   DATA: END OF CHAR_TAB.

DATA:  OBJECT         LIKE AUSP-OBJEK,
       CHARIC         LIKE CABN-ATNAM,
       G_ATINN        LIKE CABN-ATINN,
       G_ATINN_NEW    LIKE CABN-ATINN,
       TMPVAL(2)      TYPE P,
       FLAG1(1)       TYPE C VALUE 'N'.

SELECTION-SCREEN BEGIN OF BLOCK BEGIN WITH FRAME.

PARAMETERS: P_VBUKR LIKE PROJ-VBUKR OBLIGATORY MODIF ID ABC   "Company
                                    DEFAULT 'UGL'.
SELECT-OPTIONS:
            S_DIV   FOR PROJ-VERNR MODIF ID ABC,               "Div
            S_PROJ FOR PROJ-PSPID MODIF ID ABC,               "Projects
            S_TYPE FOR PRPS-PRART MODIF ID ABC                "Type
              DEFAULT '01',
            S_ATWRT FOR AUSP-ATWRT MODIF ID ABC.   "Project Control #
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
 PERFORM GET_ATINN.
 SORT CHAR_TAB BY ATINN.
 PERFORM GET_DATA.
 PERFORM WRITE_DATA.
 PERFORM FINAL.
END-OF-SELECTION.

* This routine initializes the internal tables.
FORM INITIALIZE.
 REFRESH: MAINTAB.
 CLEAR:   MAINTAB.
* move 'PROJECTCONTROLNUMBER'  to charic.
 MOVE 'PROJECT_CNTR_NUMBER'  TO CHARIC.
ENDFORM.


* This routine gathers the cost element, values, version and projects.
FORM GET_DATA.
SELECT * FROM PROJ WHERE PSPID IN S_PROJ
                     AND VERNR IN S_DIV
                     AND VBUKR = P_VBUKR.                "98/03/24
  CLEAR ATWRT.
* if proj-pspid+5(3) co '0123456789'.              "Eliminate templates
  IF PROJ-PSPID+5(2) CO '0123456789'.              "Eliminate templates
     SELECT * FROM PRPS WHERE PSPHI = PROJ-PSPNR
                          AND PRART IN S_TYPE
                          AND STUFE = '1'.         "ProjControl# level 1
       CLEAR: MAINTAB.
       CONCATENATE: 'PR' PRPS-PSPNR INTO OBJECT.   "Get characteristic
       PERFORM FIND_CHARACTERISTIC.
     ENDSELECT.

     IF  ATWRT IN S_ATWRT.
     SELECT * FROM PRPS WHERE PSPHI = PROJ-PSPNR
                          AND PRART IN S_TYPE
                          AND PLAKZ = 'X'.     "Lowest element-no rollup

       SELECT SINGLE * FROM TCJ1T WHERE PRART = PRPS-PRART
                                    AND LANGU = 'E'.
       MOVE: TCJ1T-PRATX TO MAINTAB-PRATX,
             ATWRT       TO MAINTAB-ATWRT,
             TCJ1T-PRART TO MAINTAB-PRART,
             PRPS-POSID  TO MAINTAB-WBS,
             PROJ-POST1  TO MAINTAB-POST1.
       MAINTAB-WBS+7 = '0000'.                  "Turns WBS into projects

    SELECT * FROM COSP
          WHERE OBJNR = PRPS-OBJNR
            AND GJAHR = P_YEAR
            AND VERSN = P_VERSN
            AND WRTTP = '01'.
      CHECK COSP-KSTAR = PLAN OR COSP-KSTAR = IDC OR
            COSP-KSTAR = PROCEEDS.
      PERFORM POP_PLAN.
      PERFORM POP_IDC.
      PERFORM POP_PROC.
    ENDSELECT.                                  "End of COSP
  ENDSELECT.                                    "End of PRPS
  ENDIF.                                        "End of IF ATWRT test
  ENDIF.                                        "End of templates
ENDSELECT.                                      "End of PROJ
ENDFORM.

* This routine process the data and writes the output.
FORM WRITE_DATA.
 SORT MAINTAB BY PRATX ATWRT WBS.
LOOP AT MAINTAB.
  MOVE MAINTAB-POST1 TO POST1.
  AT NEW PRATX.                        "Control break - Project type
     NEW-PAGE.
  ENDAT.

  AT NEW ATWRT.                         "Control-break on control number
    SUM.
    FORMAT COLOR 5 ON.
    WRITE: /.                                                  "MD7140
    IF MAINTAB-ATWRT = SPACE.                         "Blank Control#
       WRITE: /13 TEXT-015, TEXT-N_A.
    ELSE.
       WRITE: /13 TEXT-015, MAINTAB-ATWRT, BLANKVAR(7).
    ENDIF.
    FORMAT COLOR 5 OFF.
    FORMAT COLOR 2 ON INTENSIFIED OFF.
    WRITE: BLANKVAR(86).
    FORMAT COLOR 2 OFF INTENSIFIED ON.
 ENDAT.

  AT NEW WBS.                           "Control break - WBS/project
     SUM.
     MOVE MAINTAB-PL_AMT TO PL_AMT.
     MOVE MAINTAB-IDC_AMT TO IDC_AMT.
     MOVE MAINTAB-PROC_AMT TO PROC_AMT.

*    s_tot   = s_tot + total.
     S_PL    = S_PL + PL_AMT.
     S_IDC   = S_IDC + IDC_AMT.
     S_PROC  = S_PROC + PROC_AMT.

     TOTAL     = PL_AMT + IDC_AMT + PROC_AMT.
     TOT_PL    = TOT_PL + PL_AMT.
     TOT_IDC   = TOT_IDC + IDC_AMT.
     TOT_PROC  = TOT_PROC + PROC_AMT.
     IF TOTAL  = 0.
        CONTINUE.
     ENDIF.
*     read table subtab with key pspid = maintab-wbs.
*     if subtab-pspid ne maintab-wbs.
*        clear: subtab-post1.
*     endif.
     IF FLAG1 = 'Y'.
         FORMAT COLOR 4 ON INTENSIFIED OFF.
         WRITE: /5 MAINTAB-WBS, 16 POST1(37).
         FORMAT COLOR 4 OFF INTENSIFIED ON.
         FORMAT COLOR 2 ON INTENSIFIED OFF.
         WRITE: 44 PL_AMT DECIMALS 0, 66 IDC_AMT DECIMALS 0,
                88 PROC_AMT DECIMALS 0, 110 TOTAL DECIMALS 0.
         FORMAT COLOR 2 OFF INTENSIFIED ON.
     ELSEIF FLAG1 = 'N'.
         FORMAT COLOR 4 ON INTENSIFIED OFF.
         WRITE: /2 MAINTAB-WBS, 13 POST1.
         FORMAT COLOR 4 OFF INTENSIFIED ON.
         FORMAT COLOR 2 ON INTENSIFIED OFF.
         WRITE: 44 PL_AMT DECIMALS 0, 66 IDC_AMT DECIMALS 0,
                88 PROC_AMT DECIMALS 0, 110 TOTAL DECIMALS 0.
         FORMAT COLOR 2 OFF INTENSIFIED ON.
     ENDIF.
     CLEAR: TOTAL, PL_AMT, IDC_AMT, PROC_AMT.
  ENDAT.
  AT END OF ATWRT.
     FORMAT COLOR COL_TOTAL ON INTENSIFIED OFF.
     IF MAINTAB-ATWRT = SPACE.
        WRITE: /5 TEXT-016, 13 TEXT-015, TEXT-N_A.
     ELSE.
        WRITE: /5 TEXT-016, 13 TEXT-015, MAINTAB-ATWRT.
     ENDIF.
     S_TOT = S_PL + S_IDC + S_PROC.
     WRITE: 44 S_PL DECIMALS 0, 66 S_IDC DECIMALS 0,
            88 S_PROC DECIMALS 0, 110 S_TOT DECIMALS 0.
     FORMAT COLOR COL_NEGATIVE OFF.
     CLEAR: S_PL, S_IDC, S_PROC, S_TOT.
     FORMAT COLOR COL_TOTAL OFF INTENSIFIED ON.
  ENDAT.
  AT END OF PRATX.                        "Control break - project type
     RESERVE 6 LINES.
     SKIP 1.
     WRITE: /1 SY-ULINE.
     SUB_TOT = TOT_PL + TOT_IDC + TOT_PROC.
     GR_PL   = GR_PL + TOT_PL.
     GR_IDC  = GR_IDC + TOT_IDC.
     GR_PROC = GR_PROC + TOT_PROC.
     GR_TOT  = GR_TOT + SUB_TOT.
     FORMAT COLOR COL_NEGATIVE ON.
     WRITE: /4 TEXT-013, 13 MAINTAB-PRATX, 43 SY-VLINE,
     44 TOT_PL DECIMALS 0, 66 TOT_IDC DECIMALS 0,
     88 TOT_PROC DECIMALS 0, 110 SUB_TOT DECIMALS 0.
     WRITE: /43 SY-VLINE, 44 SY-ULINE.
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
 WRITE: /1 TEXT-017, 43 SY-VLINE.
 WRITE: 44 GR_PL DECIMALS 0, 66 GR_IDC DECIMALS 0,
        88 GR_PROC DECIMALS 0, 110 GR_TOT DECIMALS 0.
 FORMAT COLOR COL_TOTAL OFF.
 WRITE: /43 SY-VLINE, 44 SY-ULINE.
ENDFORM.

* This routine populates the maintab table with the planning values.
FORM POP_PLAN.
 IF COSP-KSTAR = PLAN.                          "Planning
    MOVE: COSP-KSTAR  TO MAINTAB-KSTAR,
          COSP-GJAHR  TO MAINTAB-GJAHR.
    ADD COSP-WKG001 FROM FIRST TO LAST GIVING MAINTAB-PL_AMT.
    APPEND MAINTAB.
    CLEAR MAINTAB-PL_AMT.
ENDIF.
ENDFORM.

* This routine populates the maintab table with the I.D.C. values.
FORM POP_IDC.
 IF COSP-KSTAR = IDC.                             "I.D.C.
    MOVE: COSP-KSTAR  TO MAINTAB-KSTAR,
          COSP-GJAHR  TO MAINTAB-GJAHR.
    ADD COSP-WKG001 FROM FIRST TO LAST GIVING MAINTAB-IDC_AMT.
    APPEND MAINTAB.
    CLEAR MAINTAB-IDC_AMT.
 ENDIF.
ENDFORM.

* This routine populates the maintab table with the proceeds values.
FORM POP_PROC.
 IF COSP-KSTAR = PROCEEDS.                         "Proceeds
    ADD COSP-WKG001 FROM FIRST TO LAST GIVING MAINTAB-PROC_AMT.
    APPEND MAINTAB.
    CLEAR MAINTAB-PROC_AMT.
 ENDIF.
ENDFORM.

* This event is used to display the main headings.
TOP-OF-PAGE.
 FORMAT COLOR COL_NORMAL ON.
 WRITE: /1 SY-ULINE.
 WRITE: /2 TEXT-001,12 SY-REPID, SY-SYSID,
        59 TEXT-002,113 TEXT-003,129 SY-PAGNO.
 WRITE: /1 TEXT-004,12 SY-UZEIT, 54 TEXT-005, 112 TEXT-006,122 SY-DATUM.
 WRITE: /1 TEXT-018, P_VBUKR.
 WRITE: /59 TEXT-014, 67 P_VERSN, 70 ',', 72 P_YEAR(4).
 ULINE: /1.
 FORMAT COLOR COL_NORMAL OFF.
 ULINE: /1.
 FORMAT COLOR COL_NEGATIVE ON.
 WRITE: /1 SY-VLINE, 2 TEXT-007, 13 MAINTAB-PRATX(22), 54 SY-VLINE.
 FORMAT COLOR COL_NEGATIVE OFF.
 ULINE: /1(54).
 FORMAT COLOR COL_GROUP ON.
 FORMAT INTENSIFIED OFF.
 WRITE: /1 TEXT-008, 13 TEXT-009, 56 TEXT-010, 80 TEXT-011,
       100 TEXT-012, 125 TEXT-013.
 FORMAT INTENSIFIED ON.
 FORMAT COLOR COL_GROUP OFF.
 SKIP 1.

* Routine used to get the internal character number for project control*
FORM GET_ATINN.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
           CLASS_TYPE                   = '014'
           FEATURE_NEUTRAL_NAME         = CHARIC
       IMPORTING
           FEATURE_ID                   = G_ATINN
       EXCEPTIONS
           INVALID_CLASS_TYPE           = 1
           MISSING_FEATURE_INFORMATION  = 2
           NO_FEATURE_FOUND             = 3
           NO_FEATURE_VALID             = 4
           NO_LANGUAGE                  = 5
           OTHERS                       = 6.
 IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTICS OF ',
 'PROJECT_CNTR_NUMBER'.
 ENDIF.
 MOVE G_ATINN                 TO G_ATINN_NEW.
ENDFORM.

* Routine used to get the value of the project control number *
FORM FIND_CHARACTERISTIC.
    REFRESH CHAR_TAB.
    CALL FUNCTION 'CLFM_SELECT_AUSP'
        EXPORTING
            MAFID           = 'O'
            CLASSTYPE       = '014'
            OBJECT          = OBJECT
        TABLES
            EXP_AUSP        = CHAR_TAB
        EXCEPTIONS
            NO_VALUES       = 1
            OTHERS          = 2.
    IF  SY-SUBRC EQ 0.
        SORT CHAR_TAB BY ATINN.
        READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_NEW BINARY SEARCH.
        IF SY-SUBRC EQ 0.
           MOVE CHAR_TAB-ATWRT TO ATWRT.
        ENDIF.
    ENDIF.
 CLEAR: OBJECT.
ENDFORM.
