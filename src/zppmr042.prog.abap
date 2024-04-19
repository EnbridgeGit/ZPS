REPORT ZPPMR042 NO STANDARD PAGE HEADING LINE-SIZE 170
                LINE-COUNT 58 MESSAGE-ID ZS.

************************************************************************
*                                                                      *
*   PROGRAM:    ZPPMR042                                               *
*   PROGRAMMER: Mohammad Khan                                          *
*   CLIENT:     Union Gas                                              *
*   DATE:       OCT 2006.                                              *
*                                                                      *
*   The purpose of this program is to produce Capital report that      *
*   would be helpful for the user to determine parameters (Variants)   *
*   data for program zppmr031.                                         *
*                                                                      *
*IMPORTANT NOTE: 1-This program is called/submitted by program ZPPMR031*
*                 Changes in this should be reflected in ZPPMR031 (if  *
*                 required).                                           *
*                2- The initialization routine is commented out becuse *
*                   when this program is called by ZPPMR031, if initia-*
*                   tion is active then it would overlap WBS passed by *
*                   program ZPPMR031. With this program ZPPMR042 can be*
*                   run through ZPPME031 or directly.                  *
************************************************************************
* CHANGES                                                              *
*                                                                      *
************************************************************************

TABLES: PRPS,               " WBS element master data
        PROJ,               " Project definition
        COSS,               " CO object: internal postings
        COSP,               " CO object: external postings
        T001,               " Company code
        ZPSDIVGR.           " Division Grouping

DATA: BEGIN OF ITAB OCCURS 0,
        PSPHI         LIKE PRPS-PSPHI,              "Proj Number
     END OF ITAB.

DATA: BEGIN OF ITAB1 OCCURS 0,
        VERNR         LIKE PRPS-VERNR,              "Division
        POSKI         LIKE PRPS-POSKI,              "Project POSKI
        POSID         LIKE PRPS-POSID,              "Proj Number
        PSPHI         LIKE PRPS-PSPHI,              "Proj Number
        PSPRI         LIKE PRPS-PSPRI,              "Proj. Priority Code
        PRART         LIKE PRPS-PRART,              "Project Type Code
        OBJNR         LIKE PRPS-OBJNR,              "Object Number
        POST1         LIKE PRPS-POST1,              "WBS Description
     END OF ITAB1.

DATA: BEGIN OF ITAB2 OCCURS 0,
        PROJC(9)      TYPE C,                       "Project POSKI+0(9)
        POST1         LIKE PROJ-POST1,   "Project Description (40)
        WBS(4)        TYPE C,            "Project POSKI+9(13)
        POST2         LIKE PRPS-POST1,   "WBS Description (40)
        VERNR         LIKE PRPS-VERNR,              "Division
        PSPHI         LIKE PRPS-PSPHI,              "Proj Number
        PSPRI         LIKE PRPS-PSPRI,              "Proj. Priority Code
        PRART         LIKE PRPS-PRART,              "Project Type Code
        ACTL_PP(8)    TYPE P,                 "Actual - Period to Period
        PLAN_PP(8)    TYPE P,                 "Plan   - Period to Period
        PLAN_YR(8)    TYPE P,                 "Plan $ - YEAR
     END OF ITAB2.

DATA: BEGIN OF REPTAB OCCURS 0,
        PROJC(9)      TYPE C,            "Project POSKI+0(9)
        POST1         LIKE PROJ-POST1,   "Project Description (40)
        WBS(4)        TYPE C,            "Project POSKI+9(4)
        POST2         LIKE PRPS-POST1,   "WBS Description (40)
        ACTL_PP(8)    TYPE P,            "Actual from Period - To Period
        PLAN_PP(8)    TYPE P,            "Plan   from Period - To Period
        PLAN_YR(8)    TYPE P,            "Plan $   - Year
     END OF REPTAB.

DATA: AMT01(8)            TYPE P,
      AMT02(8)            TYPE P,
      W_YR_PLAN(8)        TYPE P,
      W_PLAN_YEAR(8)      TYPE P,
      W_PP_ACTUAL(8)      TYPE P,
      W_OV_UND_YTD(8)     TYPE P,
      W_PP_PLAN(8)        TYPE P,
      PRINT_YTDPLAN_OVERUNDER(8) TYPE P,
      PRINT_%_COMPLETE(4)    TYPE P,
      PRINT_%_INCOMPLETE(4)  TYPE P,
      OUTLOOK_%_REMAINING(4) TYPE P,
      PRINT_YEARPLAN_OVERUNDER(8) TYPE P,
      USA_OUTLOOK(8)         TYPE P,
      AVAILABLE_TO_SPEND(8)  TYPE P,
      G_ATINN           LIKE CABN-ATINN,              "Proj ctr REQUIRED
      G_ATINN_PC        LIKE CABN-ATINN,              "Project Ctrl Char
      W_PCTRL,                                        "Project control
      PROJ_CONTROL(6),                                "Project control
      W_GRPDSCR(25)     TYPE C,
      OBJECT            LIKE AUSP-OBJEK,
      CHARIC            LIKE CABN-ATNAM,
      DASHES,
      W_GRPNUM(2)       TYPE N,
      WORK_PSPHI        LIKE PRPS-PSPHI,
      WORK_PSPRI        LIKE PRPS-PSPRI,
      WORK_PRART        LIKE PRPS-PRART,
      WORK_VERNR        LIKE PRPS-VERNR,
      WORK_PCTRL(6)     TYPE C,
      WORK_POST1(35)    TYPE C,
      WORK_POST2(35)    TYPE C,
      WORK_GRPDSCR(25)  TYPE C,
      WORK_TEXT(60)     TYPE C,
      WORK_OPTION(11)   TYPE C VALUE 'START_EXCEL',
      HEAD_PRINT(1)     VALUE 'Y',
      US_RATE(8)        TYPE P DECIMALS 5.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5) TYPE C,
        DDIC_FIELD(5) TYPE C,
        KEY TYPE C,
      END OF PROT_HEADER.
DATA  ERRORTAB    LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
DATA: RETCODE LIKE SY-SUBRC.
RANGES: R_WBS  FOR PRPS-POSID+7(5).

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME. " TITLE TEXT-005.
PARAMETERS:
P_REPTTL LIKE SY-TITLE DEFAULT 'CORPORATE IT CAPITAL VALIDATION REPORT',
P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),        "Fiscal Year
P_CCODE LIKE PRPS-PBUKR DEFAULT 'UGL',                "Company code
P_VERS  LIKE COSP-VERSN DEFAULT '0',                  "Plan Version
P_BEGMTH(2)         DEFAULT 1,                        "Start Month
P_ENDMTH(2)         DEFAULT 12.                       "End Month
SELECT-OPTIONS:
        S_PSPRI  FOR PRPS-PSPRI,                        "Priority
        S_VERNR  FOR PRPS-VERNR,                        "Division
        S_WBS    FOR PRPS-POSID+7(4).                   "WBS element
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME TITLE TEXT-003.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_EXCL RADIOBUTTON GROUP RBCR.            "EXCEL SHEET
SELECTION-SCREEN END OF BLOCK BOX5.

SELECTION-SCREEN END OF BLOCK BOX1.

*-----------------------  END of SELECTION SCREEN-----------------------
************************************************************************

*------------------------ INITIALIZATION.   ------------------------
*INITIALIZATION.
*
* S_WBS  = 'IBT76417643'.
* APPEND S_WBS.
* S_WBS  = 'IBT76717675'.
* APPEND S_WBS.
* S_WBS  = 'IBT98419842'.
* APPEND S_WBS.
* S_WBS  = 'IBT98809881'.
* APPEND S_WBS.

*-------------------------  START-OF-SELECTION ------------------------
AT SELECTION-SCREEN.
 DATA: WORK_WBS(4) TYPE N.
  CLEAR: R_WBS.
  REFRESH: R_WBS.
  LOOP AT S_WBS.
   IF S_WBS-OPTION = 'BT'.
      MOVE 'I'  TO R_WBS-SIGN.
      MOVE 'CP' TO R_WBS-OPTION.
      MOVE S_WBS-LOW+0(4) TO WORK_WBS.
      CONCATENATE '*' WORK_WBS INTO R_WBS-LOW.
      APPEND R_WBS.
    WHILE WORK_WBS <> S_WBS-HIGH.
       MOVE 'I'  TO R_WBS-SIGN.
       MOVE 'CP' TO R_WBS-OPTION.
       WORK_WBS = WORK_WBS + 1.
       CONCATENATE '*' WORK_WBS INTO R_WBS-LOW.
       APPEND R_WBS.
    ENDWHILE.
   ELSE.
      MOVE 'I'  TO R_WBS-SIGN.
      MOVE 'CP' TO R_WBS-OPTION.
      CONCATENATE '*' S_WBS-LOW INTO R_WBS-LOW.
      APPEND R_WBS.
   ENDIF.
 ENDLOOP.

************************************************************************

START-OF-SELECTION.

SELECT SINGLE * FROM T001                    "Company Code
  WHERE BUKRS = P_CCODE.

 SELECT PSPHI
   INTO TABLE ITAB
   FROM PRPS
   WHERE PBUKR = P_CCODE
     AND VERNR IN S_VERNR
     AND POSKI IN R_WBS
     AND PSPRI IN S_PSPRI
     AND LOEVM <> 'X'.                        "Deletion Indicator


 SELECT VERNR POSKI POSID PSPHI PSPRI PRART OBJNR POST1
   INTO TABLE ITAB1
   FROM PRPS
    FOR ALL ENTRIES IN ITAB
   WHERE PSPHI = ITAB-PSPHI
     AND PBUKR = P_CCODE
     AND VERNR IN S_VERNR
     AND PSPRI IN S_PSPRI
     AND BELKZ  = 'X'                         "Account Assignment
     AND LOEVM <> 'X'.                        "Deletion Indicator

 LOOP AT ITAB1.
   IF ITAB1-POSKI+7(2) CO '1234567890'.       "Eliminates templates
      PERFORM GET_AMOUNTS.
      IF W_PP_ACTUAL    <> 0              OR
         W_YR_PLAN      <> 0              OR
         W_PP_PLAN      <> 0.
         PERFORM BUILD_TABLE.
      ENDIF.
   ENDIF.
         CLEAR: W_PP_ACTUAL, W_YR_PLAN, W_PP_PLAN.
 ENDLOOP.


 SORT ITAB2 BY VERNR PROJC.

 LOOP AT ITAB2.

   MOVE ITAB2-PSPHI    TO WORK_PSPHI.
   MOVE ITAB2-POST2    TO WORK_POST2.
   MOVE ITAB2-VERNR    TO WORK_VERNR.
   IF ITAB2-PSPRI <> ' '.
      MOVE ITAB2-PSPRI TO WORK_PSPRI.
   ENDIF.
   IF ITAB2-PRART <> '  '.
      MOVE ITAB2-PRART TO WORK_PRART.
   ENDIF.
      PERFORM WRITE_DETAIL_LINE.

ENDLOOP.

IF NOT REPTAB[] IS INITIAL.
   MOVE TEXT-CLT  TO WORK_TEXT+0(7).
   MOVE SY-SYSID  TO WORK_TEXT+8(5).
   MOVE SY-MANDT  TO WORK_TEXT+14(4).
   MOVE TEXT-DTE  TO WORK_TEXT+21(5).
   WRITE SY-DATUM TO WORK_TEXT+27(10).
   MOVE TEXT-TME  TO WORK_TEXT+40(5).
   WRITE SY-UZEIT TO WORK_TEXT+46(10).
   IF P_EXCL <> 'X'.
      CLEAR WORK_OPTION.
   ENDIF.
   PERFORM REPORT_PRINTING.
ENDIF.


************************************************************************
*-------------------------  WRITE_DETAIL_LINE -------------------------*
************************************************************************
FORM WRITE_DETAIL_LINE.

  SELECT SINGLE * FROM PROJ
    WHERE PSPNR = WORK_PSPHI.
    IF SY-SUBRC = 0.
       MOVE  PROJ-POST1+0(35) TO WORK_POST1.
    ENDIF.

     MOVE: ITAB2-PROJC TO REPTAB-PROJC,
           ITAB2-WBS   TO REPTAB-WBS,
           WORK_POST1  TO REPTAB-POST1,
           WORK_POST2  TO REPTAB-POST2.
     PERFORM WRITE_DOLLARS.
     APPEND REPTAB.
     CLEAR: REPTAB, WORK_POST1, WORK_PSPRI, WORK_PRART.
ENDFORM.
************************************************************************
*-------------------------  WRITE_DOLLARS ------------------------------
************************************************************************
FORM WRITE_DOLLARS.

   MOVE: ITAB2-ACTL_PP       TO REPTAB-ACTL_PP,
         ITAB2-PLAN_PP       TO REPTAB-PLAN_PP,
         ITAB2-PLAN_YR       TO REPTAB-PLAN_YR.
ENDFORM.
*---------------------  BUILD_TABLE --------------------------
FORM BUILD_TABLE.

  ITAB2-VERNR     = ITAB1-VERNR.
  ITAB2-PROJC     = ITAB1-POSKI+0(9).
  ITAB2-WBS       = ITAB1-POSKI+10(4).
  ITAB2-POST2     = ITAB1-POST1.
  ITAB2-PSPHI     = ITAB1-PSPHI.
  ITAB2-PRART     = ITAB1-PRART.
  ITAB2-PSPRI     = ITAB1-PSPRI.
  ITAB2-ACTL_PP   = W_PP_ACTUAL.
  ITAB2-PLAN_PP   = W_PP_PLAN.
  ITAB2-PLAN_YR   = W_YR_PLAN.
  APPEND ITAB2.
  CLEAR: ITAB2, W_PP_ACTUAL, W_PP_PLAN, W_YR_PLAN.
ENDFORM.

*---------------------  GET_AMOUNTS ----------------------------
FORM GET_AMOUNTS.
*---------------------------------------------------------------
* COST TOTALS - Primary Cost
*---------------------------------------------------------------

  SELECT * FROM COSP
    WHERE OBJNR = ITAB1-OBJNR
      AND GJAHR = P_FYEAR                          "Fiscal Year
      AND WRTTP IN ('01', '04')                    "Plan and Actuals
      AND ( VERSN = P_VERS                         "For plan amount
       OR   VERSN = '000'  )                       "For actual amount
      AND BEKNZ IN ('S', 'H', 'L').                "Deb/Cred Indicator

    IF SY-SUBRC = '0'.
      ADD  COSP-WKG001 FROM 1 TO 12 GIVING AMT01.
      ADD  COSP-WKG001 FROM P_BEGMTH TO P_ENDMTH GIVING AMT02.
      CASE COSP-WRTTP.
           WHEN '01'.
             IF COSP-VERSN = P_VERS.
                W_YR_PLAN = W_YR_PLAN + AMT01.  "Plan for year $
                W_PP_PLAN = W_PP_PLAN + AMT02.  "Period to Period Plan $
             ENDIF.
           WHEN '04'.
             IF COSP-VERSN = '000'.
                W_PP_ACTUAL = W_PP_ACTUAL + AMT02.
             ENDIF.
      ENDCASE.
      CLEAR: AMT01, AMT02.
    ENDIF.
  ENDSELECT.
*----------------------------------------------------------------------*
* COST TOTALS - Secondary Cost                                         *
*----------------------------------------------------------------------*

  SELECT * FROM COSS
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_FYEAR                              "Fiscal Year
      AND WRTTP = '04'                                 "Actuals
      AND ( VERSN = P_VERS                             "Version
       OR   VERSN = '000' ).

    IF SY-SUBRC = '0'.
      ADD  COSP-WKG001 FROM P_BEGMTH TO P_ENDMTH GIVING AMT02.
      CASE COSS-WRTTP.
           WHEN '04'.
                 IF COSS-VERSN = '000'.
                    W_PP_ACTUAL  = W_PP_ACTUAL + AMT02.
                 ENDIF.
                    CLEAR: AMT01.
      ENDCASE.
    ENDIF.
  ENDSELECT.
*

ENDFORM.

*--------------------------REPORT_PRINTING.    -------------------------
* Routine used to create the report or excel sheet                     *
*-----------------------------------------------------------------------
FORM REPORT_PRINTING.
PERFORM PROT_HEADER.

    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
          EXPORTING
            BASIC_LIST_TITLE        = P_REPTTL
            HEAD_LINE1              = WORK_TEXT
            FILE_NAME               = SY-CPROG
            ADDITIONAL_OPTIONS      = WORK_OPTION
          IMPORTING
               RETURN_CODE          = RETCODE
          TABLES
               DATA_TAB             = REPTAB
               FIELDNAME_TAB        = PROT_HEADER
               ERROR_TAB            = ERRORTAB
          EXCEPTIONS
               DOWNLOAD_PROBLEM     = 1
               NO_DATA_TAB_ENTRIES  = 2
               TABLE_MISMATCH       = 3
               PRINT_PROBLEMS       = 4
               OTHERS               = 5.
  IF SY-SUBRC NE 0.
    WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFULL - REASON = ', SY-SUBRC.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PROT_HEADER                                              *
*---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-CL1 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL2 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL3 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL4 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL5 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL6 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL7 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER
