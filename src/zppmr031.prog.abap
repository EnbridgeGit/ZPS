REPORT ZPPMR031 NO STANDARD PAGE HEADING LINE-SIZE 170
                LINE-COUNT 58 MESSAGE-ID ZS.

************************************************************************
*                                                                      *
*   PROGRAM:    ZPPMR031                                               *
*   PROGRAMMER: M. Khan                                                *
*   CLIENT:     Union Gas                                              *
*   DATE:       OCT 2002.                                              *
*                                                                      *
*   The purpose of this program is to produce Corporate Capital        *
*   Report for Selected Groups like "ITE".                             *
*                                                                      *
************************************************************************
* CHANGES:                                                             *
*                                                                      *
*21/11/2006 TR141 Mohammad  Major program changes are done:            *
*                         1- A new program ZPPMR042 has been developed *
*                            to help the user to determine which proj/ *
*                            wbs to include or exclude in this program.*
*                            Variant screen has been change to run both*
*                            program from the same variant screen      *
*                ****Note: If this program need a change, please make  *
*                          sure that it will not affect ZPPMR042 and   *
*                          vice versa.                                 *
*                         2- A number of other changes are made in the *
*                            variant screen (e.g., Outlook calculation,*
*                            include projects,type, priority, etc. are *
*                            added in variant screen and program logic *
*                            changed as well).                         *
*                         3- The calculation for a number columns are  *
*                            changed.                                  *
*                                                                      *
*                                                                      *
************************************************************************

TABLES: PRPS,               " WBS element master data
        PROJ,               " Project definition
        COSS,               " CO object: internal postings
        COSP,               " CO object: external postings
        T001,               " Company code
        ZPSDIVGR.           " Division Grouping

FIELD-SYMBOLS: <FS1>, <FS2>.
DATA: BEGIN OF ITAB1 OCCURS 0,
        VERNR         LIKE PRPS-VERNR,              "Division
        POSKI         LIKE PRPS-POSKI,              "Project POSKI
        POSID         LIKE PRPS-POSID,              "Proj Number
        PSPHI         LIKE PRPS-PSPHI,              "Proj Number
        PSPRI         LIKE PRPS-PSPRI,              "Proj. Priority Code
        PRART         LIKE PRPS-PRART,              "Project Type Code
        OBJNR         LIKE PRPS-OBJNR,              "Object Number
     END OF ITAB1.

DATA: BEGIN OF ITAB2 OCCURS 0,
        GRPNUM(2)     TYPE N,
        PCTRL(6)      TYPE C,                       "Project Control
        PROJC(9)      TYPE C,                       "Project POSKI+0(9)
        GRPDSCR(30)   TYPE C,
        VERNR         LIKE PRPS-VERNR,              "Division
        PSPHI         LIKE PRPS-PSPHI,              "Proj Number
        PSPRI         LIKE PRPS-PSPRI,              "Proj. Priority Code
        PRART         LIKE PRPS-PRART,              "Project Type Code
        ACTL_YTD(8)   TYPE P,                       "Actual $ - YTD
        PLAN_YTD(8)   TYPE P,                       "Plan $   - YTD
        PLAN_YEAR(8)  TYPE P,                       "Plan $   - YEAR
        OUTLOOK(8)    TYPE P,                       "Outlook Can.
     END OF ITAB2.

DATA: BEGIN OF REPTAB OCCURS 0,
        PROJC(9)      TYPE C,                       "Project POSKI+0(9)
        DSCRPT(45)    TYPE C,
        PSPRI         LIKE PRPS-PSPRI,              "Proj. Priority Code
        PRART         LIKE PRPS-PRART,              "Project Type Code
        ACTL_YTD(8)   TYPE P,                       "Actual $ - YTD
        PLAN_YTD(8)   TYPE P,                       "Plan $   - YTD
        POVU_YTD(8)   TYPE P,                       "Over/under Plan YTD
        PCOM_YTD(8)   TYPE P,                       "% Complete YTD
        PLAN_YEAR(8)  TYPE P,                       "Plan $   - Year
        POVU_YEAR(8)  TYPE P,                       "Over/under Plan Yr.
        PLFT_YEAR(8)  TYPE P,                       "% Left Year
        OLOOK_CAN(8)  TYPE P,                       "Outlook Can $.
        OLOOK_USA(8)  TYPE P,                       "Outlook US $.
        PLNV_OLOOK(8) TYPE P,                       "Plan VS Outlook
        AVA_SPEND(8)  TYPE P,                       "Available to spend.
        PLFT_OLOOK(8) TYPE P,                       "% Left Outlook
     END OF REPTAB.

DATA: AMT01(8)            TYPE P,
      AMT02(8)            TYPE P,
      AMT03(8)            TYPE P,
      AMT04(8)            TYPE P,
      W_PLAN_YEAR(8)      TYPE P,
      W_PLAN_YTD(8)       TYPE P,
      W_ACTUAL_YTD(8)     TYPE P,
      W_OV_UND_YTD(8)     TYPE P,
      W_AMT_OLOOK(8)      TYPE P,
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
      OBJECT            LIKE AUSP-OBJEK,
      CHARIC            LIKE CABN-ATNAM,
      WORK_PSPHI        LIKE PRPS-PSPHI,
      WORK_PSPRI        LIKE PRPS-PSPRI,
      WORK_PRART        LIKE PRPS-PRART,
      WORK_VERNR        LIKE PRPS-VERNR,
      WORK_PCTRL(6)     TYPE C,
      WORK_POST1(35)    TYPE C,
      WORK_GRPDSCR(25)  TYPE C,
      WORK_TEXT1(60)    TYPE C,
      WORK_TEXT2(60)    TYPE C,
      WORK_TEXT3(60)    TYPE C,
*      W_TEXT(60)        TYPE C,
      WORK_OPTION(11)   TYPE C VALUE 'START_EXCEL',
      O_VERS  LIKE COSP-VERSN,           "Outlook Verson
      HEAD_PRINT(1)     VALUE 'Y',
      W_RSTMTH(2),
      W_CONVRT(7),
      US_RATE(8)        TYPE P DECIMALS 5.


DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5) TYPE C,
        DDIC_FIELD(5) TYPE C,
        KEY TYPE C,
      END OF PROT_HEADER.
DATA  ERRORTAB    LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
DATA: RETCODE LIKE SY-SUBRC.
CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X'.
RANGES: R_WBS  FOR PRPS-POSID+7(5).

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.   " TITLE TEXT-001.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-005.
*                                               Validation Program
PARAMETERS: B_PRG1 RADIOBUTTON GROUP PROG user-command RAD,
*                                               IT Capital Report
            B_PRG2 RADIOBUTTON GROUP PROG.  "Execute IT Capital Report
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME. " TITLE TEXT-005.
PARAMETERS:
*   P_REPTTL LIKE SY-TITLE  DEFAULT TEXT-005,
   P_REPTTL LIKE SY-TITLE  DEFAULT 'CORPORATE IT CAPITAL REPORT ',
   P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),        "Fiscal Year
   P_CCODE LIKE PRPS-PBUKR DEFAULT 'UGL',                "Company code
   P_VERS  LIKE COSP-VERSN DEFAULT '0',                  "Plan Version
*   O_VERS  LIKE COSP-VERSN DEFAULT '31',           "Outlook Verson
   P_BEGMTH(2)         DEFAULT 1,                        "Start Month
   P_ENDMTH(2)         DEFAULT 12,                       "End Month
   P_GROUP LIKE ZPSDIVGR-TGROUP DEFAULT 'D2',
   P_CONVRT  LIKE US_RATE.                               "US $ Conv.Rate

SELECT-OPTIONS:
        S_PSPRI  FOR PRPS-PSPRI,                        "Priority
        S_PRART  FOR PRPS-PRART,                        "Type
        S_VERNR  FOR PRPS-VERNR,                        "Division
        S_WBS    FOR PRPS-POSID+7(4).                   "WBS element
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-007.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     B_QTR0 RADIOBUTTON GROUP BGRP user-command OLK.
SELECTION-SCREEN COMMENT 3(46) TEXT-011.
SELECTION-SCREEN POSITION 50.
PARAMETERS: B_VERS0  LIKE COSP-VERSN DEFAULT '00'.   "Version
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     B_QTR1 RADIOBUTTON GROUP BGRP.
SELECTION-SCREEN COMMENT 3(38) TEXT-008.
SELECTION-SCREEN POSITION 50.
PARAMETERS: B_VERS1  LIKE COSP-VERSN DEFAULT '31'.   "Version
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     B_QTR2 RADIOBUTTON GROUP BGRP.
SELECTION-SCREEN COMMENT 3(38) TEXT-009.
SELECTION-SCREEN POSITION 50.
PARAMETERS: B_VERS2  LIKE COSP-VERSN DEFAULT '32'.   "Version
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     B_QTR3 RADIOBUTTON GROUP BGRP.
SELECTION-SCREEN COMMENT 3(38) TEXT-010.
SELECTION-SCREEN POSITION 50.
PARAMETERS: B_VERS3  LIKE COSP-VERSN DEFAULT '33'.   "Version
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: I_POSID FOR PRPS-POSID.        "PROJECTS(Include)
SELECT-OPTIONS: X_POSID FOR PRPS-POSID.        "PROJECTS(Exclude)
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 31(20) TEXT-004.
SELECTION-SCREEN COMMENT 56(20) TEXT-004.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END OF BLOCK BOX5.
SELECTION-SCREEN BEGIN OF BLOCK BOX6 WITH FRAME TITLE TEXT-003.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_EXCL RADIOBUTTON GROUP RBCR.            "EXCEL SHEET
SELECTION-SCREEN END OF BLOCK BOX6.

SELECTION-SCREEN END OF BLOCK BOX1.

*-----------------------  END of SELECTION SCREEN-----------------------
************************************************************************

*------------------------ INITIALIZATION.   ------------------------
INITIALIZATION.

MOVE TRUE TO B_PRG1.

 S_WBS  = 'IBT76417643'.
 APPEND S_WBS.
 S_WBS  = 'IBT76717675'.
 APPEND S_WBS.
 S_WBS  = 'IBT98419842'.
 APPEND S_WBS.
 S_WBS  = 'IBT98809881'.
 APPEND S_WBS.

************************************************************************
***                   SELECTION-SCREEN OUTPUT                        ***
************************************************************************
AT SELECTION-SCREEN OUTPUT.
LOOP AT SCREEN.
IF B_PRG1 = 'X'.
*   IF SCREEN-NAME = 'O_VERS' OR SCREEN-NAME = 'P_GROUP' OR
   IF SCREEN-NAME = 'P_GROUP'     OR SCREEN-NAME = 'P_CONVRT'      OR
      SCREEN-NAME = 'I_POSID-LOW' OR SCREEN-NAME = 'I_POSID-HIGH'  OR
      SCREEN-NAME = 'X_POSID-LOW' OR SCREEN-NAME = 'X_POSID-HIGH'  OR
      SCREEN-NAME = 'B_VERS0'     OR SCREEN-NAME = 'B_VERS1'       OR
      SCREEN-NAME = 'B_VERS2'     OR SCREEN-NAME = 'B_VERS3'       OR
      SCREEN-NAME = 'B_QTR1'        OR
      SCREEN-NAME = 'B_QTR2'      OR SCREEN-NAME = 'B_QTR3'.

*      CLEAR: O_VERS, P_GROUP, P_CONVRT.
      CLEAR: P_GROUP, P_CONVRT.
      SCREEN-INPUT = 0.
      P_REPTTL = TEXT-006.
      MODIFY SCREEN.
   ENDIF.
ELSE.
   IF SCREEN-NAME = 'P_VERS'.
      SCREEN-INPUT = 1.
*      MOVE 0 TO P_CONVRT.
      P_REPTTL = TEXT-005.
      MODIFY SCREEN.
   ENDIF.
   IF B_QTR0 = 'X'.
      IF SCREEN-NAME = 'B_VERS1' OR SCREEN-NAME = 'B_VERS2' OR
         SCREEN-NAME = 'B_VERS3'.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ELSEIF B_QTR1 = 'X'.
      IF SCREEN-NAME = 'B_VERS0' OR SCREEN-NAME = 'B_VERS2' OR
         SCREEN-NAME = 'B_VERS3'.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ELSEIF B_QTR2 = 'X'.
      IF SCREEN-NAME = 'B_VERS0' OR SCREEN-NAME = 'B_VERS1' OR
         SCREEN-NAME = 'B_VERS3'.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ELSE.
      IF SCREEN-NAME = 'B_VERS0' OR SCREEN-NAME = 'B_VERS1' OR
         SCREEN-NAME = 'B_VERS2'.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
ENDIF.
ENDLOOP.

*-------------------------  START-OF-SELECTION ------------------------
START-OF-SELECTION.

PERFORM GET_ALL_WBS_TOGETHER.
IF B_PRG1 = 'X'.
   SUBMIT ZPPMR042
   WITH P_REPTTL = P_REPTTL
   WITH P_FYEAR  = P_FYEAR
   WITH P_CCODE  = P_CCODE
   WITH P_VERS   = P_VERS
   WITH P_BEGMTH = P_BEGMTH
   WITH P_ENDMTH = P_ENDMTH
   WITH S_PSPRI  IN S_PSPRI
   WITH S_PRART  IN S_PRART
   WITH S_VERNR  IN S_VERNR
   WITH S_WBS    IN S_WBS
   WITH P_RPRT   = P_RPRT
   WITH P_EXCL   = P_EXCL
   AND RETURN.
   STOP.
ENDIF.

   IF B_QTR0 = 'X'.
      MOVE B_VERS0 TO O_VERS.
      MOVE TEXT-011 TO WORK_TEXT3.
   ELSEIF B_QTR1 = 'X'.
      MOVE B_VERS1 TO O_VERS.
      MOVE TEXT-008 TO WORK_TEXT3.
   ELSEIF B_QTR2 = 'X'.
      MOVE B_VERS2 TO O_VERS.
      MOVE TEXT-009 TO WORK_TEXT3.
   ELSE.
      MOVE B_VERS3 TO O_VERS.
      MOVE TEXT-010 TO WORK_TEXT3.
   ENDIF.

SELECT SINGLE * FROM T001                    "Company Code
  WHERE BUKRS = P_CCODE.

  MOVE 'PROJECT_CNTR_NUMBER'  TO  CHARIC.    "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_PC.

 SELECT VERNR POSKI POSID PSPHI PSPRI PRART OBJNR  "Query based on WBS
   INTO TABLE ITAB1
   FROM PRPS
   WHERE PBUKR = P_CCODE
     AND VERNR IN S_VERNR
     AND POSKI IN R_WBS
     AND PSPRI IN S_PSPRI
     AND PRART IN S_PRART
     AND LOEVM <> 'X'.                        "Deletion Indicator

IF NOT I_POSID[] IS INITIAL.
 SELECT VERNR POSKI POSID PSPHI PSPRI PRART OBJNR  "Query based on Proj
   APPENDING TABLE ITAB1
   FROM PRPS
   WHERE PBUKR = P_CCODE
     AND VERNR IN S_VERNR
     AND POSID IN I_POSID
     AND PSPRI IN S_PSPRI
     AND PRART IN S_PRART
     AND LOEVM <> 'X'.                        "Deletion Indicator
ENDIF.

 IF NOT X_POSID[] IS INITIAL.
    DELETE ITAB1 WHERE POSID IN X_POSID.
 ENDIF.

 LOOP AT ITAB1.
   IF ITAB1-POSKI+7(2) CO '1234567890'.       "Eliminates templates
      PERFORM GET_AMOUNTS.
      IF W_AMT_OLOOK   <> 0              OR
         W_PLAN_YEAR   <> 0              OR
         W_ACTUAL_YTD  <> 0.
         PERFORM SETUP_GROUP.
         IF ITAB1-VERNR = '49'.
            OBJECT = ITAB1-OBJNR.
            PERFORM FIND_CHARACTERISTIC.
         ENDIF.
         PERFORM BUILD_TABLE.
      ENDIF.
   ENDIF.
         CLEAR: W_ACTUAL_YTD, W_PLAN_YTD, W_AMT_OLOOK.
 ENDLOOP.


 SORT ITAB2 BY GRPNUM PCTRL VERNR PROJC.

 LOOP AT ITAB2.

   MOVE ITAB2-PSPHI    TO WORK_PSPHI.
   MOVE ITAB2-GRPDSCR  TO WORK_GRPDSCR.
   MOVE ITAB2-VERNR    TO WORK_VERNR.
   IF ITAB2-PSPRI <> ' '.
      MOVE ITAB2-PSPRI TO WORK_PSPRI.
   ENDIF.
   IF ITAB2-PRART <> '  '.
      MOVE ITAB2-PRART TO WORK_PRART.
   ENDIF.
   IF ITAB2-PCTRL <> '      '.
      MOVE ITAB2-PCTRL TO WORK_PCTRL.
   ENDIF.

   AT END OF PROJC.
      SUM.
      PERFORM WRITE_DETAIL_LINE.
   ENDAT.

   AT END OF PCTRL.
      SUM.
      PERFORM WRITE_PROJ_CONTROL_TOTALS.
   ENDAT.

   AT END OF GRPNUM.
      SUM.
      PERFORM WRITE_GROUP_TOTALS.
   ENDAT.

    AT LAST.
       SUM.
       PERFORM WRITE_GRAND_TOTALS.
   ENDAT.

ENDLOOP.

IF NOT REPTAB[] IS INITIAL.
   MOVE TEXT-CLT  TO WORK_TEXT1+0(7).
   MOVE SY-SYSID  TO WORK_TEXT1+8(5).
   MOVE SY-MANDT  TO WORK_TEXT1+14(4).
   MOVE TEXT-DTE  TO WORK_TEXT1+21(5).
   WRITE SY-DATUM TO WORK_TEXT1+27(10).
   MOVE TEXT-TME  TO WORK_TEXT1+40(5).
   WRITE SY-UZEIT TO WORK_TEXT1+46(10).

   MOVE P_CONVRT  TO W_CONVRT.
   MOVE TEXT-YER  TO WORK_TEXT2+0(5).
   MOVE P_FYEAR   TO WORK_TEXT2+6(4).
   MOVE TEXT-CMA  TO WORK_TEXT2+10(1).
   MOVE TEXT-PER  TO WORK_TEXT2+12(7).
   MOVE P_BEGMTH  TO WORK_TEXT2+19(2).
   MOVE TEXT-TOO  TO WORK_TEXT2+22(2).
   MOVE P_ENDMTH  TO WORK_TEXT2+25(2).
   MOVE TEXT-CMA  TO WORK_TEXT2+27(1).
   MOVE TEXT-VER  TO WORK_TEXT2+29(10).
   MOVE P_VERS    TO WORK_TEXT2+39(3).
   MOVE TEXT-CMA  TO WORK_TEXT2+42(1).
   MOVE TEXT-USR  TO WORK_TEXT2+44(8).
   MOVE W_CONVRT  TO WORK_TEXT2+52(7).

   CONCATENATE TEXT-012 WORK_TEXT3 O_VERS INTO WORK_TEXT3
               SEPARATED BY SPACE.

*    CONCATENATE TEXT-YER P_FYEAR TEXT-CMA TEXT-PER P_BEGMTH TEXT-TOO
*    P_ENDMTH TEXT-CMA TEXT-VER O_VERS TEXT-CMA TEXT-USR W_CONVRT
*    INTO WORK_TEXT2 SEPARATED BY SPACE.

   IF P_EXCL <> 'X'.
      CLEAR WORK_OPTION.
   ENDIF.
   PERFORM REPORT_PRINTING.
ENDIF.

************************************************************************
*------------------------GET_ALL_WBS_TOGETHER --------------------------
************************************************************************
FORM GET_ALL_WBS_TOGETHER.
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

*IF NOT I_POSID[] IS INITIAL.
*  LOOP AT I_POSID.
*   IF I_POSID-OPTION = 'BT'.
*      MOVE 'I'          TO R_WBS-SIGN.
*      MOVE 'CP'         TO R_WBS-OPTION.
*      MOVE I_POSID-LOW  TO R_WBS-LOW.
*      MOVE I_POSID-HIGH TO R_WBS-HIGH.
*      APPEND R_WBS.
*   ELSE.
*      MOVE 'I'  TO R_WBS-SIGN.
*      MOVE 'CP' TO R_WBS-OPTION.
*      MOVE I_POSID-LOW TO R_WBS-LOW.
*      APPEND R_WBS.
*   ENDIF.
* ENDLOOP.
*ENDIF.

ENDFORM.
************************************************************************
*-------------------------  WRITE_DETAIL_LINE --------------------------
************************************************************************
FORM WRITE_DETAIL_LINE.

  SELECT SINGLE * FROM PROJ
    WHERE PSPNR = WORK_PSPHI.
    IF SY-SUBRC = 0.
       MOVE  PROJ-POST1+0(35) TO WORK_POST1.
    ENDIF.

     MOVE: ITAB2-PROJC TO REPTAB-PROJC,
           WORK_POST1  TO REPTAB-DSCRPT,
           WORK_PSPRI  TO REPTAB-PSPRI,
           WORK_PRART  TO REPTAB-PRART.
     PERFORM WRITE_DOLLARS.
     APPEND REPTAB.
     CLEAR: REPTAB, WORK_POST1, WORK_PSPRI, WORK_PRART.
ENDFORM.
************************************************************************
*-----------------------  WRITE_PROJ_CONTROL_TOTALS --------------------
************************************************************************
FORM WRITE_PROJ_CONTROL_TOTALS.

IF WORK_VERNR = '49'.
   IF WORK_PCTRL = '      '.
      MOVE 'N/A' TO WORK_PCTRL.
   ENDIF.
ENDIF.

   MOVE '*********'  TO REPTAB-PROJC.
   CONCATENATE TEXT-042 WORK_PCTRL INTO REPTAB-DSCRPT.
   PERFORM WRITE_DOLLARS.
   APPEND REPTAB.
   CLEAR: REPTAB, WORK_PCTRL.

ENDFORM.
************************************************************************
*-------------------------  WRITE_GROUP_TOTALS -------------------------
************************************************************************
FORM WRITE_GROUP_TOTALS.

   MOVE '*********'  TO REPTAB-PROJC.
   CONCATENATE TEXT-041 WORK_GRPDSCR INTO REPTAB-DSCRPT.
   PERFORM WRITE_DOLLARS.
   APPEND REPTAB.
   CLEAR: REPTAB, WORK_GRPDSCR.

ENDFORM.
************************************************************************
*-------------------------  WRITE_GRAND_TOTALS -------------------------
************************************************************************
FORM WRITE_GRAND_TOTALS.

   MOVE   '*********'  TO REPTAB-PROJC.
   MOVE    TEXT-043    TO REPTAB-DSCRPT.
   PERFORM WRITE_DOLLARS.
   APPEND  REPTAB.
   CLEAR   REPTAB.

ENDFORM.
************************************************************************
*-------------------------  WRITE_DOLLARS ------------------------------
************************************************************************
FORM WRITE_DOLLARS.

 PRINT_YTDPLAN_OVERUNDER = ITAB2-PLAN_YTD - ITAB2-ACTL_YTD.
 IF ITAB2-PLAN_YTD <> 0.
    PRINT_%_COMPLETE = ITAB2-ACTL_YTD / ITAB2-PLAN_YTD * 100.
    PRINT_%_INCOMPLETE = 100 - PRINT_%_COMPLETE.
 ELSE.
    CLEAR: PRINT_%_COMPLETE, PRINT_%_INCOMPLETE.
 ENDIF.

 PRINT_YEARPLAN_OVERUNDER = ITAB2-PLAN_YEAR - ITAB2-ACTL_YTD.
 CLEAR: USA_OUTLOOK.
 IF P_CONVRT <> 0 AND ITAB2-OUTLOOK <> 0.
    USA_OUTLOOK = ITAB2-OUTLOOK / P_CONVRT.
 ENDIF.

 AVAILABLE_TO_SPEND = ITAB2-OUTLOOK - ITAB2-ACTL_YTD.
 IF ITAB2-OUTLOOK <> 0.
    OUTLOOK_%_REMAINING = AVAILABLE_TO_SPEND / ITAB2-OUTLOOK * 100.
 ELSE.
 CLEAR OUTLOOK_%_REMAINING.
 ENDIF.

   MOVE: ITAB2-ACTL_YTD      TO REPTAB-ACTL_YTD,
         ITAB2-PLAN_YTD      TO REPTAB-PLAN_YTD,
         ITAB2-PLAN_YEAR     TO REPTAB-PLAN_YEAR,
*         PRINT_%_INCOMPLETE  TO REPTAB-PLFT_YEAR,
         ITAB2-OUTLOOK       TO REPTAB-OLOOK_CAN,
         USA_OUTLOOK         TO REPTAB-OLOOK_USA,
         AVAILABLE_TO_SPEND  TO REPTAB-AVA_SPEND,
         OUTLOOK_%_REMAINING TO REPTAB-PLFT_OLOOK,
         PRINT_%_COMPLETE    TO REPTAB-PCOM_YTD,
         PRINT_YTDPLAN_OVERUNDER  TO REPTAB-POVU_YTD,
         PRINT_YEARPLAN_OVERUNDER TO REPTAB-POVU_YEAR.
         REPTAB-PLNV_OLOOK = ITAB2-PLAN_YEAR - ITAB2-OUTLOOK.
         IF REPTAB-POVU_YEAR <> 0 AND REPTAB-PLAN_YEAR <> 0.
           REPTAB-PLFT_YEAR = REPTAB-POVU_YEAR / REPTAB-PLAN_YEAR * 100.
         ELSE.
           CLEAR REPTAB-PLFT_YEAR.
         ENDIF.

ENDFORM.
*---------------------  SETUP_GROUP ---------------------------------
FORM SETUP_GROUP.

 SELECT SINGLE * FROM ZPSDIVGR
  WHERE TGROUP = P_GROUP
    AND VERNR  = ITAB1-VERNR.

 IF SY-SUBRC = 0.
    MOVE ZPSDIVGR-DGROUP TO  ITAB2-GRPNUM.
    MOVE ZPSDIVGR-TXT30  TO  ITAB2-GRPDSCR.
 ELSE.
    MOVE '***'            TO ITAB2-GRPNUM.
    MOVE '**NOT FOUND***' TO ITAB2-GRPDSCR.
 ENDIF.

ENDFORM.

*---------------------  BUILD_TABLE --------------------------
FORM BUILD_TABLE.

  ITAB2-VERNR     = ITAB1-VERNR.
  ITAB2-PROJC     = ITAB1-POSKI+0(9).
  ITAB2-PSPHI     = ITAB1-PSPHI.
  ITAB2-PRART     = ITAB1-PRART.
  ITAB2-PSPRI     = ITAB1-PSPRI.
  ITAB2-PCTRL     = PROJ_CONTROL.
  ITAB2-ACTL_YTD  = W_ACTUAL_YTD.
  ITAB2-PLAN_YTD  = W_PLAN_YTD.
  ITAB2-PLAN_YEAR = W_PLAN_YEAR.
  ITAB2-OUTLOOK   = W_AMT_OLOOK.
  APPEND ITAB2.
  CLEAR: ITAB2, W_ACTUAL_YTD, W_PLAN_YTD, W_PLAN_YEAR,
         W_AMT_OLOOK, PROJ_CONTROL.
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
       OR   VERSN = '000'                          "For actual amount
       OR   VERSN = O_VERS )                       "For Outlook amount
      AND BEKNZ IN ('S', 'H', 'L').                "Deb/Cred Indicator

    IF SY-SUBRC = '0'.
      ADD  COSP-WKG001 FROM 1 TO 12 GIVING AMT01.
      ADD  COSP-WKG001 FROM P_BEGMTH TO P_ENDMTH GIVING AMT02.
      IF B_QTR0 = 'X'.
         CLEAR AMT03.                                 "Outlook Actual
         ADD COSP-WKG001 FROM 1 TO 12 GIVING AMT04.   "Outlook Plan
      ELSEIF B_QTR1 = 'X'.
         ADD COSP-WKG001 FROM 1  TO 3  GIVING AMT03.   "Outlook Actual
         ADD COSP-WKG001 FROM 4  TO 12 GIVING AMT04.   "Outlook Plan
      ELSEIF B_QTR2 = 'X'.
         ADD COSP-WKG001 FROM 1  TO 6  GIVING AMT03.   "Outlook Actual
         ADD COSP-WKG001 FROM 7  TO 12 GIVING AMT04.   "Outlook Plan
      ELSE.
         ADD COSP-WKG001 FROM 1  TO 9  GIVING AMT03.   "Outlook Actual
         ADD COSP-WKG001 FROM 10 TO 12 GIVING AMT04.   "Outlook Plan
      ENDIF.

*      IF P_ENDMTH < 12.
*         W_RSTMTH = P_ENDMTH + 1.
*         ADD COSP-WKG001 FROM W_RSTMTH TO 12 GIVING AMT03.
*      ENDIF.
      CASE COSP-WRTTP.
           WHEN '01'.    "Plan
                 IF COSP-VERSN = P_VERS.
                    W_PLAN_YEAR = W_PLAN_YEAR + AMT01.   "Total Amount
                    W_PLAN_YTD  = W_PLAN_YTD  + AMT02.   "YTD Amount
                 ENDIF.

                 IF COSP-VERSN = O_VERS.
                    W_AMT_OLOOK = W_AMT_OLOOK + AMT04.
                 ENDIF.

           WHEN '04'.    "Actual
                 IF COSP-VERSN = '000'.
                    W_ACTUAL_YTD  = W_ACTUAL_YTD  + AMT02.
                    W_AMT_OLOOK = W_AMT_OLOOK + AMT03.
                 ENDIF.
      ENDCASE.
      CLEAR: AMT01, AMT02, AMT03, AMT04.
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
      ADD  COSS-WKG001 FROM 1 TO 12 GIVING AMT01.
      ADD  COSP-WKG001 FROM P_BEGMTH TO P_ENDMTH GIVING AMT02.
      IF B_QTR0 = 'X'.
         CLEAR AMT03.                                 "Outlook Actual
      ELSEIF B_QTR1 = 'X'.
         ADD COSS-WKG001 FROM 1  TO 3  GIVING AMT03.   "Outlook Actual
      ELSEIF B_QTR2 = 'X'.
         ADD COSS-WKG001 FROM 1  TO 6  GIVING AMT03.   "Outlook Actual
      ELSE.
         ADD COSS-WKG001 FROM 1  TO 9  GIVING AMT03.   "Outlook Actual
      ENDIF.
      CASE COSS-WRTTP.
           WHEN '04'.
                 IF COSS-VERSN = '000'.
                    W_ACTUAL_YTD = W_ACTUAL_YTD  + AMT02.
                    W_AMT_OLOOK  = W_AMT_OLOOK   + AMT03.
                 ENDIF.
                 CLEAR: AMT01, AMT02, AMT03.
      ENDCASE.
    ENDIF.
  ENDSELECT.
*

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
ENDFORM.

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine used to get the value of the project control number *
*-----------------------------------------------------------------------
FORM FIND_CHARACTERISTIC.
  REFRESH CHAR_TAB.
  CALL FUNCTION 'CLFM_SELECT_AUSP'
       EXPORTING
            MAFID     = 'O'
            CLASSTYPE = '014'
            OBJECT    = OBJECT
       TABLES
            EXP_AUSP  = CHAR_TAB
       EXCEPTIONS
            NO_VALUES = 1
            OTHERS    = 2.
  CLEAR: PROJ_CONTROL.
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PC BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATWRT+0(6) TO PROJ_CONTROL.
    ENDIF.
  ENDIF.
ENDFORM.

*--------------------------REPORT_PRINTING.    -------------------------
* Routine used to create the report or excel sheet                     *
*-----------------------------------------------------------------------
FORM REPORT_PRINTING.
PERFORM PROT_HEADER.

    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
          EXPORTING
            BASIC_LIST_TITLE        = P_REPTTL
            HEAD_LINE1              = WORK_TEXT1
            HEAD_LINE2              = WORK_TEXT2
            HEAD_LINE3              = WORK_TEXT3
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
  MOVE TEXT-CL8 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL9 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C10 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C11 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C12 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C13 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C16 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C14 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C15 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER
