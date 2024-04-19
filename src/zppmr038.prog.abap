REPORT ZPPMR038 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 132.
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        November, 2003.                                        *
*  Description:                                                        *
*     - The purpose of this program is to produce the report           *
*       "Capital Variance Report For Auditors With % Complete".        *
*     - Depending upon variants, this program can produce following    *
*       three reports.                                                 *
*       1. New Business Projects by GROUP & DIVISION.                  *
*       2. Individual Projects with ZERO PLAN Amounts.                 *
*       3. Individual Projects with PLAN amounts.                      *
*                                                                      *
*     - The percentage check for new bus. report is applied at the     *
*       division level.  For individual projects this check is applied *
*       to individual projects and for this is not applied to          *
*       individual projects with 0 plan $.                             *
*                                                                      *
*     - The annual variance and percentage columns in the reports are  *
*       using YTD Actual $, i.e., Annual Actuals are not used. It's    *
*       the user requirement to do so.                                 *
************************************************************************
* CHANGES:                                                             *
* Date:    Issue:    By:     Remarks                                   *
*                                                                      *
*                                                                      *
************************************************************************

TABLES: COSP,           "CO Object: Cost Totals for External Postings
        COSS,           "CO Object: Cost Totals for Internal Postings
        PROJ,           "Project definition
        PRPS,           "WBS Element Master Data
        TJ02T,          "System status texts
        JEST,           "Object status
        T001,           "Company Codes
        T001B.          "Dummy - req for variants only

*  New Business Projects (Group Projects)
DATA:
   BEGIN OF ITABGRP  OCCURS 0,
      PRART       LIKE PRPS-PRART,          "Project Type
      PSPRI       LIKE PRPS-PSPRI,          "Project Priority
      VERNR       LIKE PRPS-VERNR,          "Division
      PSPID       LIKE PROJ-PSPID,          "Project #
      OBJNR       LIKE PROJ-OBJNR,          "Object #
      POST1       LIKE PROJ-POST1,          "Project Description
      YTDACT      LIKE COSS-WKG001,         "Amount actual YTD
      YTDPLAN     LIKE COSS-WKG001,         "Amount plan YTD
      ANNPLAN     LIKE COSS-WKG001,         "Amount plan annual
   END OF ITABGRP.

DATA: BEGIN OF ITABDEL   OCCURS 0,
      PRART       LIKE PRPS-PRART,          "Project Type
      PSPRI       LIKE PRPS-PSPRI,          "Project Priority
      VERNR       LIKE PRPS-VERNR,          "Division
      PSPID       LIKE PROJ-PSPID,          "Project #
      END OF ITABDEL.

*  Individual Projects
DATA: BEGIN OF ITABINDV OCCURS 0.
      INCLUDE STRUCTURE ITABGRP.
DATA: END OF ITABINDV.

*  Zero Plan $ Projects
DATA: BEGIN OF ITABZERO OCCURS 0.
      INCLUDE STRUCTURE ITABGRP.
DATA: END OF ITABZERO.

* Table for printing reports
DATA: BEGIN OF ITABPRINT OCCURS 0.
      INCLUDE STRUCTURE ITABGRP.
DATA: END OF ITABPRINT.

DATA:
   BEGIN OF ITABALL  OCCURS 0,
      PRART       LIKE PRPS-PRART,          "Project Type
      PSPRI       LIKE PRPS-PSPRI,          "Project Priority
      VERNR       LIKE PRPS-VERNR,          "Division
      PSPID       LIKE PROJ-PSPID,          "Project #
      OBJNR       LIKE PROJ-OBJNR,          "Object #
      POST1       LIKE PROJ-POST1,          "Project Description
      WRTTP       LIKE COSS-WRTTP,          "Plan/Actual
      VERSN       LIKE COSP-VERSN,          "Plan Version
      WKG001      LIKE COSS-WKG001,         "$ for period 01
      WKG002      LIKE COSS-WKG001,         "$ for period 02
      WKG003      LIKE COSS-WKG001,         "$ for period 03
      WKG004      LIKE COSS-WKG001,         "$ for period 04
      WKG005      LIKE COSS-WKG001,         "$ for period 05
      WKG006      LIKE COSS-WKG001,         "$ for period 06
      WKG007      LIKE COSS-WKG001,         "$ for period 07
      WKG008      LIKE COSS-WKG001,         "$ for period 08
      WKG009      LIKE COSS-WKG001,         "$ for period 09
      WKG010      LIKE COSS-WKG001,         "$ for period 10
      WKG011      LIKE COSS-WKG001,         "$ for period 11
      WKG012      LIKE COSS-WKG001,         "$ for period 12
   END OF ITABALL.

DATA:
      INSTAT(40)      TYPE C,
      HEAD01          TYPE STRING,
      HEAD02          TYPE STRING,
      HEAD03          TYPE STRING,
      HEAD04          TYPE STRING,
      YTD_VARIANCE  LIKE COSP-WKG001,
      ANN_VARIANCE  LIKE COSP-WKG001,
      YTD_PERCENT(4) TYPE N,
      ANN_PERCENT(4) TYPE N,
      AMOUNT_YTD    LIKE COSP-WKG001,
      AMOUNT_ANNUAL LIKE COSP-WKG001.

RANGES: R_STAT FOR TJ02T-ISTAT.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.

PARAMETERS:
   P_CCODE LIKE PRPS-PBUKR DEFAULT 'UGL',              "Company code
   P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),      "Fiscal Year
   P_PERD1 LIKE T001B-FRPE1 OBLIGATORY DEFAULT 1,      "From Period
   P_PERD2 LIKE T001B-FRPE1 DEFAULT 12,                "To Period
   P_VERSN LIKE COSP-VERSN DEFAULT '000',              "Plan Version
   P_PRCNT(3) TYPE N DEFAULT 110.                      "% Complete

SELECT-OPTIONS:
  S_VERNR  FOR PRPS-VERNR,                 "Division
  S_PSPID  FOR PROJ-PSPID,                 "Project Number
  S_PRART  FOR PRPS-PRART,                 "Proj.Type
  S_PSPRI1 FOR PRPS-PSPRI,                 "Priority Group
  S_PSPRI2 FOR PRPS-PSPRI,                 "Priority Project
  S_STAT   FOR TJ02T-TXT04 DEFAULT 'TECO'. "Status

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
        S_KSTAR    FOR COSS-KSTAR                      "Cost element
                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-004.
PARAMETERS:     P_CHEK1     AS   CHECKBOX DEFAULT 'X',
                P_CHEK2     AS   CHECKBOX DEFAULT 'X',
                P_CHEK3     AS   CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX4.
SELECTION-SCREEN END OF BLOCK BOX.

************************************************************************
*                                                                      *
*                       INITIALIZATION.                                *
*                                                                      *
************************************************************************
INITIALIZATION.
 S_PSPRI1-SIGN   = 'I'.              "Priorities for Group of Projects
 S_PSPRI1-OPTION = 'BT'.
 S_PSPRI1-LOW    = '1'.
 S_PSPRI1-HIGH   = '2'.
 APPEND S_PSPRI1.
 CLEAR  S_PSPRI1.

 S_PSPRI1-SIGN   = 'I'.
 S_PSPRI1-OPTION = 'EQ'.
 S_PSPRI1-LOW    = 'E'.
 APPEND S_PSPRI1.
 CLEAR  S_PSPRI1.

 S_PSPRI1-SIGN   = 'I'.
 S_PSPRI1-OPTION = 'BT'.
 S_PSPRI1-LOW    = 'K'.
 S_PSPRI1-HIGH   = 'L'.
 APPEND S_PSPRI1.
 CLEAR  S_PSPRI1.

 S_PSPRI1-SIGN   = 'I'.
 S_PSPRI1-OPTION = 'BT'.
 S_PSPRI1-LOW    = 'P'.
 S_PSPRI1-HIGH   = 'Q'.
 APPEND S_PSPRI1.
 CLEAR  S_PSPRI1.

 S_PSPRI2-SIGN   = 'I'.              "Priorities for individual Projects
 S_PSPRI2-OPTION = 'BT'.
 S_PSPRI2-LOW    = '3'.
 S_PSPRI2-HIGH   = '7'.
 APPEND S_PSPRI2.
 CLEAR  S_PSPRI2.

 S_PSPRI2-SIGN   = 'I'.
 S_PSPRI2-OPTION = 'BT'.
 S_PSPRI2-LOW    = 'A'.
 S_PSPRI2-HIGH   = 'D'.
 APPEND S_PSPRI2.
 CLEAR  S_PSPRI2.

 S_PSPRI2-SIGN   = 'I'.
 S_PSPRI2-OPTION = 'BT'.
 S_PSPRI2-LOW    = 'F'.
 S_PSPRI2-HIGH   = 'H'.
 APPEND S_PSPRI2.
 CLEAR  S_PSPRI2.

 S_PSPRI2-SIGN   = 'I'.
 S_PSPRI2-OPTION = 'EQ'.
 S_PSPRI2-LOW    = 'J'.
 APPEND S_PSPRI2.
 CLEAR  S_PSPRI2.

 S_PSPRI2-SIGN   = 'I'.
 S_PSPRI2-OPTION = 'BT'.
 S_PSPRI2-LOW    = 'M'.
 S_PSPRI2-HIGH   = 'N'.
 APPEND S_PSPRI2.
 CLEAR  S_PSPRI2.

 S_PSPRI2-SIGN   = 'I'.
 S_PSPRI2-OPTION = 'BT'.
 S_PSPRI2-LOW    = 'S'.
 S_PSPRI2-HIGH   = 'T'.
 APPEND S_PSPRI2.
 CLEAR  S_PSPRI2.

 S_PSPRI2-SIGN   = 'I'.
 S_PSPRI2-OPTION = 'BT'.
 S_PSPRI2-LOW    = 'V'.
 S_PSPRI2-HIGH   = 'W'.
 APPEND S_PSPRI2.
 CLEAR  S_PSPRI2.

************************************************************************

AT SELECTION-SCREEN ON S_STAT.
   LOOP AT S_STAT.
        TRANSLATE S_STAT-LOW TO UPPER CASE.
        TRANSLATE S_STAT-HIGH TO UPPER CASE.
        MODIFY S_STAT.
   ENDLOOP.

START-OF-SELECTION.
  PERFORM SETUP_RANGES_AND_HEADINGS.
  PERFORM GET_PROJECTS_DATA.
  PERFORM SEPARATE_DATA.

  IF P_CHEK1 = 'X'.                      "New Business Report
     CLEAR   ITABPRINT.
     REFRESH ITABPRINT.
     ITABPRINT[] = ITABGRP[].
     MOVE TEXT-007 TO HEAD03.
     PERFORM PRINT_REPORT.
  ENDIF.

  IF P_CHEK2 = 'X'.                      "Individual Proj With Plan $
     IF SY-PAGNO <> 0.
        NEW-PAGE.
     ENDIF.
     CLEAR   ITABPRINT.
     REFRESH ITABPRINT.
     ITABPRINT[] = ITABINDV[].
     MOVE TEXT-008 TO HEAD03.
     PERFORM PRINT_REPORT.
  ENDIF.

  IF P_CHEK3 = 'X'.                      "Individual Proj With 0 Plan $
     IF SY-PAGNO <> 0.
        NEW-PAGE.
     ENDIF.
     CLEAR   ITABPRINT.
     REFRESH ITABPRINT.
     ITABPRINT[] = ITABZERO[].
     MOVE TEXT-009 TO HEAD03.
     PERFORM PRINT_REPORT.
  ENDIF.

  IF SY-PAGNO = 0.
         CALL FUNCTION 'POPUP_FOR_INTERACTION'
           EXPORTING
           HEADLINE    = '!! CAUTION !!'
           TEXT1       = ' '
           TEXT2       = '*** NO DATA SELECTED ***'
           TEXT3       = 'PRESS OK BUTTON TO CONTINUE...'
           BUTTON_1    = 'OK'.
     STOP.
  ENDIF.

************************************************************************
*                                                                      *
*                       SETUP_RANGES_AND_HEADINGS.                     *
*                                                                      *
************************************************************************
FORM SETUP_RANGES_AND_HEADINGS.

 R_STAT-SIGN = 'I'.
 R_STAT-OPTION = 'EQ'.

 SELECT * FROM TJ02T
  WHERE SPRAS = 'EN'
    AND TXT04 IN S_STAT.

    R_STAT-LOW = TJ02T-ISTAT.
    APPEND R_STAT.
 ENDSELECT.

 SELECT SINGLE * FROM T001 WHERE  BUKRS = P_CCODE.
 IF SY-SUBRC = 0.
    MOVE T001-BUTXT TO HEAD01.
 ENDIF.

 CONCATENATE TEXT-005 P_PRCNT '%' INTO HEAD02.

 CONCATENATE TEXT-005 P_PRCNT INTO HEAD02 SEPARATED BY SPACE.
 CONCATENATE HEAD02 '%' INTO HEAD02.
 CONCATENATE TEXT-006 P_PERD1 '-' P_PERD2 '/' P_FYEAR INTO HEAD04.

ENDFORM.
************************************************************************
*                                                                      *
*                       GET_PROJECTS_DATA.                             *
*                                                                      *
************************************************************************
FORM GET_PROJECTS_DATA.

SELECT PRPS~PRART PRPS~PSPRI PROJ~VERNR PROJ~PSPID PROJ~OBJNR
       PROJ~POST1 COSP~WRTTP COSP~VERSN
       COSP~WKG001 COSP~WKG002 COSP~WKG003 COSP~WKG004 COSP~WKG005
       COSP~WKG006 COSP~WKG007 COSP~WKG008 COSP~WKG009 COSP~WKG010
       COSP~WKG011 COSP~WKG012

  INTO TABLE ITABALL
  FROM ( ( ( PROJ
             INNER JOIN PRPS
             ON PROJ~PSPNR = PRPS~PSPHI )
             INNER JOIN COSP
             ON PRPS~OBJNR = COSP~OBJNR )
             INNER JOIN JEST
             ON PROJ~OBJNR = JEST~OBJNR )

 WHERE PROJ~PSPID IN S_PSPID
   AND PROJ~VERNR IN S_VERNR
   AND PROJ~VBUKR = P_CCODE
   AND PRPS~PRART IN S_PRART
   AND ( PRPS~PSPRI IN S_PSPRI1 OR PRPS~PSPRI IN S_PSPRI2 )
   AND ( PRPS~PLAKZ = 'X' OR PRPS~BELKZ = 'X' )
   AND PRPS~LOEVM <> 'X'
   AND COSP~GJAHR = P_FYEAR
   AND ( COSP~VERSN =  P_VERSN OR COSP~VERSN = '000' )
   AND COSP~WRTTP  IN ('01','04')           "Record with actuals & plans
   AND COSP~BEKNZ  IN ('S','H','L')         "Debit/Credit Indicator
   AND COSP~KSTAR NOT IN S_KSTAR
   AND JEST~STAT   IN R_STAT
   AND JEST~INACT <> 'X'.

SELECT PRPS~PRART PRPS~PSPRI PROJ~VERNR PROJ~PSPID PROJ~OBJNR
       PROJ~POST1 COSS~WRTTP COSS~VERSN
       COSS~WKG001 COSS~WKG002 COSS~WKG003 COSS~WKG004 COSS~WKG005
       COSS~WKG006 COSS~WKG007 COSS~WKG008 COSS~WKG009 COSS~WKG010
       COSS~WKG011 COSS~WKG012

APPENDING TABLE ITABALL
  FROM ( ( ( PROJ
             INNER JOIN PRPS
             ON PROJ~PSPNR = PRPS~PSPHI )
             INNER JOIN COSS
             ON PRPS~OBJNR = COSS~OBJNR )
             INNER JOIN JEST
             ON PROJ~OBJNR = JEST~OBJNR )

 WHERE PROJ~PSPID IN S_PSPID
   AND PROJ~VERNR IN S_VERNR
   AND PROJ~VBUKR = P_CCODE
   AND PRPS~PRART IN S_PRART
   AND ( PRPS~PSPRI IN S_PSPRI1 OR PRPS~PSPRI IN S_PSPRI2 )
   AND ( PRPS~PLAKZ = 'X' OR PRPS~BELKZ = 'X' )
   AND PRPS~LOEVM <> 'X'
   AND COSS~GJAHR = P_FYEAR
   AND ( COSS~VERSN =  P_VERSN OR COSS~VERSN = '000' )
   AND COSS~WRTTP  IN ('01','04')           "Record with actuals & plans
   AND COSS~BEKNZ  IN ('S','H','L')         "Debit/Credit Indicator
   AND COSS~KSTAR NOT IN S_KSTAR
   AND JEST~STAT   IN R_STAT
   AND JEST~INACT <> 'X'.

ENDFORM.

************************************************************************
*                                                                      *
*                    BUILD_REPORT_TABLES                               *
*                                                                      *
************************************************************************
FORM SEPARATE_DATA.
 SORT ITABALL BY PRART PSPRI VERNR PSPID.

 LOOP AT ITABALL.

 IF ITABALL-WRTTP = '01' AND ITABALL-VERSN <> P_VERSN.
 ELSE.
   ADD ITABALL-WKG001 FROM P_PERD1 TO P_PERD2 GIVING AMOUNT_YTD.
   ADD ITABALL-WKG001 FROM 1 TO 12 GIVING AMOUNT_ANNUAL.
   IF ITABALL-PSPRI IN S_PSPRI1.
      MOVE-CORRESPONDING ITABALL TO ITABGRP.
      IF ITABALL-WRTTP = '01'.                       "Plan
         MOVE AMOUNT_YTD    TO  ITABGRP-YTDPLAN.
         MOVE AMOUNT_ANNUAL TO  ITABGRP-ANNPLAN.
      ELSE.                                          "Actual
         MOVE AMOUNT_YTD    TO  ITABGRP-YTDACT.
      ENDIF.
      COLLECT ITABGRP.
      CLEAR   ITABGRP.
   ELSE.
      MOVE-CORRESPONDING ITABALL TO ITABINDV.
      IF ITABALL-WRTTP = '01'.                       "Plan
         MOVE AMOUNT_YTD    TO  ITABINDV-YTDPLAN.
         MOVE AMOUNT_ANNUAL TO  ITABINDV-ANNPLAN.
      ELSE.                                          "Actual
         MOVE AMOUNT_YTD    TO  ITABINDV-YTDACT.
      ENDIF.
      COLLECT ITABINDV.
      CLEAR:  ITABINDV.
   ENDIF.
 ENDIF.
 ENDLOOP.

 SORT ITABINDV BY PRART PSPRI VERNR PSPID.
 LOOP AT ITABINDV.
      IF ITABINDV-YTDPLAN = 0  AND  ITABINDV-ANNPLAN = 0.
         MOVE-CORRESPONDING ITABINDV TO ITABZERO.
         APPEND ITABZERO.
         CLEAR  ITABZERO.
         DELETE ITABINDV.
      ELSE.
         IF ITABINDV-YTDPLAN <> 0.
            YTD_PERCENT = ITABINDV-YTDACT / ITABINDV-YTDPLAN * 100.
         ENDIF.
         IF ITABINDV-ANNPLAN <> 0.
            ANN_PERCENT = ITABINDV-YTDACT / ITABINDV-ANNPLAN * 100.
         ENDIF.
         IF YTD_PERCENT > P_PRCNT OR ANN_PERCENT > P_PRCNT.
         ELSE.
            DELETE ITABINDV.
         ENDIF.
      ENDIF.
 ENDLOOP.

 SORT ITABGRP BY PRART PSPRI VERNR PSPID.
 LOOP AT ITABGRP.
      MOVE: ITABGRP-PRART    TO   ITABDEL-PRART,
            ITABGRP-PSPRI    TO   ITABDEL-PSPRI,
            ITABGRP-VERNR    TO   ITABDEL-VERNR,
            ITABGRP-PSPID    TO   ITABDEL-PSPID.
      AT END OF VERNR.
         SUM.
         IF ITABGRP-YTDPLAN <> 0.
            YTD_PERCENT = ITABGRP-YTDACT / ITABGRP-YTDPLAN * 100.
            ANN_PERCENT = ITABGRP-YTDACT / ITABGRP-ANNPLAN * 100.
            IF YTD_PERCENT > P_PRCNT OR ANN_PERCENT > P_PRCNT.
            ELSE.
               APPEND ITABDEL.
            ENDIF.
         ELSE.
            APPEND ITABDEL.
         ENDIF.
         CLEAR ITABDEL.
      ENDAT.
 ENDLOOP.

 LOOP AT ITABDEL.
      DELETE ITABGRP WHERE PRART = ITABDEL-PRART  AND
                           PSPRI = ITABDEL-PSPRI  AND
                           VERNR = ITABDEL-VERNR.
 ENDLOOP.

ENDFORM.
************************************************************************
*                                                                      *
*                         PRINT_REPORT                                 *
*                                                                      *
*                                                                      *
************************************************************************

FORM PRINT_REPORT.
SORT ITABPRINT BY PRART PSPRI VERNR PSPID.

LOOP AT ITABPRINT.
  PERFORM GET_PROJECT_STATUS USING ITABPRINT-OBJNR.
  CLEAR: YTD_VARIANCE, YTD_PERCENT, ANN_VARIANCE, ANN_PERCENT.
  YTD_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-YTDPLAN.
  ANN_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-ANNPLAN.

  IF ITABPRINT-YTDPLAN <> 0.
     YTD_PERCENT = ITABPRINT-YTDACT / ITABPRINT-YTDPLAN * 100.
  ENDIF.

  IF ITABPRINT-ANNPLAN <> 0.
     ANN_PERCENT = ITABPRINT-YTDACT / ITABPRINT-ANNPLAN * 100.
  ENDIF.

  WRITE: /1 ITABPRINT-PRART, 5 ITABPRINT-PSPRI, 8 ITABPRINT-VERNR+6(2),
         12 ITABPRINT-PSPID, 22(24) ITABPRINT-POST1, 48(4) INSTAT,
  53(12) ITABPRINT-YTDACT  DECIMALS 0,
  66(12) ITABPRINT-YTDPLAN DECIMALS 0,
  80(12) YTD_VARIANCE DECIMALS 0,
  94     YTD_PERCENT NO-ZERO,
  101(12) ITABPRINT-ANNPLAN DECIMALS 0,
  115(12) ANN_VARIANCE DECIMALS 0,
  129 ANN_PERCENT NO-ZERO.

  AT END OF VERNR.
     SUM.
     ULINE.
     YTD_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-YTDPLAN.
     ANN_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-ANNPLAN.
     IF ITABPRINT-YTDPLAN <> 0.
        YTD_PERCENT = ITABPRINT-YTDACT / ITABPRINT-YTDPLAN * 100.
     ENDIF.
     IF ITABPRINT-ANNPLAN <> 0.
        ANN_PERCENT = ITABPRINT-YTDACT / ITABPRINT-ANNPLAN * 100.
     ENDIF.
     WRITE: /1 TEXT-022,
     53(12) ITABPRINT-YTDACT  DECIMALS 0,
     66(12) ITABPRINT-YTDPLAN DECIMALS 0,
     80(12) YTD_VARIANCE DECIMALS 0,
     94     YTD_PERCENT NO-ZERO,
    101(12) ITABPRINT-ANNPLAN DECIMALS 0,
    115(12) ANN_VARIANCE DECIMALS 0,
    129 ANN_PERCENT NO-ZERO.
     ULINE.
  ENDAT.

  AT END OF PSPRI.
     SUM.
     YTD_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-YTDPLAN.
     ANN_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-ANNPLAN.
     IF ITABPRINT-YTDPLAN <> 0.
        YTD_PERCENT = ITABPRINT-YTDACT / ITABPRINT-YTDPLAN * 100.
     ENDIF.
     IF ITABPRINT-ANNPLAN <> 0.
        ANN_PERCENT = ITABPRINT-YTDACT / ITABPRINT-ANNPLAN * 100.
     ENDIF.
     WRITE: /1 TEXT-021,
     53(12) ITABPRINT-YTDACT  DECIMALS 0,
     66(12) ITABPRINT-YTDPLAN DECIMALS 0,
     80(12) YTD_VARIANCE DECIMALS 0,
     94     YTD_PERCENT NO-ZERO,
    101(12) ITABPRINT-ANNPLAN DECIMALS 0,
    115(12) ANN_VARIANCE DECIMALS 0,
    129 ANN_PERCENT NO-ZERO.
     ULINE.
  ENDAT.

  AT END OF PRART.
     SUM.
     YTD_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-YTDPLAN.
     ANN_VARIANCE = ITABPRINT-YTDACT - ITABPRINT-ANNPLAN.
     IF ITABPRINT-YTDPLAN <> 0.
        YTD_PERCENT = ITABPRINT-YTDACT / ITABPRINT-YTDPLAN * 100.
     ENDIF.
     IF ITABPRINT-ANNPLAN <> 0.
        ANN_PERCENT = ITABPRINT-YTDACT / ITABPRINT-ANNPLAN * 100.
     ENDIF.
     WRITE: /1 TEXT-020,
     53(12) ITABPRINT-YTDACT  DECIMALS 0,
     66(12) ITABPRINT-YTDPLAN DECIMALS 0,
     80(12) YTD_VARIANCE DECIMALS 0,
     94     YTD_PERCENT NO-ZERO,
    101(12) ITABPRINT-ANNPLAN DECIMALS 0,
    115(12) ANN_VARIANCE DECIMALS 0,
    129 ANN_PERCENT NO-ZERO.
     ULINE.
  ENDAT.

ENDLOOP.

ENDFORM.

************************************************************************
*                                                                      *
*               GET_PROJECT_STATUS                                     *
*                                                                      *
*                                                                      *
************************************************************************
FORM GET_PROJECT_STATUS USING PROJ_OBJNR.

  CLEAR INSTAT.
  CALL FUNCTION 'STATUS_TEXT_EDIT'
       EXPORTING
            OBJNR             = PROJ_OBJNR
            ONLY_ACTIVE       = 'X'
            SPRAS             = SY-LANGU
       IMPORTING
            LINE              = INSTAT
      EXCEPTIONS
           OBJECT_NOT_FOUND   = 1
           OTHERS             = 2
            .
  IF SY-SUBRC <> 0.
     WRITE: /1 'STATUS NOT FOUND ***Objnr = ', PROJ_OBJNR.
  ENDIF.
ENDFORM.

************************************************************************
*                                                                      *
*               TOP_OF_PAGE                                            *
*                                                                      *
*                                                                      *
************************************************************************
TOP-OF-PAGE.
  WRITE: /50 HEAD01.
  WRITE: /1 TEXT-RPT, SY-REPID,  38 HEAD02,
        103 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
           39 HEAD03, TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  WRITE: /46 HEAD04.
  ULINE.

  WRITE: /57 TEXT-C11, 73 TEXT-C11, 83 TEXT-C11, 94 TEXT-C11,
         106 TEXT-C12, 118 TEXT-C12, 127 TEXT-C12.

  WRITE: /1 TEXT-CL1, 5 TEXT-CL2, 8 TEXT-CL3, 12 TEXT-CL4, 22 TEXT-CL5,
         47 TEXT-CL6, 57 TEXT-CL7, 73 TEXT-CL8, 83 TEXT-CL9,
         94 TEXT-C10, 108 TEXT-CL8, 118 TEXT-CL9, 128 TEXT-C10.
   ULINE.
************************************************************************
**************************END OF PROGRAM********************************
************************************************************************
