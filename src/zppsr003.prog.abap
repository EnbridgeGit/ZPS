REPORT ZPPSR003 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        October 1999
*  Description:
*     - The purpose of this program is to produce the Capital
*       Expenditure Monthly Cashflow by WBS Report.
*       Predefined summaries are allowed by checking various boxes
*
*       The report is sorted by WBS, Type, Priority and summarized
*       by Division, Priority, Type and WBS and a final total by
*       Company.
************************************************************************
* --- Change Log ---

* yy/mm/dd --user-- #--- Description
*
* 11/05/05 mokhan   #413 After collect big_table, added clear big_table
*
* 09/10/06 mokhan   #663 Apply the Authorization check for Market Hub
*                        Companies.
* 03/12/30 mokhan   #963 Put description with plan line like it is with
*                        actual line.
*
* 01/03/07 mdemeest #812 New report of WBS/Projects summarized by WBS
*
* 99/10/21 mdemeest #507 Initial request-copied ZPPMR010 as starting
*                        point


************************************************************************

TABLES: PROJ,           "Project
        PRPS,           "WBS
        COSP,           "External Costs
        COSS,           "Internal Costs
*                    csku, jest,
        T001,           "Company Code
        T247,           "Month Table
        TCJ1T,          "Project Type Texts
        TCN7T.          "Priority Type Texts


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
      VERNA         LIKE PRPS-VERNA,          "Division name
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
       VERNA         LIKE PRPS-VERNA,           "vernr Name
       ACTUAL01(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL02(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL03(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL04(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL05(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL06(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL07(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL08(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL09(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL10(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL11(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL12(8)   TYPE P DECIMALS 0,         "Actual Amount
       ACTUAL13(8)   TYPE P DECIMALS 0,         "Total Actuals
       PLAN01(8)     TYPE P DECIMALS 0,         "Plan Amount
       PLAN02(8)     TYPE P DECIMALS 0,         "Plan Amount
       PLAN03(8)     TYPE P DECIMALS 0,         "Plan Amount
       PLAN04(8)     TYPE P DECIMALS 0,         "Plan Amount
       PLAN05(8)     TYPE P DECIMALS 0,         "Plan Amount
       PLAN06(8)     TYPE P DECIMALS 0,         "Plan Amount
       PLAN07(8)     TYPE P DECIMALS 0,         "Plan Amount
       PLAN08(8)     TYPE P DECIMALS 0,         "Plan Amt - Aug
       PLAN09(8)     TYPE P DECIMALS 0,         "Plan Amt - Sept
       PLAN10(8)     TYPE P DECIMALS 0,         "Plan Amt - Oct
       PLAN11(8)     TYPE P DECIMALS 0,         "Plan Amt - Nov
       PLAN12(8)     TYPE P DECIMALS 0,         "Plan Amt - Dec
       PLAN13(8)     TYPE P DECIMALS 0,         "TOTAL PLANS
   END OF BIG_TABLE.


DATA:   POST1         LIKE PRPS-POST1,           "Temp - description
        POST2         LIKE PROJ-POST1,           "Temp - description
        VERNA         LIKE PROJ-VERNA.

RANGES: S_BUKRS FOR T001T-BUKRS,     "TR663
        S_KOSTL FOR PROJ-KOSTL,      "TR663
        S_CCGRP FOR RGSB4-SETNR,     "TR663
        S_AUFNR FOR BSEG-AUFNR,      "TR663
        S_PSPNR FOR PROJ-PSPNR,      "TR663
        S_PSPID FOR PROJ-PSPID,      "TR663
        S_NPLNR FOR BSEG-NPLNR,      "TR663
        S_WERKS FOR PRPS-WERKS,      "TR663
        S_EKORG FOR EKKO-EKORG.      "TR663



*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:   P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL' OBLIGATORY. "tr663
*PARAMETERS:     P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL'.
SELECT-OPTIONS: SDIV FOR PRPS-VERNR.                       "Division
PARAMETERS:     P_VERS  LIKE COSP-VERSN DEFAULT '0',       "Plan version
                P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS:  SWBS    FOR PRPS-POSID+7(4),             "WBS elements
                 SPRART FOR PRPS-PRART,                   "Project Type
                 SPSPRI FOR PRPS-PSPRI.                   "Priority Type
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-036.
PARAMETER: P_CHKWBS AS CHECKBOX,
           P_CHKPRJ AS CHECKBOX,
           P_CHKDET AS CHECKBOX,
           P_WBSPrJ as checkbox.                         "2001/03/07
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.
*------------------------  AT SELECTION-SCREEN  ------------------------

AT SELECTION-SCREEN.
  IF ( P_CHKWBS IS INITIAL ) AND
     ( P_CHKPRJ IS INITIAL ) AND
     ( P_CHKDET IS INITIAL ) AND
     ( P_WBSPRJ IS INITIAL ).
     MESSAGE E100 WITH 'Must select at least one report'.
  ENDIF.

AT SELECTION-SCREEN ON P_CCODE.
 SELECT SINGLE * FROM T001                        "Get Company Name
   WHERE BUKRS = P_CCODE.

*Authorization Ckeck   "TR663
S_BUKRS-SIGN   = 'I'.
S_BUKRS-OPTION = 'EQ'.
S_BUKRS-LOW    = P_CCODE.
APPEND S_BUKRS.
CLEAR  S_BUKRS.

INCLUDE ZFICO_AUTH_CHECK_INCLUDE.       "TR663
INCLUDE ZNONFICO_AUTH_CHECK_INCLUDE.    "TR663

*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.
  PERFORM FICO_AUTHORIZATION_CHECK.     "TR663
  PERFORM NONFICO_AUTHORIZATION_CHECK.  "TR663

  PERFORM GET_WBS_ELEMENTS.
  SORT WA BY OBJNR.
  PERFORM GET_FISCAL_DATA.
  SORT BIG_TABLE BY OBJNR.
  LOOP AT BIG_TABLE.
    READ TABLE WA WITH KEY OBJNR = BIG_TABLE-OBJNR BINARY SEARCH.
    IF SY-SUBRC = '0'.
       PERFORM MODIFY_BIG_TABLE.
    ELSE.
       DELETE BIG_TABLE.
    ENDIF.
  ENDLOOP.

 SORT BIG_TABLE BY POSID PRART DISTGRP PSPRI VERNR PROJECT.

 IF P_CHKWBS = 'X'.
    PERFORM DISPLAY_TABLE_1.
 ENDIF.

 IF P_CHKPRJ = 'X'.
    PERFORM DISPLAY_TABLE_2.
 ENDIF.

 IF P_CHKDET = 'X'.
    PERFORM DISPLAY_TABLE_3.
 ENDIF.

 IF P_WBSPRJ = 'X'.                                     "2001/03/07
    sort big_table by posid project.
    perform display_table_4.
 endif.

END-OF-SELECTION.

*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
 FORMAT INTENSIFIED OFF.
 WRITE: /1 TEXT-RPT, SY-REPID,                          "Report Id
        76 T001-BUTXT,                                  "Company Name
       140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
 WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,  "Client
       63 TEXT-003,                                     "Report Title
          TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.            "Page Number
 WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS,              "Version
       75 TEXT-004, P_FYEAR.                            "Fiscal Year
 FORMAT INTENSIFIED ON.
 PERFORM PRINT_HEADINGS.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************

*------------------------ DISPLAY_TABLE_1 ------------------------------
* Report is summarized by wbs, project type, priority, project, division
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE_1.
 NEW-PAGE.
 LOOP AT BIG_TABLE.
   MOVE BIG_TABLE-WBSPOST1 TO POST1.
   MOVE BIG_TABLE-POST1    TO POST2.
   MOVE BIG_TABLE-VERNA    TO VERNA.

   AT NEW POSID.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
      WRITE: 2 BIG_TABLE-POSID, POST1(20).
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.


   AT END OF POSID.             "Summarized at WBS element
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 2(168) SY-ULINE.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).          "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).
      FORMAT COLOR COL_BACKGROUND.
      ULINE.
   ENDAT.

   AT LAST.                                     "Company TOTAL
      SUM.
      ULINE.
      PERFORM PRINT_VERT.
*      WRITE: 2 TEXT-029.                           "Issue 963
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      ULINE.
      WRITE: /.
      WRITE: /80 TEXT-028.
   ENDAT.
 ENDLOOP.
ENDFORM.
*------------------------ DISPLAY_TABLE_2 ------------------------------
* Report is summarized by wbs, project type, priority, project, division
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE_2.
 NEW-PAGE.
 LOOP AT BIG_TABLE.
   MOVE BIG_TABLE-WBSPOST1 TO POST1.
   MOVE BIG_TABLE-POST1    TO POST2.
   MOVE BIG_TABLE-VERNA    TO VERNA.

   AT NEW POSID.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
      WRITE: 2 BIG_TABLE-POSID, POST1(20).
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT NEW PRART.
      FORMAT COLOR COL_POSITIVE.
      PERFORM GET_PROJECT_TYPE.
      PERFORM PRINT_VERT.
      WRITE: 4 TCJ1T-PRATX(23).
      FORMAT COLOR COL_POSITIVE.
   ENDAT.

   AT NEW PSPRI.
      FORMAT COLOR COL_HEADING.
      PERFORM GET_PRIORITY_TYPE.
      PERFORM PRINT_VERT.
      WRITE: 5(22) TCN7T-KTEXT.
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT END OF VERNR.
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 10(160) SY-ULINE.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 10(15) VERNA.                          "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 10(15) VERNA.
      PERFORM PRINT_VERT.
      WRITE: 10(160) SY-ULINE.
      PERFORM PRINT_VERT.
   ENDAT.

   AT END OF PSPRI.
      SUM.
      FORMAT COLOR COL_HEADING.
      PERFORM GET_PRIORITY_TYPE.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 5(20) TCN7T-KTEXT.                     "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 5(20) TCN7T-KTEXT.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT END OF DISTGRP.
      IF BIG_TABLE-DISTGRP <> 'Z'.
         SUM.
         PERFORM PRINT_VERT.
         WRITE: 5(165) SY-ULINE.
*        PERFORM PRINT_DETAILS.                        "Issue 963
         PERFORM PRINT_DETAILS_01.                     "Issue 963
         IF BIG_TABLE-DISTGRP = '1'.                   "Issue 963
             WRITE: 5 TEXT-033.                        "Issue 963
          ELSEIF BIG_TABLE-DISTGRP = '2'.              "Issue 963
             WRITE: 5 TEXT-035.                        "Issue 963
          ELSEIF BIG_TABLE-DISTGRP = '3'.              "Issue 963
             WRITE: 5 TEXT-034.                        "Issue 963
          ENDIF.                                       "Issue 963
         PERFORM PRINT_DETAILS_02.                     "Issue 963

         IF BIG_TABLE-DISTGRP = '1'.
             WRITE: 5 TEXT-033.           "TOTAL NEW BUSINESS
          ELSEIF BIG_TABLE-DISTGRP = '2'. "TOTAL ADDITIONS
             WRITE: 5 TEXT-035.
          ELSEIF BIG_TABLE-DISTGRP = '3'. "TOTAL REPLACEMENTS
             WRITE: 5 TEXT-034.
          ENDIF.

          PERFORM PRINT_VERT.
          WRITE: 5(165) SY-ULINE.
      ENDIF.
   ENDAT.

   AT END OF PRART.             "Summarized at project level
      SUM.
      PERFORM GET_PROJECT_TYPE.
      FORMAT COLOR COL_POSITIVE.
      PERFORM PRINT_VERT.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 4 TCJ1T-PRATX(20).                     "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 4 TCJ1T-PRATX(20).
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT END OF POSID.             "Summarized at WBS element
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 2(168) SY-ULINE.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).          "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).
      FORMAT COLOR COL_BACKGROUND.
      ULINE.
   ENDAT.

   AT LAST.                                     "Company TOTAL
      SUM.
      ULINE.
      PERFORM PRINT_VERT.
*      WRITE: 2 TEXT-029.                           "Issue 963
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      ULINE.
      WRITE: /.
      WRITE: /80 TEXT-028.
   ENDAT.
 ENDLOOP.
ENDFORM.

*------------------------ DISPLAY_TABLE_3 ------------------------------
* Report is summarized by wbs, project type, priority, project, division
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE_3.
*----------------------- Working on this section -----------------------
 NEW-PAGE.
 LOOP AT BIG_TABLE.
   MOVE BIG_TABLE-WBSPOST1 TO POST1.
   MOVE BIG_TABLE-VERNA    TO VERNA.
   PERFORM GET_PROJECT_DESCRIPTION.

   AT NEW POSID.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
      WRITE: 2 BIG_TABLE-POSID, POST1(20).
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT NEW PRART.
      FORMAT COLOR COL_POSITIVE.
      PERFORM GET_PROJECT_TYPE.
      PERFORM PRINT_VERT.
      WRITE: 4 TCJ1T-PRATX(23).
      FORMAT COLOR COL_POSITIVE.
   ENDAT.

   AT NEW PSPRI.
      FORMAT COLOR COL_HEADING.
      PERFORM GET_PRIORITY_TYPE.
      PERFORM PRINT_VERT.
      WRITE: 5(22) TCN7T-KTEXT.
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT END OF PROJECT.
      SUM.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 5 BIG_TABLE-PROJECT(7), POST2(12).     "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 5 BIG_TABLE-PROJECT(7), POST2(12).
      PERFORM PRINT_VERT.                       "Blank line between proj
   ENDAT.

   AT END OF VERNR.
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 10(160) SY-ULINE.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 10(15) VERNA.                          "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 10(15) VERNA.
      PERFORM PRINT_VERT.
      WRITE: 10(160) SY-ULINE.
      PERFORM PRINT_VERT.
   ENDAT.

   AT END OF PSPRI.
      SUM.
      FORMAT COLOR COL_HEADING.
      PERFORM GET_PRIORITY_TYPE.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 5(20) TCN7T-KTEXT.                     "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 5(20) TCN7T-KTEXT.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT END OF DISTGRP.
      IF BIG_TABLE-DISTGRP <> 'Z'.
         SUM.
         PERFORM PRINT_VERT.
         WRITE: 5(165) SY-ULINE.
*        PERFORM PRINT_DETAILS.                        "Issue 963
         PERFORM PRINT_DETAILS_01.                     "Issue 963
         IF BIG_TABLE-DISTGRP = '1'.                   "Issue 963
             WRITE: 5 TEXT-033.                        "Issue 963
          ELSEIF BIG_TABLE-DISTGRP = '2'.              "Issue 963
             WRITE: 5 TEXT-035.                        "Issue 963
          ELSEIF BIG_TABLE-DISTGRP = '3'.              "Issue 963
             WRITE: 5 TEXT-034.                        "Issue 963
          ENDIF.                                       "Issue 963
         PERFORM PRINT_DETAILS_02.                     "Issue 963

         IF BIG_TABLE-DISTGRP = '1'.
             WRITE: 5 TEXT-033.           "TOTAL NEW BUSINESS
          ELSEIF BIG_TABLE-DISTGRP = '2'. "TOTAL ADDITIONS
             WRITE: 5 TEXT-035.
          ELSEIF BIG_TABLE-DISTGRP = '3'. "TOTAL REPLACEMENTS
             WRITE: 5 TEXT-034.
          ENDIF.

          PERFORM PRINT_VERT.
          WRITE: 5(165) SY-ULINE.
      ENDIF.
   ENDAT.

   AT END OF PRART.             "Summarized at project level
      SUM.
      PERFORM GET_PROJECT_TYPE.
      FORMAT COLOR COL_POSITIVE.
      PERFORM PRINT_VERT.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 4 TCJ1T-PRATX(20).                     "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 4 TCJ1T-PRATX(20).
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT END OF POSID.             "Summarized at WBS element
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 2(168) SY-ULINE.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).          "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).
      FORMAT COLOR COL_BACKGROUND.
      ULINE.
   ENDAT.

   AT LAST.                                     "Company TOTAL
      SUM.
      ULINE.
      PERFORM PRINT_VERT.
*      WRITE: 2 TEXT-029.                           "Issue 963
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      ULINE.
      WRITE: /.
      WRITE: /80 TEXT-028.
   ENDAT.
 ENDLOOP.
ENDFORM.

*------------------------ DISPLAY_TABLE_4 ------------------------------
* Report is summarized by wbs, project
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE_4.
 NEW-PAGE.
 LOOP AT BIG_TABLE.
   MOVE BIG_TABLE-WBSPOST1 TO POST1.
   MOVE BIG_TABLE-VERNA    TO VERNA.
   PERFORM GET_PROJECT_DESCRIPTION.

   AT NEW POSID.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
      WRITE: 2 BIG_TABLE-POSID, POST1(20).
      FORMAT COLOR COL_BACKGROUND.
   ENDAT.

   AT END OF PROJECT.
      SUM.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 5 BIG_TABLE-PROJECT(7), POST2(12).     "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 5 BIG_TABLE-PROJECT(7), POST2(12).
      PERFORM PRINT_VERT.                       "Blank line between proj
   ENDAT.

   AT END OF POSID.             "Summarized at WBS element
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 2(168) SY-ULINE.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_GROUP.
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).          "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 BIG_TABLE-POSID, POST1(18).
      FORMAT COLOR COL_BACKGROUND.
      ULINE.
   ENDAT.

   AT LAST.                                     "Company TOTAL
      SUM.
      ULINE.
      PERFORM PRINT_VERT.
*      WRITE: 2 TEXT-029.                           "Issue 963
*      PERFORM PRINT_DETAILS.                       "Issue 963
      PERFORM PRINT_DETAILS_01.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      PERFORM PRINT_DETAILS_02.                     "Issue 963
      WRITE: 2 TEXT-029.                            "Issue 963
      ULINE.
      WRITE: /.
      WRITE: /80 TEXT-028.
   ENDAT.
 ENDLOOP.
ENDFORM.


*----------------------- PRINT_HEADINGS -------------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
   FORMAT COLOR 2 ON.
   WRITE: /1(170) SY-ULINE.
   PERFORM PRINT_VERT.
   WRITE:  28 TEXT-JAN,  39 TEXT-FEB,  50 TEXT-MAR,  61 TEXT-APR,
           72 TEXT-MAY,  83 TEXT-JUN,  94 TEXT-JUL, 105 TEXT-AUG,
          116 TEXT-SEP, 127 TEXT-OCT, 138 TEXT-NOV, 149 TEXT-DEC,
          160 TEXT-TOT.
   ULINE.
   FORMAT COLOR 2 OFF.
ENDFORM.

*--------------------------  PRINT_VERT  -------------------------------
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:  /1 SY-VLINE,  27 SY-VLINE,  38 SY-VLINE,  49 SY-VLINE,
          60 SY-VLINE,  71 SY-VLINE,  82 SY-VLINE,  93 SY-VLINE,
         104 SY-VLINE, 115 SY-VLINE, 126 SY-VLINE, 137 SY-VLINE,
         148 SY-VLINE, 159 SY-VLINE, 170 SY-VLINE.
ENDFORM.

FORM PRINT_DETAILS_01.                                  "Issue 963
   PERFORM PRINT_VERT.
   WRITE: 26 'P', (10) BIG_TABLE-PLAN01 UNDER TEXT-JAN,
                  (10) BIG_TABLE-PLAN02 UNDER TEXT-FEB,
                  (10) BIG_TABLE-PLAN03 UNDER TEXT-MAR,
                  (10) BIG_TABLE-PLAN04 UNDER TEXT-APR,
                  (10) BIG_TABLE-PLAN05 UNDER TEXT-MAY,
                  (10) BIG_TABLE-PLAN06 UNDER TEXT-JUN,
                  (10) BIG_TABLE-PLAN07 UNDER TEXT-JUL,
                  (10) BIG_TABLE-PLAN08 UNDER TEXT-AUG,
                  (10) BIG_TABLE-PLAN09 UNDER TEXT-SEP,
                  (10) BIG_TABLE-PLAN10 UNDER TEXT-OCT,
                  (10) BIG_TABLE-PLAN11 UNDER TEXT-NOV,
                  (10) BIG_TABLE-PLAN12 UNDER TEXT-DEC,
                  (10) BIG_TABLE-PLAN13 UNDER TEXT-TOT.
ENDFORM.                                                 "Issue 963

FORM PRINT_DETAILS_02.                                   "Issue 963
   PERFORM PRINT_VERT.
   WRITE: 26 'A', (10) BIG_TABLE-ACTUAL01 UNDER TEXT-JAN,
                  (10) BIG_TABLE-ACTUAL02 UNDER TEXT-FEB,
                  (10) BIG_TABLE-ACTUAL03 UNDER TEXT-MAR,
                  (10) BIG_TABLE-ACTUAL04 UNDER TEXT-APR,
                  (10) BIG_TABLE-ACTUAL05 UNDER TEXT-MAY,
                  (10) BIG_TABLE-ACTUAL06 UNDER TEXT-JUN,
                  (10) BIG_TABLE-ACTUAL07 UNDER TEXT-JUL,
                  (10) BIG_TABLE-ACTUAL08 UNDER TEXT-AUG,
                  (10) BIG_TABLE-ACTUAL09 UNDER TEXT-SEP,
                  (10) BIG_TABLE-ACTUAL10 UNDER TEXT-OCT,
                  (10) BIG_TABLE-ACTUAL11 UNDER TEXT-NOV,
                  (10) BIG_TABLE-ACTUAL12 UNDER TEXT-DEC,
                  (10) BIG_TABLE-ACTUAL13 UNDER TEXT-TOT.

ENDFORM.

form heading_vernr.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_POSITIVE.
      WRITE: 2 TEXT-023, VERNA.
      FORMAT COLOR COL_BACKGROUND.
ENDFORM.

FORM HEADING_PROJECT_TYPE.
      SELECT SINGLE * FROM TCJ1T
          WHERE LANGU = SY-LANGU
            AND PRART = BIG_TABLE-PRART.
          PERFORM PRINT_VERT.
          FORMAT COLOR COL_TOTAL.
          WRITE: 4 TCJ1T-PRATX.
          FORMAT COLOR COL_BACKGROUND.
ENDFORM.

FORM GET_PROJECT_TYPE.
  SELECT SINGLE * FROM TCJ1T
      WHERE LANGU = 'E'
        AND PRART = BIG_TABLE-PRART.
ENDFORM.

FORM GET_PRIORITY_TYPE.
  SELECT SINGLE * FROM TCN7T
    WHERE LANGU = 'E'
      AND NPRIO = BIG_TABLE-PSPRI.
ENDFORM.

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
ENDFORM.

FORM BIG_TABLE_DUMP.
     WRITE: /2 BIG_TABLE-POSID,
                 111 BIG_TABLE-PLAN01,          "YTD Plan
                 129 BIG_TABLE-ACTUAL01.        "Actuals
ENDFORM.
FORM GET_FISCAL_DATA.
* COST TOTALS - External Postings
 SELECT * FROM COSP
   WHERE GJAHR = P_FYEAR                        "Fiscal Year selected
     AND OBJNR LIKE 'PR%'                       "only Project Info
     AND  ( ( VERSN = P_VERS AND WRTTP = '01' )  OR
           ( VERSN = '000'  AND WRTTP = '04' ) )
     AND BEKNZ IN ('S','H','L').                "Debit/Credit Indicator

 CLEAR BIG_TABLE.
   MOVE COSP-OBJNR TO BIG_TABLE-OBJNR.
   CASE COSP-WRTTP.
     WHEN '01'.
        MOVE COSP-WKG001 TO BIG_TABLE-PLAN01.
        MOVE COSP-WKG002 TO BIG_TABLE-PLAN02.
        MOVE COSP-WKG003 TO BIG_TABLE-PLAN03.
        MOVE COSP-WKG004 TO BIG_TABLE-PLAN04.
        MOVE COSP-WKG005 TO BIG_TABLE-PLAN05.
        MOVE COSP-WKG006 TO BIG_TABLE-PLAN06.
        MOVE COSP-WKG007 TO BIG_TABLE-PLAN07.
        MOVE COSP-WKG008 TO BIG_TABLE-PLAN08.
        MOVE COSP-WKG009 TO BIG_TABLE-PLAN09.
        MOVE COSP-WKG010 TO BIG_TABLE-PLAN10.
        MOVE COSP-WKG011 TO BIG_TABLE-PLAN11.
        MOVE COSP-WKG012 TO BIG_TABLE-PLAN12.
     WHEN '04'.
        MOVE COSP-WKG001 TO BIG_TABLE-ACTUAL01.
        MOVE COSP-WKG002 TO BIG_TABLE-ACTUAL02.
        MOVE COSP-WKG003 TO BIG_TABLE-ACTUAL03.
        MOVE COSP-WKG004 TO BIG_TABLE-ACTUAL04.
        MOVE COSP-WKG005 TO BIG_TABLE-ACTUAL05.
        MOVE COSP-WKG006 TO BIG_TABLE-ACTUAL06.
        MOVE COSP-WKG007 TO BIG_TABLE-ACTUAL07.
        MOVE COSP-WKG008 TO BIG_TABLE-ACTUAL08.
        MOVE COSP-WKG009 TO BIG_TABLE-ACTUAL09.
        MOVE COSP-WKG010 TO BIG_TABLE-ACTUAL10.
        MOVE COSP-WKG011 TO BIG_TABLE-ACTUAL11.
        MOVE COSP-WKG012 TO BIG_TABLE-ACTUAL12.
   ENDCASE.
   COLLECT BIG_TABLE.
   CLEAR   BIG_TABLE.                                    "TR314
 ENDSELECT.
* COST TOTALS - Internal Postings
 SELECT * FROM COSS
   WHERE GJAHR = P_FYEAR                       "Fiscal Year selected
     AND OBJNR LIKE 'PR%'                      "only Project Info
     AND  ( ( VERSN = P_VERS AND WRTTP = '01' )  OR
           ( VERSN = '000'  AND WRTTP = '04' ) )
     AND BEKNZ IN ('S','H','L').                "Debit/Credit Indicator

 MOVE COSS-OBJNR TO BIG_TABLE-OBJNR.
   CASE COSS-WRTTP.
     WHEN '01'.
        MOVE COSS-WKG001 TO BIG_TABLE-PLAN01.
        MOVE COSS-WKG002 TO BIG_TABLE-PLAN02.
        MOVE COSS-WKG003 TO BIG_TABLE-PLAN03.
        MOVE COSS-WKG004 TO BIG_TABLE-PLAN04.
        MOVE COSS-WKG005 TO BIG_TABLE-PLAN05.
        MOVE COSS-WKG006 TO BIG_TABLE-PLAN06.
        MOVE COSS-WKG007 TO BIG_TABLE-PLAN07.
        MOVE COSS-WKG008 TO BIG_TABLE-PLAN08.
        MOVE COSS-WKG009 TO BIG_TABLE-PLAN09.
        MOVE COSS-WKG010 TO BIG_TABLE-PLAN10.
        MOVE COSS-WKG011 TO BIG_TABLE-PLAN11.
        MOVE COSS-WKG012 TO BIG_TABLE-PLAN12.
     WHEN '04'.
        MOVE COSS-WKG001 TO BIG_TABLE-ACTUAL01.
        MOVE COSS-WKG002 TO BIG_TABLE-ACTUAL02.
        MOVE COSS-WKG003 TO BIG_TABLE-ACTUAL03.
        MOVE COSS-WKG004 TO BIG_TABLE-ACTUAL04.
        MOVE COSS-WKG005 TO BIG_TABLE-ACTUAL05.
        MOVE COSS-WKG006 TO BIG_TABLE-ACTUAL06.
        MOVE COSS-WKG007 TO BIG_TABLE-ACTUAL07.
        MOVE COSS-WKG008 TO BIG_TABLE-ACTUAL08.
        MOVE COSS-WKG009 TO BIG_TABLE-ACTUAL09.
        MOVE COSS-WKG010 TO BIG_TABLE-ACTUAL10.
        MOVE COSS-WKG011 TO BIG_TABLE-ACTUAL11.
        MOVE COSS-WKG012 TO BIG_TABLE-ACTUAL12.
   ENDCASE.
  COLLECT BIG_TABLE.
  CLEAR   BIG_TABLE.                               "TR314
 ENDSELECT.

ENDFORM.

FORM GET_WBS_ELEMENTS.
 SELECT PRART PSPRI POSID OBJNR PSPHI PLAKZ STUFE VERNR VERNA POST1
   INTO WA FROM PRPS
   WHERE PBUKR = P_CCODE                   "Company Code
     AND PKOKR = '10'                      "Controlling area
     AND VERNR IN SDIV                     "Division
     AND LOEVM <> 'X'                      "Not flagged deleted
     AND PRART IN SPRART                   "Project type
     AND PSPRI IN SPSPRI                   "Priority type
     AND ( BELKZ = 'X'                     "Lowest level - actuals
        OR PLAKZ = 'X' ).                  "Lowest level - planning

   IF WA-POSID+7(4) IN SWBS.               "WBS selection
      IF WA-POSID+5(2) CO '1234567890'.    "Eliminates templates
         APPEND WA.
      ENDIF.
  ENDIF.
 ENDSELECT.
ENDFORM.

FORM MODIFY_BIG_TABLE.
  MOVE WA-POSID(7)         TO BIG_TABLE-PROJECT.        "Project Number
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
    move wa-posid(2)       to big_table-vernr.
    MOVE WA-PRART          TO BIG_TABLE-PRART.
    MOVE WA-PSPRI          TO BIG_TABLE-PSPRI.
    MOVE WA-PSPHI          TO BIG_TABLE-PSPHI.
    MOVE WA-VERNA          TO BIG_TABLE-VERNA.
*      post1         like proj-post1,           "Project Description
    ADD BIG_TABLE-ACTUAL01 THEN BIG_TABLE-ACTUAL02
      UNTIL BIG_TABLE-ACTUAL12 GIVING BIG_TABLE-ACTUAL13.
    ADD BIG_TABLE-PLAN01   THEN BIG_TABLE-PLAN02
      UNTIL BIG_TABLE-PLAN12   GIVING BIG_TABLE-PLAN13.
  MODIFY BIG_TABLE.
ENDFORM.

FORM GET_PROJECT_DESCRIPTION.
  SELECT SINGLE * FROM PROJ
    WHERE PSPNR = BIG_TABLE-PSPHI.
  IF SY-SUBRC = '0'.
     MOVE PROJ-POST1 TO POST2.
  ENDIF.
ENDFORM.
