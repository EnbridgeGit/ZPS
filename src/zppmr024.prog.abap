REPORT ZPPMR024 NO STANDARD PAGE HEADING LINE-SIZE 110
                LINE-COUNT 58 MESSAGE-ID ZS.
***********************************************************************
*                                                                     *
*  Programmer: Mohammad Khan                                          *
*                                                                     *
*        Date: March, 2001.                                           *
*                                                                     *
*  Description: This program produces a listing of projects with no   *
*               transaction since a specified period through variants.*
*               If the user choose to close or tech.close the projects*
*               option in the variants, the program ZPPMI009 will be  *
*               submitted automatically using output of this program. *
*               Program ZPPMI009 will create a BDC session to close   *
*               or tech.close all these projects.                     *
*                                                                     *
*                                                                     *
***********************************************************************
* CHANGES:                                                            *
*                                                                     *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
***********************************************************************
TABLES: PROJ,                         " Project definition
        PRPS,                         " WBS element table
        COSS,                         " CO object: internal postings
        COSP,                         " CO object: external postings
        JEST,                         " Object Status
        T001.                         " Company code/name

DATA:  ACTUAL_AMOUNT LIKE COSP-WKG001,
       AMT           LIKE COSP-WKG001,
       INSTAT        LIKE JEST-STAT,
       KOUNT(4)         TYPE N,
       TOTREC(4)        TYPE N,
       CURRENT_YEAR(4)  TYPE N,
       CURRENT_MONTH(2) TYPE N,
       YEAR1(4)         TYPE N,
       BMONTH1(2)       TYPE N,
       EMONTH1(2)       TYPE N,
       YEAR2(4)         TYPE N,
       BMONTH2(2)       TYPE N,
       EMONTH2(2)       TYPE N,
       YEAR3(4)         TYPE N,
       BMONTH3(2)       TYPE N,
       EMONTH3(2)       TYPE N,
       YEAR4(4)         TYPE N,
       BMONTH4(2)       TYPE N,
       EMONTH4(2)       TYPE N,
       STARTYEAR(4)     TYPE N,
       STARTYEAR_PLUSONE(4)    TYPE N,
       STARTYEAR_PLUSTWO(4)    TYPE N,
       STARTYEAR_PLUSTHREE(4)  TYPE N.


DATA:  BEGIN OF PTAB OCCURS 0,
         VERNR  LIKE PROJ-VERNR,      "Division
         PSPID  LIKE PROJ-PSPID,      "Project ID
         POST1  LIKE PROJ-POST1,      "Project Description
         PSPRI  LIKE PRPS-PSPRI,      "Proj. Priority Code
         PRART  LIKE PRPS-PRART,      "Project Type Code
*        STAT   LIKE JEST-STAT.       "Project status
       END OF PTAB.

DATA:  BEGIN OF PROJECTS_TAB OCCURS 0,
         SIGN(1)   TYPE C,
         OPTION(2) TYPE C,
         LOW   LIKE PROJ-PSPID,
         HIGH  LIKE PROJ-PSPID,
       END OF PROJECTS_TAB.

DATA: BEGIN OF BDCDATA OCCURS 0.      "BDC Table
        INCLUDE STRUCTURE BDCDATA.    "Structure used for BDC Table
DATA: END OF BDCDATA.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
 PARAMETERS:     P_VBUKR LIKE PROJ-VBUKR DEFAULT 'UGL'.
 SELECT-OPTIONS: S_PSPID FOR PROJ-PSPID,
                 S_VERNR FOR PROJ-VERNR.
 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 31(6) TEXT-004.
 SELECTION-SCREEN COMMENT 37(4) TEXT-005.
 SELECTION-SCREEN END OF LINE.

 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 1(17) TEXT-003.
 SELECTION-SCREEN POSITION 33.
 PARAMETERS: P_MONTH(2) TYPE C OBLIGATORY.
 SELECTION-SCREEN POSITION 37.
 PARAMETERS: P_YEAR  LIKE COSP-GJAHR OBLIGATORY.
 SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-012.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(15) TEXT-015.
PARAMETERS:    P_TECO  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN: COMMENT 25(10) TEXT-014.
PARAMETERS:    P_RELS  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN: COMMENT 45(10) TEXT-013.
PARAMETERS:    P_CREAT RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-011.
 PARAMETERS: P_EXDATE LIKE PROJ-ERDAT DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
 PARAMETERS:     P_BDC     AS   CHECKBOX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(15) TEXT-016.
PARAMETERS:    P_CLOSE2  RADIOBUTTON GROUP OGRP.
SELECTION-SCREEN: COMMENT 25(10) TEXT-015.
PARAMETERS:    P_TECO2  RADIOBUTTON GROUP OGRP.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK BOX4.

************************************************************************
INITIALIZATION.
    MOVE '01/01'  TO  P_EXDATE+4.
*AT SELECTION-SCREEN ON P_YEAR.
AT SELECTION-SCREEN.
   MOVE SY-DATUM+0(4) TO CURRENT_YEAR.
   STARTYEAR_PLUSONE   = P_YEAR + 1.
   STARTYEAR_PLUSTWO   = P_YEAR + 2.
   STARTYEAR_PLUSTHREE = P_YEAR + 3.
   IF STARTYEAR_PLUSTHREE < CURRENT_YEAR.
      MESSAGE E019 WITH 'Can Not Go Back More Than 3 Years??'.
   ELSE.
   IF P_CREAT = 'X' AND P_BDC = 'X'.
      MESSAGE E019 WITH 'BDC session with Created status is not Valid'.
   ELSE.
   IF P_BDC = 'X' AND P_TECO = 'X' AND P_TECO2 = 'X'.
      MESSAGE E019 WITH 'Projects are already technically closed??'.
   ENDIF.
   ENDIF.
   ENDIF.

START-OF-SELECTION.
 PERFORM CHECK_SCREEN_DATA.
 PERFORM PROCESS_PROJECTS.
 PERFORM GET_COMPANY_NAME.
 PERFORM PRINT_REPORT.
 IF TOTREC > 0.
    IF P_BDC = 'X'.
       IF P_CLOSE2 = 'X'.
          SUBMIT ZPPMI009
            WITH S_PROJ  IN PROJECTS_TAB
            WITH P_CLOSE = 'X'
            WITH  P_BZDAT = SY-DATUM
          AND RETURN.
       ELSE.
       IF P_TECO2 = 'X'.
          SUBMIT ZPPMI009
            WITH S_PROJ  IN PROJECTS_TAB
            WITH P_TACT = 'X'
            WITH  P_BZDAT = SY-DATUM
          AND RETURN.
       ENDIF.
       ENDIF.
    ENDIF.
 ELSE.
     WRITE: /5 'NO PROJECTS SELECTED FOR THESE VARIANTS'.
*    MESSAGE A019 WITH 'No Projects Selected For These Variants'.
 ENDIF.

END-OF-SELECTION.
************************************************************************

* This routine will validate the variants data and set up dates*
FORM CHECK_SCREEN_DATA.

 MOVE SY-DATUM+0(4)    TO CURRENT_YEAR.
 MOVE SY-DATUM+4(2)    TO CURRENT_MONTH.
 MOVE P_YEAR           TO STARTYEAR.

 IF CURRENT_YEAR = STARTYEAR.
    MOVE STARTYEAR     TO YEAR1.
    MOVE P_MONTH       TO BMONTH1.
    MOVE CURRENT_MONTH TO EMONTH1.
 ELSE.

 IF CURRENT_YEAR = STARTYEAR_PLUSONE.
    MOVE STARTYEAR     TO YEAR1.
    MOVE P_MONTH       TO BMONTH1.
    MOVE '12'          TO EMONTH1.
    MOVE CURRENT_YEAR  TO YEAR2.
    MOVE '1'           TO BMONTH2.
    MOVE CURRENT_MONTH TO EMONTH2.
 ELSE.
 IF CURRENT_YEAR = STARTYEAR_PLUSTWO.
    MOVE STARTYEAR     TO YEAR1.
    MOVE P_MONTH       TO BMONTH1.
    MOVE '12'          TO EMONTH1.
    MOVE STARTYEAR_PLUSONE TO YEAR2.
    MOVE '1'           TO BMONTH2.
    MOVE '12'          TO EMONTH2.
    MOVE CURRENT_YEAR  TO YEAR3.
    MOVE '1'           TO BMONTH3.
    MOVE CURRENT_MONTH TO EMONTH3.
 ELSE.
    MOVE STARTYEAR     TO YEAR1.
    MOVE P_MONTH       TO BMONTH1.
    MOVE '12'          TO EMONTH1.
    MOVE STARTYEAR_PLUSONE TO YEAR2.
    MOVE '1'           TO BMONTH2.
    MOVE '12'          TO EMONTH2.
    MOVE STARTYEAR_PLUSTWO TO YEAR3.
    MOVE '1'           TO BMONTH3.
    MOVE '12'          TO EMONTH3.
    MOVE CURRENT_YEAR  TO YEAR4.
    MOVE '1'           TO BMONTH4.
    MOVE CURRENT_MONTH TO EMONTH4.
 ENDIF.
 ENDIF.
 ENDIF.

 IF P_TECO  = 'X'.
    INSTAT  = 'I0045'.
 ELSE.
 IF P_CREAT = 'X'.
    INSTAT  = 'I0001'.
 ELSE.
 IF P_RELS  = 'X'.
    INSTAT  = 'I0002'.
 ENDIF.
 ENDIF.
 ENDIF.

ENDFORM.

************************************************************************

* This routine will get all appropriate data for the selected projects.*
FORM PROCESS_PROJECTS.
 SELECT * FROM PROJ WHERE PSPID IN S_PSPID        "Project Id
                      AND VBUKR =  P_VBUKR        "Company Code
                      AND VERNR IN S_VERNR        "Division
                      AND ERDAT <  P_EXDATE       "Date project created
                      AND LOEVM <> 'X'            "Deleted indicator
               ORDER BY PSPID.
 IF SY-SUBRC = 0.
    CLEAR: ACTUAL_AMOUNT, KOUNT.
    SELECT * FROM PRPS                 "Select WBS
        WHERE PSPHI = PROJ-PSPNR
          AND BELKZ = 'X'              "Account Assignment
          AND LOEVM <> 'X'.            "Deletion flag
    IF SY-SUBRC = '0'.
       SELECT SINGLE * FROM JEST       "Select Status
           WHERE OBJNR = PRPS-OBJNR
             AND  STAT = INSTAT
             AND INACT <> 'X'.         "Not Inactie
       IF SY-SUBRC = 0.
          ADD     1       TO  KOUNT.
          PERFORM GET_ACTUAL_AMOUNT.
           IF ACTUAL_AMOUNT <> 0.
              EXIT.
           ENDIF.
       ENDIF.
    ENDIF.
    ENDSELECT.
    IF ACTUAL_AMOUNT = 0 AND KOUNT > 0.
       PERFORM BUILD_TABLE.
    ENDIF.

 ENDIF.
 ENDSELECT.
ENDFORM.

* COST TOTALS - External Postings
FORM GET_ACTUAL_AMOUNT.
  SELECT * FROM COSP
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR BETWEEN P_YEAR AND CURRENT_YEAR        "Fiscal Year
      AND WRTTP = '04'                                 "Actual amount
      AND VERSN = '000'
      AND BEKNZ IN ('S', 'H', 'L').              "Debit/Credit Indicator

    IF SY-SUBRC = '0'.
      IF COSP-GJAHR = YEAR1.
         ADD   COSP-WKG001 FROM BMONTH1 TO EMONTH1 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
      IF COSP-GJAHR = YEAR2.
         ADD   COSP-WKG001 FROM BMONTH2 TO EMONTH2 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
      IF COSP-GJAHR = YEAR3.
         ADD   COSP-WKG001 FROM BMONTH3 TO EMONTH3 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
      IF COSP-GJAHR = YEAR4.
         ADD   COSP-WKG001 FROM BMONTH4 TO EMONTH4 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
         CLEAR AMT.
      ENDIF.
      ENDIF.
      ENDIF.
      ENDIF.
    ENDIF.
    CHECK ACTUAL_AMOUNT = 0.
  ENDSELECT.
*
*
* COST TOTALS - Internal Postings

  SELECT * FROM COSS
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR BETWEEN P_YEAR AND CURRENT_YEAR        "Fiscal Year
      AND WRTTP = '04'                                 "Actuals
      AND   VERSN = '000'.

    IF SY-SUBRC = '0'.
      IF COSS-GJAHR = YEAR1.
         ADD   COSS-WKG001 FROM BMONTH1 TO EMONTH1 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
      IF COSS-GJAHR = YEAR2.
         ADD   COSS-WKG001 FROM BMONTH2 TO EMONTH2 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
      IF COSS-GJAHR = YEAR3.
         ADD   COSS-WKG001 FROM BMONTH3 TO EMONTH3 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
      IF COSS-GJAHR = YEAR4.
         ADD   COSS-WKG001 FROM BMONTH4 TO EMONTH4 GIVING AMT.
         ADD   AMT   TO ACTUAL_AMOUNT.
         CLEAR AMT.
      ELSE.
         CLEAR AMT.
      ENDIF.
      ENDIF.
      ENDIF.
      ENDIF.
   ENDIF.
 ENDSELECT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_TABLE
*&---------------------------------------------------------------------*
FORM BUILD_TABLE.

PTAB-VERNR = PROJ-VERNR.
PTAB-PSPID = PROJ-PSPID.
PTAB-POST1 = PROJ-POST1.
PTAB-PSPRI = PRPS-PSPRI.
PTAB-PRART = PRPS-PRART.
APPEND PTAB.

PROJECTS_TAB-SIGN = 'I'.
PROJECTS_TAB-OPTION = 'EQ'.
PROJECTS_TAB-LOW = PROJ-PSPID.
APPEND PROJECTS_TAB.
ADD  1  TO  TOTREC.
CLEAR: PTAB, PROJECTS_TAB.
ENDFORM.                    " BUILD_TABLE

*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
FORM PRINT_REPORT.
  LOOP AT PTAB.
    WRITE: /1 PTAB-VERNR, PTAB-PSPID UNDER TEXT-007, PTAB-POST1 UNDER
          TEXT-008, PTAB-PSPRI UNDER TEXT-009, PTAB-PRART UNDER
          TEXT-010.
  ENDLOOP.
ENDFORM.                    " PRINT_REPORT



*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
   WRITE: /1 TEXT-RPT, SY-REPID,  40 T001-BUTXT,             "Company
         80 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
          30 TEXT-TTL, 65 P_MONTH, 67 '/', 68 P_YEAR,
*          74  TEXT-PGE, 73 SY-PAGNO.
             TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   ULINE.
   WRITE: 1  TEXT-006, 15 TEXT-007, 30 TEXT-008, 80 TEXT-009,
          92 TEXT-010.
   ULINE.

*------------------------- End of Report Header ------------------------

*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_NAME
*&---------------------------------------------------------------------*
FORM GET_COMPANY_NAME.

SELECT SINGLE * FROM T001                "Company Code
  WHERE BUKRS = P_VBUKR.


ENDFORM.                    " GET_COMPANY_NAME

