REPORT ZPPMR041 NO STANDARD PAGE HEADING LINE-SIZE 100
LINE-COUNT 60 MESSAGE-ID ZS.

************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        JANUARY 2005.                                          *
*  Issue Log:   1033                                                   *
*  Description:                                                        *
*     - The purpose of this program is to populate PFA number in the   *
*       project control and provide following reports:                 *
*       1- Projects being updated for PFA number                       *
*       2- Exceptions list                                             *
*                                                                      *
*       The BDC session will be created for transaction CJ02.          *
************************************************************************
*Changes:                                                              *
*Date     Track By       Description                                   *
*-------- ----- -------- ----------------------------------------------*
* 04/10/06 TR314 Mohammad Change input/output file name and path       *
* 2010/07/07 btboundy - TR843 Include plan amount from COSS table.     *
* 2018/08/20 AKMADASU - Update PFA for level one WBS numbers           *
* 2019/08/09 JOOKONTR - CHG0153669 - deleting the In-Service date field*
*                                    in the system  D30K930093*
************************************************************************

TABLES: PROJ,         "Project
        PRPS,         "WBS
        COSP,         "External Costs
        COSS,         "Internal Costs
        T001.         "Company Code

DATA:
    BEGIN OF INTAB OCCURS 0,
        PRART         LIKE PRPS-PRART,           "Project Type
        PSPRI         LIKE PRPS-PSPRI,           "Priority
        VERNRF        LIKE PRPS-VERNR,           "Division From
        VERNRT        LIKE PRPS-VERNR,           "Division To
        PRJCTF(11)    TYPE C,                    "Project From
        PRJCTT(11)    TYPE C,                    "Project To
        PFANEW(6)     TYPE C,                    "PFA # (NEW)
    END OF INTAB.

DATA:
    BEGIN OF ITABLE OCCURS 0,
       PRART         LIKE PRPS-PRART,           "Project Type
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       VERNR         LIKE PRPS-VERNR,           "Division
       PSPHI         LIKE PRPS-PSPHI,           "Project
       POSID         LIKE PRPS-POSID,           "Project
       OBJNR         LIKE PRPS-OBJNR,           "Object Number
       POST1         LIKE PRPS-POST1,           "WBS Description
       WRTTP         LIKE COSP-WRTTP,           "Plan/Actual
       VERSN         LIKE COSP-VERSN,           "Version
       WKG001        LIKE COSP-WKG001,          "Amount Month 1
       WKG002        LIKE COSP-WKG001,          "Amount Month 2
       WKG003        LIKE COSP-WKG001,          "Amount Month 3
       WKG004        LIKE COSP-WKG001,          "Amount Month 4
       WKG005        LIKE COSP-WKG001,          "Amount Month 5
       WKG006        LIKE COSP-WKG001,          "Amount Month 6
       WKG007        LIKE COSP-WKG001,          "Amount Month 7
       WKG008        LIKE COSP-WKG001,          "Amount Month 8
       WKG009        LIKE COSP-WKG001,          "Amount Month 9
       WKG010        LIKE COSP-WKG001,          "Amount Month 10
       WKG011        LIKE COSP-WKG001,          "Amount Month 11
       WKG012        LIKE COSP-WKG001,          "Amount Month 12
   END OF ITABLE.

DATA:
    BEGIN OF REPTAB OCCURS 0,
       PRART         LIKE PRPS-PRART,           "Project Type
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       POSID(7),                                "Project
       ACTYTD        LIKE COSP-WKG001,          "Amount YTD Actual
       PLANYR        LIKE COSP-WKG001,          "Amount Plan yearly
       PFAOLD(6)     TYPE C,                    "PFA # (OLD)
       PFANEW(6)     TYPE C,                    "PFA # (NEW)
   END OF REPTAB.
**--START OF CHANGES BY AKMADASU
TYPES: BEGIN OF TY_PROJ,
       PSPID  TYPE PS_PSPID,
       PSPID1 TYPE PS_PSPID,
       POSID(7),
       END OF TY_PROJ.
DATA: LT_PROJ TYPE TABLE OF TY_PROJ,
      LS_PROJ TYPE TY_PROJ.
**-- END OF CHANGES BY AKMADASU
DATA:
    BEGIN OF ERRORTAB OCCURS 0,
        PRART         LIKE PRPS-PRART,           "Project Type
        PSPRI         LIKE PRPS-PSPRI,           "Priority Type
        POSID(9)      TYPE C,                    "Project #
        POST1         LIKE PROJ-POST1,           "Project Description
        ACTYTD(8)     TYPE P DECIMALS 0,         "Amount Actual YTD
        PLANYR(8)     TYPE P DECIMALS 0,         "Amount Plan for Year
        PFAOLD(6)     TYPE C,                    "PFA # (OLD)
        PFANEW(6)     TYPE C,                    "PFA # (NEW)
    END OF ERRORTAB.

DATA:
    BEGIN OF EXCPTAB OCCURS 0,
       REC01(90),
    END OF EXCPTAB.


DATA: EXCELTAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
DATA:
   CURR_MONTH(2) TYPE N.
*                                     "Req. fields for Proj ctr & Major
DATA: G_ATINN        LIKE CABN-ATINN,
      G_ATINN_PC     LIKE CABN-ATINN,
      CHARIC         LIKE CABN-ATNAM,
      W_PFANO(6)     TYPE C,
      W_AMOUNT       LIKE COSS-WKG001,
      W_ACTUAL       LIKE COSS-WKG001,
      W_PLAN         LIKE COSS-WKG001,
      W_VERNR        LIKE PRPS-VERNR.

*&--Start of change by JOOKONTR CHG0153669
CONSTANTS: GC_SLWID TYPE SLWID VALUE 'ZALELEM'.
*&--End of change by JOOKONTR CHG0153669

DATA: BEGIN OF CHAR_TAB OCCURS 0.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
PARAMETERS:
      P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL',                 "Company
      P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY,   "Year
      P_VERSN LIKE COSP-VERSN DEFAULT 000 OBLIGATORY.          "Version
PARAMETERS:     FNAME1   TYPE RLGRAP-FILENAME DEFAULT TEXT-004.  "TR314
*   'J:\financial accounting\capital planning\pfa table\XXXX.xls'.
PARAMETERS:     FNAME2   TYPE RLGRAP-FILENAME DEFAULT TEXT-005.  "TR314
*   'J:\financial accounting\capital planning\pfa table\error.csv'.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 31(55) TEXT-003.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_BDC  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: EX_KSTAR    FOR COSS-KSTAR
                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.

*------------------------  INITIALIZATION  -----------------------------

*------------------------  AT SELECTION-SCREEN  ------------------------
AT SELECTION-SCREEN.
  IF P_FYEAR < SY-DATUM+0(4).
    MESSAGE E019 WITH TEXT-009.
    STOP.
  ENDIF.

*----------------------- MAIN SECTION ---------------------------------*
START-OF-SELECTION.
  PERFORM GET_COMPANY_NAME.
  PERFORM F_EXCEL_UPLOAD.
  PERFORM SETUP_FOR_PROJECT_CNTR_NUMBER.
  PERFORM GET_FINANCIAL_DATA.
  IF NOT ERRORTAB[] IS INITIAL.
    PERFORM DOWNLOAD_ERROR_TABLE.
  ENDIF.
  IF P_BDC = 'X'.
    PERFORM OPEN_BDC.
    PERFORM CREATE_BDC_SESSION.
    PERFORM CLOSE_BDC.
**-- start of changes by JOOKONTR CHG0153669
**-- start of changes by AKMADASU
*    PERFORM update_pfa.
**-- END of changes by AKMADASU
**-- END OF CHANGES BY JOOKONTR CHG0153669
  ENDIF.
  PERFORM DISPLAY_ALV_GRID_DATA.

*----------------------------------------------------------------------*

FORM GET_FINANCIAL_DATA.
*
  LOOP AT INTAB.

    SELECT PRPS~PRART PRPS~PSPRI PRPS~VERNR PRPS~PSPHI PRPS~POSID PRPS~OBJNR
           PRPS~POST1  COSP~WRTTP  COSP~VERSN  COSP~WKG001 COSP~WKG002
           COSP~WKG003 COSP~WKG004 COSP~WKG005 COSP~WKG006 COSP~WKG007
           COSP~WKG008 COSP~WKG009 COSP~WKG010 COSP~WKG011 COSP~WKG012

    APPENDING TABLE ITABLE

     FROM ( PRPS INNER JOIN COSP
            ON PRPS~OBJNR = COSP~OBJNR )

     WHERE  PRPS~PBUKR   =  P_CCODE           "Company Code
       AND  PRPS~PKOKR   =  '10'              "Controlling Area
       AND  PRPS~POSID  GE  INTAB-PRJCTF      "Project # From
       AND  PRPS~POSID  LE  INTAB-PRJCTT      "Project # To
       AND  PRPS~VERNR  GE  INTAB-VERNRF      "Division From
       AND  PRPS~VERNR  LE  INTAB-VERNRT      "Division From
       AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator
       AND  PRPS~PRART  EQ  INTAB-PRART       "Type
       AND  PRPS~PSPRI  EQ  INTAB-PSPRI       "Priority
       AND  COSP~GJAHR   =  P_FYEAR           "Fiscal Year
       AND ( COSP~VERSN  =  P_VERSN           "Plan Version
        OR  COSP~VERSN   = '000' )            "Actual Version
       AND  COSP~WRTTP  IN ('01','04')        "Record with actuals & plans
       AND  COSP~KSTAR NOT IN EX_KSTAR        "Cost Element
       AND  COSP~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator

    SELECT PRPS~PRART PRPS~PSPRI PRPS~VERNR PRPS~PSPHI PRPS~POSID PRPS~OBJNR
           PRPS~POST1  COSS~WRTTP  COSS~VERSN  COSS~WKG001 COSS~WKG002
           COSS~WKG003 COSS~WKG004 COSS~WKG005 COSS~WKG006 COSS~WKG007
           COSS~WKG008 COSS~WKG009 COSS~WKG010 COSS~WKG011 COSS~WKG012

    APPENDING TABLE ITABLE

     FROM ( PRPS INNER JOIN COSS
            ON PRPS~OBJNR = COSS~OBJNR )

     WHERE  PRPS~PBUKR   =  P_CCODE           "Company Code
       AND  PRPS~PKOKR   =  '10'              "Controlling Area
       AND  PRPS~POSID  GE  INTAB-PRJCTF      "Project # From
       AND  PRPS~POSID  LE  INTAB-PRJCTT      "Project # To
       AND  PRPS~VERNR  GE  INTAB-VERNRF      "Division From
       AND  PRPS~VERNR  LE  INTAB-VERNRT      "Division From
       AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator
       AND  PRPS~PRART  EQ  INTAB-PRART       "Type
       AND  PRPS~PSPRI  EQ  INTAB-PSPRI       "Priority
       AND  COSS~GJAHR   =  P_FYEAR           "Fiscal Year
*   AND  COSS~VERSN   = '000'             "Version                                TR843
       AND ( COSS~VERSN  =  P_VERSN           "Plan Version                           TR843
        OR  COSS~VERSN   = '000' )            "Actual Version                         TR843
*   AND  COSS~WRTTP  = '04'               "Record with actuals                    TR843
       AND  COSS~WRTTP  IN ('01','04')        "Record with actuals & plans            TR843
       AND  COSS~KSTAR NOT IN EX_KSTAR        "Cost Element
       AND  COSS~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator
*
  ENDLOOP.

  IF ITABLE[] IS INITIAL.
    WRITE: /1 'NO DATA SELECTED'.
    STOP.
  ENDIF.

  MOVE SY-DATUM+4(2) TO CURR_MONTH.
  SORT ITABLE BY PRART PSPRI PSPHI.
*
  LOOP AT ITABLE.
    MOVE: ITABLE-PRART TO REPTAB-PRART,
          ITABLE-PSPRI TO REPTAB-PSPRI,
          ITABLE-POSID+0(7) TO REPTAB-POSID,
          ITABLE-VERNR TO W_VERNR.

    IF ITABLE-WRTTP = '01' AND ITABLE-VERSN = P_VERSN.
      ADD ITABLE-WKG001 FROM 1 TO 12 GIVING W_AMOUNT.
      W_PLAN = W_PLAN + W_AMOUNT.
    ELSEIF ITABLE-WRTTP = '04' AND ITABLE-VERSN = '000'.
      ADD ITABLE-WKG001 FROM 1 TO CURR_MONTH GIVING W_AMOUNT.
      W_ACTUAL = W_ACTUAL + W_AMOUNT.
    ENDIF.

    AT END OF PSPHI.
      IF W_PLAN <> 0  OR  W_ACTUAL <> 0.
        MOVE W_PLAN   TO REPTAB-PLANYR.
        MOVE W_ACTUAL TO REPTAB-ACTYTD.
        PERFORM GET_PFA_NUMBERS.
*&--Start of code change by JOOKONTR   CHG0153669
*        IF REPTAB-PFAOLD = SPACE.
*          APPEND REPTAB.
*        ELSE.
*          IF REPTAB-PFAOLD <> REPTAB-PFANEW.
*            PERFORM BUILD_ERROR_TABLE.
*          ENDIF.
*        ENDIF.
        IF REPTAB-PFANEW NE SPACE.
          APPEND REPTAB.
        ELSEIF REPTAB-PFAOLD NE SPACE.
          REPTAB-PFANEW = REPTAB-PFAOLD.
          APPEND REPTAB.
        ELSE.
          PERFORM BUILD_ERROR_TABLE.
        ENDIF.
*&--Start of code change by JOOKONTR   CHG0153669
      ENDIF.
      CLEAR:  REPTAB, ERRORTAB, W_AMOUNT, W_PLAN, W_ACTUAL.
    ENDAT.

  ENDLOOP.
*
ENDFORM.                    "GET_FINANCIAL_DATA
*
**----------------------------------------------------------------------
*
**               SETUP_FOR_PROJECT_CNTR_NUMBER.
*
**----------------------------------------------------------------------
*
FORM SETUP_FOR_PROJECT_CNTR_NUMBER.
  MOVE 'PROJECT_CNTR_NUMBER'  TO  CHARIC.    "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_PC.
  PERFORM BUILD_AUSP.
ENDFORM.                    "SETUP_FOR_PROJECT_CNTR_NUMBER
*
**-----------------------  GET_ATINN -------------------------------
** Routine used to get the internal character number for project control
**----------------------------------------------------------------------
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
    WRITE: /.
  ENDIF.
ENDFORM.                    "GET_ATINN
*
**---------------------  BUILD_AUSP ------------------------------------
FORM BUILD_AUSP.

  REFRESH CHAR_TAB.
  SELECT * FROM AUSP INTO TABLE CHAR_TAB
   WHERE   ATINN = G_ATINN_PC    AND
           MAFID = 'O'           AND
           KLART = '014'.
  SORT CHAR_TAB BY OBJEK ATINN.
ENDFORM.                    "BUILD_AUSP
*
**--------------------------  GET_PFA_NUMBERS ----------------------
** Routine to get the value of the project control
**----------------------------------------------------------------------
FORM GET_PFA_NUMBERS.
  DATA: W_OBJNR LIKE PRPS-OBJNR.
*
  SELECT SINGLE OBJNR INTO W_OBJNR
    FROM PRPS
   WHERE PSPHI = ITABLE-PSPHI
     AND STUFE = '1'.

*Existing PFA Number
  READ TABLE CHAR_TAB WITH KEY OBJEK = W_OBJNR
                               ATINN = G_ATINN_PC  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATWRT+0(6) TO REPTAB-PFAOLD.
  ENDIF.

*New PFA Number
  LOOP AT INTAB.
    IF INTAB-PRART   = REPTAB-PRART    AND
       INTAB-PSPRI   = REPTAB-PSPRI    AND
       INTAB-VERNRF >= W_VERNR         AND
       INTAB-VERNRT <= W_VERNR         AND
       REPTAB-POSID >= INTAB-PRJCTF+0(7) AND
       REPTAB-POSID <= INTAB-PRJCTT+0(7).
      MOVE INTAB-PFANEW TO REPTAB-PFANEW.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "GET_PFA_NUMBERS
*
**----------------------------------------------------------------------
*
*                      BUILD_ERROR_TABLE
*-----------------------------------------------------------------------
FORM BUILD_ERROR_TABLE.

  MOVE-CORRESPONDING REPTAB TO ERRORTAB.
  CONCATENATE REPTAB-POSID+0(2) '-' REPTAB-POSID+2(2) '-'
              REPTAB-POSID+4(3) INTO ERRORTAB-POSID.
  CLEAR: PROJ-POST1.
  SELECT SINGLE POST1
    INTO PROJ-POST1
    FROM PROJ
   WHERE PSPNR = ITABLE-PSPHI.
  IF SY-SUBRC = 0.
    MOVE PROJ-POST1  TO  ERRORTAB-POST1.
  ENDIF.
  APPEND ERRORTAB.

ENDFORM.                    "BUILD_ERROR_TABLE
*&---------------------------------------------------------------------

*&      Form  GET_COMPANY_NAME
*&---------------------------------------------------------------------

FORM GET_COMPANY_NAME.

  CLEAR T001-BUTXT.
  SELECT SINGLE BUTXT INTO T001-BUTXT FROM T001             "Company Code
    WHERE BUKRS = P_CCODE.

ENDFORM.                    " GET_COMPANY_NAME

**----------------------------------------------------------------------
FORM DISPLAY_ALV_GRID_DATA.

  DATA: W_PROJECT(9).
  SORT REPTAB BY PRART PSPRI POSID.
  LOOP AT REPTAB.
    CONCATENATE REPTAB-POSID+0(2) REPTAB-POSID+2(2) REPTAB-POSID+4(3)
                INTO W_PROJECT SEPARATED BY '-'.
    WRITE: / REPTAB-PRART UNDER TEXT-105, REPTAB-PSPRI  UNDER TEXT-106,
             W_PROJECT UNDER TEXT-107,
             REPTAB-ACTYTD UNDER TEXT-109, REPTAB-PLANYR UNDER TEXT-108,
             REPTAB-PFAOLD UNDER TEXT-110, REPTAB-PFANEW UNDER TEXT-111.
  ENDLOOP.
  SKIP.
  ULINE.
*   LOOP AT ERRORTAB.
*   WRITE: /1 ERRORTAB-PRART, ERRORTAB-PSPRI, ERRORTAB-POSID,
*             ERRORTAB-POST1, ERRORTAB-ACTYTD, ERRORTAB-PLANYR,
*             ERRORTAB-PFANEW, ERRORTAB-PFAOLD.
*             .
*   ENDLOOP.
ENDFORM.                    "DISPLAY_ALV_GRID_DATA
*
**----------------------------------------------------------------------
*
**                          TOP-OF-PAGE
*
**----------------------------------------------------------------------
*
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID,  37 T001-BUTXT,             "Company
         72 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
         32 TEXT-HDR, 72  TEXT-PGE, SY-PAGNO.
  WRITE: /30 TEXT-101, P_FYEAR, ', ', TEXT-102, P_VERSN.
  ULINE.
  WRITE: /2 TEXT-105, 8  TEXT-106, 18 TEXT-107, 27 TEXT-109,
         47 TEXT-108, 74 TEXT-110, 89 TEXT-111.
  ULINE.

************************************************************************
FORM DOWNLOAD_ERROR_TABLE.

  DATA: DWN_ACTYTD(8) TYPE C,
        DWN_PLANYR(8) TYPE C.

  CONCATENATE  'Type' 'Priority' 'Project #' 'Proj Description' 'Actual'
               'Plan' 'PFA(Old)' 'PFA(new)' INTO EXCPTAB-REC01 SEPARATED
               BY ','.
  APPEND EXCPTAB.
  CLEAR  EXCPTAB.

  LOOP AT ERRORTAB.
    MOVE ERRORTAB-ACTYTD TO DWN_ACTYTD.
    MOVE ERRORTAB-PLANYR TO DWN_PLANYR.
    CONCATENATE ERRORTAB-PRART ERRORTAB-PSPRI  ERRORTAB-POSID
                ERRORTAB-POST1 DWN_ACTYTD      DWN_PLANYR
                ERRORTAB-PFAOLD ERRORTAB-PFANEW
    INTO EXCPTAB-REC01 SEPARATED BY ','.
    APPEND EXCPTAB.
    CLEAR  EXCPTAB.
  ENDLOOP.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      FILENAME            = FNAME2
      FILETYPE            = 'ASC'
    TABLES
      DATA_TAB            = EXCPTAB
    EXCEPTIONS
      FILE_OPEN_ERROR     = 1
      FILE_WRITE_ERROR    = 2
      INVALID_FILESIZE    = 3
      INVALID_TABLE_WIDTH = 4
      INVALID_TYPE        = 5
      NO_BATCH            = 6
      UNKNOWN_ERROR       = 7
      OTHERS              = 8.

ENDFORM.                    "DOWNLOAD_ERROR_TABLE
**======================================================================
*
FORM F_EXCEL_UPLOAD.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = FNAME1
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 9
      I_END_ROW               = 999
    TABLES
      INTERN                  = EXCELTAB
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    SKIP 2.
    IF SY-SUBRC = 2.
      WRITE: / 'SY-SUBRC = ', SY-SUBRC, 'PFA MAPPING EXCEL NOT FOUND '.
    ELSE.
      WRITE: / 'SY-SUBRC = ', SY-SUBRC, ' UNSUCCESSFUL EXCEL CALL '.
    ENDIF.
    SKIP 2.
    STOP.
  ENDIF.

  LOOP AT EXCELTAB.

    IF EXCELTAB-COL = 1.
      MOVE EXCELTAB-VALUE TO INTAB-PRART.
    ELSEIF EXCELTAB-COL = 2.
      MOVE EXCELTAB-VALUE TO INTAB-PSPRI.
    ELSEIF EXCELTAB-COL = 3.
      MOVE EXCELTAB-VALUE TO INTAB-VERNRF.
    ELSEIF EXCELTAB-COL = 4.
      MOVE EXCELTAB-VALUE TO INTAB-VERNRT.
    ELSEIF EXCELTAB-COL = 5.
      CONCATENATE EXCELTAB-VALUE+0(2) EXCELTAB-VALUE+3(2)
                  EXCELTAB-VALUE+6(3) INTO INTAB-PRJCTF.
    ELSEIF EXCELTAB-COL = 6.
      CONCATENATE EXCELTAB-VALUE+0(2) EXCELTAB-VALUE+3(2)
                  EXCELTAB-VALUE+6(3) '9999' INTO INTAB-PRJCTT.
    ELSEIF EXCELTAB-COL = 7.
      MOVE EXCELTAB-VALUE TO INTAB-PFANEW.
    ENDIF.
    AT END OF ROW.
      APPEND INTAB.
      CLEAR  INTAB.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "F_EXCEL_UPLOAD
*
*---------------------------------------------------------------------*
*       CREATE_BDC_SESSION                                            *
*---------------------------------------------------------------------*
FORM CREATE_BDC_SESSION.

  LOOP AT REPTAB.

    CLEAR   BDCDATA.
    REFRESH BDCDATA.
*&--Start of change by JOOKONTR CHG0153669
*                             Change Project: Initial Screen
*    PERFORM BDC_SCREEN USING 'SAPLCJWB'    '100'.
*    PERFORM BDC_FIELD  USING '*PRPS-POSID' REPTAB-POSID.
*
*    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/5'.       "STRUCTURE BUTTON
*    PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.
*
*    PERFORM BDC_FIELD  USING  'RCJ_MARKL-MARK(1)'  'X'.
*    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.          "Summerization
*    PERFORM BDC_SCREEN USING 'SAPLCTMS' '0109'.
*    PERFORM BDC_FIELD  USING 'RCTMS-MNAME(1)' 'PROJECT_CNTR_NUMBER'.
*    PERFORM BDC_FIELD  USING 'RCTMS-MWERT(1)' REPTAB-PFANEW.
*    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'BACK'.          "Go Back
*    PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.
*    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.         "Save

    PERFORM BDC_SCREEN      USING 'SAPLCJWB'            '0100'.
    PERFORM BDC_FIELD       USING '*PRPS-POSID'         REPTAB-POSID.         "WBS Element
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '=LETB'.              "Enter

    PERFORM BDC_SCREEN      USING 'SAPLCJWB'            '0901'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '=CLVD'.              "Summerization
    PERFORM BDC_FIELD       USING 'RCJ_MARKL-MARK(01)'  'X'.

    PERFORM BDC_SCREEN      USING 'SAPLCTMS'            '0109'.
    PERFORM BDC_FIELD       USING 'RCTMS-MNAME(01)'     'PROJECT_CNTR_NUMBER'. "Characteristic assignment
    PERFORM BDC_FIELD       USING 'RCTMS-MWERT(01)'     REPTAB-PFANEW.         "Characteristic assignment
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '=BACK'.               "Back

    PERFORM BDC_SCREEN      USING 'SAPLCJWB'            '0901'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '=PICK'.               "Selct WBS Element
    PERFORM BDC_FIELD       USING 'RCJ_MARKL-MARK(01)'  'X'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'          'RCWBS-IDENT(01)'.                        "Cursor Position

    PERFORM BDC_SCREEN      USING 'SAPLCJWB'            '0999'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '=USR1'.               "Goto User Fields Tab

    PERFORM BDC_SCREEN      USING 'SAPLCJWB'            '0999'.
    PERFORM BDC_FIELD       USING 'PRPS-SLWID'          GC_SLWID.             "Enter Field Key value
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '/00'.                 "Enter

    PERFORM BDC_SCREEN      USING 'SAPLCJWB'            '0999'.
    PERFORM BDC_FIELD       USING 'PRPS-USR01'          REPTAB-PFANEW.         "Enter PFA value
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '/EBCK'.

    PERFORM BDC_SCREEN      USING 'SAPLCJWB'            '0901'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'          '=BU'.                 "Save
*&--End of change by JOOKONTR  CHG0153669

    PERFORM INSERT_BDC.
  ENDLOOP.
ENDFORM.                    "CREATE_BDC_SESSION

*---------------------------------------------------------------------*
*       FORM Open_BDC                                                 *
*---------------------------------------------------------------------*
FORM OPEN_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      CLIENT              = SY-MANDT
      GROUP               = 'ZPS_PFA_NMBR'
      KEEP                = 'X'
      USER                = SY-UNAME
    EXCEPTIONS
      CLIENT_INVALID      = 1
      DESTINATION_INVALID = 2
      GROUP_INVALID       = 3
      GROUP_IS_LOCKED     = 4
      HOLDDATE_INVALID    = 5
      INTERNAL_ERROR      = 6
      QUEUE_ERROR         = 7
      RUNNING             = 8
      SYSTEM_LOCK_ERROR   = 9
      USER_INVALID        = 10
      OTHERS              = 11.
ENDFORM.                    "Open_BDC

*---------------------------------------------------------------------*
*       FORM Insert_BDC                                               *
*---------------------------------------------------------------------*
FORM INSERT_BDC.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      TCODE          = 'CJ02'
    TABLES
      DYNPROTAB      = BDCDATA
    EXCEPTIONS
      INTERNAL_ERROR = 1
      NOT_OPEN       = 2
      QUEUE_ERROR    = 3
      TCODE_INVALID  = 4
      OTHERS         = 5.
ENDFORM.                    "Insert_BDC

*---------------------------------------------------------------------*
*       FORM Close_BDC                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CLOSE_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      NOT_OPEN    = 1
      QUEUE_ERROR = 2
      OTHERS      = 3.
ENDFORM.                    "Close_BDC
*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.                    "BDC_SCREEN

*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PFA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_PFA .
  TYPES:BEGIN OF TY_FINAL,
 PBUKR TYPE PRPS-PBUKR,
 PSPNR TYPE PRPS-PSPNR,
 PSPID TYPE PROJ-PSPID,
 SPROG TYPE PROJ-SPROG,
 EPROG TYPE PROJ-EPROG,
 ERDAT TYPE PRPS-ERDAT,
 OBJKE TYPE AUSP-OBJEK,
 ATINN TYPE AUSP-ATINN,
 ATWRT TYPE AUSP-ATWRT,
 MSG   TYPE STRING,
       END OF TY_FINAL,
       BEGIN OF TY_PROJ,
         PSPNR TYPE PROJ-PSPNR,
         PSPID TYPE PROJ-PSPID,
         ERDAT TYPE PROJ-ERDAT,
         VBUKR TYPE PROJ-VBUKR,
         SPROG TYPE PROJ-SPROG,
         EPROG TYPE PROJ-EPROG,
       END OF TY_PROJ,
       BEGIN OF TY_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         OBJNR TYPE CHAR50, "prps-objnr,
         ERDAT TYPE PRPS-ERDAT,
         PBUKR TYPE PRPS-PBUKR,
       END OF TY_PRPS,
       BEGIN OF TY_AUSP,
         OBJEK TYPE AUSP-OBJEK,
         ATINN TYPE AUSP-ATINN,
         ATWRT TYPE AUSP-ATWRT,
       END OF TY_AUSP,
       BEGIN OF TY_PRHI,
         POSNR TYPE PRHI-POSNR,
         PSPHI TYPE PRHI-PSPHI,
       END OF TY_PRHI.

  DATA:GT_FINAL      TYPE TABLE OF TY_FINAL,
       GS_FINAL      TYPE TY_FINAL,
       GT_PROJ       TYPE TABLE OF TY_PROJ,
       GS_PROJ       TYPE TY_PROJ,
       GT_PRPS       TYPE TABLE OF TY_PRPS,
       GS_PRPS       TYPE TY_PRPS,
       GT_AUSP       TYPE TABLE OF TY_AUSP,
       GS_AUSP       TYPE TY_AUSP,
       GT_PRHI       TYPE TABLE OF TY_PRHI,
       GS_PRHI       TYPE TY_PRHI,
       GS_PRJ_DEF    TYPE BAPI_PROJECT_DEFINITION,
       GS_PRJ_DEF_U  TYPE BAPI_PROJECT_DEFINITION_UP,
       GT_METH_PRJ   TYPE TABLE OF BAPI_METHOD_PROJECT,
       GS_METH_PRJ   TYPE BAPI_METHOD_PROJECT,
       GT_ACTIVITY   TYPE TABLE OF BAPI_ACT_ELEMENT,
       GS_ACTIVITY   TYPE BAPI_ACT_ELEMENT,
       GT_ACTIVITY_U TYPE TABLE OF BAPI_ACT_ELEMENT_UPD,
       GS_ACTIVITY_U TYPE BAPI_ACT_ELEMENT_UPD,
       GS_RETURN     TYPE BAPIRETURN1,
       GT_MESSAGE    TYPE TABLE OF BAPI_METH_MESSAGE,
       GS_MESSAGE    TYPE BAPI_METH_MESSAGE,
       GT_WBS_ELE    TYPE TABLE OF BAPI_WBS_ELEMENT,
       GS_WBS_ELE    TYPE BAPI_WBS_ELEMENT,
       GT_WBS_ELE_U  TYPE TABLE OF BAPI_WBS_ELEMENT_UPDATE,
       GS_WBS_ELE_U  TYPE BAPI_WBS_ELEMENT_UPDATE.
  LOOP AT REPTAB.
*    LS_PROJ-PSPNR = REPTAB-POSID.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        INPUT  = REPTAB-POSID
      IMPORTING
        OUTPUT = LS_PROJ-PSPID.
    LS_PROJ-PSPID1 = LS_PROJ-PSPID.
    LS_PROJ-POSID = REPTAB-POSID.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
      EXPORTING
        INPUT  = LS_PROJ-PSPID
      IMPORTING
        OUTPUT = LS_PROJ-PSPID.
    APPEND LS_PROJ TO LT_PROJ.
    CLEAR:LS_PROJ.
  ENDLOOP.
  IF  LT_PROJ[] IS NOT INITIAL.
    SELECT PSPNR PSPID ERDAT VBUKR SPROG EPROG
         FROM PROJ  INTO TABLE GT_PROJ
     FOR ALL ENTRIES IN LT_PROJ

         WHERE PSPID = LT_PROJ-PSPID.

    IF GT_PROJ[] IS NOT INITIAL.
      SELECT PSPNR POSID OBJNR ERDAT PBUKR
             FROM PRPS
             INTO TABLE GT_PRPS
             FOR ALL ENTRIES IN GT_PROJ
             "WHERE pspnr = gt_proj-pspnr. " Commented by akmadasu
             WHERE POSID  = GT_PROJ-PSPID.  "Added  by akmadasu
    ENDIF.
  ENDIF.
  IF GT_PRPS[] IS NOT INITIAL.
    SELECT OBJEK ATINN ATWRT
           FROM AUSP
           INTO TABLE GT_AUSP
           FOR ALL ENTRIES IN GT_PRPS
           WHERE OBJEK  = GT_PRPS-OBJNR
           AND   ATINN  = '0000004258'. "'0000000631'. "'PROJECT_CNTR_NUMBER'.

  ENDIF.
  SORT:GT_PROJ BY PSPNR,
       GT_PRPS BY OBJNR,
       GT_AUSP BY OBJEK,
       GT_PRHI BY POSNR.
  LOOP AT GT_PRPS INTO GS_PRPS.
    READ TABLE GT_AUSP INTO GS_AUSP WITH KEY OBJEK = GS_PRPS-OBJNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_FINAL-OBJKE  = GS_AUSP-OBJEK.
      GS_FINAL-ATINN  = GS_AUSP-ATINN.
    ELSE.
      GS_FINAL-OBJKE  = GS_PRPS-OBJNR.
    ENDIF.
    READ TABLE GT_PROJ INTO GS_PROJ WITH KEY
*                           pspnr = gs_prps-pspnr.
                            PSPID = GS_PRPS-POSID.
    IF SY-SUBRC = 0.
      GS_FINAL-SPROG  = GS_PROJ-SPROG.
      GS_FINAL-EPROG  = GS_PROJ-EPROG.
      GS_FINAL-PSPNR  = GS_PRPS-PSPNR.
      GS_FINAL-PSPID  = GS_PROJ-PSPID.
      GS_FINAL-ERDAT  = GS_PRPS-ERDAT.
      GS_FINAL-PBUKR  = GS_PRPS-PBUKR.

      READ TABLE REPTAB WITH KEY POSID = GS_PROJ-PSPID..
      IF SY-SUBRC IS INITIAL.
        GS_FINAL-ATWRT  = REPTAB-PFANEW.
      ENDIF.
    ENDIF.
    APPEND GS_FINAL TO GT_FINAL.
    CLEAR GS_FINAL.
  ENDLOOP.

  LOOP AT GT_FINAL INTO GS_FINAL.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        INPUT  = GS_FINAL-PSPID
      IMPORTING
        OUTPUT = GS_PRJ_DEF-PROJECT_DEFINITION.
    GS_METH_PRJ-REFNUMBER          = '000001'.
    GS_METH_PRJ-OBJECTTYPE         = 'WBS-ELEMENT'.         "#EC NOTEXT
    GS_METH_PRJ-METHOD             = 'Update'.              "#EC NOTEXT
    GS_METH_PRJ-OBJECTKEY = GS_FINAL-PSPID.

    APPEND GS_METH_PRJ TO GT_METH_PRJ.
    CLEAR GS_METH_PRJ.
    GS_METH_PRJ-METHOD = 'Save'.                            "#EC NOTEXT
    APPEND GS_METH_PRJ TO GT_METH_PRJ.
    GS_WBS_ELE-WBS_ELEMENT = GS_FINAL-PSPID.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        INPUT  = GS_FINAL-PSPID
      IMPORTING
        OUTPUT = GS_WBS_ELE-PROJECT_DEFINITION.

*        = .
    GS_WBS_ELE-USER_FIELD_KEY     = 'ZALELEM'.
    GS_WBS_ELE-USER_FIELD_CHAR20_2   = GS_FINAL-ATWRT.
    APPEND GS_WBS_ELE TO GT_WBS_ELE.
    GS_WBS_ELE_U-WBS_ELEMENT           = 'X'.
    GS_WBS_ELE_U-PROJECT_DEFINITION    = 'X'.
    GS_WBS_ELE_U-USER_FIELD_KEY         = 'X'.
    GS_WBS_ELE_U-USER_FIELD_CHAR20_2 = 'X'.
**--Start of chnages by akmadasu CHG0153669
*    gs_wbs_ele_u-user_field_date1    = 'X'.
*    gs_wbs_ele_u-user_field_date2    = 'X'.
**--end of chnages by akmadasu CHG0153669
    GS_PRJ_DEF_U-PROJECT_DEFINITION  = 'X'.
    APPEND GS_WBS_ELE_U TO GT_WBS_ELE_U.
    CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
      EXPORTING
        I_PROJECT_DEFINITION       = GS_PRJ_DEF
        I_PROJECT_DEFINITION_UPD   = GS_PRJ_DEF_U
      IMPORTING
        RETURN                     = GS_RETURN
      TABLES
        I_METHOD_PROJECT           = GT_METH_PRJ
        I_WBS_ELEMENT_TABLE_UPDATE = GT_WBS_ELE_U
        I_WBS_ELEMENT_TABLE        = GT_WBS_ELE
        E_MESSAGE_TABLE            = GT_MESSAGE.
    IF GS_RETURN-TYPE NE 'E'.
      READ TABLE GT_MESSAGE INTO GS_MESSAGE INDEX 1.
      GS_FINAL-MSG = GS_MESSAGE-MESSAGE_TEXT.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ELSE.
      READ TABLE GT_MESSAGE INTO GS_MESSAGE INDEX 1.
      GS_FINAL-MSG = GS_MESSAGE-MESSAGE_TEXT.
    ENDIF.
    MODIFY GT_FINAL FROM GS_FINAL.
    REFRESH:GT_METH_PRJ,GT_WBS_ELE,GT_WBS_ELE_U,GT_MESSAGE.
    CLEAR:GS_PRJ_DEF,GS_PRJ_DEF_U,GS_RETURN,GS_MESSAGE.
  ENDLOOP.
ENDFORM.                    " UPDATE_PFA
