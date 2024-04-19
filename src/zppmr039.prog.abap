REPORT ZPPMR039.

TYPE-POOLS: SLIS.

************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        FEB 2004.                                              *
*  Issue Log:   1045                                                   *
*  Description:                                                        *
*     - The purpose of this program is to produce the listing          *
*       of forecasted cashflow - actual and plan for a fiscal year.    *
*       How many acutal months and how many plan month depend upon     *
*       variant, however, actual start month must be January and plan  *
*       last month must be December.                                   *
*                                                                      *
************************************************************************
* 2006/06/20 mdemeest - 4.7 upgrade - excel spreadsheet abending.because
*                       column headings in packed fields.
*                       Copy XCLITAB to XCLITAB1 redefining packed field
*-----------------------------------------------------------------------
* 2010/06/24 M Khan   - TR840 Include plan amount from COSS table as
*                             well.
* 2012/08/07 M Khan   - TR995 Change C: drive to H: drive with         *
*                             directory, file selection using F4 and   *
*                             add variant field for file name.         *
*-----------------------------------------------------------------------

TABLES: PROJ,           "Project
        PRPS,           "WBS
        COSP,           "External Costs
        COSS,           "Internal Costs
        T001.           "Company Code
DATA:
    BEGIN OF ITABLE OCCURS 0,
       PRART         LIKE PRPS-PRART,           "Project Type
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       PSPHI         LIKE PRPS-PSPHI,           "Project
       POSID         LIKE PRPS-POSID,           "wbs s element - proj
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
    BEGIN OF XCLITAB OCCURS 0,
       PRART         LIKE PRPS-PRART,           "Project Type
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       PRJCT(9)      TYPE C,                    "Project
       PRJTXT(15)    TYPE C,                    "Project Description
       MAJOR         TYPE C,                    "Major project indicator
       AMOUNT01(8)   TYPE P DECIMALS 0,         "Amount Month 1
       AMOUNT02(8)   TYPE P DECIMALS 0,         "Amount Month 2
       AMOUNT03(8)   TYPE P DECIMALS 0,         "Amount Month 3
       AMOUNT04(8)   TYPE P DECIMALS 0,         "Amount Month 4
       AMOUNT05(8)   TYPE P DECIMALS 0,         "Amount Month 5
       AMOUNT06(8)   TYPE P DECIMALS 0,         "Amount Month 6
       AMOUNT07(8)   TYPE P DECIMALS 0,         "Amount Month 7
       AMOUNT08(8)   TYPE P DECIMALS 0,         "Amount Month 8
       AMOUNT09(8)   TYPE P DECIMALS 0,         "Amount Month 9
       AMOUNT10(8)   TYPE P DECIMALS 0,         "Amount Month 10
       AMOUNT11(8)   TYPE P DECIMALS 0,         "Amount Month 11
       AMOUNT12(8)   TYPE P DECIMALS 0,         "Amount Month 12
       TOTPLAN(8)    TYPE P DECIMALS 0,         "TOTAL PLANS
       TOTACTU(8)    TYPE P DECIMALS 0,         "TOTAL ACTUALS
       TOTGRND(8)    TYPE P DECIMALS 0,         "TOTAL GRAND
   END OF XCLITAB.

data:
    BEGIN OF XCLITAB1 OCCURS 0,
       PRART         LIKE PRPS-PRART,           "Project Type
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       PRJCT(9)      TYPE C,                    "Project
       PRJTXT(15)    TYPE C,                    "Project Description
       MAJOR         TYPE C,                    "Major project indicator
       AMOUNT01(8)   TYPE C,         "Amount Month 1
       AMOUNT02(8)   TYPE C,         "Amount Month 2
       AMOUNT03(8)   TYPE C,         "Amount Month 3
       AMOUNT04(8)   TYPE C,         "Amount Month 4
       AMOUNT05(8)   TYPE C,         "Amount Month 5
       AMOUNT06(8)   TYPE C,         "Amount Month 6
       AMOUNT07(8)   TYPE C,         "Amount Month 7
       AMOUNT08(8)   TYPE C,         "Amount Month 8
       AMOUNT09(8)   TYPE C,         "Amount Month 9
       AMOUNT10(8)   TYPE C,         "Amount Month 10
       AMOUNT11(8)   TYPE C,         "Amount Month 11
       AMOUNT12(8)   TYPE C,         "Amount Month 12
       TOTPLAN(8)    TYPE C,         "TOTAL PLANS
       TOTACTU(8)    TYPE C,         "TOTAL ACTUALS
       TOTGRND(8)    TYPE C,         "TOTAL GRAND
   END OF XCLITAB1.

DATA:
   BEGIN OF MONTH,
       NAME01(3) VALUE 'JAN',                   "Name of Month 1
       NAME02(3) VALUE 'FEB',                   "Name of Month 2
       NAME03(3) VALUE 'MAR',                   "Name of Month 3
       NAME04(3) VALUE 'APR',                   "Name of Month 4
       NAME05(3) VALUE 'MAY',                   "Name of Month 5
       NAME06(3) VALUE 'JUN',                   "Name of Month 6
       NAME07(3) VALUE 'JLY',                   "Name of Month 7
       NAME08(3) VALUE 'AUG',                   "Name of Month 8
       NAME09(3) VALUE 'SEP',                   "Name of Month 9
       NAME10(3) VALUE 'OCT',                   "Name of Month 10
       NAME11(3) VALUE 'NOV',                   "Name of Month 11
       NAME12(3) VALUE 'DEC',                   "Name of Month 12
   END   OF MONTH.

DATA:
   BEGIN OF COLTEXT,                 "Variable column headings
      VTEXT101(8) TYPE C,
      VTEXT102(8) TYPE C,
      VTEXT103(8) TYPE C,
      VTEXT104(8) TYPE C,
      VTEXT105(8) TYPE C,
      VTEXT106(8) TYPE C,
      VTEXT107(8) TYPE C,
      VTEXT108(8) TYPE C,
      VTEXT109(8) TYPE C,
      VTEXT110(8) TYPE C,
      VTEXT111(8) TYPE C,
      VTEXT112(8) TYPE C,
   END   OF COLTEXT.

DATA:
   CURR_MONTH(2) TYPE N,
   KOUNT1        TYPE I,
   KOUNT2        TYPE I,
   REPEAT        TYPE I,
   W_AMOUNT(12)  TYPE P DECIMALS 0,
   W_HEAD01(50)  TYPE C,
   W_HEAD02(60)  TYPE C.
*                                     "Req. fields for Proj ctr & Major
DATA: G_ATINN           LIKE CABN-ATINN,
      G_ATINN_MP        LIKE CABN-ATINN,
      MAJOR_PROJ,                                     "Major project
      OBJECT            LIKE AUSP-OBJEK,
      CHARIC            LIKE CABN-ATNAM,
      W_MAJOR           TYPE C,
      W_PRJTXT          LIKE XCLITAB-PRJTXT,
      LAST_PSPHI        LIKE PRPS-PSPHI.

DATA: BEGIN OF CHAR_TAB OCCURS 0.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

FIELD-SYMBOLS: <FS1>, <FS2>, <FS3>, <FS4>.
*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:
      P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL',                 "Company
      P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY,   "Year
      P_VERSN LIKE COSP-VERSN DEFAULT 000 OBLIGATORY.          "Version
SELECT-OPTIONS:
                S_DIV FOR PRPS-VERNR,                         "Division
                S_MONTH FOR SY-DATUM+4(2),
                S_PRART FOR PRPS-PRART,                   "Project Type
                S_PSPRI FOR PRPS-PSPRI,                  "Priority Type
                S_POSID FOR PRPS-POSID.                 "Project Number
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 33(20) TEXT-003.
SELECTION-SCREEN COMMENT 58(20) TEXT-003.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: EX_KSTAR    FOR COSS-KSTAR
                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-001.
PARAMETER: B_RPRT RADIOBUTTON GROUP BUTN,                "Report format
           B_EXCL RADIOBUTTON GROUP BUTN,                  "Excel sheet
           P_FILE LIKE RLGRAP-FILENAME DEFAULT 'H:\SAPTEMP\'. "TR995

SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.
*------------------------  AT SELECTION-SCREEN  ------------------------
INITIALIZATION.
  MOVE SY-DATUM+4(2) TO CURR_MONTH.
  CURR_MONTH = CURR_MONTH - 1.
  IF CURR_MONTH < 1.
     CURR_MONTH = 12.
  ENDIF.
  S_MONTH-SIGN   = 'I'.
  S_MONTH-OPTION = 'BT'.
  S_MONTH-LOW    = '1'.
  S_MONTH-HIGH   = CURR_MONTH.
  APPEND S_MONTH.
  CLEAR  S_MONTH.

AT SELECTION-SCREEN OUTPUT.
LOOP AT SCREEN.
IF SCREEN-NAME = 'S_MONTH-LOW'.
SCREEN-INPUT = '0'.
MODIFY SCREEN.
ENDIF.
ENDLOOP.

*----------------------------------------------------------------------*
*       AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
*End of TR995 changes

START-OF-SELECTION.
   LOOP AT S_MONTH.
        IF S_MONTH-LOW   <> '1'.
           CALL FUNCTION 'POPUP_FOR_INTERACTION'
             EXPORTING
             HEADLINE    = '!! ERROR !!'
             TEXT1       = ' '
             TEXT2       = '*** ACTUAL MONTHS MUST START FROM 1 ***'
             TEXT3       = 'PRESS OK BUTTON TO CORRECT VARIANTS'
             BUTTON_1    = 'OK'.
           STOP.
        ENDIF.
   ENDLOOP.

*----------------------- MAIN SECTION ---------------------------------*
IF SY-BATCH = 'X'.
   WRITE: /1 'This Program Must Be Run Foreground'.
   STOP.
ENDIF.
PERFORM MAKE_SETUP_FOR_MAJOR_INDICATOR.
PERFORM VARIABLE_COLUMN_HEADING.
PERFORM GET_FINANCIAL_DATA.
SORT XCLITAB BY  PRART PSPRI PRJCT.
IF B_RPRT = 'X'.
   PERFORM DISPLAY_ALV_GRID_DATA.
ELSE.
   PERFORM DISPLAY_EXCEL_SHEET.
ENDIF.
*----------------------------------------------------------------------*
FORM GET_FINANCIAL_DATA.

SELECT PRPS~PRART PRPS~PSPRI PRPS~PSPHI PRPS~POSID COSP~WRTTP COSP~VERSN
 COSP~WKG001 COSP~WKG002 COSP~WKG003 COSP~WKG004 COSP~WKG005 COSP~WKG006
 COSP~WKG007 COSP~WKG008 COSP~WKG009 COSP~WKG010 COSP~WKG011 COSP~WKG012

INTO  TABLE ITABLE

 FROM ( PRPS INNER JOIN COSP
        ON PRPS~OBJNR = COSP~OBJNR )

 WHERE  PRPS~POSID  IN  S_POSID           "Project
   AND  PRPS~VERNR  IN  S_DIV             "Division
   AND  PRPS~PBUKR   =  P_CCODE           "Company Code
   AND  PRPS~PKOKR   =  '10'              "Controlling Area
   AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator
   AND  PRPS~PRART  IN  S_PRART           "Type
   AND  PRPS~PSPRI  IN  S_PSPRI           "Priority
   AND  COSP~GJAHR   =  P_FYEAR           "Fiscal Year
   AND ( COSP~VERSN  =  P_VERSN
    OR  COSP~VERSN   = '000' )            "Version
   AND  COSP~WRTTP  IN ('01','04')        "Record with actuals & plans
   AND  COSP~KSTAR NOT IN EX_KSTAR        "Cost Element
   AND  COSP~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator

SELECT PRPS~PRART PRPS~PSPRI PRPS~PSPHI PRPS~POSID COSS~WRTTP COSS~VERSN
 COSS~WKG001 COSS~WKG002 COSS~WKG003 COSS~WKG004 COSS~WKG005 COSS~WKG006
 COSS~WKG007 COSS~WKG008 COSS~WKG009 COSS~WKG010 COSS~WKG011 COSS~WKG012

APPENDING TABLE ITABLE

 FROM ( PRPS INNER JOIN COSS
        ON PRPS~OBJNR = COSS~OBJNR )

 WHERE  PRPS~POSID  IN  S_POSID           "Project
   AND  PRPS~VERNR  IN  S_DIV             "Division
   AND  PRPS~PBUKR   =  P_CCODE           "Company Code
   AND  PRPS~PKOKR   =  '10'              "Controlling Area
   AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator
   AND  PRPS~PRART  IN  S_PRART           "Type
   AND  PRPS~PSPRI  IN  S_PSPRI           "Priority
   AND  COSS~GJAHR   =  P_FYEAR           "Fiscal Year
   AND ( COSS~VERSN  =  P_VERSN           "Plan Version          TR840
    OR  COSS~VERSN   = '000' )            "Version               TR840
*   AND  COSS~VERSN   = '000'             "Version               TR840
*   AND  COSS~WRTTP  = '04'        "Record with actuals & plans  TR840
   AND  COSS~WRTTP  IN ('01','04') "Record with actuals & plans  TR840
   AND  COSS~KSTAR NOT IN EX_KSTAR        "Cost Element
   AND  COSS~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator

DATA: FROM_MONTH(2) TYPE N.
SORT ITABLE BY PRART PSPRI PSPHI.
LOOP AT ITABLE.
IF ITABLE-WRTTP = '01' AND ITABLE-VERSN <> P_VERSN.
ELSE.
 MOVE-CORRESPONDING ITABLE TO XCLITAB.
 CONCATENATE ITABLE-POSID+0(2) '-' ITABLE-POSID+2(2) '-'
             ITABLE-POSID+4(3) INTO XCLITAB-PRJCT.
 IF ITABLE-WRTTP = '04'.                           "Actual
    ADD ITABLE-WKG001 FROM 1 TO S_MONTH-HIGH
                                   GIVING XCLITAB-TOTACTU.
    KOUNT1 = 6.
    KOUNT2 = 5.
    DO S_MONTH-HIGH TIMES.
       KOUNT1 = KOUNT1 + 1.
       KOUNT2 = KOUNT2 + 1.
       ASSIGN COMPONENT KOUNT1 OF STRUCTURE ITABLE  TO <FS1>.
       ASSIGN COMPONENT KOUNT2 OF STRUCTURE XCLITAB TO <FS2>.
       MOVE <FS1> TO <FS2>.
    ENDDO.
 ELSE.                                          "Plan
    FROM_MONTH = S_MONTH-HIGH + 1.
    ADD ITABLE-WKG001 FROM FROM_MONTH TO 12
                                 GIVING XCLITAB-TOTPLAN.
    REPEAT = 12 - S_MONTH-HIGH.
    KOUNT1 = S_MONTH-HIGH + 6.
    KOUNT2 = S_MONTH-HIGH + 5.
    DO REPEAT TIMES.
       KOUNT1 = KOUNT1 + 1.
       KOUNT2 = KOUNT2 + 1.
       ASSIGN COMPONENT KOUNT1 OF STRUCTURE ITABLE  TO <FS1>.
       ASSIGN COMPONENT KOUNT2 OF STRUCTURE XCLITAB TO <FS2>.
       MOVE <FS1> TO <FS2>.
    ENDDO.
 ENDIF.
XCLITAB-TOTGRND = XCLITAB-TOTACTU + XCLITAB-TOTPLAN.

*Don't add a row if all months individualy have zero amount

 IF   XCLITAB-AMOUNT01 <> 0 OR XCLITAB-AMOUNT02 <> 0
   OR XCLITAB-AMOUNT03 <> 0 OR XCLITAB-AMOUNT04 <> 0
   OR XCLITAB-AMOUNT05 <> 0 OR XCLITAB-AMOUNT06 <> 0
   OR XCLITAB-AMOUNT07 <> 0 OR XCLITAB-AMOUNT08 <> 0
   OR XCLITAB-AMOUNT09 <> 0 OR XCLITAB-AMOUNT10 <> 0
   OR XCLITAB-AMOUNT11 <> 0 OR XCLITAB-AMOUNT12 <> 0.
   IF ITABLE-PSPHI <> LAST_PSPHI.
      MOVE ITABLE-PSPHI TO LAST_PSPHI.
      CLEAR: W_MAJOR, W_PRJTXT.
      PERFORM GET_MAJOR_PROJECT_INDICATOR.
      PERFORM GET_PROJECT_DESCRIPTION.
   ENDIF.
   MOVE W_PRJTXT  TO XCLITAB-PRJTXT.
   MOVE W_MAJOR   TO XCLITAB-MAJOR.
   COLLECT XCLITAB.
 ENDIF.
 CLEAR   XCLITAB.
ENDIF.
ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*               MAKE_SETUP_FOR_MAJOR_INDICATOR.                        *
*----------------------------------------------------------------------*
FORM MAKE_SETUP_FOR_MAJOR_INDICATOR.
  MOVE 'MAJORPROJECT'         TO  CHARIC.    "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_MP.
  PERFORM BUILD_AUSP.
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
    WRITE: /.
  ENDIF.
ENDFORM.

*---------------------  BUILD_AUSP -------------------------------------
FORM BUILD_AUSP.

     REFRESH CHAR_TAB.
     SELECT * FROM AUSP INTO TABLE CHAR_TAB
      WHERE   ATINN = G_ATINN_MP    AND
              MAFID = 'O'           AND
              KLART = '014'.
     SORT CHAR_TAB BY OBJEK ATINN.
ENDFORM.

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine to get the value of the project control & major indicator
*-----------------------------------------------------------------------
FORM GET_MAJOR_PROJECT_INDICATOR.
DATA: W_OBJNR LIKE PRPS-OBJNR.
  CLEAR: XCLITAB-MAJOR.

  SELECT SINGLE OBJNR INTO W_OBJNR
    FROM PRPS
   WHERE PSPHI = ITABLE-PSPHI
     AND STUFE = '1'.

    READ TABLE CHAR_TAB WITH KEY OBJEK = W_OBJNR
                                 ATINN = G_ATINN_MP  BINARY SEARCH.
    IF SY-SUBRC EQ 0.
       MOVE CHAR_TAB-ATWRT+0(6) TO W_MAJOR.
    ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*               GET_PROJECT_DESCRIPTION.
*----------------------------------------------------------------------*
FORM GET_PROJECT_DESCRIPTION.

   SELECT SINGLE POST1 INTO PROJ-POST1
     FROM PROJ
    WHERE PSPNR = ITABLE-PSPHI.

    IF SY-SUBRC = 0.
       MOVE PROJ-POST1+0(15) TO W_PRJTXT.
    ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*                     VARIABLE_COLUMN_HEADING
*----------------------------------------------------------------------*
FORM VARIABLE_COLUMN_HEADING.
DATA: W_VARIABLE(5).
   MOVE 'Act-' TO W_VARIABLE.
   KOUNT1 = 0.
   DO S_MONTH-HIGH TIMES.
      KOUNT1 = KOUNT1 + 1.
      ASSIGN COMPONENT KOUNT1 OF STRUCTURE MONTH   TO <FS3>.
      ASSIGN COMPONENT KOUNT1 OF STRUCTURE COLTEXT TO <FS4>.
      CONCATENATE W_VARIABLE <FS3> INTO <FS4>.
   ENDDO.
   MOVE 'Plan-' TO W_VARIABLE.
   REPEAT = 12 - S_MONTH-HIGH.
   DO REPEAT TIMES.
      KOUNT1 = KOUNT1 + 1.
      ASSIGN COMPONENT KOUNT1 OF STRUCTURE MONTH   TO <FS3>.
      ASSIGN COMPONENT KOUNT1 OF STRUCTURE COLTEXT TO <FS4>.
      CONCATENATE W_VARIABLE <FS3> INTO <FS4>.
   ENDDO.
   CLEAR KOUNT1.
ENDFORM.
*----------------------------------------------------------------------*
*               DISPLAY_ALV_GRID_DATA.                                 *
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

  CONCATENATE TEXT-100 P_FYEAR TEXT-101 P_VERSN
              INTO W_HEAD01 SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  VARIANT-REPORT = REPID.
  REFRESH FIELDCAT.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'XCLITAB'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'PRART'.
          FC_STR-SELTEXT_L = TEXT-C01.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'PSPRI'.
          FC_STR-SELTEXT_L = TEXT-C02.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'PRJCT'.
          FC_STR-SELTEXT_L = TEXT-C03.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'PRJTXT'.
          FC_STR-SELTEXT_L = TEXT-C04.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MAJOR'.
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'AMOUNT01'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT101.  " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT02'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT102.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT03'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT103.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT04'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT104.  " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT05'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT105.  " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT06'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT106.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT07'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT107.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT08'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT108.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT09'.
          FC_STR-KEY    = ' '.                  " Key columns -not first
          FC_STR-SELTEXT_L = COLTEXT-VTEXT109.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT10'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT110.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT11'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT111.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'AMOUNT12'.
          FC_STR-SELTEXT_L = COLTEXT-VTEXT112.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'TOTPLAN'.
          FC_STR-SELTEXT_L = TEXT-C17.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'TOTACTU'.
          FC_STR-SELTEXT_L = TEXT-C18.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'TOTGRND'.
          FC_STR-SELTEXT_L = TEXT-C19.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
*          I_SAVE       = 'A'
*          IS_VARIANT   = variant
*          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = XCLITAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- SELECTION LINE: TYPE S
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
*  LS_LINE-KEY   = 'CLIENT: '.
  LS_LINE-INFO  = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
*  ls_line-typ  = 'S'.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.


ENDFORM.
*----------------------------------------------------------------------*
*               DISPLAY_EXCEL_SHEET.
*----------------------------------------------------------------------*
FORM DISPLAY_EXCEL_SHEET.

  DATA: BEGIN OF LT_FNAMES OCCURS 0,
        TEXT(60) TYPE C,
        END OF LT_FNAMES.

        LT_FNAMES-TEXT = TEXT-C01.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C02.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C03.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C04.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C05.
        APPEND LT_FNAMES.

        LT_FNAMES-TEXT = COLTEXT-VTEXT101.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT102.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT103.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT104.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT105.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT106.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT107.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT108.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT109.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT110.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT111.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = COLTEXT-VTEXT112.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C17.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C18.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C19.
        APPEND LT_FNAMES.
*-----------------------------------------------------------------------
* This loop created to move info from packed fields to character fields
* for 4.7 upgrade.
*-----------------------------------------------------------------------
loop at xclitab.
   clear xclitab1.
   move-corresponding xclitab to xclitab1.
   append xclitab1.
endloop.
*-----------------------------------------------------------------------

  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
*           FILE_NAME                 = 'C:\SAPTEMP'    "TR995
            FILE_NAME                 = P_FILE          "TR995
            CREATE_PIVOT              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       TABLES
*           PIVOT_FIELD_TAB           =
            DATA_TAB                  = XCLITAB1
            FIELDNAMES                = LT_FNAMES
       EXCEPTIONS
            FILE_NOT_EXIST            = 1
            FILENAME_EXPECTED         = 2
            COMMUNICATION_ERROR       = 3
            OLE_OBJECT_METHOD_ERROR   = 4
            OLE_OBJECT_PROPERTY_ERROR = 5
            INVALID_FILENAME          = 6
            INVALID_PIVOT_FIELDS      = 7
            DOWNLOAD_PROBLEM          = 8
            OTHERS                    = 9.
  IF SY-SUBRC NE 0.
  ENDIF.

ENDFORM.
