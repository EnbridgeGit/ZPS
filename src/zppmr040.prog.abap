REPORT ZPPMR040 NO STANDARD PAGE HEADING LINE-SIZE 235
                LINE-COUNT 60 MESSAGE-ID ZS.

************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        MARCH 2004.                                            *
*  Issue Log:   1024                                                   *
*  Description:                                                        *
*     - The purpose of this program is to produce the listing          *
*       of Estimated Gross Cost of Plant Additions based upon Actual   *
*       and Plan Amounts.                                              *
*       How many acutal months and how many plan month depend upon     *
*       variant, however, actual start month must be January and plan  *
*       last month must be December.                                   *
*       This report will include prior years data as well and this data*
*       will be input through excel sheet.                             *
*       This program must be run in foreground.                        *
*----------------------------------------------------------------------
* 2012/07/17 M Khan     TR995 Change C: drive to H: drive with default
*                             directory and Valide the directory path.
*
* 2010/07/07 btboundy - TR843 Include plan amount from COSS table.
************************************************************************

TABLES: PROJ,         "Project
        PRPS,         "WBS
        COSP,         "External Costs
        COSS,         "Internal Costs
        T001,         "Company Code
        COBRA,        "Settlement Rule For Order Settlement
        ZPWBS.        "Group WBS elements by asset class & overhead type

DATA:
    BEGIN OF P_TAB OCCURS 0,
        ANLKL         LIKE ZPWBS-ANLKL,         "Group/Asset Class
        OHTYP         LIKE ZPWBS-OHTYP,         "Overhead Type- DST, GSO
        WBSEL         LIKE ZPWBS-WBSEL,         "WBS
        PRVTXT(15)    TYPE C,                   "Text for Prior year
        BZDAT         LIKE COBRA-BZDAT,         "A.V.D
        MAJOR         TYPE C,                   "Major proj. indicator
        AMOUNT        LIKE COSS-WKG001,
        PRJCT(9)      TYPE C,                   "Project #
    END OF P_TAB.

DATA:
    BEGIN OF ITABLE OCCURS 0,
       PSPHI         LIKE PRPS-PSPHI,           "Project
       PRART         LIKE PRPS-PRART,           "Project Type
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       POSID         LIKE PRPS-POSID,           "wbs element - proj
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
    BEGIN OF WA,
       ANLKL         LIKE ZPWBS-ANLKL,          "WBS Group (Asset Class)
       WBS(4)        TYPE C,                    "WBS
       OHTYP         LIKE ZPWBS-OHTYP,          "Overhead Type-Dist, GSO
       PRJCT(9)      TYPE C,                    "Project #
       PRJTXT(15)    TYPE C,                    "Project Description
       WBSTXT(15)    TYPE C,                    "WBS Description
       PRART         LIKE PRPS-PRART,           "Project Type
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       BZDAT         LIKE COBRA-BZDAT,          "A.V.D or Finish Date
       MAJOR         TYPE C,                    "Major project indicator
   END OF WA.

DATA: BEGIN OF ALL_DOLLARS OCCURS 0,
       OHSOURCE      TYPE C,          "O-From System, R-Alloc. In Report
*      OHTYPE        TYPE C,          "O-From System, R-Alloc. In Report
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
      END OF ALL_DOLLARS.

DATA:
    BEGIN OF XCLTABL OCCURS 0.
        INCLUDE STRUCTURE WA.
        INCLUDE STRUCTURE ALL_DOLLARS.
DATA: END OF XCLTABL.

DATA: BEGIN OF REPTAB OCCURS 0.
        INCLUDE STRUCTURE XCLTABL.
DATA: END OF REPTAB.

DATA:
   BEGIN OF ERRTAB OCCURS 0,
       WBS(4)        TYPE C,                   "WBS
       PRART         LIKE PRPS-PRART,          "Project Type
       ERRTXT(25)    TYPE C,                   "Error Text
   END OF ERRTAB.

DATA:
   BEGIN OF MONTH,                              "Month Names
       NAME01(3) VALUE 'JAN',
       NAME02(3) VALUE 'FEB',
       NAME03(3) VALUE 'MAR',
       NAME04(3) VALUE 'APR',
       NAME05(3) VALUE 'MAY',
       NAME06(3) VALUE 'JUN',
       NAME07(3) VALUE 'JLY',
       NAME08(3) VALUE 'AUG',
       NAME09(3) VALUE 'SEP',
       NAME10(3) VALUE 'OCT',
       NAME11(3) VALUE 'NOV',
       NAME12(3) VALUE 'DEC',
   END   OF MONTH.

DATA:
   BEGIN OF COL,                    "Variable column headings
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
   END   OF COL.

DATA:
   CURR_MONTH(2) TYPE N,
   KOUNT1        TYPE I,
   KOUNT2        TYPE I,
   KOUNT3        TYPE I,
   REPEAT        TYPE I,
   W_DATE        LIKE PROJ-PLSEZ,
   FINISH_DATE   LIKE PROJ-PLSEZ,
   W_YEAR        LIKE COSP-GJAHR,
   W_MONTH(2)    TYPE N,
   W_AMOUNT(12)  TYPE P DECIMALS 0,
   W_HEAD01(50)  TYPE C,
   W_HEAD02(60)  TYPE C,
   W_ANLKL       LIKE ZPWBS-ANLKL,
   W_GPRCNT(7)   TYPE C,
   W_DPRCNT(7)   TYPE C,
   ZPWBS_FLAG    TYPE C,
   WRK_WBSTXT    LIKE REPTAB-WBSTXT,
   WRK_ANLKL     LIKE REPTAB-ANLKL,
   WRK_WBS       LIKE REPTAB-WBS,
   W_OHTYPE      LIKE ZPWBS-OHTYP.
*                                     "Req. fields for Proj ctr & Major
DATA: G_ATINN        LIKE CABN-ATINN,
      G_ATINN_MP     LIKE CABN-ATINN,
      MAJOR_PROJ,                                  "Major project
      OBJECT         LIKE AUSP-OBJEK,
      CHARIC         LIKE CABN-ATNAM,
      W_MAJOR        TYPE C,
      W_PRJTXT       LIKE XCLTABL-PRJTXT,
      LAST_PSPHI     LIKE PRPS-PSPHI,
      RETURN_CODE    LIKE SY-SUBRC.

*DATA: EXCELTAB TYPE KCDE_CELLS OCCURS 0 WITH HEADER LINE. "4.6C Upgrade
DATA: EXCELTAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF CHAR_TAB OCCURS 0.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

DATA: LV_DIR TYPE STRING,                                   "TR995
      LV_BOL TYPE C,        "abap_bool.                     "TR995
      W_FILE(128) TYPE C,                                   "TR995
*      w_error(50) TYPE c,                                  "TR995
      WNAME TYPE STRING.                                    "TR995


*CONSTANTS: DPATH(3) TYPE C VALUE 'H:\',                    "TR995
*           FILENAME TYPE STRING VALUE 'CWIP02.xls'.        "TR995

FIELD-SYMBOLS: <FS1>, <FS2>, <FS3>, <FS4>, <FS5>.
*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 15(55) TEXT-110.
SELECTION-SCREEN END   OF LINE.
*SELECTION-SCREEN SKIP 1.
PARAMETERS:
      P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL',                 "Company
      P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY,   "Year
*      P_FYEAR LIKE COSP-GJAHR DEFAULT '2003' OBLIGATORY,      "Year
      P_VERSN LIKE COSP-VERSN DEFAULT 000 OBLIGATORY,          "Version
      P_DOHED(4) TYPE P DECIMALS 3 DEFAULT 11,           "Dist OverHead
      P_GOHED(4) TYPE P DECIMALS 3 DEFAULT 11.           "GSO  OverHead

SELECT-OPTIONS:
                S_DIV FOR PRPS-VERNR,                         "Division
                S_MONTH FOR SY-DATUM+4(2) OBLIGATORY,    "Actual Months
                S_PRART FOR PRPS-PRART,                   "Project Type
                S_PSPRI FOR PRPS-PSPRI,                  "Proj Priority
                S_POSID FOR PRPS-POSID DEFAULT '01-03-042'
                            TO '01-03-400-9999'.         "Project Number

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 33(20) TEXT-003.
SELECTION-SCREEN COMMENT 58(20) TEXT-003.
SELECTION-SCREEN END   OF LINE.

*PARAMETERS:     FNAME      TYPE RLGRAP-FILENAME
*                      DEFAULT 'C:\SAPTEMP\CWIP02.xls'.
PARAMETERS:     FNAME(128) TYPE C DEFAULT 'H:\' OBLIGATORY. "TR995
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-004.
PARAMETERS: P_DSTPRJ(9) TYPE C DEFAULT '37-04-081',
            P_WBSDST(4) TYPE N DEFAULT '5821',
            P_WBSGSO(4) TYPE N DEFAULT '5822',
            P_DPRART LIKE PRPS-PRART DEFAULT '07',     "Default Type
            P_DPSPRI LIKE PRPS-PSPRI DEFAULT '5'.      "Default Priority
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: EX_KSTAR    FOR COSS-KSTAR
                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-001.
PARAMETER: B_SRPRT RADIOBUTTON GROUP BUTN.             "Summry Report
PARAMETER: B_DRPRT RADIOBUTTON GROUP BUTN.             "Detail Report
*PARAMETER: B_EXCL  RADIOBUTTON GROUP BUTN.             "Excel sheet
SELECTION-SCREEN END OF BLOCK BOX4.
SELECTION-SCREEN END OF BLOCK BOX.

*------------------------  AT SELECTION-SCREEN  ------------------------
INITIALIZATION.
  MOVE FNAME TO WNAME.                                      "TR995
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

AT SELECTION-SCREEN.
  LOOP AT S_MONTH.
    IF S_MONTH-LOW  <> '1' AND S_MONTH-HIGH <> SPACE.
      MESSAGE E019 WITH TEXT-009.
      STOP.
    ENDIF.
  ENDLOOP.

  LOOP AT S_MONTH.
    IF S_MONTH-HIGH = SPACE.
      IF S_MONTH-LOW <> 0.
        S_MONTH-OPTION = 'BT'.
        S_MONTH-HIGH = S_MONTH-LOW.
        MODIFY S_MONTH.
      ENDIF.
    ENDIF.
  ENDLOOP.

*START OF TR995 CHANGES
AT SELECTION-SCREEN ON VALUE-REQUEST FOR FNAME.
  DATA: TAB TYPE FILETABLE,
        WCOUNT TYPE I,
        LINE_TAB LIKE LINE OF TAB,
        GD_PATH  TYPE STRING.

*Slect Directory
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      WINDOW_TITLE    = '*** SELECT DIRECTORY PATH ***'
      INITIAL_FOLDER  = WNAME    "'H:'
    CHANGING
      SELECTED_FOLDER = GD_PATH.

  CALL METHOD CL_GUI_CFW=>FLUSH.
*        IF gd_path = space.
*           FNAME = dpath.
*        ELSE.
*           FNAME = gd_path.
*        ENDIF.

* Select File.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE      = '*** SELECT FILE ***'
      INITIAL_DIRECTORY = GD_PATH
    CHANGING
      FILE_TABLE        = TAB
      RC                = WCOUNT.

  LOOP AT TAB INTO LINE_TAB.
    MOVE LINE_TAB-FILENAME TO FNAME.
  ENDLOOP.

AT SELECTION-SCREEN ON FNAME.
  PERFORM CHECK_FILE_PATH.
*End Of TR995 changes

*----------------------- MAIN SECTION ---------------------------------*
START-OF-SELECTION.
  PERFORM VARIABLE_COLUMN_HEADING.
  PERFORM GET_COMPANY_NAME.
  PERFORM F_EXCEL_UPLOAD.
  PERFORM MAKE_SETUP_FOR_MAJOR_INDICATOR.
  PERFORM GET_FINANCIAL_DATA.
  PERFORM INSERT_PREVIOUS_YEAR_DATA.
  PERFORM CALCULATE_OVERHEAD.
  PERFORM PRINT_ERROR_REPORT.

  IF B_DRPRT = 'X'.                  "Detail Report
    PERFORM PRINT_DETAIL_REPORT.
*ELSEIF B_SRPRT = 'X'.
  ELSE.                              "Summry Report
    PERFORM PRINT_SUMMARY_REPORT.
*ELSE.
*   PERFORM DISPLAY_EXCEL_SHEET.
  ENDIF.
*----------------------------------------------------------------------*
FORM GET_FINANCIAL_DATA.

  SELECT PRPS~PSPHI  PRPS~PRART  PRPS~PSPRI  PRPS~POSID  PRPS~OBJNR
         PRPS~POST1  COSP~WRTTP  COSP~VERSN  COSP~WKG001 COSP~WKG002
         COSP~WKG003 COSP~WKG004 COSP~WKG005 COSP~WKG006 COSP~WKG007
         COSP~WKG008 COSP~WKG009 COSP~WKG010 COSP~WKG011 COSP~WKG012

  INTO  TABLE ITABLE

   FROM ( PRPS INNER JOIN COSP
          ON PRPS~OBJNR = COSP~OBJNR )

   WHERE  PRPS~PBUKR   =  P_CCODE           "Company Code
     AND  PRPS~PKOKR   =  '10'              "Controlling Area
     AND  PRPS~POSID  IN  S_POSID           "Project
     AND  PRPS~VERNR  IN  S_DIV             "Division
     AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator
     AND  PRPS~PRART  IN  S_PRART           "Type
     AND  PRPS~PSPRI  IN  S_PSPRI           "Priority
     AND  COSP~GJAHR   =  P_FYEAR           "Fiscal Year
     AND ( COSP~VERSN  =  P_VERSN           "Plan Version
      OR  COSP~VERSN   = '000' )            "Actual Version
     AND  COSP~WRTTP  IN ('01','04')        "Record with actuals & plans
     AND  COSP~KSTAR NOT IN EX_KSTAR        "Cost Element
     AND  COSP~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator

  SELECT PRPS~PSPHI  PRPS~PRART  PRPS~PSPRI  PRPS~POSID  PRPS~OBJNR
         PRPS~POST1  COSS~WRTTP  COSS~VERSN  COSS~WKG001 COSS~WKG002
         COSS~WKG003 COSS~WKG004 COSS~WKG005 COSS~WKG006 COSS~WKG007
         COSS~WKG008 COSS~WKG009 COSS~WKG010 COSS~WKG011 COSS~WKG012

  APPENDING TABLE ITABLE

   FROM ( PRPS INNER JOIN COSS
          ON PRPS~OBJNR = COSS~OBJNR )

   WHERE  PRPS~PBUKR   =  P_CCODE           "Company Code
     AND  PRPS~PKOKR   =  '10'              "Controlling Area
     AND  PRPS~POSID  IN  S_POSID           "Project
     AND  PRPS~VERNR  IN  S_DIV             "Division
     AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator
     AND  PRPS~PRART  IN  S_PRART           "Type
     AND  PRPS~PSPRI  IN  S_PSPRI           "Priority
     AND  COSS~GJAHR   =  P_FYEAR           "Fiscal Year
*   AND  COSS~VERSN   = '000'             "Version                               TR843
     AND ( COSS~VERSN  =  P_VERSN           "Plan Version                          TR843
      OR  COSS~VERSN   = '000' )            "Actual Version                        TR843
*   AND  COSS~WRTTP  = '04'               "Record with actuals                   TR843
     AND  COSS~WRTTP  IN ('01','04')        "Record with actuals & plans           TR843
     AND  COSS~KSTAR NOT IN EX_KSTAR        "Cost Element
     AND  COSS~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator

  IF ITABLE[] IS INITIAL.
    WRITE: /1 'NO DATA SELECTED'.
    STOP.
  ENDIF.
  DATA: FROM_MONTH(2) TYPE N.
  SORT ITABLE BY PSPHI PRART PSPRI.

  LOOP AT ITABLE.
    IF ITABLE-WRTTP = '01' AND ITABLE-VERSN <> P_VERSN.
    ELSE.
      MOVE-CORRESPONDING ITABLE TO XCLTABL.
      CONCATENATE ITABLE-POSID+0(2) '-' ITABLE-POSID+2(2) '-'
                  ITABLE-POSID+4(3) INTO XCLTABL-PRJCT.
      MOVE ITABLE-POSID+7(4)  TO XCLTABL-WBS.
      MOVE ITABLE-POST1+0(15) TO XCLTABL-WBSTXT.
      IF ITABLE-WRTTP = '04'.                           "Actual
        ADD ITABLE-WKG001 FROM 1 TO S_MONTH-HIGH
                                       GIVING XCLTABL-TOTACTU.
        KOUNT1 = 8.
        KOUNT2 = 11.
        DO S_MONTH-HIGH TIMES.
          KOUNT1 = KOUNT1 + 1.
          KOUNT2 = KOUNT2 + 1.
          ASSIGN COMPONENT KOUNT1 OF STRUCTURE ITABLE  TO <FS1>.
          ASSIGN COMPONENT KOUNT2 OF STRUCTURE XCLTABL TO <FS2>.
          MOVE <FS1> TO <FS2>.
        ENDDO.
      ELSE.                                          "Plan
        FROM_MONTH = S_MONTH-HIGH + 1.
        ADD ITABLE-WKG001 FROM FROM_MONTH TO 12
                                     GIVING XCLTABL-TOTPLAN.
        REPEAT = 12 - S_MONTH-HIGH.
        KOUNT1 = S_MONTH-HIGH + 8.
        KOUNT2 = S_MONTH-HIGH + 11.
        DO REPEAT TIMES.
          KOUNT1 = KOUNT1 + 1.
          KOUNT2 = KOUNT2 + 1.
          ASSIGN COMPONENT KOUNT1 OF STRUCTURE ITABLE  TO <FS1>.
          ASSIGN COMPONENT KOUNT2 OF STRUCTURE XCLTABL TO <FS2>.
          MOVE <FS1> TO <FS2>.
        ENDDO.
      ENDIF.
      XCLTABL-TOTGRND = XCLTABL-TOTACTU + XCLTABL-TOTPLAN.

*Don't add a row if all months individualy have zero amount

      IF   XCLTABL-AMOUNT01 <> 0 OR XCLTABL-AMOUNT02 <> 0
        OR XCLTABL-AMOUNT03 <> 0 OR XCLTABL-AMOUNT04 <> 0
        OR XCLTABL-AMOUNT05 <> 0 OR XCLTABL-AMOUNT06 <> 0
        OR XCLTABL-AMOUNT07 <> 0 OR XCLTABL-AMOUNT08 <> 0
        OR XCLTABL-AMOUNT09 <> 0 OR XCLTABL-AMOUNT10 <> 0
        OR XCLTABL-AMOUNT11 <> 0 OR XCLTABL-AMOUNT12 <> 0.
        IF ITABLE-PSPHI <> LAST_PSPHI.
          MOVE ITABLE-PSPHI TO LAST_PSPHI.
          CLEAR: W_MAJOR, W_PRJTXT, W_ANLKL, W_OHTYPE.
          PERFORM GET_MAJOR_PROJECT_INDICATOR.
          PERFORM GET_PROJECT_DESCRIPTION.
        ENDIF.
        PERFORM GET_WBS_GROUP.
        IF ZPWBS_FLAG = 'N'.
          CONTINUE.
        ENDIF.
        MOVE W_ANLKL   TO XCLTABL-ANLKL.
        CLEAR: W_DATE, W_YEAR, W_MONTH.
        IF W_MAJOR = 'Y'.
          PERFORM GET_AVD_OR_FINISH_DATE.
          IF W_YEAR > P_FYEAR.
            CONCATENATE 'CWIP-' W_OHTYPE INTO XCLTABL-ANLKL.
          ELSE.
            IF W_MONTH > '00'.
              KOUNT3 = W_MONTH + 11.
              ASSIGN COMPONENT KOUNT3 OF STRUCTURE XCLTABL TO <FS5>.
              ADD XCLTABL-AMOUNT01 FROM 1 TO W_MONTH GIVING <FS5>.
              KOUNT3 = 11.
              W_MONTH = W_MONTH - 1.
              DO W_MONTH TIMES.
                KOUNT3 = KOUNT3 + 1.
                ASSIGN COMPONENT KOUNT3 OF STRUCTURE XCLTABL TO <FS5>.
                CLEAR <FS5>.
              ENDDO.
              ADD XCLTABL-AMOUNT01 FROM S_MONTH-LOW TO S_MONTH-HIGH
                                        GIVING XCLTABL-TOTACTU.
              ADD XCLTABL-AMOUNT01 FROM FROM_MONTH TO REPEAT
                                        GIVING XCLTABL-TOTPLAN.
            ENDIF.
          ENDIF.
        ENDIF.
        MOVE W_PRJTXT  TO XCLTABL-PRJTXT.
        MOVE W_MAJOR   TO XCLTABL-MAJOR.
        MOVE W_OHTYPE  TO XCLTABL-OHTYP.
        MOVE W_DATE    TO XCLTABL-BZDAT.
        COLLECT XCLTABL.
      ENDIF.
      CLEAR   XCLTABL.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "GET_FINANCIAL_DATA

*----------------------------------------------------------------------*
*               MAKE_SETUP_FOR_MAJOR_INDICATOR.                        *
*----------------------------------------------------------------------*
FORM MAKE_SETUP_FOR_MAJOR_INDICATOR.
  MOVE 'MAJORPROJECT'         TO  CHARIC.    "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_MP.
  PERFORM BUILD_AUSP.
ENDFORM.                    "MAKE_SETUP_FOR_MAJOR_INDICATOR

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
ENDFORM.                    "GET_ATINN

*---------------------  BUILD_AUSP -------------------------------------
FORM BUILD_AUSP.

  REFRESH CHAR_TAB.
  SELECT * FROM AUSP INTO TABLE CHAR_TAB
   WHERE   ATINN = G_ATINN_MP    AND
           MAFID = 'O'           AND
           KLART = '014'.
  SORT CHAR_TAB BY OBJEK ATINN.
ENDFORM.                    "BUILD_AUSP

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine to get the value of the project control & major indicator
*-----------------------------------------------------------------------
FORM GET_MAJOR_PROJECT_INDICATOR.
  DATA: W_OBJNR LIKE PRPS-OBJNR.
  CLEAR: XCLTABL-MAJOR.

  SELECT SINGLE OBJNR INTO W_OBJNR
    FROM PRPS
   WHERE PSPHI = ITABLE-PSPHI
     AND STUFE = '1'.

  READ TABLE CHAR_TAB WITH KEY OBJEK = W_OBJNR
                               ATINN = G_ATINN_MP  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATWRT+0(6) TO W_MAJOR.
  ENDIF.

ENDFORM.                    "GET_MAJOR_PROJECT_INDICATOR

*----------------------------------------------------------------------*
*               GET_PROJECT_DESCRIPTION.
*----------------------------------------------------------------------*
FORM GET_PROJECT_DESCRIPTION.

  CLEAR: PROJ-POST1, FINISH_DATE.
  SELECT SINGLE POST1 PLSEZ
    INTO (PROJ-POST1, FINISH_DATE)
    FROM PROJ
   WHERE PSPNR = ITABLE-PSPHI.

  IF SY-SUBRC = 0.
    MOVE PROJ-POST1+0(15) TO W_PRJTXT.
  ENDIF.

ENDFORM.                    "GET_PROJECT_DESCRIPTION

*----------------------------------------------------------------------*
*               GET_WBS_GROUP.                                         *
*----------------------------------------------------------------------*
FORM GET_WBS_GROUP.

  DATA: W_WBS(4) TYPE C.
  MOVE ITABLE-POSID+7(4) TO W_WBS.
  CLEAR: W_ANLKL, W_OHTYPE, ZPWBS_FLAG.

  SELECT SINGLE ANLKL OHTYP
    INTO (W_ANLKL, W_OHTYPE)
    FROM ZPWBS
   WHERE WBSEL = W_WBS
     AND PRART = ITABLE-PRART.
  IF SY-SUBRC <> 0.
    MOVE 'N'   TO ZPWBS_FLAG.
    MOVE W_WBS TO ERRTAB-WBS.
    MOVE ITABLE-PRART TO ERRTAB-PRART.
    MOVE TEXT-100 TO     ERRTAB-ERRTXT.
    APPEND ERRTAB.
    CLEAR  ERRTAB.
  ENDIF.
ENDFORM.                    "GET_WBS_GROUP

*----------------------------------------------------------------------*
*               GET_ASSET_VALUE_DATE                                   *
*----------------------------------------------------------------------*
FORM GET_AVD_OR_FINISH_DATE.

  SELECT SINGLE * FROM COBRA           "Asset Valuation Date
    WHERE OBJNR = ITABLE-OBJNR.
  IF SY-SUBRC = '0'.
    IF COBRA-BZDAT <> '00000000'.
      MOVE COBRA-BZDAT TO W_DATE.
    ELSEIF FINISH_DATE <> '00000000'.
      MOVE FINISH_DATE TO W_DATE.
    ENDIF.
    MOVE W_DATE+0(4) TO W_YEAR.
    MOVE W_DATE+4(2) TO W_MONTH.
  ENDIF.


ENDFORM.                    "GET_AVD_OR_FINISH_DATE

*----------------------------------------------------------------------*
*               INSERT_PREVIOUS_YEAR_DATA.                             *
*           Append the data at the end of table xcltabl                *
*----------------------------------------------------------------------*
FORM INSERT_PREVIOUS_YEAR_DATA.
  DATA: W_MONTH TYPE I.
  LOOP AT P_TAB.
    MOVE: P_TAB-ANLKL  TO  XCLTABL-ANLKL,
          P_TAB-OHTYP  TO  XCLTABL-OHTYP,
          P_TAB-WBSEL  TO  XCLTABL-WBS,
          P_TAB-PRVTXT TO  XCLTABL-PRJTXT,
          P_TAB-MAJOR  TO  XCLTABL-MAJOR.
    CONCATENATE P_TAB-PRJCT+0(2) '-' P_TAB-PRJCT+2(2) '-'
            P_TAB-PRJCT+4(3) INTO XCLTABL-PRJCT.
    IF  P_TAB-BZDAT  <>  '00000000'.
      MOVE P_TAB-BZDAT  TO  XCLTABL-BZDAT.
    ENDIF.

*  move O/HEAD default data to XCLTABL

    IF P_TAB-BZDAT = '00000000'.
      MOVE P_TAB-AMOUNT TO  XCLTABL-AMOUNT01.
      MOVE 1            TO  W_MONTH.
    ELSE.
      MOVE P_TAB-BZDAT+4(2) TO W_MONTH.
      W_MONTH = W_MONTH + 11.   "No of columns before amt fields
      ASSIGN COMPONENT W_MONTH OF STRUCTURE XCLTABL TO <FS3>.
      MOVE P_TAB-AMOUNT TO <FS3>.
    ENDIF.
    IF W_MONTH > S_MONTH-HIGH.
      MOVE P_TAB-AMOUNT TO XCLTABL-TOTPLAN.
    ELSE.
      MOVE P_TAB-AMOUNT TO XCLTABL-TOTACTU.
    ENDIF.
    MOVE P_TAB-AMOUNT    TO XCLTABL-TOTGRND.
    APPEND XCLTABL.
    CLEAR  XCLTABL.
  ENDLOOP.

  SORT XCLTABL BY ANLKL WBS OHTYP.

ENDFORM.                    "INSERT_PREVIOUS_YEAR_DATA

*----------------------------------------------------------------------*
*                     CALCULATE_OVERHEAD                               *
*----------------------------------------------------------------------*
FORM CALCULATE_OVERHEAD.

  MOVE P_GOHED TO W_GPRCNT.
  MOVE P_DOHED TO W_DPRCNT.
  LOOP AT XCLTABL.
    MOVE-CORRESPONDING XCLTABL TO: REPTAB, WA.
    APPEND REPTAB.
    AT END OF ANLKL.
      SUM.
      IF REPTAB-ANLKL = 'OHEAD' OR WA-OHTYP = 'NON'.
        PERFORM DO_NOTHING.
      ELSE.
        MOVE-CORRESPONDING XCLTABL TO REPTAB.
        MOVE: P_DSTPRJ     TO  REPTAB-PRJCT,
              P_DPRART     TO  REPTAB-PRART,
              P_DPSPRI     TO  REPTAB-PSPRI,
              SPACE        TO  REPTAB-MAJOR.
        KOUNT2 = 11.
        DO 15 TIMES.
          KOUNT2 = KOUNT2 + 1.
          ASSIGN COMPONENT KOUNT2 OF STRUCTURE XCLTABL TO <FS3>.
          ASSIGN COMPONENT KOUNT2 OF STRUCTURE REPTAB  TO <FS4>.
          IF <FS3> <> 0.
            IF WA-OHTYP = 'GSO'.
              MOVE P_WBSGSO   TO  REPTAB-WBS.
              CONCATENATE TEXT-120 W_GPRCNT INTO REPTAB-PRJTXT.
              <FS4> = <FS3> * P_GOHED / 100.
            ELSEIF WA-OHTYP = 'DST'.
              MOVE P_WBSDST   TO  REPTAB-WBS.
              CONCATENATE TEXT-120 W_DPRCNT INTO REPTAB-PRJTXT.
              <FS4> = <FS3> * P_DOHED / 100.
            ELSE.
              <FS4> = <FS3>.
            ENDIF.
          ENDIF.
        ENDDO.

        MOVE 'R' TO ALL_DOLLARS-OHSOURCE.
        ALL_DOLLARS-AMOUNT01 = REPTAB-AMOUNT01 * -1.
        ALL_DOLLARS-AMOUNT02 = REPTAB-AMOUNT02 * -1.
        ALL_DOLLARS-AMOUNT03 = REPTAB-AMOUNT03 * -1.
        ALL_DOLLARS-AMOUNT04 = REPTAB-AMOUNT04 * -1.
        ALL_DOLLARS-AMOUNT05 = REPTAB-AMOUNT05 * -1.
        ALL_DOLLARS-AMOUNT06 = REPTAB-AMOUNT06 * -1.
        ALL_DOLLARS-AMOUNT07 = REPTAB-AMOUNT07 * -1.
        ALL_DOLLARS-AMOUNT08 = REPTAB-AMOUNT08 * -1.
        ALL_DOLLARS-AMOUNT09 = REPTAB-AMOUNT09 * -1.
        ALL_DOLLARS-AMOUNT10 = REPTAB-AMOUNT10 * -1.
        ALL_DOLLARS-AMOUNT11 = REPTAB-AMOUNT11 * -1.
        ALL_DOLLARS-AMOUNT12 = REPTAB-AMOUNT12 * -1.
        ALL_DOLLARS-TOTPLAN  = REPTAB-TOTPLAN  * -1.
        ALL_DOLLARS-TOTACTU  = REPTAB-TOTACTU  * -1.
        ALL_DOLLARS-TOTGRND  = REPTAB-TOTGRND  * -1.
        COLLECT ALL_DOLLARS.
        APPEND REPTAB.
      ENDIF.
    ENDAT.
  ENDLOOP.
  CLEAR: REPTAB, ALL_DOLLARS.
ENDFORM.                    "CALCULATE_OVERHEAD
*----------------------------------------------------------------------*
*                     VARIABLE_COLUMN_HEADING                          *
*----------------------------------------------------------------------*
FORM VARIABLE_COLUMN_HEADING.
  DATA: W_VARIABLE(5).
  MOVE 'Act-' TO W_VARIABLE.
  KOUNT1 = 0.
  DO S_MONTH-HIGH TIMES.
    KOUNT1 = KOUNT1 + 1.
    ASSIGN COMPONENT KOUNT1 OF STRUCTURE MONTH   TO <FS3>.
    ASSIGN COMPONENT KOUNT1 OF STRUCTURE COL TO <FS4>.
    CONCATENATE W_VARIABLE <FS3> INTO <FS4>.
  ENDDO.
  MOVE 'Plan-' TO W_VARIABLE.
  REPEAT = 12 - S_MONTH-HIGH.
  DO REPEAT TIMES.
    KOUNT1 = KOUNT1 + 1.
    ASSIGN COMPONENT KOUNT1 OF STRUCTURE MONTH   TO <FS3>.
    ASSIGN COMPONENT KOUNT1 OF STRUCTURE COL TO <FS4>.
    CONCATENATE W_VARIABLE <FS3> INTO <FS4>.
  ENDDO.
  CLEAR KOUNT1.
ENDFORM.                    "VARIABLE_COLUMN_HEADING
*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_NAME
*&---------------------------------------------------------------------*
FORM GET_COMPANY_NAME.

  SELECT SINGLE * FROM T001                "Company Code
    WHERE BUKRS = P_CCODE.


ENDFORM.                    " GET_COMPANY_NAME

*----------------------------------------------------------------------*
*                        PRINT_ERROR_REPORT                            *
*----------------------------------------------------------------------*
FORM PRINT_ERROR_REPORT.
  IF NOT ERRTAB[] IS INITIAL.
    WRITE: /15 TEXT-121.
    WRITE: /15 TEXT-117.
    WRITE: /15 TEXT-102, 23 TEXT-122.
    LOOP AT ERRTAB.
      WRITE: /15 ERRTAB-WBS, 23 ERRTAB-PRART, 35 ERRTAB-ERRTXT.
    ENDLOOP.
    STOP.
    SKIP 2.
  ENDIF.
ENDFORM.                    "PRINT_ERROR_REPORT
*----------------------------------------------------------------------*
*                        PRINT_DETAIL_REPORT                           *
*----------------------------------------------------------------------*
FORM PRINT_DETAIL_REPORT.


  CLEAR: WRK_WBSTXT, WRK_ANLKL, WRK_WBS.

  LOOP AT REPTAB.

    AT NEW ANLKL.
      RESERVE 4 LINES.
      WRITE: /1 TEXT-116, REPTAB-ANLKL.
      WRITE: /1 TEXT-117.
    ENDAT.
    RESERVE 2 LINES.
    WRITE: /1 REPTAB-WBS, REPTAB-PRJCT UNDER TEXT-103,
           REPTAB-PRJTXT UNDER TEXT-104, REPTAB-PRART UNDER TEXT-105,
           REPTAB-PSPRI UNDER TEXT-106.
    IF REPTAB-BZDAT <> '00000000' AND REPTAB-BZDAT <> '********'.
      WRITE  REPTAB-BZDAT UNDER TEXT-107.
    ENDIF.
    WRITE REPTAB-MAJOR UNDER TEXT-108.
    MOVE REPTAB-WBSTXT TO WRK_WBSTXT.
    MOVE REPTAB-ANLKL  TO WRK_ANLKL.
    MOVE REPTAB-WBS    TO WRK_WBS.
    PERFORM WRITE_DOLLARS_01.

    AT END OF WBS.
      SUM.
      IF WRK_WBS = P_WBSDST OR WRK_WBS = P_WBSGSO
                               OR WRK_ANLKL   = 'OHEAD'.
      ELSE.
        WRITE: /1 TEXT-119, WRK_WBSTXT.
        PERFORM WRITE_DOLLARS_01.
        SKIP 1.
      ENDIF.
    ENDAT.

    AT END OF ANLKL.
      SUM.
      ULINE.
      WRITE: /1 TEXT-118, WRK_ANLKL.
      PERFORM WRITE_DOLLARS_01.
      IF REPTAB-ANLKL = 'OHEAD'.
        MOVE-CORRESPONDING REPTAB TO ALL_DOLLARS.
        MOVE 'O' TO ALL_DOLLARS-OHSOURCE.
        COLLECT ALL_DOLLARS.
      ENDIF.
      ULINE.
      SKIP 1.
    ENDAT.

    AT LAST.
      PERFORM WRITE_OVER_HEADS.
      SUM.
      ULINE.
      WRITE: /1 TEXT-123.
      PERFORM CALCULATE_COMPANY_TOTAL.
      PERFORM WRITE_DOLLARS_01.
      ULINE.
      SKIP 1.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "PRINT_DETAIL_REPORT

*----------------------------------------------------------------------*
*               PRINT_SUMMARY_REPORT                                   *
*----------------------------------------------------------------------*
FORM PRINT_SUMMARY_REPORT.

  CLEAR: WRK_WBSTXT, WRK_ANLKL, WRK_WBS.

  LOOP AT REPTAB.

    MOVE REPTAB-WBSTXT TO WRK_WBSTXT.
    MOVE REPTAB-ANLKL  TO WRK_ANLKL.
    MOVE REPTAB-WBS    TO WRK_WBS.

    AT END OF ANLKL.
      SUM.
      SKIP 1.
      WRITE: /1 TEXT-118, WRK_ANLKL.
      PERFORM WRITE_DOLLARS_01.
      IF REPTAB-ANLKL = 'OHEAD'.
        MOVE-CORRESPONDING REPTAB TO ALL_DOLLARS.
        MOVE 'O' TO ALL_DOLLARS-OHSOURCE.
        COLLECT ALL_DOLLARS.
      ENDIF.
      ULINE.
    ENDAT.

    AT LAST.
      PERFORM WRITE_OVER_HEADS.
      SUM.
      ULINE.
      PERFORM CALCULATE_COMPANY_TOTAL.
      WRITE: /1 TEXT-123.
      PERFORM WRITE_DOLLARS_01.
      ULINE.
      SKIP 1.
    ENDAT.

  ENDLOOP.

ENDFORM.                    "PRINT_SUMMARY_REPORT


*----------------------------------------------------------------------*
*                   CALCULATE_COMPANY_TOTAL                            *
*----------------------------------------------------------------------*
FORM CALCULATE_COMPANY_TOTAL.
  REPTAB-AMOUNT01 = REPTAB-AMOUNT01 + ALL_DOLLARS-AMOUNT01.
  REPTAB-AMOUNT02 = REPTAB-AMOUNT02 + ALL_DOLLARS-AMOUNT02.
  REPTAB-AMOUNT03 = REPTAB-AMOUNT03 + ALL_DOLLARS-AMOUNT03.
  REPTAB-AMOUNT04 = REPTAB-AMOUNT04 + ALL_DOLLARS-AMOUNT04.
  REPTAB-AMOUNT05 = REPTAB-AMOUNT05 + ALL_DOLLARS-AMOUNT05.
  REPTAB-AMOUNT06 = REPTAB-AMOUNT06 + ALL_DOLLARS-AMOUNT06.
  REPTAB-AMOUNT07 = REPTAB-AMOUNT07 + ALL_DOLLARS-AMOUNT07.
  REPTAB-AMOUNT08 = REPTAB-AMOUNT08 + ALL_DOLLARS-AMOUNT08.
  REPTAB-AMOUNT09 = REPTAB-AMOUNT09 + ALL_DOLLARS-AMOUNT09.
  REPTAB-AMOUNT10 = REPTAB-AMOUNT10 + ALL_DOLLARS-AMOUNT10.
  REPTAB-AMOUNT11 = REPTAB-AMOUNT11 + ALL_DOLLARS-AMOUNT11.
  REPTAB-AMOUNT12 = REPTAB-AMOUNT12 + ALL_DOLLARS-AMOUNT12.
  REPTAB-TOTPLAN  = REPTAB-TOTPLAN  + ALL_DOLLARS-TOTPLAN.
  REPTAB-TOTACTU  = REPTAB-TOTACTU  + ALL_DOLLARS-TOTACTU.
  REPTAB-TOTGRND  = REPTAB-TOTGRND  + ALL_DOLLARS-TOTGRND.
ENDFORM.                    "CALCULATE_COMPANY_TOTAL
*----------------------------------------------------------------------*
*                   WRITE_OVER_HEADS                                   *
*----------------------------------------------------------------------*
FORM WRITE_OVER_HEADS.
  SORT ALL_DOLLARS BY OHSOURCE.
  LOOP AT ALL_DOLLARS.
    IF ALL_DOLLARS-OHSOURCE = 'O'.
    ELSE.
      WRITE: /1 TEXT-124.
      PERFORM WRITE_DOLLARS_02.
    ENDIF.

    AT LAST.
      SUM.
      ULINE.
      WRITE: /1 TEXT-125.
      PERFORM WRITE_DOLLARS_02.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "WRITE_OVER_HEADS

*----------------------------------------------------------------------*
*                   WRITE_DOLLARS_01                                   *
*----------------------------------------------------------------------*
FORM WRITE_DOLLARS_01.
  WRITE: 57(11) REPTAB-AMOUNT01,  69(11) REPTAB-AMOUNT02,
         80(11) REPTAB-AMOUNT03,  92(11) REPTAB-AMOUNT04,
        104(11) REPTAB-AMOUNT05, 116(11) REPTAB-AMOUNT06,
        127(11) REPTAB-AMOUNT07, 138(11) REPTAB-AMOUNT08,
        149(11) REPTAB-AMOUNT09, 160(11) REPTAB-AMOUNT10,
        171(11) REPTAB-AMOUNT11, 182(11) REPTAB-AMOUNT12,
        195(12) REPTAB-TOTPLAN,  209(12) REPTAB-TOTACTU,
        223(12) REPTAB-TOTGRND.

ENDFORM.                    "WRITE_DOLLARS_01

*----------------------------------------------------------------------*
*                   WRITE_DOLLARS_02                                   *
*----------------------------------------------------------------------*
FORM WRITE_DOLLARS_02.
  WRITE: 57(11) ALL_DOLLARS-AMOUNT01,  69(11) ALL_DOLLARS-AMOUNT02,
         80(11) ALL_DOLLARS-AMOUNT03,  92(11) ALL_DOLLARS-AMOUNT04,
        104(11) ALL_DOLLARS-AMOUNT05, 116(11) ALL_DOLLARS-AMOUNT06,
        127(11) ALL_DOLLARS-AMOUNT07, 138(11) ALL_DOLLARS-AMOUNT08,
        149(11) ALL_DOLLARS-AMOUNT09, 160(11) ALL_DOLLARS-AMOUNT10,
        171(11) ALL_DOLLARS-AMOUNT11, 182(11) ALL_DOLLARS-AMOUNT12,
        195(12) ALL_DOLLARS-TOTPLAN,  209(12) ALL_DOLLARS-TOTACTU,
        223(12) ALL_DOLLARS-TOTGRND.

ENDFORM.                    "WRITE_DOLLARS_02

*----------------------------------------------------------------------*
*                          TOP-OF-PAGE                                 *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID,  105 T001-BUTXT,             "Company
        200 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
         80 TEXT-HDR, 200  TEXT-PGE, SY-PAGNO.
  IF B_SRPRT = 'X'.
    WRITE: /93 TEXT-007, 100 TEXT-101, P_FYEAR, TEXT-011, P_VERSN.
  ELSE.
    WRITE: /97 TEXT-101, P_FYEAR, TEXT-011, P_VERSN.
  ENDIF.
  ULINE.
  WRITE: /1 TEXT-102, 6 TEXT-103, 17 TEXT-104, 34 TEXT-105,
         38 TEXT-106, 41 TEXT-107, 54 TEXT-108,
         60 COL-VTEXT101, 72 COL-VTEXT102, 83 COL-VTEXT103,
         94 COL-VTEXT104, 106 COL-VTEXT105, 118 COL-VTEXT106,
         129 COL-VTEXT107, 140 COL-VTEXT108, 151 COL-VTEXT109,
         162 COL-VTEXT110, 173 COL-VTEXT111, 184 COL-VTEXT112,
         198 TEXT-112,     210 TEXT-114,     225 TEXT-115.
  ULINE.

*&---------------------------------------------------------------------*
*&      Form  DO_NOTHING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DO_NOTHING.
ENDFORM.                    "DO_NOTHING
*----------------------------------------------------------------------*
*               DISPLAY_EXCEL_SHEET.                                   *
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
  LT_FNAMES-TEXT = TEXT-C06.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = TEXT-C07.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = TEXT-C08.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = TEXT-C09.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = TEXT-C10.
  APPEND LT_FNAMES.

  LT_FNAMES-TEXT = COL-VTEXT101.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT102.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT103.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT104.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT105.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT106.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT107.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT108.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT109.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT110.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT111.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = COL-VTEXT112.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = TEXT-112.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = TEXT-114.
  APPEND LT_FNAMES.
  LT_FNAMES-TEXT = TEXT-115.
  APPEND LT_FNAMES.


  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
            FILE_NAME                 = 'C:\SAPTEMP'
            CREATE_PIVOT              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       TABLES
*           PIVOT_FIELD_TAB           =
            DATA_TAB                  = XCLTABL
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

ENDFORM.                    "DISPLAY_EXCEL_SHEET
*======================================================================*

* Subroutine to upload data from excel file from local PC              *

*----------------------------------------------------------------------*

*  --->  P_TAB        Internal table for data to be uploaded           *

*  --->  P_FILENAME   File to be uploaded                              *

*  <---  P_RC         Return Code                                      *

*======================================================================*
FORM F_EXCEL_UPLOAD.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = FNAME
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
      WRITE: / 'SY-SUBRC = ', SY-SUBRC, ' CWIP EXCEL SHEET NOT FOUND '.
    ELSE.
      WRITE: / 'SY-SUBRC = ', SY-SUBRC, ' UNSUCCESSFUL EXCEL CALL '.
    ENDIF.
    SKIP 2.
  ENDIF.

**************** Code for testing, please don't remove *********
** LOOP  AT EXCELTAB.
**       WRITE /1 EXCELTAB.
** ENDLOOP.
**************** End of testing code                   *********

*
* CLEAR LAST_COLUMN.

  LOOP AT EXCELTAB.

    IF EXCELTAB-COL = 1.
      CONCATENATE '000' EXCELTAB-VALUE INTO P_TAB-ANLKL.
    ELSEIF EXCELTAB-COL = 2.
      MOVE EXCELTAB-VALUE TO P_TAB-OHTYP.
    ELSEIF EXCELTAB-COL = 3.
      MOVE EXCELTAB-VALUE TO P_TAB-WBSEL.
    ELSEIF EXCELTAB-COL = 4.
      MOVE EXCELTAB-VALUE TO P_TAB-PRVTXT.
    ELSEIF EXCELTAB-COL = 5.
      CONCATENATE EXCELTAB-VALUE+0(4) EXCELTAB-VALUE+5(2)
                  EXCELTAB-VALUE+8(2) INTO P_TAB-BZDAT.
    ELSEIF EXCELTAB-COL = 6.
      MOVE EXCELTAB-VALUE TO P_TAB-MAJOR.
    ELSEIF EXCELTAB-COL = 7.
      MOVE EXCELTAB-VALUE TO P_TAB-AMOUNT.
    ELSEIF EXCELTAB-COL = 8.
      MOVE EXCELTAB-VALUE TO P_TAB-PRJCT.
    ENDIF.
    AT END OF ROW.
      IF P_TAB-OHTYP = SPACE.
        MOVE 'NON' TO P_TAB-OHTYP.
      ENDIF.
      APPEND P_TAB.
      CLEAR  P_TAB.
    ENDAT.
  ENDLOOP.

*LOOP  AT P_TAB.
*    WRITE /1 P_TAB.
*ENDLOOP.
* STOP.
ENDFORM.                    "F_EXCEL_UPLOAD

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
  DATA: SEP_FILE TYPE STRING,
        SEP_PATH TYPE STRING.
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      FULL_NAME     = FNAME
    IMPORTING
      STRIPPED_NAME = SEP_FILE
      FILE_PATH     = SEP_PATH
    EXCEPTIONS
      X_ERROR       = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LV_DIR = SEP_PATH.
  IF LV_DIR+0(1) = 'C' OR LV_DIR+0(1) = 'c'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
  ELSE.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
      EXPORTING
        DIRECTORY            = LV_DIR
      RECEIVING
        RESULT               = LV_BOL
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        WRONG_PARAMETER      = 3
        NOT_SUPPORTED_BY_GUI = 4
        OTHERS               = 5.
    IF LV_BOL IS INITIAL.
*   CONCATENATE TEXT-099 sep_path into w_error.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH TEXT-099 SEP_PATH.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
