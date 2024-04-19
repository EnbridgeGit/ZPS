REPORT ZPPMR015 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 255
                MESSAGE-ID ZP.
************************************************************************
*  Author:      Nancy Gilligan, OmniLogic -  KEY D30K906527
*  Date:        November 1998
*  Description:
*     - This report lists costs by primary cost elements/wbs elements
*       with its associated activity.
*
************************************************************************
************************************************************************

TABLES: COSP, COSS, PROJ, PRPS, T001, TCJ1T, TCN7T.


DATA:
    begin of TABLE1 occurs 10000,
       BUKRS         LIKE T001-BUKRS,           "COMPANY CODE
       POSID(4)      TYPE C,                    "WBS ELEMENT
       PRART         LIKE PRPS-PRART,          "PROJECT TYPE
       PSPRI         LIKE PRPS-PSPRI,           "PRIORITY CODE
       VERNA         LIKE PROJ-VERNA,           "DIVISION
       PSPID         LIKE PROJ-PSPID,           "PROJECT
       WBSPOST1      LIKE PRPS-POST1,           "WBS DESCRIPTION
       KSTAR1        LIKE COSP-WKG001,          "PIPES
       KSTAR2        LIKE COSP-WKG001,          "MATERIAL
       KSTAR3        LIKE COSP-WKG001,          "LABOUR
       KSTAR4        LIKE COSP-WKG001,          "OTHER
       TOTAL_COST    LIKE COSP-WKG001,          "Total Costs
       POST1         LIKE PROJ-POST1,           "PROJECT DESCRIPTION
       LENGTH(6)     TYPE P DECIMALS 2,  "like ??,
       UNIT(6)       TYPE P DECIMALS 2,  "like ??,
   end of TABLE1.
*
*
DATA:   VALUE         LIKE COSP-WKG001 VALUE 0,
        KSTAR         LIKE COSP-KSTAR,
        P_BEGNME(9),
        P_ENDNME(9) VALUE SPACE,
        BEGMTH(2)     TYPE C,
        ENDMTH(2)     TYPE C,
        WBSPOST1      LIKE PRPS-POST1,           "WBS DESCRIPTION
        KSTAR1        LIKE COSP-WKG001,            "PIPE
        KSTAR2        LIKE COSP-WKG001,            "MATERIAL
        KSTAR3        LIKE COSP-WKG001,            "LABOUR
        KSTAR4        LIKE COSP-WKG001,            "OTHER
        TOTAL_COST    LIKE COSP-WKG001,
        LENGTH(6)     TYPE P DECIMALS 2,  "like ??,
        UNIT(6)       TYPE P DECIMALS 2,  "like ??,
        AVE_PIPE(6)   TYPE P DECIMALS 2,
        AVE_MATS(6)   TYPE P DECIMALS 2,
        AVE_LAB(6)    TYPE P DECIMALS 2,
        AVE_OTHR(6)   TYPE P DECIMALS 2,
        AVE_COST(6)   TYPE P DECIMALS 2.

DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_LENGTH    LIKE CABN-ATINN,       " Length Char
       G_ATINN_UNIT      LIKE CABN-ATINN,       " Unit Char
       G_ATINN_WORK      LIKE CABN-ATINN,       " Unit Char
       WORK(10),
       WORK_SOURCE(10).

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

DATA: BEGIN OF MTH_TAB OCCURS 12.
        INCLUDE STRUCTURE T247.
DATA: END OF MTH_TAB.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: S_VBUKR FOR PROJ-VBUKR,                 "COMPANY CODE
                S_VERNR FOR PRPS-VERNR,                  "DIVISION
                S_PSPID FOR PROJ-PSPID.                  "PROJECT
SELECT-OPTIONS: S_PRART FOR PRPS-PRART,                   "PROJECT TYPE
                S_PSPRI FOR PRPS-PSPRI.                   "PRIORITY CODE
PARAMETERS:     P_GJAHR LIKE COSS-GJAHR OBLIGATORY.        "WBS elements
SELECT-OPTIONS: SMTH FOR SY-DATUM+4(2) OBLIGATORY         "Months
                      DEFAULT SY-DATUM+4(2) TO '12',
                S_POSID FOR PRPS-POSID+7(4).              "WBS elements
SELECTION-SCREEN END OF BLOCK BOX.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_CONT RADIOBUTTON GROUP RBCR,            "WORK SOURCE
                P_COMP RADIOBUTTON GROUP RBCR,            "WORK SOURCE
                P_ALL  RADIOBUTTON GROUP RBCR.            "WORK SOURCE
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_KSTAR1 FOR COSP-KSTAR,         "PIPE cost elements
                S_KSTAR2 FOR COSP-KSTAR,         "MATERIAL cost elements
                S_KSTAR3 FOR COSP-KSTAR.         "LABOUR cost elements
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETER: P_CHKWBS AS CHECKBOX,
           P_CHKPRJ AS CHECKBOX,
           P_CHKDET AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX3.

PARAMETERS:     P_NOSHOW AS CHECKBOX DEFAULT 'X'.

*------------------------  AT SELECTION-SCREEN  ------------------------

AT SELECTION-SCREEN.
  IF ( P_CHKWBS IS INITIAL ) AND
     ( P_CHKPRJ IS INITIAL ) AND
     ( P_CHKDET IS INITIAL ).
     MESSAGE E100 WITH 'Must select at least one report'.
  ENDIF.

AT SELECTION-SCREEN ON S_VBUKR.
 SELECT SINGLE BUKRS BUTXT FROM T001
     INTO (T001-BUKRS, T001-BUTXT)                     "Get Company Name
    WHERE BUKRS IN S_VBUKR.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP RBCR.
  IF P_CONT EQ 'X'.
    WORK_SOURCE = 'CONTRACTOR'.
  ENDIF.
  IF P_COMP EQ 'X'.
    WORK_SOURCE = 'COMPANY'.
  ENDIF.

AT SELECTION-SCREEN ON SMTH.
 CALL FUNCTION 'MONTH_NAMES_GET'                       "Month Name table
    EXPORTING
              LANGUAGE = SY-LANGU
    TABLES    MONTH_NAMES = MTH_TAB.

 READ TABLE MTH_TAB INDEX SMTH+3(2).
 P_BEGNME = MTH_TAB-LTX.
 BEGMTH = SMTH+3(2).

 P_ENDNME = SPACE.
 IF SMTH+5(2) <> '00'.
    ENDMTH = SMTH+5(2).
    READ TABLE MTH_TAB INDEX SMTH+5(2).
    P_ENDNME = MTH_TAB-LTX.
 ELSE.
    ENDMTH = BEGMTH.
 ENDIF.
*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.

  CHARIC = 'ACTUAL_LENGTH'.
  PERFORM GET_ATINN.
  G_ATINN_LENGTH =  G_ATINN.

  CHARIC = 'ACTUAL_UNITS'.
  PERFORM GET_ATINN.
  G_ATINN_UNIT = G_ATINN.

  CHARIC = 'WORK_SOURCE'.
  PERFORM GET_ATINN.
  G_ATINN_WORK = G_ATINN.

 SELECT * FROM PROJ
    WHERE PSPID IN S_PSPID                                 "project
      AND VERNR IN S_VERNR                                 "division
      AND VBUKR IN S_VBUKR.                                "company code

 IF PROJ-PSPID+5(2) CO '1234567890'.               "Eliminates templates
   SELECT * FROM PRPS
     WHERE PSPHI = PROJ-PSPNR
       AND PRART IN S_PRART
       AND PSPRI IN S_PSPRI
*       and posid in s_posid
       AND PRART IN S_PRART                        "Project type
       AND PSPRI IN S_PSPRI                        "Priority type
       AND PKOKR = '10'                            "Controlling area
       AND VERNR = PROJ-VERNR                      "Division
       AND LOEVM <> 'X'                            "Not flagged deleted
       AND BELKZ = 'X'.                          "Lowest level - actuals

  IF PRPS-POSID+7(4) IN S_POSID.
     OBJECT = PRPS-OBJNR.
     PERFORM GET_CHARIC_VALUES.
*----------------------- marylou ---------------------------------------
     IF LENGTH NE SPACE OR UNIT NE SPACE OR P_NOSHOW EQ SPACE.
        IF WORK_SOURCE = WORK OR P_ALL = 'X'.
           CLEAR: KSTAR1, KSTAR2, KSTAR3, KSTAR4.
           PERFORM GET_FISCAL_DATA.
           PERFORM GET_DESCRIPTIONS.

           IF P_NOSHOW = ' '.
              PERFORM BUILD_TABLE.
           ELSEIF KSTAR1 <> 0 OR
                  KSTAR2 <> 0 OR
                  KSTAR3 <> 0 OR
                  KSTAR4 <> 0.
                 PERFORM BUILD_TABLE.
          ENDIF.
     ENDIF.
  ENDIF.
  ENDIF.
   ENDSELECT.
 ENDIF.
 ENDSELECT.

 IF SY-SUBRC EQ 0.
    PERFORM DISPLAY_TABLE.
 ENDIF.


*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
 FORMAT INTENSIFIED OFF.
 WRITE: /1 TEXT-RPT, SY-REPID,                           "Report Id
       107 T001-BUTXT,                                   "Company Name
       220 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.       "Date/Time
 WRITE: /  TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,  "Client
       100 TEXT-TTL,                                     "Report Title
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.            "Page Number
 WRITE: / TEXT-VRS UNDER TEXT-RPT, '0',                  "Version
       107 TEXT-TT1, P_GJAHR.                            "Fiscal Year
 IF P_ENDNME = SPACE.                                    "Time Frame
    WRITE: /107 TEXT-TT3, P_BEGNME.
 ELSE.
    WRITE: /100 TEXT-TT2, P_BEGNME, TEXT-010, P_ENDNME.  "Periods
 ENDIF.
 FORMAT INTENSIFIED ON.
 PERFORM PRINT_HEADINGS.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

SORT TABLE1 BY BUKRS POSID PRART PSPRI VERNA PSPID.
LOOP AT TABLE1.
 WBSPOST1 = TABLE1-WBSPOST1.

AT NEW POSID.
   PERFORM PRINT_VERT.
  WRITE:   TABLE1-POSID UNDER TEXT-011, WBSPOST1(30).
ENDAT.

AT NEW PRART.
    SELECT SINGLE PRATX FROM TCJ1T INTO TCJ1T-PRATX
                                  WHERE PRART = TABLE1-PRART
                                    AND LANGU = SY-LANGU.
   PERFORM PRINT_VERT.
   WRITE: '  ' UNDER TEXT-011 NO-GAP, TCJ1T-PRATX(30).
ENDAT.

AT NEW PSPRI.
   SELECT SINGLE KTEXT FROM TCN7T  INTO TCN7T-KTEXT
                                 WHERE NPRIO = TABLE1-PSPRI
                                   AND LANGU = SY-LANGU.
   PERFORM PRINT_VERT.
   WRITE:  '    ' UNDER TEXT-011 NO-GAP, TCN7T-KTEXT(30).
ENDAT.

AT NEW VERNA.
   PERFORM PRINT_VERT.
  WRITE:  '      ' UNDER TEXT-011 NO-GAP, TABLE1-VERNA.
ENDAT.

* PRINT THE DATA IN THE TABLE.
   PERFORM CALC_TABLE.
   PERFORM PRINT_VERT.                                          "PROJECT
   WRITE: '        ' UNDER TEXT-011 NO-GAP, TABLE1-PSPID,
           TABLE1-KSTAR1      UNDER TEXT-012,
           TABLE1-KSTAR2      UNDER TEXT-013,
           TABLE1-KSTAR3      UNDER TEXT-014,
           TABLE1-KSTAR4      UNDER TEXT-015,
           TABLE1-TOTAL_COST  UNDER TEXT-016,
           TABLE1-LENGTH      UNDER TEXT-019,
           TABLE1-UNIT        UNDER TEXT-020,
           AVE_PIPE           UNDER TEXT-112,
           AVE_MATS           UNDER TEXT-113,
           AVE_LAB            UNDER TEXT-114,
           AVE_OTHR           UNDER TEXT-115,
           AVE_COST           UNDER TEXT-022.

AT END OF VERNA.
   SUM.
   PERFORM PRINT_VERT.
   WRITE: '      ' UNDER TEXT-011 NO-GAP, TEXT-100, TABLE1-VERNA.
   PERFORM WRITE_TOTALS.
ENDAT.

AT END OF PSPRI.
   SUM.
   PERFORM PRINT_VERT.
   WRITE: '    ' UNDER TEXT-011 NO-GAP, TEXT-100, TCN7T-KTEXT(30).
  PERFORM WRITE_TOTALS.
ENDAT.

AT END OF PRART.
   SUM.
   PERFORM PRINT_VERT.
   WRITE: '  ' UNDER TEXT-011 NO-GAP, TEXT-100, TCJ1T-PRATX(30).
  PERFORM WRITE_TOTALS.
ENDAT.

AT END OF POSID.
   SUM.
   PERFORM PRINT_VERT.
  WRITE:   TEXT-100  UNDER TEXT-011, TABLE1-POSID, WBSPOST1(25).
  PERFORM WRITE_TOTALS.
  ULINE.
ENDAT.

AT END OF BUKRS.
   SUM.
   PERFORM PRINT_VERT.
  WRITE:  TEXT-101  UNDER TEXT-011.
  PERFORM WRITE_TOTALS.
ENDAT.



ENDLOOP.
ENDFORM.

*----------------------- PRINT_HEADINGS -------------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
   FORMAT COLOR 2 ON.
   WRITE: /1(255) SY-ULINE.
   PERFORM PRINT_VERT.
* 1st line
  WRITE:  132 TEXT-016,                        "total cost
          155  TEXT-018,                        "activity - lenghts
          189  TEXT-021,                        "average cost - element
          242  TEXT-022.                        "average cost
* 2nd line
   PERFORM PRINT_VERT.
   WRITE:  2 TEXT-011,                        "description
           44  TEXT-012,                        "pipe
           66  TEXT-013,                        "material
           88  TEXT-014,                        "labour
          110  TEXT-015,                        "other
               TEXT-017 UNDER TEXT-016,         "total cost
          154  TEXT-019,                        "lengths
          169  TEXT-020,                        "units
          184  TEXT-112,                        "pipe
          199  TEXT-113,                        "material
          214  TEXT-114,                        "labour
          228  TEXT-115,                        "other
               TEXT-017 UNDER TEXT-022.         "average cost
   PERFORM PRINT_VERT.
   WRITE: /1(255) SY-ULINE.
   PERFORM PRINT_VERT.
   FORMAT COLOR 2 OFF.
ENDFORM.

*--------------------------  PRINT_VERT  -------------------------------
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:  /0 SY-VLINE,  43 SY-VLINE,
          65 SY-VLINE, 87 SY-VLINE, 109 SY-VLINE, 131 SY-VLINE,
         153 SY-VLINE, 167 SY-VLINE, 182 SY-VLINE, 197 SY-VLINE,
         212 SY-VLINE, 227 SY-VLINE, 241 SY-VLINE, 255 SY-VLINE.
ENDFORM.

************************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_DESCRIPTIONS
*&---------------------------------------------------------------------*
FORM GET_DESCRIPTIONS.

* get project type

* get priority code

ENDFORM.                    " GET_DESCRIPTIONS

*&---------------------------------------------------------------------*
*&      Form  CALC_TABLE
*&---------------------------------------------------------------------*
FORM CALC_TABLE.

CLEAR: AVE_PIPE, AVE_MATS, AVE_LAB, AVE_OTHR.
IF TABLE1-LENGTH <> 0.
    AVE_PIPE =  TABLE1-KSTAR1 / TABLE1-LENGTH.
    AVE_MATS =  TABLE1-KSTAR2 / TABLE1-LENGTH.
    AVE_LAB  =  TABLE1-KSTAR3 / TABLE1-LENGTH.
    AVE_OTHR =  TABLE1-KSTAR4 / TABLE1-LENGTH.
ELSEIF TABLE1-UNIT <> 0.
    AVE_PIPE =  TABLE1-KSTAR1 / TABLE1-UNIT.
    AVE_MATS =  TABLE1-KSTAR2 / TABLE1-UNIT.
    AVE_LAB  =  TABLE1-KSTAR3 / TABLE1-UNIT.
    AVE_OTHR =  TABLE1-KSTAR4 / TABLE1-UNIT.
ENDIF.

AVE_COST = AVE_PIPE + AVE_MATS + AVE_LAB + AVE_OTHR.
ENDFORM.                                                   " CALC_TABLE
*&---------------------------------------------------------------------*
*&      Form  WRITE_TOTALS
*&---------------------------------------------------------------------*
FORM WRITE_TOTALS.

  WRITE:   TABLE1-KSTAR1 UNDER TEXT-012,
           TABLE1-KSTAR2 UNDER TEXT-013,
           TABLE1-KSTAR3 UNDER TEXT-014,
           TABLE1-KSTAR4 UNDER TEXT-015,
           TABLE1-TOTAL_COST UNDER TEXT-016.

ENDFORM.                    " WRITE_TOTALS

*---------------------- SUBROUTINES ------------------------------------

*------------------------ BUILD_TABLE-----------------------------------
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE.
  CLEAR TABLE1.

    TABLE1-BUKRS     = T001-BUKRS.                 "COMPANY CODE
    TABLE1-POSID     = PRPS-POSID+7(4).            "WBS ELEMENT
    TABLE1-PRART     = PRPS-PRART.                 "PROJECT TYPE
    TABLE1-PSPRI     = PRPS-PSPRI.                 "PRIORITY CODE
    TABLE1-VERNA     = PROJ-VERNA.                 "DIVISION
    TABLE1-PSPID     = PROJ-PSPID.                 "PROJECT
    TABLE1-KSTAR1    = KSTAR1.                     "PIPES
    TABLE1-KSTAR2    = KSTAR2.                     "MATERIAL
    TABLE1-KSTAR3    = KSTAR3.                     "LABOUR
    TABLE1-KSTAR4    = KSTAR4.                     "OTHER
    TABLE1-TOTAL_COST = KSTAR1 + KSTAR2 + KSTAR3 + KSTAR4.
    TABLE1-POST1     = PROJ-POST1.                 "PROJECT DESCRIPTION
    TABLE1-WBSPOST1  = PRPS-POST1.                 "WBS DESCRIPTION
    TABLE1-LENGTH    = LENGTH.                     "Actual lengths
    TABLE1-UNIT      = UNIT.                      "Actual units

    APPEND TABLE1.

ENDFORM.

*--------------------- CHECK_COLUMN ------------------------------------
FORM CHECK_COLUMN.
  IF KSTAR IN S_KSTAR1 AND S_KSTAR1 NE SPACE.                "pipe
    KSTAR1 = KSTAR1 + VALUE.
  ELSEIF KSTAR IN S_KSTAR2 AND S_KSTAR2 NE SPACE.            "material
    KSTAR2 = KSTAR2 + VALUE.
  ELSEIF KSTAR IN S_KSTAR3 AND S_KSTAR3 NE SPACE.            "labour
    KSTAR3 = KSTAR3 + VALUE.
  ELSE.                                                      "other
    KSTAR4 = KSTAR4 + VALUE.
  ENDIF.
ENDFORM.

*--------------------- GET_FISCAL_DATA ---------------------------------
FORM GET_FISCAL_DATA.

  SELECT * FROM COSP                "EXTERNAL POSTINGS
      WHERE OBJNR = PRPS-OBJNR              "Matching objects
        AND GJAHR = P_GJAHR                 "Fiscal Year selected
        AND VERSN = '000'                   "Version
        AND WRTTP = '04'                    "Record with actuals & plans
        AND BEKNZ IN ('S','H','L').         "Debit/Credit Indicator

     ADD COSP-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.
     KSTAR = COSP-KSTAR.
     PERFORM CHECK_COLUMN.
  ENDSELECT.                                "END of COSP

   SELECT * FROM COSS                "INTERNAL POSTINGS
      WHERE OBJNR = PRPS-OBJNR               "Matching objects
        AND GJAHR = P_GJAHR                  "Fiscal Year selected
        AND VERSN = '000'                    "Version
        AND WRTTP IN ('04').                 "Record with actuals

     ADD COSS-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.
     KSTAR = COSP-KSTAR.
     PERFORM CHECK_COLUMN.
  ENDSELECT.                                 "END of COSS

ENDFORM.


*----------------------- GET_ATINN ------------------------------------*
* determine internal number of characteristic                          *
*----------------------------------------------------------------------*
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
  ENDIF.

ENDFORM.

*--------------------------GET_CHARIC_VALUES --------------------------*
* Character  values in "ATWRT', numeric values in"ATFLV".              *
*----------------------------------------------------------------------*
FORM GET_CHARIC_VALUES.

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
  CLEAR: LENGTH, UNIT, WORK.
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
*  actual length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_LENGTH BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LENGTH = CHAR_TAB-ATFLV.
    ENDIF.
*  ACTUAL UNITS
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_UNIT BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      UNIT = CHAR_TAB-ATFLV.
    ENDIF.
*  WORK SOURCE
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_WORK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WORK = CHAR_TAB-ATWRT.
    ENDIF.

  ENDIF.
  CLEAR: OBJECT.

ENDFORM.                    " GET_CHARIC_VALUES
