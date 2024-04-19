REPORT ZPPMR026 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 120
                MESSAGE-ID ZS.
************************************************************************
*  Author:      Mohammad Khan.
*  Date:        July, 2001.
*  Issue log:   866
*  Description:
*     - The purpose of this program is to produce a report of mains
*       lengths or services units depending upon selection criteria.
************************************************************************
TABLES:
        PROJ,                          "Project Definition
        PRPS,                          "WBS Element Master Data
        COBRA,                         "Asset Valuation Date
        COSP,                          "External Postings
        BPJA,                          "Total record..(Budget Amount)
        JEST,                          "Status of WBS Element
        TJ02T,                         "System Status Texts
        T001.                          "Company Code Table

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,                   "REQUIRED
       CHARIC            LIKE CABN-ATNAM,                   "REQUIRED
       G_ATINN           LIKE CABN-ATINN,                   "REQUIRED
       G_ATINN_BUD_LGTH  LIKE CABN-ATINN,       "Budget Length Char
       G_ATINN_ACT_LGTH  LIKE CABN-ATINN,       "Actual Length Char
       G_ATINN_PLN_LGTH  LIKE CABN-ATINN,       "Plan   Length Char
       G_ATINN_BUD_UNIT  LIKE CABN-ATINN,       "Budget Unit   Char
       G_ATINN_ACT_UNIT  LIKE CABN-ATINN,       "Actual Unit   Char
       G_ATINN_PLN_UNIT  LIKE CABN-ATINN.       "Plan   Unit   Char


DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.
*-----------------------------------------------------------------------

DATA: BUD_VALU(5)    TYPE P,           "Budget Length/Units
      ACT_VALU(5)    TYPE P,           "Actual Length/Units
      PLN_VALU(5)    TYPE P,           "Plan   Length/Units
      STOT_PLN_VALUE TYPE P,
      STOT_BUD_VALUE TYPE P,
      STOT_ACT_VALUE TYPE P,
      TOT_PLN_VALUE  TYPE P,
      TOT_BUD_VALUE  TYPE P,
      TOT_ACT_VALUE  TYPE P,
      PHEAD(6)       TYPE C VALUE 'Length',
      TOT_AMOUNT     LIKE COSP-WKG001.

DATA:                                  "Report Data
  BEGIN OF ITAB OCCURS 0,
      POSKI      LIKE PRPS-POSKI,      "Project id
      OBJNR      LIKE PRPS-OBJNR,      "WBS - Object number
      POST1      LIKE PROJ-POST1,      "Project Description
      PSPRI      LIKE PRPS-PSPRI,      "Priority
      TXT04      LIKE TJ02T-TXT04,     "WBS Status
      BZDAT      LIKE COBRA-BZDAT,     "Asset Valuation Date
      WKG001     LIKE COSP-WKG001,
      WKG002     LIKE COSP-WKG002,
      WKG003     LIKE COSP-WKG003,
      WKG004     LIKE COSP-WKG004,
      WKG005     LIKE COSP-WKG005,
      WKG006     LIKE COSP-WKG006,
      WKG007     LIKE COSP-WKG007,
      WKG008     LIKE COSP-WKG008,
      WKG009     LIKE COSP-WKG009,
      WKG010     LIKE COSP-WKG010,
      WKG011     LIKE COSP-WKG011,
      WKG012     LIKE COSP-WKG012,
END OF ITAB,

BEGIN OF ITABLE OCCURS 0,
    PTYPE,                            "Priority Type
    PSPRI         LIKE PRPS-PSPRI,    "Priority code
    POSKI         LIKE PRPS-POSKI,    "Project ID
    POST1         LIKE PROJ-POST1,    "Project Description
    TXT04         LIKE TJ02T-TXT04,   "WBS status
    BZDAT         LIKE COBRA-BZDAT,   "Asset value date
    OBJNR         LIKE PRPS-OBJNR,    "WBS - Object no.

END OF ITABLE.

DATA WRKAREA LIKE LINE OF ITAB.

*----------------------  SELECTION SCREEN  -----------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME..
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-013.
SELECTION-SCREEN POSITION 12.
PARAMETERS: PMAINS RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN COMMENT 16(35) TEXT-020.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-014.
SELECTION-SCREEN POSITION 12.
PARAMETERS: PUNITS RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN COMMENT 16(20) TEXT-021.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: SWBS    FOR PRPS-POSID+7(4) OBLIGATORY.   "WBS

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
PARAMETERS:     PBUKRS  LIKE T001T-BUKRS  DEFAULT 'UGL'.   "Company code
SELECT-OPTIONS: SPSPID  FOR PROJ-PSPID,                    "Projects
                SPSPRI1 FOR PRPS-PSPRI,                    "Priority Add
                SPSPRI2 FOR PRPS-PSPRI,                    "Priority Rep
                SVERNR  FOR PRPS-VERNR,                    "Division
                SSTAT   FOR JEST-STAT.                     "Status
PARAMETERS:     PFYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
                PVERSN LIKE COSP-VERSN DEFAULT '000'.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-024.
*SELECT-OPTIONS:  PAVD FOR COBRA-BZDAT.
PARAMETERS:      P_CHECK AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(60) TEXT-023.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX.

*-----------------------------------------------------------------------
*........................INITIALIZATION ...........................
INITIALIZATION.
 SPSPRI1-SIGN   = 'I'.                     " Addition Prorities
 SPSPRI1-OPTION = 'BT'.
 SPSPRI1-LOW    = '1'.
 SPSPRI1-HIGH   = '6'.
 APPEND SPSPRI1.
 CLEAR  SPSPRI1.
 SPSPRI1-SIGN   = 'I'.
 SPSPRI1-OPTION = 'EQ'.
 SPSPRI1-LOW    = '8'.
 APPEND SPSPRI1.
 SPSPRI1-SIGN   = 'I'.
 SPSPRI1-OPTION = 'EQ'.
 SPSPRI1-LOW    = 'A'.
 APPEND SPSPRI1.
 SPSPRI1-SIGN   = 'I'.
 SPSPRI1-OPTION = 'EQ'.
 SPSPRI1-LOW    = 'K'.
 APPEND SPSPRI1.
 SPSPRI1-SIGN   = 'I'.
 SPSPRI1-OPTION = 'EQ'.
 SPSPRI1-LOW    = 'M'.
 APPEND SPSPRI1.
 SPSPRI1-SIGN   = 'I'.
 SPSPRI1-OPTION = 'EQ'.
 SPSPRI1-LOW    = 'P'.
 APPEND SPSPRI1.
 SPSPRI1-SIGN   = 'I'.
 SPSPRI1-OPTION = 'EQ'.
 SPSPRI1-LOW    = 'S'.
 APPEND SPSPRI1.
 SPSPRI1-SIGN   = 'I'.
 SPSPRI1-OPTION = 'EQ'.
 SPSPRI1-LOW    = 'V'.
 APPEND SPSPRI1.

 SPSPRI2-SIGN   = 'I'.                     " Replacement Prorities
 SPSPRI2-OPTION = 'EQ'.
 SPSPRI2-LOW    = '7'.
 APPEND SPSPRI2.
 SPSPRI2-SIGN   = 'I'.
 SPSPRI2-OPTION = 'EQ'.
 SPSPRI2-LOW    = '9'.
 APPEND SPSPRI2.
 SPSPRI2-SIGN   = 'I'.
 SPSPRI2-OPTION = 'BT'.
 SPSPRI2-LOW    = 'B'.
 SPSPRI2-HIGH   = 'E'.
 APPEND SPSPRI2.
 CLEAR  SPSPRI2.
 SPSPRI2-SIGN   = 'I'.
 SPSPRI2-OPTION = 'EQ'.
 SPSPRI2-LOW    = 'L'.
 APPEND SPSPRI2.
 SPSPRI2-SIGN   = 'I'.
 SPSPRI2-OPTION = 'EQ'.
 SPSPRI2-LOW    = 'N'.
 APPEND SPSPRI2.
 SPSPRI2-SIGN   = 'I'.
 SPSPRI2-OPTION = 'EQ'.
 SPSPRI2-LOW    = 'Q'.
 APPEND SPSPRI2.
 SPSPRI2-SIGN   = 'I'.
 SPSPRI2-OPTION = 'EQ'.
 SPSPRI2-LOW    = 'T'.
 APPEND SPSPRI2.
 SPSPRI2-SIGN   = 'I'.
 SPSPRI2-OPTION = 'EQ'.
 SPSPRI2-LOW    = 'W'.
 APPEND SPSPRI2.

* PAVD-SIGN   = 'I'.                     "AVD
* PAVD-OPTION = 'EQ'.
* CONCATENATE PFYEAR '0101' INTO PAVD.
* PAVD-LOW    = 'W'.
* APPEND PAVD.


  CALL FUNCTION 'SET_PRINT_PARAMETERS'    "To print the variants
       EXPORTING
            COVER_PAGE = 'X'.

*.................END OF INITIALIZATION ...........................


AT SELECTION-SCREEN.
   LOOP AT SWBS.
        IF PMAINS = 'X'.
           IF SWBS-OPTION = 'BT'.
              IF SWBS-LOW > '5399' AND SWBS-HIGH < '5500'.
                 EXIT.
              ELSE.
              IF SWBS-LOW > '4599' AND SWBS-HIGH < '4700'.
                 EXIT.
              ELSE.
                 MESSAGE E019 WITH 'WBS Entered are not for Mains'.
              ENDIF.
              ENDIF.
           ELSE.
              IF SWBS-LOW > '5400' AND SWBS-LOW < '5499'.
                 EXIT.
              ELSE.
              IF SWBS-LOW > '4599' AND SWBS-LOW < '4700'.
                 EXIT.
              ELSE.
                 MESSAGE E019 WITH 'WBS Entered are not for Mains'.
              ENDIF.
              ENDIF.
           ENDIF.
       ELSE.
           IF SWBS-OPTION = 'BT'.
              IF SWBS-LOW < '5200' OR SWBS-HIGH > '5239'.
                 MESSAGE E019 WITH 'WBS Entered are not for Units'.
              ENDIF.
           ELSE.
              IF SWBS-LOW < '5200' OR SWBS-LOW > '5239'.
                 MESSAGE E019 WITH 'WBS Entered are not for Mains'.
              ENDIF.
           ENDIF.
        ENDIF.

   ENDLOOP.


START-OF-SELECTION.

  IF PMAINS = 'X'.
    PERFORM GET_LENGTHS.
  ELSE.
    PERFORM GET_UNITS.
    MOVE 'Units' TO PHEAD.
  ENDIF.

  SELECT SINGLE * FROM T001            "Company Code Description
    WHERE BUKRS = PBUKRS.

  SELECT PRPS~POSKI  PRPS~OBJNR  PROJ~POST1
         PRPS~PSPRI  TJ02T~TXT04 COBRA~BZDAT
    INTO TABLE ITAB
    FROM ( ( ( ( PROJ
           INNER JOIN PRPS
           ON PROJ~PSPNR   = PRPS~PSPHI )
           LEFT OUTER JOIN COBRA
           ON COBRA~OBJNR  = PRPS~OBJNR )
           INNER JOIN JEST
           ON JEST~OBJNR   = PRPS~OBJNR )
           INNER JOIN TJ02T
           ON TJ02T~ISTAT  = JEST~STAT  )
  WHERE  PROJ~VBUKR  =  PBUKRS            "Company code
    AND  PROJ~PSPID  IN SPSPID            "Project number
    AND  PROJ~VERNR  IN SVERNR            "Division
    AND  ( PRPS~PSPRI  IN SPSPRI1   OR    "Priority
           PRPS~PSPRI  IN SPSPRI2 )
    AND  ( PRPS~PLAKZ = 'X' OR PRPS~BELKZ = 'X' )
    AND  JEST~STAT   IN SSTAT              "WBS status
    AND  JEST~INACT  <> 'X'                "Active
    AND  TJ02T~SPRAS = 'EN'.               "Language

  IF ITAB[] IS INITIAL.                    "No data selected
    STOP.
  ENDIF.

  IF SY-SUBRC = 0.
    SORT ITAB BY POSKI OBJNR.
    LOOP AT ITAB.
      IF P_CHECK = 'X' AND
         ITAB-BZDAT+4(4)  = '0101'.
      ELSE.
      IF ITAB-POSKI+10(4) IN SWBS.
         MOVE ITAB TO WRKAREA.
         PERFORM CHECK_AMOUNTS.
      ENDIF.
      ENDIF.
*      AT END OF OBJNR.
         IF TOT_AMOUNT <> 0.
            PERFORM BUILD_TABLE.
            CLEAR TOT_AMOUNT.
         ENDIF.
*      ENDAT.
    ENDLOOP.
  ENDIF.



  PERFORM DISPLAY_TABLE.
  SKIP 2.
  WRITE: / TEXT-022 UNDER TEXT-005.    "End of Report


************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*---------------------  TOP-OF-PAGE  -----------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: /53 T001-BUTXT.                                   "Company
  WRITE: / TEXT-RPT, SY-REPID, 50 TEXT-001,                 "Title
           95 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
         TEXT-002 UNDER TEXT-001, PFYEAR,                   "Fiscal Year
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

  ULINE.
  FORMAT INTENSIFIED ON.

  WRITE: /1 TEXT-003, 12 TEXT-004,  28 TEXT-005,  70 TEXT-006,
         77 TEXT-007,  91 TEXT-011,  103 TEXT-012,  114 TEXT-015.

  WRITE: /   PHEAD UNDER TEXT-011,
             PHEAD UNDER TEXT-012,
             PHEAD UNDER TEXT-015.

  ULINE.


*------------------------  GET AMOUNTS  --------------------------------
*   This subroutine  gets AMOUNT from COSP and BPJA Tables
*-----------------------------------------------------------------------

 FORM CHECK_AMOUNTS.

 CLEAR TOT_AMOUNT.
 SELECT * FROM COSP
    WHERE OBJNR = WRKAREA-OBJNR                 "Matching objects
      AND GJAHR = PFYEAR                        "Fiscal Year selected
      AND VERSN = PVERSN                        "Version
      AND WRTTP IN ('01', '04')                 "Plan/Actual Amount
      AND BEKNZ IN ('S','H','L').               "Debit/Credit Indicator
    IF SY-SUBRC = 0.
       ADD COSP-WKG001 FROM 1 TO 12 GIVING TOT_AMOUNT.
    ENDIF.
 ENDSELECT.
    IF TOT_AMOUNT = 0.
       PERFORM CHECK_BUDGET_AMOUNT.
    ENDIF.
 ENDFORM.

 FORM CHECK_BUDGET_AMOUNT.
  SELECT * FROM BPJA                            "Budget Info
       WHERE OBJNR = WRKAREA-OBJNR
         AND WRTTP = '41'
         AND GJAHR = PFYEAR
         AND VERSN = PVERSN.                      "Version
       IF SY-SUBRC = '0'.
          ADD  BPJA-WLJHR TO TOT_AMOUNT.
       ENDIF.
  ENDSELECT.

 ENDFORM.


*------------------------  GET LENGTHS  ------------------------------
*   This subroutine  gets characteristics data for mains lengths
*-----------------------------------------------------------------------
FORM GET_LENGTHS.

  MOVE 'ACTUAL_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_ACT_LGTH.

  MOVE 'BUDGET_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_BUD_LGTH.

  MOVE 'PLANNED_LENGTH' TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_PLN_LGTH.

ENDFORM.

*------------------------  GET UNITS  ----------------------------------
*   This subroutine  gets characteristics data for services units
*-----------------------------------------------------------------------
FORM GET_UNITS.

  MOVE 'ACTUAL_UNITS'   TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_ACT_UNIT.

  MOVE 'BUDGET_UNITS'     TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_BUD_UNIT.

  MOVE 'PLANNED_UNITS'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_PLN_UNIT.

ENDFORM.

*------------------------  DISPLAY_TABLE  ------------------------------
*   This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

  SORT ITABLE BY PTYPE POSKI PSPRI.

  LOOP AT ITABLE.

    AT NEW PTYPE.
      IF ITABLE-PTYPE = 'A'.
        WRITE: /1 TEXT-016.
      ELSE.
        SKIP 1.
        WRITE: /1 TEXT-019.
      ENDIF.
      WRITE: /1 SY-ULINE(11).
    ENDAT.
    OBJECT = ITABLE-OBJNR.
    PERFORM FIND_CHARACTERISTIC.
    WRITE:  / ITABLE-PSPRI        UNDER TEXT-003,      "Project Priority
              ITABLE-POSKI        UNDER TEXT-004,      "Project
              ITABLE-POST1        UNDER TEXT-005,      "Description
              ITABLE-TXT04        UNDER TEXT-006,      "Status
              ITABLE-BZDAT        UNDER TEXT-007,      "AVD
          (6) PLN_VALU DECIMALS 0 UNDER TEXT-011,      "Plan
          (6) BUD_VALU DECIMALS 0 UNDER TEXT-012,      "Budget
          (6) ACT_VALU DECIMALS 0 UNDER TEXT-015.      "Actual
          ADD PLN_VALU TO STOT_PLN_VALUE.
          ADD BUD_VALU TO STOT_BUD_VALUE.
          ADD ACT_VALU TO STOT_ACT_VALUE.

     AT END OF PTYPE.
     WRITE: /.
     WRITE: /63 SY-ULINE.
     WRITE: /2 TEXT-025, (7) STOT_PLN_VALUE DECIMALS 0 UNDER TEXT-011,
                         (7) STOT_BUD_VALUE DECIMALS 0 UNDER TEXT-012,
                         (7) STOT_ACT_VALUE DECIMALS 0 UNDER TEXT-015.
     ADD: STOT_PLN_VALUE TO TOT_PLN_VALUE,
          STOT_BUD_VALUE TO TOT_BUD_VALUE,
          STOT_ACT_VALUE TO TOT_ACT_VALUE.
     CLEAR: STOT_PLN_VALUE, STOT_BUD_VALUE, STOT_ACT_VALUE.


     WRITE: /63 SY-ULINE.
    ENDAT.
     AT LAST.
     WRITE: /.
     WRITE: /63 SY-ULINE.
     WRITE: /2 TEXT-026, (8) TOT_PLN_VALUE DECIMALS 0 UNDER TEXT-011,
                         (8) TOT_BUD_VALUE DECIMALS 0 UNDER TEXT-012,
                         (8) TOT_ACT_VALUE DECIMALS 0 UNDER TEXT-015.

     WRITE: /63 SY-ULINE.
    ENDAT.
ENDLOOP.

ENDFORM.


*--------------------  BUILD_TABLE  --------------------------------
FORM BUILD_TABLE.
  IF WRKAREA-PSPRI IN SPSPRI1.
    MOVE 'A'   TO ITABLE-PTYPE.
  ELSE.
    MOVE 'B'   TO ITABLE-PTYPE.
  ENDIF.
  MOVE WRKAREA-PSPRI  TO ITABLE-PSPRI.
  MOVE WRKAREA-POSKI  TO ITABLE-POSKI.
  MOVE WRKAREA-POST1  TO ITABLE-POST1.
  MOVE WRKAREA-OBJNR  TO ITABLE-OBJNR.
  MOVE WRKAREA-TXT04  TO ITABLE-TXT04.
  MOVE WRKAREA-BZDAT  TO ITABLE-BZDAT.

  APPEND ITABLE.

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
  ENDIF.
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
* Character  values in "ATWRT', numeric values in"ATFLV".
  CLEAR: ACT_VALU, BUD_VALU, PLN_VALU.
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
    IF PMAINS = 'X'.
*  plan length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PLN_LGTH BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE CHAR_TAB-ATFLV TO PLN_VALU.
      ENDIF.
*  actual length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_ACT_LGTH BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE CHAR_TAB-ATFLV TO ACT_VALU.
      ENDIF.
*  budget length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_BUD_LGTH BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE CHAR_TAB-ATFLV TO BUD_VALU.
      ENDIF.
    ELSE.
*  plan units
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PLN_UNIT BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE CHAR_TAB-ATFLV TO PLN_VALU.
      ENDIF.
*  actual units
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_ACT_UNIT BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE CHAR_TAB-ATFLV TO ACT_VALU.
      ENDIF.
*  budget units
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_BUD_UNIT BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE CHAR_TAB-ATWRT  TO BUD_VALU.
      ENDIF.

    ENDIF.
  ENDIF.
  CLEAR: OBJECT.
ENDFORM.

