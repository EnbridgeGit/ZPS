REPORT ZPPMR008 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        March 1998
*  Requestor:   Murray Caughy
*  Description:
*     - The purpose of this program is to produce a report of Service
*       WBS Elements of Actual Lengths & Units by Asset Number.
*
*  NB: Budget Units - in client 030 under atwrt
*                     in client 050 under atflv
************************************************************************
* 00/04/26 mokhan #--- Remove rows from report if actual amount is 0.
* 98/03/23 md7140 #--- Change in template definition
* 98/03/09 md7140 #427 Characteristic is BUDG_UNITS not BUDGET_UNITS
* 98/03/03 md7140 #427 Copied ZPPMR005 as template
*                 blankets are required - 001--199
*
************************************************************************
TABLES:
        PROJ,                          "Project Definition
        PRPS,                          "WBS Element Master Data
        COBRA,                         "Asset Valuation Date
        COBRB,                         "Settlement Rule Order
        COSP,                          "External Postings
        JEST,                          "Status of WBS Element
        TJ02T,                "System Status Texts (ie TECO, REL, etc)
        T001,                          "Company Code Table
        ANKA.                          "Asset Class

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_ACT_LGTH  LIKE CABN-ATINN,       "Actual Length Char
       G_ATINN_ACT_UNIT  LIKE CABN-ATINN,       "Actual Units Char
       G_ATINN_BUD_LGTH  LIKE CABN-ATINN,       "Budget Length Char
       G_ATINN_BUD_UNIT  LIKE CABN-ATINN.       "Budget Unit Char

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.
*-----------------------------------------------------------------------
DATA: BEGIN OF COSP_TABLE.
        INCLUDE STRUCTURE COSP.
DATA: END OF COSP_TABLE.

DATA: STATUS         LIKE JEST-STAT,   "Status
      FLAG(3)        TYPE C,
      STATUSDESC     LIKE TJ02T-TXT04, "Status Description
      BZDAT          LIKE COBRA-BZDAT, "Valuation Date
      ANLN(16)       TYPE C,           "Asset Number - anln1 & anln2
      PROZS          LIKE COBRB-PROZS, "Settlement Rule %
      BUD_LGTH       LIKE AUSP-ATFLV,  "Budget Length
      ACT_LGTH       LIKE AUSP-ATFLV,  "Actual Length
      BUD_UNIT       LIKE AUSP-ATFLV,  "Budget Units
      ACT_UNIT       LIKE AUSP-ATFLV,  "Actual Units
      AUDITOR        LIKE AUSP-ATWRT,  "Auditor
      BUDGET         LIKE COSP-WTG001, "Budget Total
      ACTUAL         LIKE COSP-WTG001, "Actual Total
      ACTTEMP        LIKE COSP-WTG001, "Actual Total

*     PERPROJ(4)     TYPE I,           "Project Totals      4.6b changes
      ACTPROJ        LIKE COSP-WTG001,
      BUDPROJ        LIKE COSP-WTG001,
      VARPROJ        LIKE COSP-WTG001.


DATA:                                  "Report Data
  BEGIN OF BIG_TABLE OCCURS 10000,
      ANLN1SRT(5)   TYPE C,            "Page Control Break
      ANLN(20)      TYPE C,
      POSID         LIKE PRPS-POSID,   "Project Number
      POST1         LIKE PRPS-POST1,   "Description
      STATUS        LIKE JEST-STAT,    "Status
      ACTLGTH(4)    TYPE P DECIMALS 0, "Actual Length
      BUDLGTH(4)    TYPE P DECIMALS 0, "Budget Length
      ACTUNIT(4)    TYPE P DECIMALS 0, "Actual Units
      BUDUNIT(4)    TYPE P DECIMALS 0, "Budget Units
      BZDAT         LIKE COBRA-BZDAT,  "Asset Valuation Date
      ACTUAL        LIKE COSP-WTG001,  "Actual Total
      VARIANCE      LIKE COSP-WTG001,  "Budget - Actual
*      anln1         like cobrb-anln1,          "Asset Number
      PROZS(5)       TYPE P DECIMALS 2,"Settlement Rule - %
  END OF BIG_TABLE.


*----------------------  SELECTION SCREEN  -----------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETER:       PBUKRS  LIKE PRPS-PBUKR OBLIGATORY.    "Company Code
SELECT-OPTIONS:  SVERNR  FOR  PRPS-VERNR,               "Division
                 SPSPID  FOR  PROJ-PSPID.               "Project
PARAMETER:       PGJAHR  LIKE COSP-GJAHR DEFAULT SY-DATUM(4), "FiscYr
                 PVERS   LIKE COSP-VERSN DEFAULT '00'.   "Version
PARAMETER:       PANLNL  LIKE ANKA-ANLKL.                "Asset Class
SELECT-OPTIONS:  SSTAT   FOR JEST-STAT."Status
SELECTION-SCREEN END OF BLOCK BOX.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.        "Comment 1 on screen
SELECTION-SCREEN COMMENT 1(37) TEXT-ST3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.        "Comment 2 on screen
SELECTION-SCREEN COMMENT 1(37) TEXT-ST1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.        "Comment 3 on screen
SELECTION-SCREEN COMMENT 1(37) TEXT-ST2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.        "Asset Class Comment
SELECTION-SCREEN COMMENT 1(50) TEXT-ST4.
SELECTION-SCREEN END OF LINE.


*-----------------------------------------------------------------------

START-OF-SELECTION.

  MOVE 'ACTUAL_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_ACT_LGTH.

  MOVE 'ACTUAL_UNITS'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_ACT_UNIT.

  MOVE 'BUDGET_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_BUD_LGTH.

  MOVE 'BUDG_UNITS'     TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_BUD_UNIT.

  SELECT SINGLE * FROM T001            "Company Code Description
    WHERE BUKRS = PBUKRS.

  SELECT * FROM PROJ
       WHERE VBUKR = PBUKRS            "Company Code
         AND PSPID IN SPSPID
         AND VERNR IN SVERNR.
*   if proj-pspid+4(3) co '1234567890'."No templates
    IF PROJ-PSPID+5(2) CO '1234567890'."No templates     98/03/23
*     if proj-pspid+4(3) > '199'.      "001-199 excluded - Blankets
        PERFORM PROCESS_TABLES.
*     endif.                           "End of 001-199 exclusion
    ENDIF.                             "End of Template
  ENDSELECT.
  PERFORM DISPLAY_TABLE.

  WRITE: / TEXT-003 UNDER TEXT-001.    "End of Report


************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*---------------------  TOP-OF-PAGE  -----------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-RPT, SY-REPID, 55 TEXT-001,                  "Title
           140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
* WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-002 UNDER TEXT-001, PGJAHR,                   "Fiscal Year
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  WRITE: / TEXT-VRS UNDER TEXT-RPT, PVERS,
           T001-BUTXT UNDER TEXT-001.  "Company
  ULINE.
  FORMAT INTENSIFIED ON.

  WRITE: /2 TEXT-004,  18 TEXT-005,  59 TEXT-006,  66 TEXT-007,
         77 TEXT-011, 86  TEXT-027, 95 TEXT-019,
        117 TEXT-024, 138 TEXT-015, 150 TEXT-029, 159 TEXT-031.
  PERFORM PRINT_VERT.

WRITE: / TEXT-008 UNDER TEXT-007,      "Asset Valuation Date
           TEXT-012 UNDER TEXT-011,    "Actual Length
           TEXT-028 UNDER TEXT-027,    "Actual Units
           TEXT-020 UNDER TEXT-019,    "Ytd Actual $
           TEXT-025 UNDER TEXT-024,    "Asset Number
           TEXT-016 UNDER TEXT-015,    "Settlement Rule %
           TEXT-030 UNDER TEXT-029,    "Budget Length
           TEXT-032 UNDER TEXT-031.    "Budget Units
  PERFORM PRINT_VERT.

WRITE: / TEXT-009 UNDER TEXT-007,      "Asset Valuation Date
           TEXT-023 UNDER TEXT-015.    "Settlement Rule %
  PERFORM PRINT_VERT.

  ULINE.

*------------------------  PRINT_VERT  ---------------------------------
*  Print vertical lines to separate one column from another
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:   1 SY-VLINE,  17 SY-VLINE,  58 SY-VLINE,  65 SY-VLINE,
          76 SY-VLINE,  85 SY-VLINE,  94 SY-VLINE,
         116 SY-VLINE, 137 SY-VLINE, 149 SY-VLINE,
         158 SY-VLINE, 167 SY-VLINE.
ENDFORM.

*------------------------  DISPLAY_TABLE  ------------------------------
*   This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

*loop at big_table.                 "To see information in big_table
*  write: / big_table-anln1srt, big_table-posid, big_table-post1,
*           big_table-actual,   big_table-prozs.
*endloop.

  SORT BIG_TABLE BY ANLN1SRT ANLN POSID.
  LOOP AT BIG_TABLE.
    AT NEW ANLN1SRT.
      NEW-PAGE.
    ENDAT.

    CLEAR STATUSDESC.
    SELECT SINGLE * FROM TJ02T
        WHERE ISTAT = BIG_TABLE-STATUS
         AND SPRAS = SY-LANGU.
    IF  SY-SUBRC = '0'.
      STATUSDESC = TJ02T-TXT04.
    ENDIF.
    WRITE:  / BIG_TABLE-POSID    UNDER TEXT-004,      "Project Number
              BIG_TABLE-POST1    UNDER TEXT-005,      "Description
              BIG_TABLE-BZDAT    UNDER TEXT-007,      "Valuation Date
              STATUSDESC         UNDER TEXT-006,      "Status
              BIG_TABLE-ANLN     UNDER TEXT-024
                USING EDIT MASK '_____ __ _____-____',"Asset Number
              BIG_TABLE-PROZS    UNDER TEXT-015.      "Settlement Rule %

    IF  BIG_TABLE-ACTUAL < 0.                 "ACTUAL
      FORMAT COLOR COL_NEGATIVE INVERSE ON.
    ENDIF.
    WRITE: BIG_TABLE-ACTUAL UNDER TEXT-019.
    FORMAT COLOR COL_BACKGROUND INVERSE OFF.

    IF  BIG_TABLE-ACTLGTH < 0.                "ACTUAL LENGTH
      FORMAT COLOR COL_NEGATIVE INVERSE ON.
    ENDIF.
    WRITE: BIG_TABLE-ACTLGTH UNDER TEXT-011.
    FORMAT COLOR COL_BACKGROUND INVERSE OFF.

    WRITE: BIG_TABLE-ACTUNIT UNDER TEXT-027,  "Actual Units
           BIG_TABLE-BUDLGTH UNDER TEXT-029,  "Budget Length
           BIG_TABLE-BUDUNIT UNDER TEXT-031.  "Budget unit

    PERFORM PRINT_VERT.


    AT END OF ANLN.
      SUM.
      FORMAT COLOR COL_GROUP.
      WRITE:  / TEXT-026 UNDER TEXT-005,
                BIG_TABLE-ANLN
                  USING EDIT MASK '_____ __ _____-____',"Asset Number
                BIG_TABLE-ACTLGTH  UNDER TEXT-011,      "Actual Length
                BIG_TABLE-ACTUAL   UNDER TEXT-019,      "Actual
                BIG_TABLE-ACTUNIT  UNDER TEXT-027,      "Actual Units
                BIG_TABLE-BUDLGTH  UNDER TEXT-029,      "Budget Length
                BIG_TABLE-BUDUNIT  UNDER TEXT-031.      "Budget unit
      FORMAT COLOR COL_BACKGROUND.
      PERFORM PRINT_VERT.
    ENDAT.

    AT END OF ANLN1SRT.
      SUM.
      FORMAT COLOR COL_TOTAL.
      WRITE:  / TEXT-026 UNDER TEXT-005,
                BIG_TABLE-ANLN1SRT,
                BIG_TABLE-ACTLGTH  UNDER TEXT-011,      "Actual Length
                BIG_TABLE-ACTUAL   UNDER TEXT-019,      "Actual
                BIG_TABLE-ACTUNIT  UNDER TEXT-027,      "Actual Units
                BIG_TABLE-BUDLGTH  UNDER TEXT-029,      "Budget Length
                BIG_TABLE-BUDUNIT  UNDER TEXT-031.      "Budget unit
      FORMAT COLOR COL_BACKGROUND.
      PERFORM PRINT_VERT.


      ULINE.

    ENDAT.
  ENDLOOP.

ENDFORM.


*--------------------  PROCESS_TABLES  ---------------------------------
*   This routine selects all PRPS row which require Interest
*   Calculation (PRPS-ZSCHM = 'CTRA/UN')
*-----------------------------------------------------------------------
FORM PROCESS_TABLES.
  CLEAR BIG_TABLE.
  SELECT * FROM PRPS
    WHERE PSPHI = PROJ-PSPNR.
  CLEAR: ACTUAL.

    IF  ( PRPS-POSID+7(4) BETWEEN '5211' AND '5213' OR
          PRPS-POSID+7(4) BETWEEN '5231' AND '5233' OR
          PRPS-POSID+7(4) = '5811' OR
          PRPS-POSID+7(4) = '5841' ).
      PERFORM GET_STATUS.
      IF FLAG = 'yes'.
        OBJECT = PRPS-OBJNR.
        PERFORM FIND_CHARACTERISTIC.
        PERFORM ASSET_VALUATION_DATE.
        IF FLAG = 'yes'.
          PERFORM GET_ASSET_NUMBER.
          IF FLAG = 'yes'.
            PERFORM COSP_ACTUAL.
            IF ACTUAL <> 0.                   "changes 00/04/26  mokhan
               PERFORM BUILD_BIG_TABLE.
            ENDIF.                            "changes 00/04/26  mokhan
          ENDIF.
        ENDIF.
      ENDIF.                           "End of FLAG check
    ENDIF.
  ENDSELECT.

ENDFORM.

*--------------------  ASSET VALUATION DATE ----------------------------
FORM ASSET_VALUATION_DATE.
  CLEAR BZDAT.
  SELECT SINGLE * FROM COBRA           "Valuation Date
    WHERE OBJNR = PRPS-OBJNR.
  IF SY-SUBRC = '0'.
    BZDAT = COBRA-BZDAT.
*    if cobra-bzdat ca '123456789'.
*       flag = 'no'.
*    endif.
  ENDIF.
ENDFORM.

*--------------------  GET_ASSET_NUMBER  -------------------------------
FORM GET_ASSET_NUMBER.
  CLEAR: ANLN, PROZS.

  MOVE 'no' TO FLAG.
  SELECT * FROM COBRB
    WHERE OBJNR = PRPS-OBJNR
      AND PERBZ = 'GES'.               "==> Code for FULL SETTLEMENT
    IF SY-SUBRC = '0'.
      IF ( COBRB-ANLN1(5) CP PANLNL+3(5) ) OR
         ( PANLNL = SPACE ).
        MOVE 'yes' TO FLAG.
        ANLN = COBRB-ANLN1.
        ANLN+12(4) = COBRB-ANLN2.
        PROZS = COBRB-PROZS.
      ENDIF.
    ELSEIF PANLNL = SPACE.
      MOVE 'yes' TO FLAG.
    ENDIF.
  ENDSELECT.
ENDFORM.


*--------------------  COSP_ACTUAL  ------------------------------------
FORM COSP_ACTUAL.
  CLEAR ACTTEMP.
  SELECT * FROM COSP INTO COSP_TABLE
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = PGJAHR
      AND VERSN = PVERS
      AND WRTTP = '04'
      AND KSTAR NOT IN ('0000491001', '0000491002').
    IF SY-SUBRC = '0'.                        "changes 00/04/26  mokhan
       ADD COSP_TABLE-WTG001 FROM 1 TO 12 GIVING ACTTEMP.
    ENDIF.                                    "changes 00/04/26  mokhan
    ADD ACTTEMP TO ACTUAL.
  ENDSELECT.
ENDFORM.

*--------------------  BUILD_BIG_TABLE  --------------------------------
FORM BUILD_BIG_TABLE.
  MOVE ANLN(5)     TO BIG_TABLE-ANLN1SRT.     "Page Control Break
  MOVE PRPS-POSID  TO BIG_TABLE-POSID. "Project Number
  MOVE PRPS-POST1  TO BIG_TABLE-POST1. "Description
  MOVE STATUS      TO BIG_TABLE-STATUS."Status
  MOVE BZDAT       TO BIG_TABLE-BZDAT. "Asset Valuation Date
  MOVE ACT_LGTH    TO BIG_TABLE-ACTLGTH.      "Actual Length
  MOVE ACT_UNIT    TO BIG_TABLE-ACTUNIT.      "Actual Units
  MOVE BUD_LGTH    TO BIG_TABLE-BUDLGTH.      "Budget Length
  MOVE BUD_UNIT    TO BIG_TABLE-BUDUNIT.      "Budget Units
  MOVE ACTUAL      TO BIG_TABLE-ACTUAL."Actual Total
  MOVE ANLN        TO BIG_TABLE-ANLN.  "Asset Number
  MOVE PROZS       TO BIG_TABLE-PROZS. "Settlement Rule - %
  APPEND BIG_TABLE.

ENDFORM.

*------------------------  GET_STATUS  ---------------------------------
*  Only RELEASED (I0002) or TECHNICALLY COMPLETE STATUS are acceptable
*-----------------------------------------------------------------------
FORM GET_STATUS.
  CLEAR: STATUS, FLAG.
  SELECT  * FROM JEST
    WHERE OBJNR = PRPS-OBJNR
      AND INACT <> 'X'                 "Ignore Inactives
      AND STAT IN SSTAT.
    IF  SY-SUBRC = '0'.
      MOVE JEST-STAT TO STATUS.
      MOVE 'yes'     TO FLAG.
    ENDIF.
  ENDSELECT.
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
  CLEAR: ACT_LGTH, BUD_LGTH, ACT_UNIT, BUD_UNIT.
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
*  actual length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_ACT_LGTH BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATFLV TO ACT_LGTH.
    ENDIF.
*  actual unit
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_ACT_UNIT BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATFLV TO ACT_UNIT.
    ENDIF.
*  budget length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_BUD_LGTH BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATFLV TO BUD_LGTH.
    ENDIF.
*  budget unit
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_BUD_UNIT BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATFLV TO BUD_UNIT.
    ENDIF.
  ENDIF.
  CLEAR: OBJECT.
ENDFORM.
