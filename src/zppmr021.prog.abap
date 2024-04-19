REPORT ZPPMR021 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170.
*
************************************************************************
*  AUTHOR:      MOHAMMAD KHAN
*  DATE:        NOVEMBER 2000.
*  Description:
*     - The purpose of this program is to produce Capital Expenditures
*       Report By PCE Groupings For WAVE.
*
*
*Changes:
*  Date:Oct.31, 2001   By: Mokhan   Issue-Log: 843
*  Description: Add F & G as default priorities.
*
************************************************************************
TABLES: PROJ,             "Project Table
        PRPS,             "Work Breakdown Structure-Elements Master Data
        COSP,             "CO Object: Cost total for External Postings
        COSS,             "CO Object: Cost totals for Internal Postings
        T001,             "Company Code
        T247,             "Month name and short text
        CABN,             "Field characteristic
        AUSP.             "Characteristic Value

DATA:
  BEGIN OF BIG_TABLE OCCURS 10000,
        BUKRS         LIKE PROJ-VBUKR,         "Company code
        WBS(4)        TYPE C,                   "WBS
        ADDRPLC       TYPE C,                   "Addition/Replacement
        PSPID         LIKE PROJ-PSPID,          "Project Number
        TYPE          LIKE PRPS-PRART,          "Project Type
        PRIORITY      LIKE PRPS-PSPRI,          "Project Priority
        OBJNR         LIKE PRPS-OBJNR,          "Object Number
        COLABOUR      LIKE COSP-WKG001,         "Company Labour
        MATERIALS     LIKE COSP-WKG001,                     "Materials
        CONTRACTOR    LIKE COSP-WKG001,         "Contractors
        OTHERS        LIKE COSP-WKG001,                     "Other
        TOTAL         LIKE COSP-WKG001,                     "Total
        AUNITS(5)     TYPE P,                   "Actual Units
        ALENGTH(5)    TYPE P,                   "Actual Length
        MAJOR(3),                               "Major project
        PROJPOST1     LIKE PROJ-POST1,         "Project Description
        WBSPOST1      LIKE PRPS-POST1,         "WBS Description
        STAT(4)       TYPE C,                               "Status

  END OF BIG_TABLE.


DATA:  P_BEGNME(9),
       P_ENDNME(9),
       BEGMTH(2)     TYPE C,
       ENDMTH(2)     TYPE C,
       VALUE         LIKE COSP-WKG001 VALUE 0,
       ACTUAL        LIKE COSP-WKG001,
       PROJPOST1     LIKE PRPS-POST1,           "Temp - description
       WBSPOST1      LIKE PRPS-POST1,           "Temp - description
       KSTAR         LIKE COSP-KSTAR,
       NEWSTATUS,
       STAT(4)       TYPE C,
       COLABOUR      LIKE COSP-WKG001,
       MATERIALS     LIKE COSP-WKG001,
       CONTRACTOR    LIKE COSP-WKG001,
       OTHERS        LIKE COSP-WKG001,
       TOTAL         LIKE COSP-WKG001,
       STATUS(40)    TYPE C,                "All valid status of object
       HEAD_PRINT(1)     VALUE 'Y',
*                                     "Req. fields for Proj ctr & Major
      G_ATINN           LIKE CABN-ATINN,
      G_ATINN_MP        LIKE CABN-ATINN,
      G_ATINN_AL        LIKE CABN-ATINN,
      G_ATINN_AU        LIKE CABN-ATINN,

      MAJOR_PROJ(3),                                  "Major project
      ACT_LENGTH(5)     TYPE P,                       "Actual Length
      ACT_UNITS(5)      TYPE P,                       "Actual Units
      OBJECT            LIKE AUSP-OBJEK,
      CHARIC            LIKE CABN-ATNAM.

DATA: BEGIN OF LASTWBS,
       OPARANT           VALUE '(',
       NUMBER(4)         TYPE C,
       CPARANT           VALUE ')',
      END OF LASTWBS.

DATA: BEGIN OF CHAR_TAB OCCURS 0.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:     P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL'.
SELECT-OPTIONS: SWBS    FOR PRPS-POSID+7(4) OBLIGATORY,    "WBS Elements
                SPSPHI  FOR PRPS-PSPHI,                     "Projects
                SPSPRI1 FOR PRPS-PSPRI,                    "Priority Add
                SPSPRI2 FOR PRPS-PSPRI,                    "Priority Rep
                SPRART  FOR PRPS-PRART,                     "Type
                SDIV    FOR PRPS-VERNR,                     "Division
                SSTAT   FOR BIG_TABLE-STAT OBLIGATORY.      "Status
PARAMETERS:     PFYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS: SMTH    FOR SY-DATUM+4(2) OBLIGATORY        "Months
                            DEFAULT SY-DATUM+4(2).
SELECTION-SCREEN END OF BLOCK BOX.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-038.
SELECT-OPTIONS: SKSTAR1 FOR COSP-KSTAR
                            DEFAULT '0000410001' TO '0000417999',
                SKSTAR2 FOR COSP-KSTAR
                            DEFAULT '0000420001' TO '0000421099',
                SKSTAR3 FOR COSP-KSTAR.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-039.
SELECT-OPTIONS: EX_KSTAR    FOR COSS-KSTAR
                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-040.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_FILE RADIOBUTTON GROUP RBCR.              "EXCEL FILE
SELECTION-SCREEN END OF BLOCK BOX4.


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
  SPSPRI1-SIGN   = 'I'.                        "Issue Log 843
  SPSPRI1-OPTION = 'EQ'.                       "Issue Log 843
  SPSPRI1-LOW    = 'F'.                        "Issue Log 843
  APPEND SPSPRI1.                              "Issue Log 843
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
  SPSPRI2-SIGN   = 'I'.                         "Issue Log 843
  SPSPRI2-OPTION = 'EQ'.                        "Issue Log 843
  SPSPRI2-LOW    = 'G'.                         "Issue Log 843
  APPEND SPSPRI2.                               "Issue Log 843
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

  SSTAT-SIGN   = 'I'.                                       "Status
  SSTAT-OPTION = 'EQ'.
  SSTAT-LOW    = 'CLSD'.
  APPEND SSTAT.
  SSTAT-SIGN   = 'I'.
  SSTAT-OPTION = 'EQ'.
  SSTAT-LOW    = 'TECO'.
  APPEND SSTAT.
  SSTAT-SIGN   = 'I'.
  SSTAT-OPTION = 'EQ'.
  SSTAT-LOW    = 'REL '.
  APPEND SSTAT.
  SSTAT-SIGN   = 'I'.
  SSTAT-OPTION = 'EQ'.
  SSTAT-LOW    = 'DLIN'.
  APPEND SSTAT.

  SKSTAR3-SIGN = 'I'.
  SKSTAR3-OPTION = 'BT'.
  SKSTAR3-LOW  = '0000446041'.
  SKSTAR3-HIGH = '0000446043'.
  APPEND SKSTAR3.
  SKSTAR3-SIGN = 'I'.
  SKSTAR3-OPTION = 'EQ'.
  SKSTAR3-LOW  = '0000480116'.
  SKSTAR3-HIGH = '          '.
  APPEND SKSTAR3.

*------------------------  AT SELECTION-SCREEN  ------------------------

AT SELECTION-SCREEN ON P_CCODE.
  SELECT SINGLE * FROM T001                        "Get Company Name
    WHERE BUKRS = P_CCODE.

AT SELECTION-SCREEN ON SMTH.
 SELECT SINGLE * FROM T247                        "Get Month Name(Begin)
    WHERE SPRAS = 'E'
    AND MNR = SMTH+3(2).
  P_BEGNME = T247-LTX.

  P_ENDNME = SPACE.
  IF SMTH+5(2) <> '00'.
    SELECT SINGLE * FROM T247                      "Get Month Name(End)
       WHERE SPRAS = 'E'
         AND MNR = SMTH+5(2).
    P_ENDNME = T247-LTX.
  ENDIF.

  BEGMTH = SMTH+3(2).
  ENDMTH = SMTH+5(2).
  IF ENDMTH = '00'.
    ENDMTH = BEGMTH.
  ENDIF.
*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.
  IF P_RPRT EQ 'X'.
    PERFORM PRINT_VARIANT.
  ENDIF.

  MOVE 'MAJORPROJECT'         TO  CHARIC.    "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_MP.

  MOVE 'ACTUAL_LENGTH'        TO  CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_AL.

  MOVE 'ACTUAL_UNITS'         TO  CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_AU.

  PERFORM BUILD_AUSP.

  SELECT * FROM PRPS
     WHERE PSPHI IN SPSPHI
       AND PKOKR = '10'                             "Controlling area
       AND LOEVM <> 'X'                             "Not flagged deleted
      AND ( BELKZ = 'X' OR PLAKZ = 'X' )           "Indicator: Acct/Plan
       AND PRART IN SPRART                                  "Type
       AND ( PSPRI IN SPSPRI1 OR PSPRI IN SPSPRI2 )         "Priority
       AND VERNR IN SDIV                                    "Division
       AND PBUKR = P_CCODE.                         "Company Code

    IF ( SY-SUBRC = '0' )   AND
       ( PRPS-POSID+7(4) IN  SWBS ).
      NEWSTATUS = 'Y'.
      PERFORM GET_STATUS.
      IF NEWSTATUS = 'Y'.
        PERFORM GET_FISCAL_DATA.
        IF     COLABOUR   <> 0 OR                "Print only none zero
               MATERIALS  <> 0 OR
               CONTRACTOR <> 0 OR
               OTHERS     <> 0.
          PERFORM BUILD_TABLE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDSELECT.

  PERFORM DISPLAY_TABLE.


*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
  IF HEAD_PRINT = 'Y'.
    FORMAT INTENSIFIED OFF.
    WRITE: /1 TEXT-RPT, SY-REPID,                           "Report Id
           80 T001-BUTXT,                                  "Company Name
          140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.       "Date/Time
    WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,   "Client
           70 TEXT-003,                                    "Report Title
              TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.           "Page Number

    IF P_ENDNME = SPACE.                                    "Time Frame
      WRITE: /85 P_BEGNME, PFYEAR.
    ELSE.
      WRITE: /80 P_BEGNME, TEXT-035, P_ENDNME, TEXT-036, PFYEAR.
    ENDIF.
    FORMAT INTENSIFIED ON.
    WRITE: /1 SY-ULINE.
    WRITE: /1 TEXT-007, 11 TEXT-008, 55 TEXT-009, 61 TEXT-010,
           67 TEXT-011, 74 TEXT-012, 82 TEXT-013, 97 TEXT-014,
          113 TEXT-015, 127 TEXT-016, 140 TEXT-017, 155 TEXT-018,
          163 TEXT-019.

    ULINE.
    IF P_FILE = 'X'.
      HEAD_PRINT = 'N'.
    ENDIF.

  ENDIF.
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

  SORT BIG_TABLE BY BUKRS WBS ADDRPLC PSPID.
  CLEAR BIG_TABLE.

  LOOP AT BIG_TABLE.
    MOVE BIG_TABLE-WBSPOST1    TO WBSPOST1.
    MOVE BIG_TABLE-PROJPOST1   TO PROJPOST1.

    AT NEW WBS.
      WRITE: /1  TEXT-005, BIG_TABLE-WBS, TEXT-006, WBSPOST1.
      WRITE: /1(30) SY-ULINE.
      MOVE BIG_TABLE-WBS  TO  LASTWBS-NUMBER.
    ENDAT.

    AT NEW ADDRPLC.                               "Addition/Replacement
      IF BIG_TABLE-ADDRPLC = '1'.
        WRITE:/1 TEXT-020.
      ELSE.
        WRITE:/1 TEXT-021.
      ENDIF.
      WRITE:/1(10) SY-ULINE.
    ENDAT.


    WRITE:/ BIG_TABLE-PSPID      UNDER TEXT-007,
            BIG_TABLE-PROJPOST1  UNDER TEXT-008,
            BIG_TABLE-MAJOR      UNDER TEXT-009,
            BIG_TABLE-TYPE       UNDER TEXT-010,
            BIG_TABLE-STAT       UNDER TEXT-011,
            BIG_TABLE-PRIORITY   UNDER TEXT-012,
            (12) BIG_TABLE-COLABOUR   DECIMALS 0 UNDER TEXT-013,
            (12) BIG_TABLE-MATERIALS  DECIMALS 0 UNDER TEXT-014,
            (12) BIG_TABLE-CONTRACTOR DECIMALS 0 UNDER TEXT-015,
            (12) BIG_TABLE-OTHERS     DECIMALS 0 UNDER TEXT-016,
            (14) BIG_TABLE-TOTAL      DECIMALS 0 UNDER TEXT-017,
            (6)  BIG_TABLE-AUNITS     DECIMALS 0 UNDER TEXT-018,
            (6)  BIG_TABLE-ALENGTH    DECIMALS 0 UNDER TEXT-019.

    IF P_FILE <> 'X'.
     AT END OF ADDRPLC.                            "Addition/Replacement
        SUM.
        WRITE: /15 SY-ULINE.
        IF BIG_TABLE-ADDRPLC = '1'.
          WRITE:/15 TEXT-022, TEXT-020, LASTWBS.
        ELSE.
          WRITE:/15 TEXT-022, TEXT-021, LASTWBS.
        ENDIF.
        PERFORM AMOUNT_TOTALS.
        WRITE: /15 SY-ULINE.
      ENDAT.

      AT END OF WBS.                                "WBS element
        SUM.
        WRITE:/15 TEXT-022, TEXT-005, LASTWBS.
        PERFORM AMOUNT_TOTALS.
        WRITE: /15 SY-ULINE.
      ENDAT.

      AT END OF BUKRS.
        SUM.
        WRITE:/15 TEXT-023.
        PERFORM AMOUNT_TOTALS.
        WRITE: /15 SY-ULINE.
      ENDAT.
    ENDIF.

  ENDLOOP.

ENDFORM.

*------------------------  PROJECT DATA   ------------------------------
FORM GET_PROJECT_DATA.

  SELECT SINGLE * FROM PROJ
         WHERE PSPNR = PRPS-PSPHI.

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
         WHERE ( ATINN = G_ATINN_MP OR
                 ATINN = G_ATINN_AL OR
                 ATINN = G_ATINN_AU ) AND
                 MAFID = 'O'          AND
                 KLART = '014'.
  SORT CHAR_TAB BY OBJEK ATINN.
ENDFORM.

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine to get the value of the CHARACTER FIELDS
*-----------------------------------------------------------------------
FORM FIND_CHARACTERISTIC.

  CLEAR: MAJOR_PROJ, ACT_UNITS, ACT_LENGTH.
  MOVE   'n/a'   TO MAJOR_PROJ.
  READ TABLE CHAR_TAB WITH KEY OBJEK = PRPS-OBJNR
                               ATINN = G_ATINN_MP  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATWRT+0(6) TO MAJOR_PROJ.
  ENDIF.

  READ TABLE CHAR_TAB WITH KEY OBJEK = PRPS-OBJNR
                               ATINN = G_ATINN_AL  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATFLV TO ACT_LENGTH.
  ENDIF.

  READ TABLE CHAR_TAB WITH KEY OBJEK = PRPS-OBJNR
                               ATINN = G_ATINN_AU  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATFLV TO ACT_UNITS.
  ENDIF.

ENDFORM.

*------------------------  AMOUNT_TOTALS  ------------------------------
FORM AMOUNT_TOTALS.
  WRITE: (12)  BIG_TABLE-COLABOUR   DECIMALS 0 UNDER TEXT-013,
         (12)  BIG_TABLE-MATERIALS  DECIMALS 0 UNDER TEXT-014,
         (12)  BIG_TABLE-CONTRACTOR DECIMALS 0 UNDER TEXT-015,
         (12)  BIG_TABLE-OTHERS     DECIMALS 0 UNDER TEXT-016,
         (14) BIG_TABLE-TOTAL      DECIMALS 0 UNDER TEXT-017,
         (6)  BIG_TABLE-AUNITS     DECIMALS 0 UNDER TEXT-018,
         (6)  BIG_TABLE-ALENGTH    DECIMALS 0 UNDER TEXT-019.
ENDFORM.

*-----------------------------------------------------------------------
*  FORM GET_STATUS.
*-----------------------------------------------------------------------
FORM GET_STATUS.
 CALL FUNCTION 'AIP9_STATUS_READ'                  "Retrieve Status Info
        EXPORTING
             I_OBJNR = PRPS-OBJNR
             I_SPRAS = SY-LANGU
        IMPORTING
             E_SYSST = STATUS
        EXCEPTIONS
             OTHERS = 1.

  IF STATUS CS 'CLSD'.
    STAT = 'CLSD'.
  ELSEIF STATUS CS 'TECO'.
    STAT = 'TECO'.
  ELSEIF STATUS CS 'REL'.
    STAT = 'REL'.
  ELSEIF STATUS CS 'DLIN'.
    STAT = 'DLIN'.
  ELSE.
    MOVE 'N' TO NEWSTATUS.
  ENDIF.
  IF NOT STAT IN SSTAT.
    MOVE 'N' TO NEWSTATUS.
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------
*  FORM GET_FISCAL_DATA.
*-----------------------------------------------------------------------
FORM GET_FISCAL_DATA.
* COST TOTALS - External Postings
  CLEAR: COLABOUR, MATERIALS, CONTRACTOR, OTHERS, VALUE, TOTAL.
  SELECT * FROM COSP
     WHERE OBJNR = PRPS-OBJNR                    "Matching objects
       AND GJAHR = PFYEAR                        "Fiscal Year selected
       AND VERSN = '000'
       AND WRTTP = '04'                          "Actual Amount
       AND BEKNZ IN ('S','H','L')                "Debit/Credit Indicator
       AND KSTAR NOT IN EX_KSTAR.
    IF SY-SUBRC = 0.
      ADD COSP-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.
      KSTAR = COSP-KSTAR.
      PERFORM CHECK_COLUMN.
    ENDIF.

  ENDSELECT.


* COST TOTALS - Internal Postings
  SELECT * FROM COSS
    WHERE OBJNR = PRPS-OBJNR                      "Matching Objects
      AND GJAHR = PFYEAR                         "Fiscal Year Selected
      AND VERSN = '000'                           "Version  0 for Actual
      AND WRTTP IN ('04')                         "Record with Actuals
      AND KSTAR NOT IN EX_KSTAR.

    IF SY-SUBRC = 0.
      ADD COSS-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.
*      KSTAR = COSP-KSTAR.
      KSTAR = COSS-KSTAR.
      PERFORM CHECK_COLUMN.
    ENDIF.

  ENDSELECT.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM BUILD_TABLE
*-----------------------------------------------------------------------
FORM BUILD_TABLE.
  CLEAR BIG_TABLE.
  PERFORM GET_PROJECT_DATA.
  PERFORM FIND_CHARACTERISTIC.
  MOVE PRPS-POSID+7(4) TO BIG_TABLE-WBS.
  MOVE PROJ-PSPID      TO BIG_TABLE-PSPID.
  MOVE PROJ-POST1      TO BIG_TABLE-PROJPOST1.
  MOVE PRPS-PRART      TO BIG_TABLE-TYPE.
  MOVE PRPS-PSPRI      TO BIG_TABLE-PRIORITY.
  MOVE PRPS-POST1      TO BIG_TABLE-WBSPOST1.
  MOVE PRPS-OBJNR      TO BIG_TABLE-OBJNR.
  MOVE STAT            TO BIG_TABLE-STAT.
  MOVE COLABOUR        TO BIG_TABLE-COLABOUR.
  MOVE MATERIALS       TO BIG_TABLE-MATERIALS.
  MOVE CONTRACTOR      TO BIG_TABLE-CONTRACTOR.
  MOVE OTHERS          TO BIG_TABLE-OTHERS.
  MOVE TOTAL           TO BIG_TABLE-TOTAL.
  MOVE P_CCODE         TO BIG_TABLE-BUKRS.
  MOVE MAJOR_PROJ      TO BIG_TABLE-MAJOR.
  MOVE ACT_UNITS       TO BIG_TABLE-AUNITS.
  MOVE ACT_LENGTH      TO BIG_TABLE-ALENGTH.

  IF PRPS-PSPRI IN SPSPRI1.
    MOVE '1' TO BIG_TABLE-ADDRPLC.
  ELSE.
    MOVE '2' TO BIG_TABLE-ADDRPLC.
  ENDIF.
  APPEND BIG_TABLE.
  CLEAR: COLABOUR, MATERIALS, CONTRACTOR, OTHERS, VALUE, TOTAL.
ENDFORM.

*-----------------------------------------------------------------------
*    FORM CHECK_COLUMN
*-----------------------------------------------------------------------
FORM CHECK_COLUMN.
  IF KSTAR IN SKSTAR1 AND SKSTAR1 NE SPACE.
    COLABOUR  = COLABOUR + VALUE.
  ELSEIF KSTAR IN SKSTAR2 AND SKSTAR2 NE SPACE.
    MATERIALS = MATERIALS + VALUE.
  ELSEIF KSTAR IN SKSTAR3 AND SKSTAR3 NE SPACE.
    CONTRACTOR = CONTRACTOR + VALUE.
  ELSE.
    OTHERS = OTHERS + VALUE.
  ENDIF.
  TOTAL = TOTAL + VALUE.
ENDFORM.

*------------------- PRINT_VARIANT -------------------------------------
FORM PRINT_VARIANT.
  WRITE: /20 TEXT-100, 40 TEXT-101.

  WRITE: / SY-ULINE(19) UNDER TEXT-100.

  WRITE: / TEXT-005 UNDER TEXT-100.                   "WBS
  IF SWBS = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SWBS.
      WRITE: SWBS-OPTION UNDER TEXT-101, SWBS-LOW, SWBS-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

  WRITE: / TEXT-007 UNDER TEXT-100.                         "Projects
  IF SPSPHI-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SPSPHI.
      WRITE: SPSPHI-OPTION UNDER TEXT-101, SPSPHI-LOW, SPSPHI-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

WRITE: / TEXT-102 UNDER TEXT-100.                  "Priority - Additions
  IF SPSPRI1-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SPSPRI1.
      WRITE: SPSPRI1-OPTION UNDER TEXT-101, SPSPRI1-LOW, SPSPRI1-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

WRITE: / TEXT-103 UNDER TEXT-100.                  "Priority - Replacem.
  IF SPSPRI2-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SPSPRI2.
      WRITE: SPSPRI2-OPTION UNDER TEXT-101, SPSPRI2-LOW, SPSPRI2-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

  WRITE: / TEXT-010 UNDER TEXT-100.                  "Project - Type
  IF SPRART-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SPRART.
      WRITE: SPRART-OPTION UNDER TEXT-101, SPRART-LOW, SPRART-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

  WRITE: / TEXT-104 UNDER TEXT-100.                         "Division
  IF SDIV-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SDIV.
      WRITE: SDIV-OPTION UNDER TEXT-101, SDIV-LOW, SDIV-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

  WRITE: / TEXT-011 UNDER TEXT-100.                         "Status
  IF SSTAT-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SSTAT.
      WRITE: SSTAT-OPTION UNDER TEXT-101, SSTAT-LOW, SSTAT-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

  WRITE: / TEXT-105 UNDER TEXT-100.                  "Fiscal Year
  WRITE: PFYEAR  UNDER  TEXT-101.
  WRITE: /.

  WRITE: / TEXT-106 UNDER TEXT-100.                         "Months
  IF SMTH-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SMTH.
      WRITE: SMTH-OPTION UNDER TEXT-101, SMTH-LOW, SMTH-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

 WRITE: / TEXT-107 UNDER TEXT-100.                  "Cost Elements Group
  WRITE: / SY-ULINE(19) UNDER TEXT-100.

  WRITE: / TEXT-013 UNDER TEXT-100.                  "Company Labour
  IF SKSTAR1-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SKSTAR1.
      WRITE: SKSTAR1-OPTION UNDER TEXT-101, SKSTAR1-LOW, SKSTAR1-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

  WRITE: / TEXT-014 UNDER TEXT-100.                         "Materials
  IF SKSTAR2-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SKSTAR2.
      WRITE: SKSTAR2-OPTION UNDER TEXT-101, SKSTAR2-LOW, SKSTAR2-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

  WRITE: / TEXT-015 UNDER TEXT-100.                         "Contractor
  IF SKSTAR3-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT SKSTAR3.
      WRITE: SKSTAR3-OPTION UNDER TEXT-101, SKSTAR3-LOW, SKSTAR3-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.

WRITE: / TEXT-108 UNDER TEXT-100.                  "Exclude Cost Element
  IF EX_KSTAR-OPTION = SPACE.
    WRITE: /.
  ELSE.
    LOOP AT EX_KSTAR.
      WRITE: EX_KSTAR-OPTION UNDER TEXT-101, EX_KSTAR-LOW,
                                             EX_KSTAR-HIGH.
      WRITE: /.
    ENDLOOP.
  ENDIF.


  NEW-PAGE.
ENDFORM.
