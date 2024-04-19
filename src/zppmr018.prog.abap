REPORT ZPPMR018 NO STANDARD PAGE HEADING LINE-SIZE 170
                LINE-COUNT 58 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    zppmr018
*   PROGRAMMER: M. Khan
*   CLIENT:     Union Gas
*   DATE:       APR 2000.
*
*   The purpose of this program is to produce a list of project
*   relating to structures or selected otherwise from the variants.
*
************************************************************************
* CHANGES
* 6/12/2007 Mohammad TR314 Adjust the columns to avoid valid to date
*                          truncation.
* 15/5/2009 Mohammad TR314 Change to select asset starting with 4 and 5.
************************************************************************

TABLES: PRPS,               " WBS element master data
        PROJ,               " Project definition
        COSS,               " CO object: internal postings
        COSP,               " CO object: external postings
        BPJA,               " Totals record for annual total
        COBRB,              " Settlement rules by object nbr
        T001.               " Company code

DATA: BEGIN OF TABLE OCCURS 0,
        VERNR         LIKE PRPS-VERNR,              "Division
        POSID         LIKE PRPS-POSID,              "WBS element
        ANLN1         LIKE COBRB-ANLN1,             "Main asset Number
        ANLN2         LIKE COBRB-ANLN2,             "Sub Asset Number
        PSPHI         LIKE PRPS-PSPHI,              "Project Number
        PSPRI         LIKE PRPS-PSPRI,              "Proj. Priority Code
        PRART         LIKE PRPS-PRART,              "Project Type Code
        PCTRL(6),                                   "Project Control
        WKGNNP(8)     TYPE P DECIMALS 2,            "Plan Amount
        WLJHR(8)      TYPE P DECIMALS 2,            "Budget Amount
        WKGNNA(8)     TYPE P DECIMALS 2,            "Actual Amount
        GABPE         LIKE COBRB-GABPE,             "Valid from period
        GABJA         LIKE COBRB-GABJA,             "Valid from year
        GBISP         LIKE COBRB-GBISP,             "Valid to period
        GBISJ         LIKE COBRB-GBISJ,             "Valid to year
        PROZS         LIKE COBRB-PROZS,             "Percentage
     END OF TABLE.

DATA: AMT(8)            TYPE P DECIMALS 2,
      AMT_PLAN(8)       TYPE P DECIMALS 2,
      AMT_BUDGET(8)     TYPE P DECIMALS 2,
      AMT_ACTUAL(8)     TYPE P DECIMALS 2,
      DIV_PLAN(8)       TYPE P DECIMALS 2,
      DIV_BUDGET(8)     TYPE P DECIMALS 2,
      DIV_ACTUAL(8)     TYPE P DECIMALS 2,
      TOT_PLAN(8)       TYPE P DECIMALS 2,
      TOT_BUDGET(8)     TYPE P DECIMALS 2,
      TOT_ACTUAL(8)     TYPE P DECIMALS 2,
      PREV_POSID        LIKE PRPS-POSID,
      G_ATINN           LIKE CABN-ATINN,              "Proj ctr REQUIRED
      G_ATINN_PC        LIKE CABN-ATINN,              "Project Ctrl Char
      PROJ_CONTROL(6),                                "Project control
      OBJECT            LIKE AUSP-OBJEK,              "Project control
      CHARIC            LIKE CABN-ATNAM,              "Project control
      DASHES,
      HEAD_PRINT(1)     VALUE 'Y'.

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.
*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.

PARAMETERS:
   P_REPTTL(50)  DEFAULT 'Listing of Projects relating to Structures',
   P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),        "Fiscal Year
   P_CCODE LIKE PRPS-PBUKR DEFAULT 'UGL',                "Company code
   P_VERS  LIKE COSP-VERSN DEFAULT '0'.                  "Budget Version

SELECT-OPTIONS:
        S_WBS    FOR PRPS-POSID+7(4),                    "WBS element
        S_VERNR  FOR PRPS-VERNR,                         "Division
        S_PSPRI  FOR PRPS-PSPRI,                         "Priority
        S_PRART  FOR PRPS-PRART,                         "Project Type
        S_AMT    FOR COSP-WKG001 DEFAULT '100000000-' TO '100000000'.

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_KSTAR    FOR COSS-KSTAR
                DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-004.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_FILE RADIOBUTTON GROUP RBCR.            "EXCEL FILE
SELECTION-SCREEN END OF BLOCK BOX3.

*-----------------------  END of SELECTION SCREEN-----------------------
************************************************************************
*-------------------------  START-OF-SELECTION -------------------------
AT SELECTION-SCREEN.
 LOOP AT S_WBS.
   IF S_WBS-HIGH = SPACE.
      S_WBS-HIGH = S_WBS-LOW.
      S_WBS-OPTION = 'BT'.
      MODIFY S_WBS INDEX SY-TABIX.
   ELSE.
   IF S_WBS-LOW  = SPACE.
      S_WBS-LOW  = S_WBS-HIGH.
      S_WBS-OPTION = 'BT'.
      MODIFY S_WBS INDEX SY-TABIX.
   ENDIF.
   ENDIF.
 ENDLOOP.
 IF SY-SUBRC = '4'.
    S_WBS-SIGN   = 'I'.
    S_WBS-LOW  = '0000'.
    S_WBS-HIGH = '9999'.
    S_WBS-OPTION = 'BT'.
    APPEND S_WBS.
 ENDIF.

 LOOP AT S_VERNR.
   IF S_VERNR-HIGH = SPACE.
      S_VERNR-HIGH = S_VERNR-LOW.
      MODIFY S_VERNR INDEX SY-TABIX.
   ENDIF.
 ENDLOOP.
 LOOP AT S_PSPRI.
   IF S_PSPRI-HIGH = SPACE.
      S_PSPRI-HIGH = S_PSPRI-LOW.
      MODIFY S_PSPRI INDEX SY-TABIX.
   ENDIF.
 ENDLOOP.
 LOOP AT S_PRART.
   IF S_PRART-HIGH = SPACE.
      S_PRART-HIGH = S_PRART-LOW.
      MODIFY S_PRART INDEX SY-TABIX.
   ENDIF.
 ENDLOOP.
 LOOP AT S_KSTAR.
   IF S_KSTAR-HIGH = SPACE.
      S_KSTAR-HIGH = S_KSTAR-LOW.
      MODIFY S_KSTAR INDEX SY-TABIX.
   ENDIF.
 ENDLOOP.

 LOOP AT S_AMT.
   IF S_AMT-HIGH = SPACE.
      S_AMT-HIGH = S_AMT-LOW.
      MODIFY S_AMT INDEX SY-TABIX.
   ENDIF.
 ENDLOOP.


START-OF-SELECTION.

SELECT SINGLE * FROM T001                "Company Code
  WHERE BUKRS = P_CCODE.

IF P_RPRT EQ 'X'.
   PERFORM PRINT_VARIANT.
ENDIF.

  MOVE 'PROJECT_CNTR_NUMBER'  TO  CHARIC.    "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN      TO   G_ATINN_PC.

 SELECT * FROM PRPS
   WHERE PBUKR = P_CCODE
     AND VERNR IN S_VERNR
     AND PSPRI IN S_PSPRI
     AND PRART IN S_PRART.

     IF ( SY-SUBRC = '0' )   AND
        ( PRPS-POSID+7(4) IN  S_WBS ).
          AMT   = 0.
          PERFORM GET_AMOUNTS.
          IF ( AMT_PLAN    <> 0    AND
               AMT_PLAN   IN  S_AMT )    OR
             ( AMT_BUDGET  <> 0    AND
               AMT_BUDGET IN  S_AMT )    OR
             ( AMT_ACTUAL  <> 0    AND
               AMT_ACTUAL IN  S_AMT ).
                OBJECT = PRPS-OBJNR.
                PERFORM FIND_CHARACTERISTIC.
                PERFORM BUILD_TABLE.
          ENDIF.

         CLEAR: AMT_ACTUAL, AMT_PLAN, AMT_BUDGET.
     ENDIF.
 ENDSELECT.                                         "End of PRPS


    SORT TABLE BY VERNR POSID.

 LOOP AT TABLE.
   IF TABLE-POSID <> PREV_POSID.
      PREV_POSID = TABLE-POSID.
      SELECT SINGLE * FROM PROJ
        WHERE PSPNR = TABLE-PSPHI.
      IF TABLE-ANLN2 = '    '.
         DASHES = ' '.
      ELSE.
         DASHES = '-'.
      ENDIF.

      WRITE: /1 TABLE-POSID,
             16 PROJ-POST1,
             61 TABLE-PSPRI,
             65 TABLE-PRART,
             68 TABLE-PCTRL,
            128 TABLE-ANLN1,
            140 DASHES,
            141 TABLE-ANLN2.
*            148 TABLE-GABPE,
*            151 '-',
*            152 TABLE-GABJA,
*            157 TABLE-GBISP,
*            160 '-',
*            161 TABLE-GBISJ,
  IF TABLE-GABPE > 0.
     WRITE: 148 TABLE-GABPE+1(2).
  ENDIF.
  IF TABLE-GABJA > 0.
     WRITE: 150 '-',
            151 TABLE-GABJA.
  ENDIF.
  IF TABLE-GBISP > 0.
     WRITE  156 TABLE-GBISP+1(2).
  ENDIF.
  IF TABLE-GBISJ > 0.
     WRITE: 158 '-',
            159 TABLE-GBISJ.
  ENDIF.
     WRITE  164 TABLE-PROZS DECIMALS 0.

            DIV_ACTUAL = DIV_ACTUAL + TABLE-WKGNNA.
         IF TABLE-WKGNNP <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE:  74 TABLE-WKGNNP DECIMALS 0.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE:  74 TABLE-WKGNNP DECIMALS 0.
         ENDIF.
         IF TABLE-WLJHR <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE:  91 TABLE-WLJHR DECIMALS 0.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE:  91 TABLE-WLJHR DECIMALS 0.
         ENDIF.
         IF TABLE-WKGNNA <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE: 108 TABLE-WKGNNA DECIMALS 0.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE: 108 TABLE-WKGNNA DECIMALS 0.
         ENDIF.
   ENDIF.
     AT END OF VERNR.
     WRITE: /.
     WRITE: /110(15) SY-ULINE.
     WRITE: /2 TEXT-003,  TABLE-VERNR+6(2),  TEXT-025.
         IF DIV_ACTUAL   <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE: 108 DIV_ACTUAL DECIMALS 0.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE: 108 DIV_ACTUAL   DECIMALS 0.
         ENDIF.
     WRITE: /110(15) SY-ULINE.
      TOT_ACTUAL = TOT_ACTUAL + DIV_ACTUAL.
      CLEAR: DIV_ACTUAL.
     WRITE: /.
    ENDAT.

 ENDLOOP.

END-OF-SELECTION.

     WRITE: /.
     WRITE: /110(15) SY-ULINE.
     WRITE: /2 TEXT-026.
         IF TOT_ACTUAL   <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE: 108 TOT_ACTUAL DECIMALS 0.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE: 108 TOT_ACTUAL   DECIMALS 0.
         ENDIF.
     WRITE: /110(15) SY-ULINE.

************************************************************************
*                       SUBROUTINES
************************************************************************

*---------------------  BUILD_TABLE ------------------------------------
FORM BUILD_TABLE.
  TABLE-POSID   = PRPS-POSID.
  TABLE-VERNR   = PRPS-VERNR.
  TABLE-PRART   = PRPS-PRART.
  TABLE-PSPHI   = PRPS-PSPHI.
  TABLE-PSPRI   = PRPS-PSPRI.
  TABLE-PCTRL   = PROJ_CONTROL.
  TABLE-WKGNNP  = AMT_PLAN.                             "Plan Amount
  TABLE-WKGNNA  = AMT_ACTUAL.                           "Actual Amount
  TABLE-WLJHR   = AMT_BUDGET.                           "Budget Amount
  PERFORM SETTLEMENT_RULES.
  APPEND TABLE.
  CLEAR: TABLE, AMT_ACTUAL, AMT_PLAN, AMT_BUDGET.
ENDFORM.

*---------------------  SETTLEMENT RULES -------------------------------
FORM SETTLEMENT_RULES.
         SELECT * FROM COBRB
            WHERE OBJNR = PRPS-OBJNR
              AND GABJA GE P_FYEAR
              AND GBISJ LE P_FYEAR.
*              IF SY-SUBRC = '0' AND COBRB-ANLN1+0(1) = '4'.    "TR314
            IF SY-SUBRC = '0' AND COBRB-ANLN1+0(1) BETWEEN '4' AND '5'.
                 TABLE-GABPE   = COBRB-GABPE.
                 TABLE-GABJA   = COBRB-GABJA.
                 TABLE-GBISP   = COBRB-GBISP.
                 TABLE-GBISJ   = COBRB-GBISJ.
                 TABLE-PROZS   = COBRB-PROZS.
                 TABLE-ANLN1   = COBRB-ANLN1.
                 TABLE-ANLN2   = COBRB-ANLN2.
                 EXIT.
            ENDIF.
         ENDSELECT.
ENDFORM.

*---------------------  GET_AMOUNTS ------------------------------------
FORM GET_AMOUNTS.
* COST TOTALS - External Postings

  SELECT * FROM COSP
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_FYEAR                              "Fiscal Year
      AND WRTTP IN ('01', '04')                        "Plan and Actuals
      AND ( VERSN = P_VERS                             "Version
       OR   VERSN = '000' )
      AND BEKNZ IN ('S', 'H', 'L')               "Debit/Credit Indicator
      AND KSTAR NOT IN  S_KSTAR.

    IF SY-SUBRC = '0'.
      ADD  COSP-WKG001 FROM 1 TO 12 GIVING AMT.
      CASE COSP-WRTTP.
           WHEN '01'.
                 IF COSP-VERSN = P_VERS.
                    AMT_PLAN = AMT_PLAN + AMT.
                 ENDIF.
                    CLEAR AMT.
           WHEN '04'.
                 IF COSP-VERSN = '000'.
                    AMT_ACTUAL = AMT_ACTUAL + AMT.
                 ENDIF.
                    CLEAR AMT.
      ENDCASE.
    ENDIF.
  ENDSELECT.
*
* COST TOTALS - Internal Postings

  SELECT * FROM COSS
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_FYEAR                              "Fiscal Year
      AND WRTTP = '04'                                 "Actuals
      AND ( VERSN = P_VERS                             "Version
       OR   VERSN = '000' )
      AND KSTAR NOT IN  S_KSTAR.

    IF SY-SUBRC = '0'.
      ADD  COSS-WKG001 FROM 1 TO 12 GIVING AMT.
      CASE COSS-WRTTP.
           WHEN '04'.
                 IF COSS-VERSN = '000'.
                    AMT_ACTUAL = AMT_ACTUAL + AMT.
                 ENDIF.
                    CLEAR AMT.
      ENDCASE.
    ENDIF.
  ENDSELECT.
*
  SELECT * FROM BPJA                                  "Budget Info
       WHERE OBJNR = PRPS-OBJNR
         AND WRTTP = '41'
         AND GJAHR = P_FYEAR
         AND VERSN = P_VERS.                          "Version
       IF SY-SUBRC = '0'.
          ADD  BPJA-WLJHR TO AMT_BUDGET.
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
    WRITE: /.
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
  CLEAR: PROJ_CONTROL.
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PC BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATWRT+0(6) TO PROJ_CONTROL.
    ENDIF.
  ENDIF.
ENDFORM.
*------------------- PRINT VARIANT -------------------------------------

FORM PRINT_VARIANT.
   WRITE: /20 TEXT-100, 50 TEXT-101.
   WRITE: / SY-ULINE(19) UNDER TEXT-100.

   WRITE: / TEXT-102 UNDER  TEXT-100,
            P_FYEAR  UNDER  TEXT-101.
   SKIP.
   WRITE: / TEXT-103 UNDER  TEXT-100,
            P_CCODE  UNDER  TEXT-101.
   SKIP.
   WRITE: / TEXT-104 UNDER  TEXT-100,
            P_VERS   UNDER  TEXT-101.
   SKIP.
   WRITE: / TEXT-106 UNDER TEXT-100.
   LOOP AT S_WBS.
        IF S_WBS-OPTION = 'EQ'.
           WRITE: S_WBS-OPTION UNDER TEXT-101, S_WBS-LOW.
        ELSE.
           WRITE: S_WBS-OPTION UNDER TEXT-101, S_WBS-LOW, S_WBS-HIGH.
        ENDIF.
           SKIP.
   ENDLOOP.

   WRITE: / TEXT-107 UNDER TEXT-100.
   LOOP AT S_VERNR.
     IF S_VERNR-OPTION = 'EQ'.
       WRITE: S_VERNR-OPTION UNDER TEXT-101, S_VERNR-LOW.
     ELSE.
       WRITE: S_VERNR-OPTION UNDER TEXT-101, S_VERNR-LOW, S_VERNR-HIGH.
     ENDIF.
       SKIP.
   ENDLOOP.

   WRITE: / TEXT-108 UNDER TEXT-100.
   LOOP AT S_PSPRI.
     IF S_PSPRI-OPTION = 'EQ'.
       WRITE: S_PSPRI-OPTION UNDER TEXT-101, S_PSPRI-LOW.
     ELSE.
       WRITE: S_PSPRI-OPTION UNDER TEXT-101, S_PSPRI-LOW, S_PSPRI-HIGH.
     ENDIF.
       SKIP.
   ENDLOOP.

   WRITE: / TEXT-109 UNDER TEXT-100.
   LOOP AT S_PRART.
     IF S_PRART-OPTION = 'EQ'.
       WRITE: S_PRART-OPTION UNDER TEXT-101, S_PRART-LOW.
     ELSE.
       WRITE: S_PRART-OPTION UNDER TEXT-101, S_PRART-LOW, S_PRART-HIGH.
     ENDIF.
       SKIP.
   ENDLOOP.

   WRITE: / TEXT-110 UNDER TEXT-100.
   LOOP AT S_AMT.
     IF S_AMT-OPTION = 'EQ'.
       WRITE: S_AMT-OPTION UNDER TEXT-101, S_AMT-LOW.
     ELSE.
       WRITE: S_AMT-OPTION UNDER TEXT-101, S_AMT-LOW, S_AMT-HIGH.
     ENDIF.
       SKIP.
   ENDLOOP.

   WRITE: / TEXT-111 UNDER TEXT-100.
   LOOP AT S_KSTAR.
     IF S_KSTAR-OPTION = 'EQ'.
       WRITE: S_KSTAR-OPTION UNDER TEXT-101, S_KSTAR-LOW.
     ELSE.
       WRITE: S_KSTAR-OPTION UNDER TEXT-101, S_KSTAR-LOW, S_KSTAR-HIGH.
     ENDIF.
       SKIP.
   ENDLOOP.
   NEW-PAGE.

ENDFORM.
*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
IF HEAD_PRINT = 'Y'.
   WRITE: /1 TEXT-RPT, SY-REPID,  65 T001-BUTXT,             "Company
         143 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
          55 P_REPTTL,
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   WRITE: / TEXT-VER UNDER TEXT-RPT, P_VERS,
          65 TEXT-TL2, P_FYEAR.                           "Fiscal Year
   ULINE.
   WRITE: 1  TEXT-006, 16 TEXT-007, 55 TEXT-009, 64 TEXT-010,
          69 TEXT-011, 84 TEXT-012, 101 TEXT-014, 118 TEXT-016,
         128 TEXT-018, 148 TEXT-020, 157 TEXT-022, 167 TEXT-024.
   ULINE.
   IF P_FILE = 'X'.
      HEAD_PRINT = 'N'.
   ENDIF.
ENDIF.
*------------------------- End of Report Header ------------------------
