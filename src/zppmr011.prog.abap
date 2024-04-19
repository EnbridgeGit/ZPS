REPORT ZPPMR011 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        April 1998
*  Description:
*     - The purpose of this program is to produce Summary of Capital
*       Expenditures Report by Cost Element
************************************************************************
* 98/05/20 md7140 #--- Users wanted to change columns being selected
* 98/05/03 md7140 #--- Company Labour is not compulsory
* 98/04/23 md7140 #480 Created program - used ZPPMR010 for start
***** ***** ***** ***** ***** ****** ***** ***** ***** ***** ***** *****
*  modified by Nancy Gilligan, OmniLogic October 28, 1998 - D30K906248
*     changes marked by "omning
*    - changed budget and actuals to select only version '000'         *
************************************************************************
TABLES: PROJ,                             "Project Table
        BPJA, COSP, COSS, CSKU, JEST, PRPS, T001, T247, TCJ1T,
        TCN7T.


DATA:
  BEGIN OF BIG_TABLE OCCURS 10000,
        PSPID         LIKE PROJ-PSPID,           "Project (7 digits)
        WBS(4)        TYPE C,
        KSTAR1        LIKE COSP-WKG001,
        KSTAR2        LIKE COSP-WKG001,
        KSTAR3        LIKE COSP-WKG001,
        KSTAR4        LIKE COSP-WKG001,
        KSTAR5        LIKE COSP-WKG001,
*       kstar6        like cosp-wkg001,
        KSTAR7        LIKE COSP-WKG001,
        KSTAR8        LIKE COSP-WKG001,
        KSTAR9        LIKE COSP-WKG001,
        KSTARA        LIKE COSP-WKG001,
        KSTARB        LIKE COSP-WKG001,
        KSTARC        LIKE COSP-WKG001,
        KSTARD        LIKE COSP-WKG001,
*       kstarz        like cosp-wkg001,
        PLAN          LIKE COSP-WKG001,
        BUDGET        LIKE COSP-WKG001,
        VERNR         LIKE PROJ-VERNR,           "Div/dept
        POST1         LIKE PROJ-POST1,           "Project Description
        WBSPOST1      LIKE PRPS-POST1,
*       verna          like proj-verna,          "vernr Name
  END OF BIG_TABLE.


DATA:  P_BEGNME(9),
       P_ENDNME(9),
       BEGMTH(2)     TYPE C,
       ENDMTH(2)     TYPE C,
       PERCENT(5)    TYPE P  DECIMALS 1,
       KSTAR1        LIKE COSP-WKG001,     "Construction Material
       KSTAR2        LIKE COSP-WKG001,     "Pipe
       KSTAR3        LIKE COSP-WKG001,     "Company Labour
       KSTAR4        LIKE COSP-WKG001,     "Employee Expenses
       KSTAR5        LIKE COSP-WKG001,     "Contract Services
*      kstar6        like cosp-wkg001,     "Contract Expenses
       KSTAR7        LIKE COSP-WKG001,     "Contract Supervision
       KSTAR8        LIKE COSP-WKG001,     "Consultants
       KSTAR9        LIKE COSP-WKG001,     "Radiography
       KSTARA        LIKE COSP-WKG001,     "Inspection Services
       KSTARB        LIKE COSP-WKG001,     "IDC
       KSTARC        LIKE COSP-WKG001,     "Other
       VALUE         LIKE COSP-WKG001 VALUE 0,
*      value1        like cosp-wkg001,      "planned values -     omning
       PLAN          LIKE COSP-WKG001,
       BUDGET        LIKE COSP-WKG001,
       POST1         LIKE PRPS-POST1,           "Temp - description
       WBSPOST1      LIKE PRPS-POST1,           "Temp - description
       WBS(4)        TYPE C,
       VERNA         LIKE PROJ-VERNA,
       KSTAR         LIKE COSP-KSTAR.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:     P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL'.
SELECT-OPTIONS: SPSPID  FOR PROJ-PSPID,                    "Projects
                SDIV    FOR PRPS-VERNR.                    "Division
PARAMETERS:     P_VERS  LIKE COSP-VERSN DEFAULT '0',       "Plan version
                P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS: SMTH    FOR SY-DATUM+4(2) OBLIGATORY       "Months
                            DEFAULT SY-DATUM+4(2).
PARAMETER:  P_CHECK AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-038.
SELECT-OPTIONS: S_KSTAR1 FOR COSP-KSTAR,
                S_KSTAR2 FOR COSP-KSTAR,
                S_KSTAR3 FOR COSP-KSTAR,
                S_KSTAR4 FOR COSP-KSTAR,
                S_KSTAR5 FOR COSP-KSTAR,
*               s_kstar6 for cosp-kstar,
                S_KSTAR7 FOR COSP-KSTAR,
                S_KSTAR8 FOR COSP-KSTAR,
                S_KSTAR9 FOR COSP-KSTAR,
                S_KSTARA FOR COSP-KSTAR,
                S_KSTARB FOR COSP-KSTAR.
SELECTION-SCREEN END OF BLOCK BOX2.
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
 PERFORM PRINT_VARIANT.
 SELECT * FROM PROJ                                   "PROJ TABLE SELECT
   WHERE PSPID IN SPSPID
     AND VERNR IN SDIV
     AND VBUKR = P_CCODE.
   IF PROJ-PSPID+5(2) CO '1234567890'.             "Eliminates templates
      SELECT * FROM PRPS
        WHERE PSPHI = PROJ-PSPNR
          AND PKOKR = '10'                          "Controlling area
          AND LOEVM <> 'X'                          "Not flagged deleted
          AND ( BELKZ = 'X' OR PLAKZ = 'X' ).
        CLEAR: KSTAR1, KSTAR2, KSTAR3, KSTAR4,
*               kstar5, kstar6, kstar7, kstar8,
                KSTAR5, KSTAR7, KSTAR8,
               KSTAR9, KSTARA, KSTARB, KSTARC,
               PLAN, BUDGET.
        PERFORM GET_FISCAL_DATA.
        IF P_CHECK = ' '.             "Print all WBS
          PERFORM BUILD_TABLE.
        ELSEIF PLAN <> 0 OR           "Print only none zero
           BUDGET <> 0 OR
           KSTAR1 <> 0 OR
           KSTAR2 <> 0 OR
           KSTAR3 <> 0 OR
           KSTAR4 <> 0 OR
           KSTAR5 <> 0 OR
*          kstar6 <> 0 or
           KSTAR7 <> 0 OR
           KSTAR8 <> 0 OR
           KSTAR9 <> 0 OR
           KSTARA <> 0 OR
           KSTARB <> 0 OR
           KSTARC <> 0.
          PERFORM BUILD_TABLE.
        ENDIF.
      ENDSELECT.                                    "PRPS end
   ENDIF.
 ENDSELECT.                                         "PROJ end

 IF SY-SUBRC EQ 0.
    PERFORM DISPLAY_TABLE.
 ENDIF.


*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
 FORMAT INTENSIFIED OFF.
 WRITE: /1 TEXT-RPT, SY-REPID,                          "Report Id
        80 T001-BUTXT,                                  "Company Name
       140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
 WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,  "Client
        70 TEXT-003,                                    "Report Title
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.           "Page Number
 WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS,              "Version
        78 TEXT-004, P_FYEAR.                           "Fiscal Year

 IF P_ENDNME = SPACE.                                   "Time Frame
    WRITE: /85 P_BEGNME.
 ELSE.
    WRITE: /70 P_BEGNME, TEXT-035, P_ENDNME, TEXT-036.
 ENDIF.
 WRITE: /85 TEXT-039.
 FORMAT INTENSIFIED ON.
 ULINE.
 PERFORM PRINT_VERT.
 WRITE:  2 TEXT-005,  12 TEXT-006,  41 TEXT-009,  51 TEXT-012,
        61 TEXT-007,  71 TEXT-018,  81 TEXT-017,  91 TEXT-024,
       101 TEXT-022, 111 TEXT-015, 121 TEXT-020,
       141 TEXT-027, 151 TEXT-026, 161 TEXT-028.
 PERFORM PRINT_VERT.
 WRITE:  6 TEXT-011,
          TEXT-010 UNDER TEXT-009,
          TEXT-021 UNDER TEXT-012,
          TEXT-008 UNDER TEXT-007,
          TEXT-019 UNDER TEXT-018,
          TEXT-023 UNDER TEXT-022,
*         text-014 under text-013,
          TEXT-016 UNDER TEXT-015,
          TEXT-025 UNDER TEXT-024,
          TEXT-029 UNDER TEXT-028,
          TEXT-040 UNDER TEXT-020.
 ULINE.
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

SORT BIG_TABLE BY PSPID WBS.
 LOOP AT BIG_TABLE.
  MOVE BIG_TABLE-WBS      TO WBS.
  MOVE BIG_TABLE-POST1    TO POST1.
  MOVE BIG_TABLE-WBSPOST1 TO WBSPOST1.

 AT NEW PSPID.
    NEW-PAGE.
    PERFORM PRINT_VERT.
    WRITE:  BIG_TABLE-PSPID UNDER TEXT-005,
       (28) POST1 UNDER TEXT-006.
    PERFORM PRINT_VERT.
 ENDAT.

 AT NEW WBS.
    PERFORM PRINT_VERT.
    WRITE: WBS UNDER TEXT-011,
      (28) WBSPOST1 UNDER TEXT-006.
 ENDAT.

 AT END OF WBS.                                "WBS element
    SUM.
    PERFORM AMOUNT_TOTALS.
 ENDAT.

 AT END OF PSPID.                              "WBS element
    PERFORM PRINT_VERT.
    WRITE: 41(139) SY-ULINE.
    SUM.
    PERFORM AMOUNT_TOTALS.

    CLEAR PERCENT.               "Calculate percent complete for project
    WRITE: /41 TEXT-041.
    IF BIG_TABLE-BUDGET = 0.
       PERCENT = 100.
    ELSE.
       COMPUTE PERCENT = BIG_TABLE-KSTARD * 100 / BIG_TABLE-BUDGET.
    ENDIF.
    WRITE: 50 PERCENT.
    ULINE.
 ENDAT.

 ENDLOOP.
*endif.
ENDFORM.

*------------------------  AMOUNT_TOTALS  ------------------------------
FORM AMOUNT_TOTALS.
PERFORM PRINT_VERT.
WRITE: 30 TEXT-033,
       (9) BIG_TABLE-PLAN   DECIMALS 0 UNDER TEXT-028.  "Total PLANS
PERFORM PRINT_VERT.
WRITE: 30 TEXT-032,
       (9) BIG_TABLE-BUDGET DECIMALS 0 UNDER TEXT-028.  "Total BUDGET
PERFORM PRINT_VERT.
WRITE: 30 TEXT-034,                                     "ACTUALS
       (9) BIG_TABLE-KSTAR1 DECIMALS 0 UNDER TEXT-009,  "Const.Material
       (9) BIG_TABLE-KSTAR2 DECIMALS 0 UNDER TEXT-012,  "Pipe
       (9) BIG_TABLE-KSTAR3 DECIMALS 0 UNDER TEXT-007,  "company labour
       (9) BIG_TABLE-KSTAR4 DECIMALS 0 UNDER TEXT-018,  "employee exp
       (9) BIG_TABLE-KSTAR5 DECIMALS 0 UNDER TEXT-022,  "contr services
*      (9) big_table-kstar6 decimals 0 under text-013,  "contr Expenses
       (9) BIG_TABLE-KSTAR7 DECIMALS 0 UNDER TEXT-015,  "contr supervisn
       (9) BIG_TABLE-KSTAR8 DECIMALS 0 UNDER TEXT-017,  "consultants
       (9) BIG_TABLE-KSTAR9 DECIMALS 0 UNDER TEXT-020,  "radiography
       (9) BIG_TABLE-KSTARA DECIMALS 0 UNDER TEXT-024,  "insp services
       (9) BIG_TABLE-KSTARB DECIMALS 0 UNDER TEXT-026,  "IDC
       (9) BIG_TABLE-KSTARC DECIMALS 0 UNDER TEXT-027.  "Other
FORMAT COLOR COL_POSITIVE.
WRITE: (9) BIG_TABLE-KSTARD DECIMALS 0 UNDER TEXT-028.  "Total project
FORMAT COLOR COL_BACKGROUND.
PERFORM PRINT_VERT.
WRITE: 6(164) SY-ULINE.
ENDFORM.
*----------------------- PRINT_HEADINGS -------------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------

*--------------------------  PRINT_VERT  -------------------------------
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:  /1 SY-VLINE,  40 SY-VLINE,  50 SY-VLINE,  60 SY-VLINE,
          70 SY-VLINE,  80 SY-VLINE,  90 SY-VLINE, 100 SY-VLINE,
         110 SY-VLINE, 120 SY-VLINE, 130 SY-VLINE, 140 SY-VLINE,
         150 SY-VLINE, 160 SY-VLINE, 170 SY-VLINE.
ENDFORM.

form heading_vernr.
*     perform print_vert.
*     format color col_positive.
*     write: 2 text-023, verna.
*     format color col_background.
ENDFORM.



FORM GET_FISCAL_DATA.
* COST TOTALS - External Postings
 SELECT * FROM COSP
    WHERE OBJNR = PRPS-OBJNR                    "Matching objects
      AND GJAHR = P_FYEAR                       "Fiscal Year selected
      AND ( VERSN = P_VERS                        "Version
          OR VERSN = '000' )                                     "omning
      AND WRTTP IN ('01', '04')
      AND BEKNZ IN ('S','H','L').               "Debit/Credit Indicator

    ADD COSP-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.

    CASE COSP-WRTTP.
      WHEN '01'.
        IF COSP-VERSN = P_VERS.                                  "omning
          PLAN = PLAN + VALUE.
        ENDIF.                                                   "omning
      WHEN '04'.
        IF COSP-VERSN = '000'.                                   "omning
          KSTAR = COSP-KSTAR.
          PERFORM CHECK_COLUMN.
        ENDIF.                                                   "omning
    ENDCASE.

 ENDSELECT.


* COST TOTALS - Internal Postings
 SELECT * FROM COSS
   WHERE OBJNR = PRPS-OBJNR                      "Matching objects
     AND GJAHR = P_FYEAR                         "Fiscal Year selected
     AND VERSN = '000'                  " Version  0 for actual - omning
     AND WRTTP IN ('04').                       "Record with actuals
*    and beknz in ('S','H','L').                 "Debit/Credit Indicator

    ADD COSS-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.

  KSTAR = COSS-KSTAR.
  PERFORM CHECK_COLUMN.

 ENDSELECT.
* End of COST TOTALS - Internal Postings

* Budget info
   SELECT * FROM BPJA                 "Budget Info
     WHERE OBJNR = PRPS-OBJNR
       AND WRTTP = '41'
       AND GJAHR = P_FYEAR
       AND VERSN = '000'.                               "omning
*     select * from jest
*       where objnr = prps-objnr
*         and inact <> 'X'
*          and stat in ('I0067','I0076')
*          order by stat.
*          case bpja-wrttp.
*             when '41'.
*                case jest-stat.
*               when 'I0076'.
*                  clear: budget.               "Deleted
*               when others.
           BUDGET = BPJA-WTJHR.         "Unlocked 97/10/06

*             endcase.
*      endselect.
  ENDSELECT.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM BUILD_TABLE
*-----------------------------------------------------------------------
*   Description:
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE.
    CLEAR BIG_TABLE.
    MOVE PROJ-PSPID      TO BIG_TABLE-PSPID.
    MOVE PRPS-POSID+7(4) TO BIG_TABLE-WBS.
    MOVE KSTAR1          TO BIG_TABLE-KSTAR1.
    MOVE KSTAR2          TO BIG_TABLE-KSTAR2.
    MOVE KSTAR3          TO BIG_TABLE-KSTAR3.
    MOVE KSTAR4          TO BIG_TABLE-KSTAR4.
    MOVE KSTAR5          TO BIG_TABLE-KSTAR5.
*   move kstar6          to big_table-kstar6.
    MOVE KSTAR7          TO BIG_TABLE-KSTAR7.
    MOVE KSTAR8          TO BIG_TABLE-KSTAR8.
    MOVE KSTAR9          TO BIG_TABLE-KSTAR9.
    MOVE KSTARA          TO BIG_TABLE-KSTARA.
    MOVE KSTARB          TO BIG_TABLE-KSTARB.
    MOVE KSTARC          TO BIG_TABLE-KSTARC.
    MOVE PLAN            TO BIG_TABLE-PLAN.
    MOVE BUDGET          TO BIG_TABLE-BUDGET.
    MOVE PROJ-POST1      TO BIG_TABLE-POST1.
    MOVE PRPS-POST1      TO BIG_TABLE-WBSPOST1.
    MOVE PROJ-VERNR      TO BIG_TABLE-VERNR.

    ADD BIG_TABLE-KSTAR1 FROM 1 TO 12 GIVING BIG_TABLE-KSTARD.
    APPEND BIG_TABLE.
ENDFORM.

FORM CHECK_COLUMN.
  IF KSTAR IN S_KSTAR1 AND S_KSTAR1 NE SPACE.
    KSTAR1 = KSTAR1 + VALUE.
  ELSEIF KSTAR IN S_KSTAR2 AND S_KSTAR2 NE SPACE.
    KSTAR2 = KSTAR2 + VALUE.
  ELSEIF KSTAR IN S_KSTAR3 AND S_KSTAR3 NE SPACE.
    KSTAR3 = KSTAR3 + VALUE.
  ELSEIF KSTAR IN S_KSTAR4 AND S_KSTAR4 NE SPACE.
    KSTAR4 = KSTAR4 + VALUE.
  ELSEIF KSTAR IN S_KSTAR5 AND S_KSTAR5 NE SPACE.
    KSTAR5 = KSTAR5 + VALUE.
* elseif kstar in s_kstar6 and s_kstar6 ne space.
*   kstar6 = kstar6 + value.
  ELSEIF KSTAR IN S_KSTAR7 AND S_KSTAR7 NE SPACE.
    KSTAR7 = KSTAR7 + VALUE.
  ELSEIF KSTAR IN S_KSTAR8 AND S_KSTAR8 NE SPACE.
    KSTAR8 = KSTAR8 + VALUE.
  ELSEIF KSTAR IN S_KSTAR9 AND S_KSTAR9 NE SPACE.
    KSTAR9 = KSTAR9 + VALUE.
  ELSEIF KSTAR IN S_KSTARA AND S_KSTARA NE SPACE.
    KSTARA = KSTARA + VALUE.
  ELSEIF KSTAR IN S_KSTARB AND S_KSTARB NE SPACE.
    KSTARB = KSTARB + VALUE.
  ELSE.
    KSTARC = KSTARC + VALUE.
  ENDIF.
ENDFORM.

*------------------- PRINT_VARIANT -------------------------------------
FORM PRINT_VARIANT.
WRITE: /20 TEXT-100, 40 TEXT-101.

WRITE: / SY-ULINE(19) UNDER TEXT-100.

WRITE: / TEXT-005 UNDER TEXT-100.                   "Projects
IF SPSPID+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT SPSPID.
  IF SPSPID+1(2) = 'EQ'.
     WRITE: SPSPID+1(2) UNDER TEXT-101, SPSPID+3(7).
  ELSE.
     WRITE: SPSPID+1(2) UNDER TEXT-101, SPSPID+3(7), SPSPID+27(7).
  ENDIF.
  WRITE: /.
ENDLOOP.

WRITE: / TEXT-102 UNDER TEXT-100.                  "Division
IF SDIV+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT SDIV.
  IF SDIV+1(2) = 'EQ'.
     WRITE: SDIV+1(2) UNDER TEXT-101, SDIV+3(8).
  ELSE.
     WRITE: SDIV+1(2) UNDER TEXT-101, SDIV+3(8), SDIV+11(8).
  ENDIF.
  WRITE: /.
ENDLOOP.

WRITE: / TEXT-038 UNDER TEXT-100.               "Cost Element Groupings
WRITE: / SY-ULINE(22) UNDER TEXT-100.

* kstar1 = Construction Material
WRITE: / TEXT-009 UNDER TEXT-100, TEXT-010.
IF S_KSTAR1+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR1.
  IF S_KSTAR1+1(2) = 'EQ'.
     WRITE: S_KSTAR1+1(2) UNDER TEXT-101, S_KSTAR1+3(10).
  ELSE.
     WRITE: S_KSTAR1+1(2) UNDER TEXT-101,
                              S_KSTAR1+3(10), S_KSTAR1+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstar2 = Pipe/Valves
WRITE: / TEXT-012 UNDER TEXT-100 NO-GAP, TEXT-021.
IF S_KSTAR2+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR2.
  IF S_KSTAR2+1(2) = 'EQ'.
     WRITE: S_KSTAR2+1(2) UNDER TEXT-101, S_KSTAR2+3(10).
  ELSE.
     WRITE: S_KSTAR2+1(2) UNDER TEXT-101,
                              S_KSTAR2+3(10), S_KSTAR2+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstar3 = Company Labour
WRITE: / TEXT-007 UNDER TEXT-100 NO-GAP, TEXT-008.
IF S_KSTAR3+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR3.
  IF S_KSTAR3+1(2) = 'EQ'.
     WRITE: S_KSTAR3+1(2) UNDER TEXT-101, S_KSTAR3+3(10).
  ELSE.
     WRITE: S_KSTAR3+1(2) UNDER TEXT-101,
                              S_KSTAR3+3(10), S_KSTAR3+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstar4 = Employee Expenses
WRITE: / TEXT-018 UNDER TEXT-100 NO-GAP, TEXT-019.
IF S_KSTAR4+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR4.
  IF S_KSTAR4+1(2) = 'EQ'.
     WRITE: S_KSTAR4+1(2) UNDER TEXT-101, S_KSTAR4+3(10).
  ELSE.
     WRITE: S_KSTAR4+1(2) UNDER TEXT-101,
                              S_KSTAR4+3(10), S_KSTAR4+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstar5 = Contract Services
WRITE: / TEXT-022 UNDER TEXT-100, TEXT-023.
IF S_KSTAR5+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR5.
  IF S_KSTAR5+1(2) = 'EQ'.
     WRITE: S_KSTAR5+1(2) UNDER TEXT-101, S_KSTAR5+3(10).
  ELSE.
     WRITE: S_KSTAR5+1(2) UNDER TEXT-101,
                              S_KSTAR5+3(10), S_KSTAR5+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstar6 = contract expenses
*write: / text-013 under text-100 no-gap, text-014.
*if s_kstar6+1(2) = space.
*   write: /.
*endif.
*loop at s_kstar6.
*  if s_kstar6+1(2) = 'EQ'.
*     write: s_kstar6+1(2) under text-101, s_kstar6+3(10).
*  else.
*     write: s_kstar6+1(2) under text-101,
*                              s_kstar6+3(10), s_kstar6+13(10).
*  endif.
*  write: /.
*endloop.

* kstar7 = contract supervision
WRITE: / TEXT-015 UNDER TEXT-100, TEXT-016.
IF S_KSTAR7+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR7.
  IF S_KSTAR7+1(2) = 'EQ'.
     WRITE: S_KSTAR7+1(2) UNDER TEXT-101, S_KSTAR7+3(10).
  ELSE.
     WRITE: S_KSTAR7+1(2) UNDER TEXT-101,
                              S_KSTAR7+3(10), S_KSTAR7+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstar8 = consultants
WRITE: / TEXT-017 UNDER TEXT-100.
IF S_KSTAR8+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR8.
  IF S_KSTAR8+1(2) = 'EQ'.
     WRITE: S_KSTAR8+1(2) UNDER TEXT-101, S_KSTAR8+3(10).
  ELSE.
     WRITE: S_KSTAR8+1(2) UNDER TEXT-101,
                              S_KSTAR8+3(10), S_KSTAR8+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstar9 = radiography
WRITE: / TEXT-020 UNDER TEXT-100.
IF S_KSTAR9+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTAR9.
  IF S_KSTAR9+1(2) = 'EQ'.
     WRITE: S_KSTAR9+1(2) UNDER TEXT-101, S_KSTAR9+3(10).
  ELSE.
     WRITE: S_KSTAR9+1(2) UNDER TEXT-101,
                              S_KSTAR9+3(10), S_KSTAR9+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstara = inspection services
WRITE: / TEXT-024 UNDER TEXT-100 NO-GAP, TEXT-025.
IF S_KSTARA+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTARA.
  IF S_KSTARA+1(2) = 'EQ'.
     WRITE: S_KSTARA+1(2) UNDER TEXT-101, S_KSTARA+3(10).
  ELSE.
     WRITE: S_KSTARA+1(2) UNDER TEXT-101,
                              S_KSTARA+3(10), S_KSTARA+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

* kstarb = interest during construction
WRITE: / TEXT-026 UNDER TEXT-100.
IF S_KSTARB+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT S_KSTARB.
  IF S_KSTARB+1(2) = 'EQ'.
     WRITE: S_KSTARB+1(2) UNDER TEXT-101, S_KSTARB+3(10).
  ELSE.
     WRITE: S_KSTARB+1(2) UNDER TEXT-101,
                              S_KSTARB+3(10), S_KSTARB+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

ENDFORM.
