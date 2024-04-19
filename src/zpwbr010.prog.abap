REPORT ZPWBR001 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 255
                MESSAGE-ID ZP.
************************************************************************
*  Author:   Dorothy Bialkowska
*  Brief Description:
*     - The purpose of this program is to produce a report summarizing
*       projects based on following criteria: Project Type, Priority and
*       WBS elements.
*
************************************************************************

TABLES:   BPJA, COSP, JEST, PRPS, TCJ1T, TCN7T, T001T.

DATA:     VALUE       LIKE     COSP-WTG001,
          PLAN        LIKE     VALUE,
          PLAN1       LIKE     VALUE,
          PLAN2       LIKE     VALUE,
          COST        LIKE     VALUE,
          DELTA1      LIKE     VALUE,
          DELTA2      LIKE     VALUE,
          BUDGET      LIKE     BPJA-WTJHR,
          APPROVAL    LIKE     BUDGET.

DATA:    BEGIN OF BIG_TAB OCCURS 100000,
               PRIORITY           LIKE     PRPS-PSPRI,
               WBS(4)             TYPE     C,
               OBJNR              LIKE     PRPS-OBJNR,
               TITLE              LIKE     PRPS-POST1,
               PLAN               LIKE     COSP-WTG001,
               COST               LIKE     COSP-WTG001,
               DELTA1             LIKE     COSP-WTG001,
               BUDGET             LIKE     COSP-WTG001,
               APPROVAL           LIKE     COSP-WTG001,
               DELTA2             LIKE     COSP-WTG001,
               PLAN1              LIKE     BPJA-WTJHR,
               PLAN2              LIKE     BPJA-WTJHR,
         END OF BIG_TAB.

DATA:    BEGIN OF A_TAB OCCURS 1000,
               DIVNR(2)           TYPE     C,
               PRIORITY           LIKE     PRPS-PSPRI,
               WBSEL(4)           TYPE     C,
               OBJNR              LIKE     PRPS-OBJNR,
               TITLE              LIKE     PRPS-POST1,
               PLAN               LIKE     COSP-WTG001,
               COST               LIKE     COSP-WTG001,
               DELTA1             LIKE     COSP-WTG001,
               BUDGET             LIKE     COSP-WTG001,
               APPROVAL           LIKE     COSP-WTG001,
               DELTA2             LIKE     COSP-WTG001,
               PLAN1              LIKE     BPJA-WTJHR,
               PLAN2              LIKE     BPJA-WTJHR,
         END OF A_TAB.

DATA:    BEGIN OF B_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF B_TAB.

DATA:    BEGIN OF C_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF C_TAB.

DATA:    BEGIN OF D_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF D_TAB.

DATA:    BEGIN OF E_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF E_TAB.

DATA:    BEGIN OF F_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF F_TAB.

DATA:    BEGIN OF G_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF G_TAB.

DATA:    BEGIN OF H_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF H_TAB.

DATA:    BEGIN OF I_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF I_TAB.

DATA:    BEGIN OF J_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF J_TAB.

DATA:    BEGIN OF K_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF K_TAB.

DATA:    BEGIN OF L_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF L_TAB.

DATA:    BEGIN OF Z_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF Z_TAB.

DATA:    BEGIN OF ZZ_TAB OCCURS 1000.
        INCLUDE STRUCTURE A_TAB.
DATA:    END OF ZZ_TAB.

DATA:    BEGIN OF DIV_TAB OCCURS 50,
               DIVNR(2)           TYPE    C,
         END OF DIV_TAB.

DATA:     LEN           TYPE I,
          COUNTER       TYPE I,
          START         TYPE I,
          LINE          TYPE I,
          SYMBOL(4)     TYPE C,
          CURRENT(2)    TYPE C,
          NEXT          LIKE CURRENT,
          TEMP          LIKE CURRENT,
          TOT1          LIKE COSP-WTG001,
          TOT2          LIKE TOT1,
          TOT3          LIKE TOT1,
          TOT4          LIKE TOT1,
          TOT5          LIKE TOT1,
          TOT6          LIKE TOT1,
          TOT7          LIKE TOT1,
          TOT8          LIKE TOT1,
          BIGTOT1       LIKE TOT1,
          BIGTOT2       LIKE TOT1,
          BIGTOT3       LIKE TOT1,
          BIGTOT4       LIKE TOT1,
          BIGTOT5       LIKE TOT1,
          BIGTOT6       LIKE TOT1,
          BIGTOT7       LIKE TOT1,
          BIGTOT8       LIKE TOT1,
          GRAND1        LIKE TOT1,
          GRAND2        LIKE TOT1,
          GRAND3        LIKE TOT1,
          GRAND4        LIKE TOT1,
          GRAND5        LIKE TOT1,
          GRAND6        LIKE TOT1,
          GRAND7        LIKE TOT1,
          GRAND8        LIKE TOT1,
          MEGA1         LIKE TOT1,
          MEGA2         LIKE TOT1,
          MEGA3         LIKE TOT1,
          MEGA4         LIKE TOT1,
          MEGA5         LIKE TOT1,
          MEGA6         LIKE TOT1,
          MEGA7         LIKE TOT1,
          MEGA8         LIKE TOT1.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS S_CCODE FOR T001T-BUKRS DEFAULT 'UGL' NO INTERVALS.
PARAMETERS: P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS S_DIV   FOR PRPS-POSID+0(2).
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 6(26) TEXT-013.
PARAMETER P_VERS1 LIKE TKVST-VERSI DEFAULT '0'.
SELECTION-SCREEN COMMENT 52(24) TEXT-014.
PARAMETER P_VERS2 LIKE TKVST-VERSI DEFAULT '0'.
SELECTION-SCREEN END OF LINE .
SELECTION-SCREEN END OF BLOCK BOX.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS S_WBE1  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE2  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE20 FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE4  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE5  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE7  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE6  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE3  FOR PRPS-POSID+19(4).
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS S_WBE8  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE21 FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE9  FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE10 FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE12 FOR PRPS-POSID+19(4).
SELECT-OPTIONS S_WBE11 FOR PRPS-POSID+19(4).
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-029.
PARAMETERS:    P_CHECK1     AS CHECKBOX,
               P_CHECK2     AS CHECKBOX.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 33(26) TEXT-030.
SELECTION-SCREEN END OF BLOCK BOX3.

AT SELECTION-SCREEN.
  IF ( P_CHECK1 EQ 'X' AND P_CHECK2 EQ 'X' ) OR ( P_CHECK1 EQ SPACE
      AND P_CHECK2 EQ SPACE ).
    MESSAGE E002 WITH TEXT-031 TEXT-032.
  ENDIF.
  IF P_CHECK2 EQ 'X'.
    LEN = STRLEN( S_DIV-LOW ).
    IF LEN < 2 AND S_DIV-LOW NE SPACE.
      MESSAGE E003.
    ENDIF.
    LEN = STRLEN( S_DIV-HIGH ).
    IF LEN < 2 AND S_DIV-HIGH NE SPACE.
      MESSAGE E003.
    ENDIF.
  ENDIF.

*   End of the selection screen
*-----------------------------------------------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-011, SY-DATUM, 240 TEXT-012, SY-PAGNO.
  WRITE: / SY-UZEIT UNDER SY-DATUM, SY-REPID UNDER TEXT-012.
  FORMAT COLOR 7 ON.
  WRITE: /115(150) TEXT-010.
  ULINE.
  FORMAT INTENSIFIED ON.

*-----------------------------------------------------------------------
START-OF-SELECTION.
  IF P_CHECK1 = 'X'.
    PERFORM PRINT_SUBHEADING USING 115 TEXT-001.

    PERFORM COLLECT_DATA1 TABLES S_WBE1 USING '08' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '08'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      ULINE.
      PERFORM PRINT_TOTALS.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA TABLES S_WBE2 USING '01' '1' '2' '3' '4' '5'
                                              '6' '7' '8'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '01'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      ULINE.
      PERFORM PRINT_TOTALS.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE20 USING '02' '5' '7' .
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '02'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      ULINE.
      PERFORM PRINT_TOTALS.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE4 USING '03' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '03'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE5 USING '04' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '04'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE7 USING '05' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '05'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE6 USING '06' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '06'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE3 USING '07' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '07'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

                                       "~~~~~~~~~~~~~~~~~~"
    SKIP 3.
    PERFORM PRINT_SUBHEADING USING 104 TEXT-003.
    PERFORM COLLECT_DATA TABLES S_WBE8 USING '01' '1' '2' '3' '4' '5'
                                             '6' '7' '8'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '01'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE21 USING '02' '5' '7' .
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '02'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      ULINE.
      PERFORM PRINT_TOTALS.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE9 USING '03' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '03'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE10 USING '04' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '04'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE12 USING '05' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '05'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP 3.
      CLEAR LINE.
    ENDIF.

    PERFORM COLLECT_DATA1 TABLES S_WBE11 USING '06' '5' '7'.
    DESCRIBE TABLE BIG_TAB LINES LINE.
    IF LINE GE 1.
      PERFORM PRINT_TYPE USING '06'.
      PERFORM PRINT_HEADINGS.
      PERFORM BUILD_BODY.
      PERFORM PRINT_TOTALS.
      ULINE.
      SKIP.
      CLEAR LINE.
    ENDIF.

    ULINE.
    FORMAT COLOR 5 ON.
    WRITE: /50 TEXT-033, 73 GRAND1, GRAND2, 118 GRAND3, 141 GRAND4,
            164 GRAND5, 187 GRAND6, 211 GRAND7, 234 GRAND8.
    FORMAT COLOR 5 OFF.
    ULINE.
  ENDIF.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
  IF P_CHECK2 = 'X'.
    CLEAR: TOT1, TOT2, TOT3, TOT4, TOT5, TOT6, TOT7, TOT8, BIGTOT1,
           BIGTOT2, BIGTOT3, BIGTOT4, BIGTOT5, BIGTOT6, BIGTOT7,
           BIGTOT8, GRAND1, GRAND2, GRAND3, GRAND4, GRAND5, GRAND6,
           GRAND7, GRAND8.

    COUNTER = 1.
    SELECT * FROM PRPS
       WHERE PBUKR IN S_CCODE
       AND ( PRART = '01' OR PRART = '02' OR PRART = '03' OR
             PRART = '04' OR PRART = '05' OR PRART = '06' OR
             PRART = '07' OR PRART = '08' )
       AND PKOKR = '10'
       AND LOEVM NE 'X'
       ORDER BY POSID.

      CHECK PRPS-POSID+0(2) IN S_DIV.

      LEN = STRLEN( PRPS-POSID ).
      CHECK LEN GE 4.
      START = LEN - 4.
      SYMBOL = PRPS-POSID+START(4).
      CHECK SYMBOL IN S_WBE1 OR SYMBOL IN S_WBE2 OR SYMBOL IN S_WBE3 OR
            SYMBOL IN S_WBE4 OR SYMBOL IN S_WBE5 OR SYMBOL IN S_WBE6 OR
            SYMBOL IN S_WBE7 OR SYMBOL IN S_WBE8 OR SYMBOL IN S_WBE9 OR
            SYMBOL IN S_WBE10 OR SYMBOL IN S_WBE11 OR
            SYMBOL IN S_WBE12 OR SYMBOL IN S_WBE20 OR SYMBOL IN S_WBE21.

      IF COUNTER = 1.
        MOVE PRPS-POSID+0(2) TO CURRENT.
        MOVE PRPS-POSID+0(2) TO NEXT.
        MOVE CURRENT TO DIV_TAB-DIVNR.
        APPEND DIV_TAB.
        CLEAR DIV_TAB.
        COUNTER = COUNTER + 1.
      ELSE.
        MOVE PRPS-POSID+0(2) TO CURRENT.
      ENDIF.
      IF CURRENT NE NEXT.
        MOVE CURRENT TO DIV_TAB-DIVNR.
        APPEND DIV_TAB.
        CLEAR DIV_TAB.
      ENDIF.

      PERFORM GET_FISCAL_DATA.
      PERFORM GET_BUDGET.

      SELECT SINGLE * FROM JEST
         WHERE OBJNR = PRPS-OBJNR
         AND STAT = 'I0063'.
      IF SY-SUBRC EQ 0.
        APPROVAL = BUDGET.
      ENDIF.
      PERFORM GET_PLAN USING P_VERS1 PLAN1.
      PERFORM GET_PLAN USING P_VERS2 PLAN2.

      DELTA1 = COST - PLAN.
      DELTA2 = BUDGET - APPROVAL.

      IF PRPS-PRART = '01' AND SYMBOL IN S_WBE2.
        PERFORM BUILD_GENTAB TABLES B_TAB.
      ENDIF.

      IF PRPS-PRART = '01' AND SYMBOL IN S_WBE8.
        PERFORM BUILD_GENTAB TABLES H_TAB.
      ENDIF.

      IF PRPS-PRART = '02' AND SYMBOL IN S_WBE20.
        PERFORM BUILD_GENTAB TABLES Z_TAB.
      ENDIF.

      IF PRPS-PRART = '02' AND SYMBOL IN S_WBE21.
        PERFORM BUILD_GENTAB TABLES ZZ_TAB.
      ENDIF.

      IF PRPS-PRART = '03' AND SYMBOL IN S_WBE4.
        PERFORM BUILD_GENTAB TABLES D_TAB.
      ENDIF.

      IF PRPS-PRART = '03' AND SYMBOL IN S_WBE9.
        PERFORM BUILD_GENTAB TABLES I_TAB.
      ENDIF.

      IF PRPS-PRART = '04' AND SYMBOL IN S_WBE5.
        PERFORM BUILD_GENTAB TABLES E_TAB.
      ENDIF.

      IF PRPS-PRART = '04' AND SYMBOL IN S_WBE10.
        PERFORM BUILD_GENTAB TABLES J_TAB.
      ENDIF.

      IF PRPS-PRART = '05' AND SYMBOL IN S_WBE7.
        PERFORM BUILD_GENTAB TABLES G_TAB.
      ENDIF.

      IF PRPS-PRART = '05' AND SYMBOL IN S_WBE12.
        PERFORM BUILD_GENTAB TABLES L_TAB.
      ENDIF.

      IF PRPS-PRART = '06' AND SYMBOL IN S_WBE6.
        PERFORM BUILD_GENTAB TABLES F_TAB.
      ENDIF.

      IF PRPS-PRART = '06' AND SYMBOL IN S_WBE11.
        PERFORM BUILD_GENTAB TABLES K_TAB.
      ENDIF.

      IF PRPS-PRART = '07' AND SYMBOL IN S_WBE3.
        PERFORM BUILD_GENTAB TABLES C_TAB.
      ENDIF.

      IF PRPS-PRART = '08' AND SYMBOL IN S_WBE1.
        PERFORM BUILD_GENTAB TABLES A_TAB.
      ENDIF.

      MOVE PRPS-POSID+0(2) TO NEXT.
      CLEAR: COST, PLAN, BUDGET, APPROVAL.
    ENDSELECT.

    SORT A_TAB BY DIVNR PRIORITY WBSEL.
    SORT B_TAB BY DIVNR PRIORITY WBSEL.
    SORT C_TAB BY DIVNR PRIORITY WBSEL.
    SORT D_TAB BY DIVNR PRIORITY WBSEL.
    SORT E_TAB BY DIVNR PRIORITY WBSEL.
    SORT F_TAB BY DIVNR PRIORITY WBSEL.
    SORT G_TAB BY DIVNR PRIORITY WBSEL.
    SORT H_TAB BY DIVNR PRIORITY WBSEL.
    SORT I_TAB BY DIVNR PRIORITY WBSEL.
    SORT J_TAB BY DIVNR PRIORITY WBSEL.
    SORT K_TAB BY DIVNR PRIORITY WBSEL.
    SORT L_TAB BY DIVNR PRIORITY WBSEL.
    SORT Z_TAB BY DIVNR PRIORITY WBSEL.
    SORT ZZ_TAB BY DIVNR PRIORITY WBSEL.

    LOOP AT DIV_TAB.
      PERFORM PRINT_SUBHEADING USING 115 TEXT-001.
      WRITE: / 'DIVISION: ', DIV_TAB-DIVNR.

      READ TABLE A_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES A_TAB USING '08'.
      ENDIF.

      READ TABLE B_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES B_TAB USING '01'.
      ENDIF.

      READ TABLE Z_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES Z_TAB USING '02'.
      ENDIF.

      READ TABLE D_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES D_TAB USING '03'.
      ENDIF.

      READ TABLE E_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES E_TAB USING '04'.
      ENDIF.

      READ TABLE G_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES G_TAB USING '05'.
      ENDIF.

      READ TABLE F_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES F_TAB USING '06'.
      ENDIF.


      READ TABLE C_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES C_TAB USING '07'.
      ENDIF.
                                       "~~~~~~~~~~~~~~~~~~~"
      SKIP 3.
      PERFORM PRINT_SUBHEADING USING 104 TEXT-003.

      READ TABLE H_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES H_TAB USING '01'.
      ENDIF.

      READ TABLE ZZ_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES ZZ_TAB USING '02'.
      ENDIF.

      READ TABLE I_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES I_TAB USING '03'.
      ENDIF.

      READ TABLE J_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES J_TAB USING '04'.
      ENDIF.

      READ TABLE L_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES L_TAB USING '05'.
      ENDIF.

      READ TABLE K_TAB WITH KEY DIVNR = DIV_TAB-DIVNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM MATCH_FOUND TABLES K_TAB USING '06'.
      ENDIF.

      ULINE.
      FORMAT COLOR 4 ON.
      WRITE: /50 TEXT-036, 73 GRAND1, GRAND2, 118 GRAND3, 141 GRAND4,
              164 GRAND5, 187 GRAND6, 211 GRAND7, 234 GRAND8.
      FORMAT COLOR 4 OFF.
      ULINE.

      MEGA1 = MEGA1 + GRAND1.
      MEGA2 = MEGA2 + GRAND2.
      MEGA3 = MEGA3 + GRAND3.
      MEGA4 = MEGA4 + GRAND4.
      MEGA5 = MEGA5 + GRAND5.
      MEGA6 = MEGA6 + GRAND6.
      MEGA7 = MEGA7 + GRAND7.
      MEGA8 = MEGA8 + GRAND8.

      CLEAR: GRAND1, GRAND2, GRAND3, GRAND4, GRAND5, GRAND6, GRAND7,
             GRAND8.
      WRITE: / 'END OF DIVISION ', DIV_TAB-DIVNR.
      SKIP 3.
    ENDLOOP.

    SKIP 3.
    ULINE.
    FORMAT COLOR 5 ON.
    WRITE: /50 TEXT-033, 73 MEGA1, MEGA2, 118 MEGA3, 141 MEGA4,
            164 MEGA5, 187 MEGA6, 211 MEGA7, 234 MEGA8.
    FORMAT COLOR 5 OFF.
    ULINE.

  ENDIF.

************************************************************************
*                 Subroutines used by a program                        *

*-----------------------------------------------------------------------
*     FORM PRINT_HEADINGS
*-----------------------------------------------------------------------
*   Description:
*   - This is used to display column headings for report.  Column
*     headings are displayed for each project type.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
  WRITE: /1(255) SY-ULINE.
  FORMAT COLOR 3 INTENSIFIED OFF.
  PERFORM PRINT_DIVIDER.
  WRITE: 3 TEXT-015 , 32 TEXT-016, 81 TEXT-017, 100 TEXT-019, 125
         TEXT-020, 146 TEXT-022, 169 TEXT-023, 190 TEXT-025, 216
         TEXT-026, 239 TEXT-026.
  PERFORM PRINT_DIVIDER.
  WRITE: 76 TEXT-018, 121 TEXT-021, 166 TEXT-024, 217 TEXT-027,
         P_VERS1, 240 TEXT-027, P_VERS2."255 SY-VLINE.
  FORMAT COLOR 1.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_DIVIDER
*-----------------------------------------------------------------------
*   Description:
*   - This is used to display vertical lines.
*-----------------------------------------------------------------------
FORM PRINT_DIVIDER.
  WRITE: /1 SY-VLINE, 16 SY-VLINE, 71 SY-VLINE, 94 SY-VLINE, 117
            SY-VLINE, 140 SY-VLINE, 163 SY-VLINE, 186 SY-VLINE, 209
            SY-VLINE, 232 SY-VLINE, 255 SY-VLINE.
ENDFORM.

*-----------------------------------------------------------------------
*       FORM PRINT_DIVIDER1
*-----------------------------------------------------------------------
*   Description:
*   - A variation of the 'PRINT_DIVIDER'.  The only difference is that
*     a vertical line is printed without 'LINE FEED'.
*-----------------------------------------------------------------------
FORM PRINT_DIVIDER1.
  WRITE: 1 SY-VLINE, 16 SY-VLINE, 71 SY-VLINE, 94 SY-VLINE, 117
            SY-VLINE, 140 SY-VLINE, 163 SY-VLINE, 186 SY-VLINE, 209
            SY-VLINE, 232 SY-VLINE, 255 SY-VLINE.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_DIVIDER2
*-----------------------------------------------------------------------
*   Description:
*   - Another variation of 'PRINT_DIVIDER'
*-----------------------------------------------------------------------
FORM PRINT_DIVIDER2.
  WRITE: /1 SY-VLINE, 71 SY-VLINE, 94 SY-VLINE, 117
            SY-VLINE, 140 SY-VLINE, 163 SY-VLINE, 186 SY-VLINE, 209
            SY-VLINE, 232 SY-VLINE, 255 SY-VLINE.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_DIVIDER3
*-----------------------------------------------------------------------
*   Description:
*   - Another variation of 'PRINT_DIVIDER'
*-----------------------------------------------------------------------
FORM PRINT_DIVIDER3.
  WRITE: 1 SY-VLINE, 71 SY-VLINE, 94 SY-VLINE, 117
            SY-VLINE, 140 SY-VLINE, 163 SY-VLINE, 186 SY-VLINE, 209
            SY-VLINE, 232 SY-VLINE, 255 SY-VLINE.
ENDFORM.

*-----------------------------------------------------------------------
*    FORM PRINT_SUBHEADINGS.
*-----------------------------------------------------------------------
*   Description:
*   - This routine is used to display names given to the different parts
*     of the report.
*-----------------------------------------------------------------------
FORM PRINT_SUBHEADING USING LOC TEXT.
  FORMAT INTENSIFIED ON.
  FORMAT COLOR 3.
  WRITE: /LOC(255) TEXT.
  FORMAT INTENSIFIED OFF.
  FORMAT COLOR 1.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_TYPE.
*-----------------------------------------------------------------------
*   Description:
*   - The body of the report is divided to reference different project
*     types.  Eech project type is dealt one at the time.  This routine
*     display the name of the prject type in the upper case.
*-----------------------------------------------------------------------
FORM PRINT_TYPE USING TYPE.
  DATA: L_LEN     TYPE I.

  SELECT SINGLE * FROM TCJ1T
     WHERE LANGU = SY-LANGU
     AND PRART = TYPE.
  FORMAT INTENSIFIED OFF.
  TRANSLATE TCJ1T-PRATX TO UPPER CASE.
  L_LEN = STRLEN( TCJ1T-PRATX ).
  L_LEN = L_LEN + 1.
  SKIP.
  WRITE: /3(L_LEN) TCJ1T-PRATX COLOR 2.
  FORMAT INTENSIFIED OFF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM COLLECT_DATA.
*-----------------------------------------------------------------------
*   Description:
*   - There will be several variation of this subroutine to accomodate
*     not only different project types but different priorities as well.
*     This is a major form for a program and its purpose is to collect
*     and display all data for a selected part of the report.
*-----------------------------------------------------------------------
FORM COLLECT_DATA TABLES WBS_EL STRUCTURE S_WBE1
                  USING TYPE PRITY_1 PRITY_2 PRITY_3 PRITY_4 PRITY_5
                  PRITY_6 PRITY_7 PRITY_8.

  DATA:     L_LEN           TYPE I,
            L_START         TYPE I,
            L_SYMBOL(4)     TYPE C.


  SELECT * FROM PRPS
     WHERE PBUKR IN S_CCODE
     AND PRART = TYPE
     AND PKOKR = '10'
     AND LOEVM NE 'X'
     AND ( PSPRI = PRITY_1 OR PSPRI = PRITY_2 OR PSPRI = PRITY_3 OR
           PSPRI = PRITY_4 OR PSPRI = PRITY_5 OR PSPRI = PRITY_6 OR
           PSPRI = PRITY_7 OR PSPRI = PRITY_8 )
     ORDER BY PSPRI.


    L_LEN = STRLEN( PRPS-POSID ).
    CHECK L_LEN GE 4.
    L_START = L_LEN - 4.

    L_SYMBOL = PRPS-POSID+L_START(4).
    CHECK L_SYMBOL IN WBS_EL.

    PERFORM GET_FISCAL_DATA.
    PERFORM GET_BUDGET.

    SELECT SINGLE * FROM JEST
       WHERE OBJNR = PRPS-OBJNR
       AND STAT = 'I0063'.
    IF SY-SUBRC EQ 0.
      APPROVAL = BUDGET.
    ENDIF.
    PERFORM GET_PLAN USING P_VERS1 PLAN1.
    PERFORM GET_PLAN USING P_VERS2 PLAN2.

    DELTA1 = COST - PLAN.
    DELTA2 = BUDGET - APPROVAL.

    PERFORM BUILD_TABLE USING L_SYMBOL.
  ENDSELECT.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_FISCAL_DATA
*-----------------------------------------------------------------------
*   Description:
*   - For every WBS element, this subroutine calculates year-to-date
*     approved plan and cost.  Consideration is given to the selected
*     fiscal year.
*-----------------------------------------------------------------------
FORM GET_FISCAL_DATA.
  SELECT * FROM COSP
     WHERE OBJNR = PRPS-OBJNR
     AND  ( WRTTP = '01' OR WRTTP = '04' )
     AND GJAHR = P_FYEAR
     ORDER BY OBJNR.

    IF COSP-WRTTP = '01'.
      CHECK COSP-VERSN = P_VERS1.
      PERFORM CALC_VALUE.
      PLAN = PLAN + VALUE.
      CLEAR VALUE.
    ENDIF.
    IF COSP-WRTTP = '04'.
      PERFORM CALC_VALUE.
      COST = COST + VALUE.
      CLEAR VALUE.
    ENDIF.
  ENDSELECT.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM CALC_VALUE.
*-----------------------------------------------------------------------
*   Description:
*   - 'Year-to-date' values are calulated from the beginning of a fiscal
*     year up to (and included) current month.
*-----------------------------------------------------------------------
FORM CALC_VALUE.
  IF SY-DATUM+4(2) = '01'.
    VALUE = COSP-WTG001.
  ENDIF.
  IF SY-DATUM+4(2) = '02'.
    VALUE = COSP-WTG001 + COSP-WTG002.
  ENDIF.
  IF SY-DATUM+4(2) = '03'.
    VALUE = COSP-WTG001 + COSP-WTG002 + COSP-WTG003.
  ENDIF.
  IF SY-DATUM+4(2) = '04'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004.
  ENDIF.
  IF SY-DATUM+4(2) = '05'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005.
  ENDIF.
  IF SY-DATUM+4(2) = '06'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006.
  ENDIF.
  IF SY-DATUM+4(2) = '07'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006 + COSP-WTG007.
  ENDIF.
  IF SY-DATUM+4(2) = '08'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006 + COSP-WTG007 + COSP-WTG008.
  ENDIF.
  IF SY-DATUM+4(2) = '09'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006 + COSP-WTG007 + COSP-WTG008 +
            COSP-WTG009.
  ENDIF.
  IF SY-DATUM+4(2) = '10'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006 + COSP-WTG007 + COSP-WTG008 +
            COSP-WTG009 + COSP-WTG010.
  ENDIF.
  IF SY-DATUM+4(2) = '11'.

    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006 + COSP-WTG007 + COSP-WTG008 +
            COSP-WTG009 + COSP-WTG010 + COSP-WTG011.

  ENDIF.
  IF SY-DATUM+4(2) = '12'.
    VALUE =
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006 + COSP-WTG007 + COSP-WTG008 +
            COSP-WTG009 + COSP-WTG010 + COSP-WTG011 + COSP-WTG012.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_BUDGET.
*-----------------------------------------------------------------------
*   Description:
*   - Yearly budget for WBS element.
*-----------------------------------------------------------------------
FORM GET_BUDGET.
  SELECT * FROM BPJA
     WHERE OBJNR = PRPS-OBJNR
     AND WRTTP = '41'
     AND GJAHR = P_FYEAR
     ORDER BY OBJNR.
    BUDGET = BUDGET + BPJA-WTJHV.
  ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_PLAN USING VERSION TOTAL
*-----------------------------------------------------------------------
*   Description:
*   - This subrotine is used to calculate Total Plan for a given WBS
*     element and project version.
*-----------------------------------------------------------------------
FORM GET_PLAN USING VERSION TOTAL.
  SELECT * FROM COSP
     WHERE OBJNR = PRPS-OBJNR
     AND WRTTP = '01'
     AND GJAHR = P_FYEAR
     AND VERSN = VERSION.
    TOTAL = TOTAL +
            COSP-WTG001 + COSP-WTG002 + COSP-WTG003 + COSP-WTG004 +
            COSP-WTG005 + COSP-WTG006 + COSP-WTG007 + COSP-WTG008 +
            COSP-WTG009 + COSP-WTG010 + COSP-WTG011 + COSP-WTG012.
  ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BUILD_TABLE.
*-----------------------------------------------------------------------
*   Description:
*   - When a 'generic' form of report is selected, this subroutine
*     stores all required information in BIG_TAB table.
*-----------------------------------------------------------------------
FORM BUILD_TABLE USING WBS.
  MOVE PRPS-OBJNR     TO BIG_TAB-OBJNR.
  MOVE WBS            TO BIG_TAB-WBS.
  MOVE PRPS-POST1     TO BIG_TAB-TITLE.
  MOVE PRPS-PSPRI     TO BIG_TAB-PRIORITY.
  MOVE PLAN           TO BIG_TAB-PLAN.
  MOVE COST           TO BIG_TAB-COST.
  MOVE DELTA1         TO BIG_TAB-DELTA1.
  MOVE BUDGET         TO BIG_TAB-BUDGET.
  MOVE APPROVAL       TO BIG_TAB-APPROVAL.
  MOVE DELTA2         TO BIG_TAB-DELTA2.
  MOVE PLAN1          TO BIG_TAB-PLAN1.
  MOVE PLAN2          TO BIG_TAB-PLAN2.

  APPEND BIG_TAB.
  CLEAR BIG_TAB.

  BIGTOT1 = BIGTOT1 + PLAN.
  BIGTOT2 = BIGTOT2 + COST.
  BIGTOT3 = BIGTOT3 + DELTA1.
  BIGTOT4 = BIGTOT4 + BUDGET.
  BIGTOT5 = BIGTOT5 + APPROVAL.
  BIGTOT6 = BIGTOT6 + DELTA2.
  BIGTOT7 = BIGTOT7 + PLAN1.
  BIGTOT8 = BIGTOT8 + PLAN2.

  CLEAR: PLAN, PLAN1, PLAN2, COST, VALUE, BUDGET, APPROVAL, DELTA1,
         DELTA2.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BUILD_BODY.
*-----------------------------------------------------------------------
*   Description:
*   - All data that satisfy selection criteria and all calculted data
*     for a given WBS element is stored in the internal table.  This
*     subroutine display the table in the form required by a user.
*-----------------------------------------------------------------------
FORM BUILD_BODY.
  DATA:     L_STRING     LIKE    PRPS-POST1.

  SORT BIG_TAB BY PRIORITY WBS.

  LOOP AT BIG_TAB.
    ON CHANGE OF BIG_TAB-PRIORITY.
      SELECT SINGLE * FROM TCN7T
         WHERE LANGU = SY-LANGU
         AND NPRIO = BIG_TAB-PRIORITY.
      WRITE: /1(255) SY-ULINE.
      WRITE: /3 TEXT-028, TCN7T-KTEXT, 1 SY-VLINE, 255 SY-VLINE.
      WRITE: /1(255) SY-ULINE.
    ENDON.

    MOVE BIG_TAB-TITLE TO L_STRING.
    AT END OF WBS.
      SUM.
    IF ( BIG_TAB-PLAN NE 0 ) OR ( BIG_TAB-COST NE 0 ) OR
       ( BIG_TAB-BUDGET NE 0 ) OR ( BIG_TAB-APPROVAL NE 0 ) OR
       ( BIG_TAB-PLAN1 NE 0 ) OR ( BIG_TAB-PLAN2 NE 0 ).
        PERFORM PRINT_DIVIDER2.
        WRITE: 7 BIG_TAB-WBS, 18 L_STRING, 73 BIG_TAB-PLAN,
               BIG_TAB-COST, 118 BIG_TAB-DELTA1, 141 BIG_TAB-BUDGET,
               164 BIG_TAB-APPROVAL, 187 BIG_TAB-DELTA2,
               211 BIG_TAB-PLAN1, 234 BIG_TAB-PLAN2.
        TOT1 = TOT1 + BIG_TAB-PLAN.
        TOT2 = TOT2 + BIG_TAB-COST.
        TOT3 = TOT3 + BIG_TAB-DELTA1.
        TOT4 = TOT4 + BIG_TAB-BUDGET.
        TOT5 = TOT5 + BIG_TAB-APPROVAL.
        TOT6 = TOT6 + BIG_TAB-DELTA2.
        TOT7 = TOT7 + BIG_TAB-PLAN1.
        TOT8 = TOT8 + BIG_TAB-PLAN2.

        PERFORM PRINT_DIVIDER1.
    ENDIF.
    ENDAT.

    AT END OF PRIORITY.
      WRITE : / SY-ULINE.
      PERFORM PRINT_DIVIDER2.
      WRITE: 45 TEXT-034, 73 TOT1, TOT2, 118 TOT3, 141 TOT4, 164 TOT5,
             187 TOT6, 211 TOT7, 234 TOT8.
      PERFORM PRINT_DIVIDER3.
      WRITE : SY-ULINE.
      CLEAR : TOT1, TOT2, TOT3, TOT4, TOT5, TOT6, TOT7, TOT8.
    ENDAT.
  ENDLOOP.

ENDFORM.

*-----------------------------------------------------------------------
*    FORM COLLECT_DATA1.
*-----------------------------------------------------------------------
*   Description:
*   -
*-----------------------------------------------------------------------
FORM COLLECT_DATA1 TABLES WBS_EL STRUCTURE S_WBE1
                                           USING TYPE PRITY_5 PRITY_7 .

  DATA:     L_LEN           TYPE I,
            L_START         TYPE I,
            L_SYMBOL(4)     TYPE C.

  REFRESH BIG_TAB.

  SELECT * FROM PRPS
     WHERE PBUKR IN S_CCODE
     AND PRART = TYPE
     AND PKOKR = '10'
     AND LOEVM NE 'X'
     AND ( PSPRI = PRITY_5 OR PSPRI = PRITY_7 )
     ORDER BY PSPRI.


    L_LEN = STRLEN( PRPS-POSID ).
    CHECK L_LEN GE 4.
    L_START = L_LEN - 4.

    L_SYMBOL = PRPS-POSID+L_START(4).
    CHECK L_SYMBOL IN WBS_EL.

    PERFORM GET_FISCAL_DATA.
    PERFORM GET_BUDGET.

    SELECT SINGLE * FROM JEST
       WHERE OBJNR = PRPS-OBJNR
       AND STAT = 'I0063'.
    IF SY-SUBRC EQ 0.
      APPROVAL = BUDGET.
    ENDIF.
    PERFORM GET_PLAN USING P_VERS1 PLAN1.
    PERFORM GET_PLAN USING P_VERS2 PLAN2.

    DELTA1 = COST - PLAN.
    DELTA2 = BUDGET - APPROVAL.

    PERFORM BUILD_TABLE USING L_SYMBOL.
  ENDSELECT.
ENDFORM.
*-----------------------------------------------------------------------
*     FORM BUILD_GENTAB.
*-----------------------------------------------------------------------
*   Description:
*-----------------------------------------------------------------------
FORM BUILD_GENTAB TABLES GENTAB STRUCTURE A_TAB.
  MOVE PRPS-POSID+0(2) TO GENTAB-DIVNR.
  MOVE PRPS-OBJNR      TO GENTAB-OBJNR.
  MOVE SYMBOL          TO GENTAB-WBSEL.
  MOVE PRPS-POST1      TO GENTAB-TITLE.
  MOVE PRPS-PSPRI      TO GENTAB-PRIORITY.
  MOVE PLAN            TO GENTAB-PLAN.
  MOVE COST            TO GENTAB-COST.
  MOVE DELTA1          TO GENTAB-DELTA1.
  MOVE BUDGET          TO GENTAB-BUDGET.
  MOVE APPROVAL        TO GENTAB-APPROVAL.
  MOVE DELTA2          TO GENTAB-DELTA2.
  MOVE PLAN1           TO GENTAB-PLAN1.
  MOVE PLAN2           TO GENTAB-PLAN2.
  APPEND GENTAB.
  CLEAR GENTAB.

  CLEAR: PLAN, PLAN1, PLAN2, COST, VALUE, BUDGET, APPROVAL, DELTA1,
         DELTA2.

ENDFORM.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM GET_INFO TABLES GENTAB STRUCTURE A_TAB.
  DATA:     L_STRING     LIKE    PRPS-POST1.

  SORT GENTAB BY DIVNR PRIORITY WBSEL.

  LOOP AT GENTAB.
    CHECK GENTAB-DIVNR = TEMP.
    ON CHANGE OF GENTAB-PRIORITY.
      SELECT SINGLE * FROM TCN7T
         WHERE LANGU = SY-LANGU
         AND NPRIO = GENTAB-PRIORITY.
      WRITE: /1(255) SY-ULINE.
      WRITE: /3 TEXT-028, TCN7T-KTEXT, 1 SY-VLINE, 255 SY-VLINE.
      WRITE: /1(255) SY-ULINE.
    ENDON.

    MOVE GENTAB-TITLE TO L_STRING.
    AT END OF WBSEL.
      SUM.
      IF GENTAB-PLAN NE 0 OR GENTAB-COST NE 0 OR GENTAB-BUDGET NE 0 OR
         GENTAB-APPROVAL NE 0 OR GENTAB-PLAN1 NE 0 OR GENTAB-PLAN2 NE 0.
        PERFORM PRINT_DIVIDER2.
        WRITE: 7 GENTAB-WBSEL, 18 L_STRING, 73 GENTAB-PLAN, GENTAB-COST,
              118 GENTAB-DELTA1, 141 GENTAB-BUDGET, 164 GENTAB-APPROVAL,
               187 GENTAB-DELTA2, 211 GENTAB-PLAN1, 234 GENTAB-PLAN2.
        TOT1 = TOT1 + GENTAB-PLAN.
        TOT2 = TOT2 + GENTAB-COST.
        TOT3 = TOT3 + GENTAB-DELTA1.
        TOT4 = TOT4 + GENTAB-BUDGET.
        TOT5 = TOT5 + GENTAB-APPROVAL.
        TOT6 = TOT6 + GENTAB-DELTA2.
        TOT7 = TOT7 + GENTAB-PLAN1.
        TOT8 = TOT8 + GENTAB-PLAN2.

        PERFORM PRINT_DIVIDER1.
      ENDIF.
    ENDAT.
    AT END OF PRIORITY.
      WRITE : / SY-ULINE.
      PERFORM PRINT_DIVIDER2.
      WRITE: 45 TEXT-034, 73 TOT1, TOT2, 118 TOT3, 141 TOT4, 164 TOT5,
             187 TOT6, 211 TOT7, 234 TOT8.
      PERFORM PRINT_DIVIDER3.
      WRITE : SY-ULINE.

      BIGTOT1 = BIGTOT1 + TOT1.
      BIGTOT2 = BIGTOT2 + TOT2.
      BIGTOT3 = BIGTOT3 + TOT3.
      BIGTOT4 = BIGTOT4 + TOT4.
      BIGTOT5 = BIGTOT5 + TOT5.
      BIGTOT6 = BIGTOT6 + TOT6.
      BIGTOT7 = BIGTOT7 + TOT7.
      BIGTOT8 = BIGTOT8 + TOT8.

      CLEAR : TOT1, TOT2, TOT3, TOT4, TOT5, TOT6, TOT7, TOT8.
    ENDAT.
  ENDLOOP.
  FORMAT COLOR 2 INTENSIFIED OFF.
  WRITE: /41 TEXT-035, 73 BIGTOT1, BIGTOT2, 118 BIGTOT3,
          141 BIGTOT4, 164 BIGTOT5, 187 BIGTOT6, 211 BIGTOT7,
          234 BIGTOT8.
  FORMAT COLOR 2 INTENSIFIED ON.

  GRAND1 = GRAND1 + BIGTOT1.
  GRAND2 = GRAND2 + BIGTOT2.
  GRAND3 = GRAND3 + BIGTOT3.
  GRAND4 = GRAND4 + BIGTOT4.
  GRAND5 = GRAND5 + BIGTOT5.
  GRAND6 = GRAND6 + BIGTOT6.
  GRAND7 = GRAND7 + BIGTOT7.
  GRAND8 = GRAND8 + BIGTOT8.

  CLEAR : BIGTOT1, BIGTOT2, BIGTOT3, BIGTOT4, BIGTOT5, BIGTOT6,
          BIGTOT7, BIGTOT8.

ENDFORM.

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM MATCH_FOUND TABLES GENTAB STRUCTURE A_TAB USING TYPE.
  MOVE DIV_TAB TO TEMP.
  PERFORM PRINT_TYPE USING TYPE.
  PERFORM PRINT_HEADINGS.
  PERFORM GET_INFO TABLES GENTAB.
ENDFORM.

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM PRINT_TOTALS.
  IF SY-SUBRC = 0.
    FORMAT COLOR 2 INTENSIFIED OFF.
    WRITE: /41 TEXT-035, 73 BIGTOT1, BIGTOT2, 118 BIGTOT3,
            141 BIGTOT4, 164 BIGTOT5, 187 BIGTOT6, 211 BIGTOT7,
            234 BIGTOT8.
    FORMAT COLOR 2 INTENSIFIED ON.
    GRAND1 = GRAND1 + BIGTOT1.
    GRAND2 = GRAND2 + BIGTOT2.
    GRAND3 = GRAND3 + BIGTOT3.
    GRAND4 = GRAND4 + BIGTOT4.
    GRAND5 = GRAND5 + BIGTOT5.
    GRAND6 = GRAND6 + BIGTOT6.
    GRAND7 = GRAND7 + BIGTOT7.
    GRAND8 = GRAND8 + BIGTOT8.
    CLEAR : BIGTOT1, BIGTOT2, BIGTOT3, BIGTOT4, BIGTOT5, BIGTOT6,
            BIGTOT7, BIGTOT8.
  ENDIF.
ENDFORM.
