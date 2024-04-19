REPORT ZPPMR037 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 132.
*
************************************************************************
*  Author:      Mohammad T. Khan
*  Date:        October, 2003
*  Description:
*     - The purpose of this program is to produce report for Additional
*       charges after the Audit.
*
************************************************************************
* 2010/07/07 btboundy - TR843 Include plan amount from COSS table.
************************************************************************

TABLES: COSP,              "CO Object: Cost Totals for External Postings
        COSS,              "CO Object: Cost Totals for Internal Postings
        PROJ,              "Project definition
        PRPS,              "WBS Element Master Data
        BPJA,              "Budget- Totals record for annual total
        T001B,             "Dummy - req for variants only
        AUSP,              "Characteristics value
        T247.              "Month name

DATA:
   BEGIN OF ITAB  OCCURS 0,
      PSPHI       LIKE PRPS-PSPHI,          "WBS
      ACTUAL      LIKE COSS-WKG001,         "amt actual for period
      YTDACT      LIKE COSS-WKG001,         "amt actual YTD
      BUDGET      LIKE COSS-WKG001,         "amt budget
      PLAN        LIKE COSS-WKG001,         "amt plan
   END OF ITAB.

DATA: BEGIN OF ITABAUDIT OCCURS 0,
         OBJEK LIKE AUSP-OBJEK,
         ATWRT LIKE AUSP-ATWRT,
      END OF ITABAUDIT.

DATA: BEGIN OF ITABLCHG OCCURS 0,
         OBJEK LIKE AUSP-OBJEK,
         ATFLV LIKE AUSP-ATFLV,
      END OF ITABLCHG.

DATA:
   BEGIN OF EXCLTAB  OCCURS 0,
      PROJECT(9)     TYPE C,                  "Project
      POST1   LIKE PROJ-POST1,                "Proj. descripton
      AUDTBY(10)     TYPE C,                  "Proj. Audited By
      LCDATE(10)     TYPE C,                  "Proj. last changed
      PERACTUAL      TYPE P DECIMALS 0,       "Actual Amount for Period
      YTDACTUAL      TYPE P DECIMALS 0,       "YTD Actual Amount
      YTDBUDGET      TYPE P DECIMALS 0,       "YTD Budget Amount
      YTDPLAN        TYPE P DECIMALS 0,       "YTD Plan Amount
   END OF EXCLTAB.

DATA:
      W_OPTION(11)  TYPE C VALUE 'START_EXCEL',
      W_TEXT1(65)   TYPE C,
      W_TEXT2(60)   TYPE C,
      W_DATE        TYPE I,
      W_DATE8(8)    TYPE C,
      AUDIT_FLAG    TYPE C,
      H_DOLAR(12)   TYPE C,
      W_BEGNME(9)   TYPE C,
      W_ENDNME(9)   TYPE C,
      SAVE_PSPHI    LIKE PRPS-PSPHI,
      AMOUNT        LIKE COSP-WKG001,
      PLAN$         LIKE COSP-WKG001,
      ACTUAL$       LIKE COSP-WKG001,
      PLAN$_WBS     LIKE COSP-WKG001,
      ACTUAL$_WBS   LIKE COSP-WKG001,
      BUDGET$_WBS   LIKE COSP-WKG001,
      TOTAL_PLAN$   LIKE COSP-WKG001,
      TOTAL_ACTUAL$ LIKE COSP-WKG001,
      TOTAL_BUDGET$ LIKE COSP-WKG001.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5) TYPE C,
        DDIC_FIELD(5) TYPE C,
        KEY TYPE C,
      END OF PROT_HEADER.

DATA  ERRORTAB LIKE HRERROR OCCURS 0 WITH HEADER LINE.
DATA: RETCODE  LIKE SY-SUBRC.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.

SELECT-OPTIONS:  S_VERNR FOR PRPS-VERNR.                   "Division
PARAMETERS:
 P_PERD1 LIKE T001B-FRPE1 OBLIGATORY,                      "From Period
 P_PERD2 LIKE T001B-FRPE1 OBLIGATORY,                      "To Period
 P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY, "Fiscal Year
 P_VERS  LIKE COSP-VERSN DEFAULT '000',                    "Plan version
 P_DOLR  LIKE COSP-WKG001 DEFAULT 100000.                "$ greater than

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
        S_KSTAR    FOR COSS-KSTAR                          "Cost element
                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-004.
PARAMETERS:     P_SRT1 RADIOBUTTON GROUP SORT,          "Sort By Amount
                P_SRT2 RADIOBUTTON GROUP SORT.          "Sort By Project
SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME TITLE TEXT-005.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_EXCL RADIOBUTTON GROUP RBCR.            "EXCEL FILE
SELECTION-SCREEN END OF BLOCK BOX5.

SELECTION-SCREEN END OF BLOCK BOX.
************************************************************************
AT SELECTION-SCREEN.
  DATA: MSG_TXT(45) TYPE C.
  IF P_PERD2 < P_PERD1.
    MOVE 'PERIOD-FROM CAN NOT BE GREATER THAN PERIOD-TO' TO MSG_TXT.
  ELSEIF P_PERD2 > 12.
    MOVE 'PERIOD-TO CAN NOT BE GREATER THAN 12' TO MSG_TXT.
  ELSEIF P_PERD1 < 1.
    MOVE 'PERIOD-FROM CAN NOT BE EQUAL TO ZERO ' TO MSG_TXT.
  ENDIF.

  IF MSG_TXT <> SPACE.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING
        HEADLINE = '!! ERROR !!'
        TEXT1    = MSG_TXT
        TEXT2    = 'PRESS OK BUTTON TO CONTINUE...'
        BUTTON_1 = 'OK'.
    STOP.
  ENDIF.

************************************************************************

START-OF-SELECTION.
  PERFORM GET_AUDITED_PROJECTS.
  PERFORM GET_PRPS_DATA.
  PERFORM GET_AUDITED_PROJECTS.
  IF NOT EXCLTAB[] IS INITIAL.
    PERFORM PRINT_REPORT.
  ENDIF.

************************************************************************
*                                                                      *
*                 GET_AUDITED_PROJECTS.                                *
*                                                                      *
************************************************************************
FORM GET_AUDITED_PROJECTS.

  SELECT OBJEK ATWRT
  INTO   TABLE ITABAUDIT
  FROM   AUSP
  WHERE  OBJEK LIKE 'PR%'
  AND    ATINN = 613
  AND    KLART = '014'.
  SORT   ITABAUDIT BY OBJEK.

ENDFORM.                    "GET_AUDITED_PROJECTS

************************************************************************
*                                                                      *
*                    GET_PRPS_DATA                                     *
*                                                                      *
************************************************************************
FORM GET_PRPS_DATA.

  SELECT PSPHI OBJNR STUFE
   INTO (PRPS-PSPHI, PRPS-OBJNR, PRPS-STUFE)
   FROM  PRPS
  WHERE  PSPNR LIKE '%'
    AND  VERNR IN S_VERNR
    AND ( STUFE = 1 OR BELKZ = 'X' OR PLAKZ = 'X' )
    AND  LOEVM  <>  'X'
    ORDER BY PSPHI.

    ON CHANGE OF PRPS-PSPHI.
      PERFORM BUILD_ITAB_DATA.
    ENDON.

    MOVE PRPS-PSPHI TO SAVE_PSPHI.
* CLEAR ITAB.
    IF PRPS-STUFE <> 1.
      SELECT WRTTP VERSN WKG001 WKG002 WKG003 WKG004 WKG005 WKG006
                         WKG007 WKG008 WKG009 WKG010 WKG011 WKG012
        INTO  (COSP-WRTTP,  COSP-VERSN,  COSP-WKG001, COSP-WKG002,
               COSP-WKG003, COSP-WKG004, COSP-WKG005, COSP-WKG006,
               COSP-WKG007, COSP-WKG008, COSP-WKG009, COSP-WKG010,
               COSP-WKG011, COSP-WKG012)
        FROM   COSP
       WHERE   OBJNR = PRPS-OBJNR
         AND   GJAHR =  P_FYEAR
         AND ( VERSN =  P_VERS              "Version
               OR VERSN   = '000' )
         AND   WRTTP  IN ('01','04')        "Record with actuals & plans
         AND   BEKNZ  IN ('S','H','L')      "Debit/Credit Indicator
         AND   KSTAR  NOT IN  S_KSTAR.

        CASE COSP-WRTTP.
          WHEN '01'.
            IF COSP-VERSN = P_VERS.
              ADD COSP-WKG001 FROM 1 TO 12 GIVING AMOUNT.
              ITAB-PLAN = ITAB-PLAN + AMOUNT.
            ENDIF.
          WHEN '04'.
            IF COSP-VERSN = '000'.
              ADD COSP-WKG001 FROM P_PERD1 TO P_PERD2 GIVING AMOUNT.
              ITAB-ACTUAL = ITAB-ACTUAL + AMOUNT.
              ADD COSP-WKG001 FROM 1 TO P_PERD2 GIVING AMOUNT.
              ITAB-YTDACT = ITAB-YTDACT + AMOUNT.
            ENDIF.
        ENDCASE.
      ENDSELECT.

      SELECT WRTTP VERSN WKG001 WKG002 WKG003 WKG004 WKG005 WKG006
                         WKG007 WKG008 WKG009 WKG010 WKG011 WKG012
        INTO  (COSS-WRTTP,  COSS-VERSN,  COSS-WKG001, COSS-WKG002,
               COSS-WKG003, COSS-WKG004, COSS-WKG005, COSS-WKG006,
               COSS-WKG007, COSS-WKG008, COSS-WKG009, COSS-WKG010,
               COSS-WKG011, COSS-WKG012)
        FROM   COSS
       WHERE   OBJNR = PRPS-OBJNR
         AND   GJAHR =  P_FYEAR
         AND ( VERSN =  P_VERS             "Version                               TR843
               OR VERSN = '000' )          "                                      TR843
*        AND   WRTTP ='04'                 "Record with actuals                   TR843
         AND   WRTTP IN ('01', '04')       "Record with actuals and plan          TR843
         AND   BEKNZ  IN ('S','H','L')      "Debit/Credit Indicator
         AND   KSTAR  NOT IN  S_KSTAR.

        CASE COSS-WRTTP.                                                        " TR843
          WHEN '01'.                                                            " TR843
            IF COSS-VERSN = P_VERS.                                             " TR843
              ADD COSS-WKG001 FROM 1 TO 12 GIVING AMOUNT.                       " TR843
              ITAB-PLAN = ITAB-PLAN + AMOUNT.                                   " TR843
            ENDIF.                                                              " TR843
          WHEN '04'.                                                            " TR843
            IF COSS-VERSN = '000'.                                              " TR843
                ADD COSS-WKG001 FROM P_PERD1 TO P_PERD2 GIVING AMOUNT.
                ITAB-ACTUAL = ITAB-ACTUAL + AMOUNT.
                ADD COSS-WKG001 FROM 1 TO P_PERD2 GIVING AMOUNT.
                ITAB-YTDACT = ITAB-YTDACT + AMOUNT.
            ENDIF.                                                              " TR843
        ENDCASE.                                                                " TR843


      ENDSELECT.

    ELSE.

      SELECT SINGLE WLJHR INTO BPJA-WLJHR             "Budget $
         FROM   BPJA
         WHERE  OBJNR = PRPS-OBJNR
         AND    WRTTP = '41'
         AND    GJAHR = P_FYEAR
         AND    VERSN = '000'.                        "Version
      IF SY-SUBRC = 0.
        ITAB-BUDGET = BPJA-WLJHR.
      ELSE.
        CLEAR ITAB-BUDGET.
      ENDIF.
    ENDIF.

  ENDSELECT.

  LOOP AT ITAB.
    MOVE 'N' TO AUDIT_FLAG.
    PERFORM CHECK_AUDITED_BY.
    IF AUDIT_FLAG = 'Y'.
      PERFORM GET_PROJECT_DESCRIPTION.
      CONCATENATE PROJ-PSPID+0(2) '-' PROJ-PSPID+2(2)
                                  '-' PROJ-PSPID+4(3)
                                  '-' PROJ-PSPID+7(4)
                                 INTO EXCLTAB-PROJECT.
      MOVE PROJ-POST1    TO  EXCLTAB-POST1.
      MOVE ITAB-ACTUAL   TO  EXCLTAB-PERACTUAL.
      MOVE ITAB-YTDACT   TO  EXCLTAB-YTDACTUAL.
      MOVE ITAB-PLAN     TO  EXCLTAB-YTDPLAN.
      MOVE ITAB-BUDGET   TO  EXCLTAB-YTDBUDGET.
      APPEND EXCLTAB.
      CLEAR  EXCLTAB.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "GET_PRPS_DATA

************************************************************************
*                                                                      *
*                    BUILD_ITAB_DATA                                   *
*                                                                      *
************************************************************************
FORM BUILD_ITAB_DATA.

  DATA: P_DOLR_MINUS LIKE P_DOLR.
  P_DOLR_MINUS = P_DOLR * -1.
  IF ITAB-ACTUAL <> 0.
    IF ITAB-YTDACT > P_DOLR    OR  ITAB-YTDACT < P_DOLR_MINUS  OR
       ITAB-PLAN   > P_DOLR    OR  ITAB-PLAN   < P_DOLR_MINUS  OR
       ITAB-BUDGET > P_DOLR    OR  ITAB-BUDGET < P_DOLR_MINUS.
      MOVE SAVE_PSPHI TO ITAB-PSPHI.
      APPEND ITAB.
    ENDIF.
  ENDIF.
  CLEAR ITAB.

ENDFORM.                    "BUILD_ITAB_DATA
************************************************************************
*                                                                      *
*                    CHECK_AUDITED_BY                                  *
*                                                                      *
************************************************************************
FORM CHECK_AUDITED_BY.

  SELECT SINGLE OBJNR INTO PRPS-OBJNR
  FROM   PRPS
  WHERE  PSPHI = ITAB-PSPHI
  AND    STUFE = 1.

  READ TABLE ITABAUDIT WITH KEY OBJEK = PRPS-OBJNR BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE ITABAUDIT-ATWRT+0(15) TO EXCLTAB-AUDTBY.
    MOVE 'Y'  TO  AUDIT_FLAG.
    PERFORM GET_LAST_CHANGED_DATE.
  ELSE.
    CLEAR EXCLTAB-AUDTBY.
  ENDIF.

ENDFORM.                    "CHECK_AUDITED_BY
************************************************************************
*                                                                      *
*              GET_LAST_CHANGED_DATE .                                 *
*                                                                      *
************************************************************************
FORM GET_LAST_CHANGED_DATE.
  DATA W_PKDATE TYPE P DECIMALS 0.
  SELECT SINGLE ATFLV
  INTO   AUSP-ATFLV
  FROM   AUSP
  WHERE  OBJEK = PRPS-OBJNR
  AND    ATINN = 550
  AND    KLART = '014'.

  IF SY-SUBRC = 0.
    MOVE AUSP-ATFLV TO W_PKDATE.
    MOVE AUSP-ATFLV TO W_DATE.
    MOVE W_DATE TO W_DATE8.
    CONCATENATE W_DATE8+0(4) '/' W_DATE8+4(2) '/'
                W_DATE8+6(2) INTO EXCLTAB-LCDATE.
  ELSE.
    CLEAR EXCLTAB-LCDATE.
  ENDIF.

ENDFORM.                    "GET_LAST_CHANGED_DATE
************************************************************************
*                                                                      *
*                GET_PROJECT_DESCRIPTION.                              *
*                                                                      *
************************************************************************
FORM GET_PROJECT_DESCRIPTION.
  SELECT SINGLE PSPID POST1
  INTO   (PROJ-PSPID, PROJ-POST1)
  FROM   PROJ
  WHERE  PSPNR = ITAB-PSPHI.
ENDFORM.                    "GET_PROJECT_DESCRIPTION
************************************************************************
*                                                                      *
*                    PRINT_REPORT.                                     *
*                                                                      *
************************************************************************

FORM PRINT_REPORT.

  PERFORM PROT_HEADER.
  IF P_SRT1 = 'X'.
    SORT EXCLTAB BY PERACTUAL DESCENDING.
  ELSE.
    SORT EXCLTAB BY PROJECT.
  ENDIF.

  IF SY-BATCH = 'X'  OR  P_EXCL <> 'X'.

    SELECT SINGLE * FROM T247                   "Get Month Name(Begin)
      WHERE SPRAS = 'E'
        AND MNR = P_PERD1.
    W_BEGNME = T247-LTX.
    W_ENDNME = SPACE.
    IF P_PERD2 <> SPACE AND P_PERD1 <> P_PERD2.
      SELECT SINGLE * FROM T247                "Get Month Name(End)
        WHERE SPRAS = 'E'
          AND MNR = P_PERD2.
      W_ENDNME = T247-LTX.
    ENDIF.

    CLEAR W_OPTION.
    MOVE P_DOLR TO H_DOLAR.
    IF W_ENDNME <> SPACE.
      CONCATENATE 'FOR' W_BEGNME 'Thru' W_ENDNME '/' P_FYEAR '&'
                 'AMOUNT >' H_DOLAR INTO W_TEXT1 SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'FOR' W_BEGNME '/' P_FYEAR '&'
                 'AMOUNT >' H_DOLAR INTO W_TEXT1 SEPARATED BY SPACE.
    ENDIF.
    MOVE TEXT-016  TO W_TEXT2+0(5).
    WRITE SY-DATUM TO W_TEXT2+6(10).
    MOVE TEXT-017  TO W_TEXT2+18(5).
    WRITE SY-UZEIT TO W_TEXT2+25(10).

    MOVE TEXT-018  TO W_TEXT2+36(7).
    MOVE SY-MANDT  TO W_TEXT2+44(4).
    MOVE SY-SYSID  TO W_TEXT2+48(5).

  ENDIF.

  IF SY-BATCH = 'X'.
    PERFORM PRINT_BACKGROUND.
  ELSE.
    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
      EXPORTING
        BASIC_LIST_TITLE    = 'ZPPMR037 - Additional Charges after the Audit'
        HEAD_LINE1          = W_TEXT1
        HEAD_LINE2          = W_TEXT2
        FILE_NAME           = SY-CPROG
        ADDITIONAL_OPTIONS  = W_OPTION
      IMPORTING
        RETURN_CODE         = RETCODE
      TABLES
        DATA_TAB            = EXCLTAB
        FIELDNAME_TAB       = PROT_HEADER
        ERROR_TAB           = ERRORTAB
      EXCEPTIONS
        DOWNLOAD_PROBLEM    = 1
        NO_DATA_TAB_ENTRIES = 2
        TABLE_MISMATCH      = 3
        PRINT_PROBLEMS      = 4
        OTHERS              = 5.
    IF SY-SUBRC NE 0.
      WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFULL - REASON = ', SY-SUBRC.
    ENDIF.
  ENDIF.
ENDFORM.                    "PRINT_REPORT
**---------------------------------------------------------------------*
**       FORM PROT_HEADER                                              *
**---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-CL1 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL2 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL3 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL4 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL5 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL6 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL7 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL8 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER
**---------------------------------------------------------------------*
**       FORM PRINT_BACKGROUND                                         *
**---------------------------------------------------------------------*
FORM PRINT_BACKGROUND.
  PERFORM REPORT_HEADING.
  LOOP AT EXCLTAB.
    WRITE: /2 EXCLTAB-PROJECT, 12(38) EXCLTAB-POST1,
            EXCLTAB-AUDTBY UNDER TEXT-CL3, EXCLTAB-LCDATE UNDER TEXT-CL4,
            75(11) EXCLTAB-PERACTUAL,  91(11) EXCLTAB-YTDACTUAL,
           106(11) EXCLTAB-YTDBUDGET, 120(11) EXCLTAB-YTDPLAN.
  ENDLOOP.

ENDFORM.                    "PRINT_BACKGROUND
*------------------- REPORT HEADING -----------------------------------
FORM REPORT_HEADING..
  WRITE: /1 TEXT-RPT, SY-REPID,  48 TEXT-HED,
        103 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
           40 W_TEXT1,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  ULINE.
  WRITE: /2 TEXT-CL1, 12 TEXT-CL2,  52 TEXT-CL3, 64 TEXT-CL4,
         78 TEXT-CL5, 94 TEXT-CL6, 108 TEXT-CL7, 123 TEXT-CL8.
  ULINE.
ENDFORM.                    "REPORT_HEADING
************************************************************************
**************************END OF PROGRAM********************************
************************************************************************
