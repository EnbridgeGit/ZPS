REPORT ZPPMR017 NO STANDARD PAGE HEADING LINE-SIZE 170
                LINE-COUNT 58 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    zppmr017
*   PROGRAMMER: M DeMeester
*   CLIENT:     Union Gas
*   DATE:       DEC 1998.
*
*   The purpose of this program is to produce a list of project not
*   settled to the asset during the fiscal year.
*
************************************************************************
* CHANGES
* 09/05/15 #314 M.Khan   Change to select assets starting with 4 and 5
*                        instead of starting with 4 only.
* 99/01/11 #476 mdemeest gabpe & gbisp are 3 characters not 2
* 98/12/21 #436 mdemeest add receiver general info on first report
* 98/12/03 #436 mdemeest original request
************************************************************************

TABLES: PRPS,               " WBS element master data
        COSS,               " CO object: internal postings
        COSP,               " CO object: external postings
        COBRB,              " Settlement rules by object nbr
        T001,               " company codes
        JEST,               " object status
        TCJ1T,              " project type description
        TJ02T,
        TVARV.              "Posting periods

DATA:  WA_TITLE(50) TYPE C.

DATA: BEGIN OF S_ISTAT OCCURS 0,
        SIGN(1),
        OPTION(2),
        LOW      LIKE TJ02T-ISTAT,
        HIGH     LIKE TJ02T-ISTAT,
      END OF S_ISTAT.

data:  status(40)  type c.   "All statuses from the function call

DATA: BEGIN OF TABLE OCCURS 0,
        VERNR         LIKE PROJ-VERNR,                  "Division
        POSID         LIKE PRPS-POSID,                  "WBS Elemnt
        POST1         LIKE PRPS-POST1,                  "Description
        VERNA         LIKE PROJ-VERNA,                  "Division Name
        WKGNNN        LIKE COSP-WKG001,                 "Total Costs
        STAT          LIKE JEST-STAT,                   "Status
        OBJNR         LIKE PRPS-OBJNR,
        ANLN1         LIKE COBRB-ANLN1,
        ANLN2         LIKE COBRB-ANLN2,
        PS_PSP_PNR    LIKE COBRB-PS_PSP_PNR,
        PRART         LIKE PRPS-PRART,                  "Project type
        HKONT         LIKE COBRB-HKONT,                "G/L Account #
     END OF TABLE.

DATA: BEGIN OF TABLE2 OCCURS 0,
        VERNR         LIKE PROJ-VERNR,                  "Division
        KONTY         LIKE COBRB-KONTY,                 "Acct assign Cat
        POSID         LIKE PRPS-POSID,                  "WBS Elemnt
        POST1         LIKE PRPS-POST1,                  "Description
        VERNA         LIKE PROJ-VERNA,                  "Division Name
        STAT          LIKE JEST-STAT,                   "Status
        WKGNNN        LIKE COSP-WKG001,                 "Total Costs
        OBJNR         LIKE PRPS-OBJNR,
        ANLN1         LIKE COBRB-ANLN1,
        ANLN2         LIKE COBRB-ANLN2,
        PS_PSP_PNR    LIKE COBRB-PS_PSP_PNR,
        PRART         LIKE PRPS-PRART,                  "Project type
        HKONT         LIKE COBRB-HKONT,                "G/L Account #
     END OF TABLE2.

DATA: VERNA           LIKE PRPS-VERNA.
DATA: VALUE           LIKE COSP-WKG001.        "WBS Element Sum
DATA: AMT             LIKE COSP-WKG001.        "Cost Centre Sum
DATA: REPORT1(3).                              "Info to be reported
DATA: REPORT2(3).                              "Info to be reported
DATA: OPEN_PERIOD(6).
DATA: TO_PERIOD(6).
DATA: FROM_PERIOD(6).

************************************************************************
*-----------------------  SELECTION SCREEN  ----------------------------

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
  SELECTION-SCREEN COMMENT 1(50) TEXT-011.
    PARAMETERS:     P_RPT1     AS CHECKBOX DEFAULT 'X',
                    P_RPT2     AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK BOX4.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
  SELECTION-SCREEN COMMENT 1(50) TEXT-001.
    PARAMETERS:     P_COMP     LIKE PRPS-PBUKR OBLIGATORY.
    SELECT-OPTIONS: S_VERNR    FOR PRPS-VERNR,
                    S_PRART    FOR PRPS-PRART,
                    S_STAT for tj02t-txt04 NO INTERVALS DEFAULT 'TECO'
                                    lower case.
    PARAMETERS:     P_VERS     LIKE COSP-VERSN DEFAULT '000',
                    P_GJAHR    LIKE COSP-GJAHR,"default sy-datum(4),
                    P_MTH      LIKE SY-DATUM+4(2)."default sy-datum+4(2)

    SELECT-OPTIONS: S_HKONT    FOR COBRB-HKONT NO INTERVALS.
*                   p_empge    like dkobr-empge.
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
  SELECTION-SCREEN COMMENT 1(50) TEXT-002.
    SELECT-OPTIONS: S_KSTAR    FOR COSS-KSTAR
                                DEFAULT '0000491001' TO '0000491002'.
  SELECTION-SCREEN END OF BLOCK BOX3.

************************************************************************

*AT SELECTION-SCREEN.
initialization.
  IF SY-MANDT+2(1) = '0'.                   "Company Code initialization
     MOVE 'UGL' TO P_COMP.
  ELSEIF SY-MANDT+2(1) = '1'.
     MOVE 'UEC' TO P_COMP.
  ENDIF.
* get posting period
 SELECT SINGLE * FROM TVARV
  WHERE NAME = 'ZCO_CUR_PERIOD_YEAR_M'.
  P_GJAHR = TVARV-LOW.
 SELECT SINGLE * FROM TVARV
   WHERE NAME = 'ZCO_CUR_PERIOD_M'.
  P_MTH = TVARV-LOW.

*-------------------------  START-OF-SELECTION -------------------------
START-OF-SELECTION.
SELECT SINGLE * FROM T001                "Company Code Selection
  WHERE BUKRS = P_COMP.

OPEN_PERIOD = P_GJAHR.
OPEN_PERIOD+4(2) = P_MTH.

* select appropriate divisions
 SELECT * FROM PRPS
   WHERE VERNR IN S_VERNR
     AND PBUKR = P_COMP
     AND PRART IN S_PRART
     AND BELKZ = 'X'
     and loevm <> 'X'.
*     and objnr = 'PR00111473'.
   CLEAR: REPORT1, REPORT2.
   IF PRPS-POSID+5(2) CO '0123456789'.     "Eliminate templates
      perform get_statuses.
      if status cs 'DLIN'.                 "Eliminate DELETED WBS
      else.
        loop at s_stat.
          if status cs s_stat+3(4).
             value = 0.
             perform get_amounts.
             if value = 0.
             else.
               REPORT1 = 'YES'.              "Assume required for report
               REPORT2 = 'NO'.               "Assume required for report
*
* all settlement rules must be examined before deciding if wbs should
* appear on report.
*
               SELECT * FROM COBRB
                 WHERE OBJNR = PRPS-OBJNR
                   and perbz <> 'JHR'.        "Ignore AUCs 2001/12/13
                   PERFORM REPORT1.
                   PERFORM REPORT2.
               ENDSELECT.                     "End of COBRB

             IF SY-SUBRC = '4'.             " ==> no FXA's
                MOVE 'no'  TO REPORT1.
                MOVE 'NO'  TO REPORT2.
             ENDIF.
             IF REPORT1 = 'YES'.
                PERFORM BUILD_TABLE.
             ENDIF.                          "end of CREATE REPORT1

             if report2 = 'YES'.
                perform build_table2.
             endif.
                                                 "end of CREATE REPORT1
              endif.
           endif.
        endloop.
     ENDIF.                                 "STATUS DELETE end
  ENDIF.                                    "End of templates
ENDSELECT.                                  "END OF PRPS

 IF P_RPT1 = 'X'.
    move 'WBS WITH NO SETTLEMENT RULES' to WA_TITLE.
    SORT TABLE BY VERNR POSID.

    LOOP AT TABLE.
    MOVE TABLE-VERNA TO VERNA.
    AT NEW VERNR.
     NEW-PAGE.
     PERFORM PRINT_VERT.
     WRITE: VERNA UNDER TEXT-003.
    ENDAT.

*    SELECT SINGLE * FROM TJ02T
*        WHERE ISTAT = TABLE-STAT
*          AND SPRAS = SY-LANGU.
    PERFORM PRINT_VERT.
    WRITE:  TABLE-POSID  UNDER TEXT-006,
            TABLE-POST1  UNDER TEXT-007,
            TABLE-WKGNNN UNDER TEXT-008,
            Table-stat   UNDER TEXT-012.
*           160 table-objnr.                       "to help with testing
    IF TABLE-PS_PSP_PNR > 0.
       WRITE: 150 TABLE-PS_PSP_PNR.
    ELSEIF TABLE-HKONT > 0.
       WRITE: 150 TABLE-HKONT.
    ELSE.
       WRITE: 150 TABLE-ANLN1 NO-GAP, '-' NO-GAP, TABLE-ANLN2.
    ENDIF.

    AT END OF POSID(7).
     PERFORM PRINT_VERT.
     WRITE: 34 SY-ULINE(136).
    ENDAT.


    AT END OF VERNR.
     SUM.
     PERFORM PRINT_VERT.
     WRITE: TEXT-010 UNDER TEXT-003, VERNA,
            TABLE-WKGNNN UNDER TEXT-008.
    ENDAT.

  ENDLOOP.
 ENDIF.

 IF P_RPT2 = 'X'.
    move 'PROJECTS WITH NO ASSET SETTLEMENT RULES' to WA_TITLE.

    SORT TABLE2 BY VERNR KONTY  POSID.

    LOOP AT TABLE2.
    MOVE TABLE2-VERNA TO VERNA.
    AT NEW VERNR.
     NEW-PAGE.
     PERFORM PRINT_VERT.
     WRITE: VERNA UNDER TEXT-003.
    ENDAT.


    AT NEW KONTY.
     PERFORM PRINT_VERT.
     WRITE: 30 TABLE2-KONTY.
    ENDAT.

    PERFORM PRINT_VERT.
    WRITE:  TABLE2-POSID  UNDER TEXT-006,
            TABLE2-POST1  UNDER TEXT-007,
            TABLE2-WKGNNN UNDER TEXT-008,
            Table2-stat   UNDER TEXT-012.
    IF TABLE2-PS_PSP_PNR > 0.
       WRITE: 150 TABLE2-PS_PSP_PNR.
    ELSEIF TABLE2-HKONT > 0.
       WRITE: 150 TABLE2-HKONT.
    ENDIF.
*       write:  150 table2-anln1 no-gap, '-' no-gap, table2-anln2.

    AT END OF POSID(7).
     PERFORM PRINT_VERT.
     WRITE: 34 SY-ULINE(136).
    ENDAT.


    AT END OF VERNR.
     SUM.
     PERFORM PRINT_VERT.
     WRITE: TEXT-010 UNDER TEXT-003, VERNA,
            TABLE2-WKGNNN UNDER TEXT-008.
    ENDAT.

  ENDLOOP.
 ENDIF.

write: /55 'END of REPORT'.

END-OF-SELECTION.


************************************************************************
*                       SUBROUTINES
************************************************************************

*---------------------  PRINT_VERT  ------------------------------------
FORM PRINT_VERT.
 WRITE: /1 SY-VLINE,  34 SY-VLINE,
        59 SY-VLINE, 114 SY-VLINE, 121 SY-VLINE, 143 SY-VLINE.
ENDFORM.

*---------------------  BUILD_TABLE ------------------------------------
FORM BUILD_TABLE.
  TABLE-VERNR   = PRPS-VERNR.
  TABLE-VERNA   = PRPS-VERNA.
  TABLE-PRART   = PRPS-PRART.
  TABLE-POSID   = PRPS-POSID.
  TABLE-POST1   = PRPS-POST1.
  TABLE-OBJNR   = PRPS-OBJNR.
* gets latest active status
  if     status cs 'CLSD'. move 'CLSD' to table-stat.
  elseif status cs 'TECO'. move 'TECO' to table-stat.
  elseif status cs 'REL'.  move 'REL'  to table-stat.
  elseif status cs 'CRTD'. move 'CRTD' to table-stat.  endif.
  TABLE-WKGNNN  = VALUE.                                "Total Costs
  TABLE-PS_PSP_PNR = COBRB-PS_PSP_PNR.
  TABLE-ANLN1   = COBRB-ANLN1.
  TABLE-ANLN2   = COBRB-ANLN2.
  TABLE-HKONT   = COBRB-HKONT.
  APPEND TABLE.
  CLEAR TABLE.
ENDFORM.

FORM BUILD_TABLE2.
   IF COBRB-HKONT IN S_HKONT.
     TABLE2-VERNR   = PRPS-VERNR.
     TABLE2-VERNA   = PRPS-VERNA.
     TABLE2-PRART   = PRPS-PRART.
     TABLE2-POSID   = PRPS-POSID.
     TABLE2-POST1   = PRPS-POST1.
     TABLE2-OBJNR   = PRPS-OBJNR.
     TABLE2-WKGNNN  = VALUE.                                "Total Costs
     TABLE2-KONTY   = COBRB-KONTY.
     TABLE2-PS_PSP_PNR = COBRB-PS_PSP_PNR.
     TABLE2-ANLN1   = COBRB-ANLN1.
     TABLE2-ANLN2   = COBRB-ANLN2.
* gets latest active status                            "2001/11/23
     if     status cs 'CLSD'. move 'CLSD' to table2-stat.
     elseif status cs 'TECO'. move 'TECO' to table2-stat.
     elseif status cs 'REL'.  move 'REL'  to table2-stat.
     elseif status cs 'CRTD'. move 'CRTD' to table2-stat.  endif.

     TABLE2-HKONT   = COBRB-HKONT.
     APPEND TABLE2.
     CLEAR TABLE2.
  ENDIF.
ENDFORM.

*---------------------  GET_AMOUNTS ------------------------------------
FORM GET_AMOUNTS.
  SELECT * FROM COSP
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_GJAHR                               "Fiscal Year
      AND WRTTP = '04'                                  "Actuals
      AND VERSN = P_VERS
      AND KSTAR NOT IN S_KSTAR.
    ADD  COSP-WKG001 FROM 1 TO 12 GIVING AMT.
    VALUE = VALUE + AMT.
   ENDSELECT.

  SELECT * FROM COSS
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_GJAHR                               "Fiscal Year
      AND WRTTP = '04'                                  "Actuals
      AND VERSN = P_VERS
      AND KSTAR NOT IN S_KSTAR.
    ADD COSS-WKG001 FROM 1 TO 12 GIVING AMT.
    VALUE = VALUE + AMT.
   ENDSELECT.
ENDFORM.

FORM REPORT1.
  FROM_PERIOD = '000000'.
  IF cobrb-gabja <> '0000'.                   "Build asset from period
     FROM_PERIOD      = COBRB-GABJA.
     FROM_PERIOD+4(2) = COBRB-GABPE+1(2).                     "#476
  ENDIF.

  to_period = '999999'.
  if cobrb-gbisp <> '0000'.
     TO_PERIOD      = COBRB-GBISJ.
     TO_PERIOD+4(2) = COBRB-GBISP+1(2).                      "#476
  ENDIF.

  IF open_period between from_period and to_period and
*     COBRB-ANLN1(1) = '4'.                                  "TR413
     COBRB-ANLN1(1) BETWEEN '4' AND '5'.                     "TR413
     REPORT1 = 'NO'.
  ENDIF.

  IF COBRB-KONTY = 'SK'.
     REPORT1 = 'NO'.
  ENDIF.
ENDFORM.

FORM REPORT2.
 IF COBRB-KONTY = 'AN'.
 ELSE.
*    VALUE = 0.
*    PERFORM GET_AMOUNTS.
    if  open_period between from_period and to_period
        and cobrb-hkont in s_hkont.
        move 'YES' to report2.
*       PERFORM BUILD_TABLE2.
    endif.
 ENDIF.                              "end of CREATE REPORT1
ENDFORM.
*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
 WRITE: /1 TEXT-RPT, SY-REPID,  55 T001-BUTXT,             "Company
       140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
 WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
        50 WA_TITLE,
          TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
 WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS,
          TEXT-009 UNDER T001-BUTXT, P_GJAHR.           "Fiscal Year
 ULINE.
 PERFORM PRINT_VERT.
 PERFORM PRINT_VERT.
 WRITE:   2 TEXT-003,  35 TEXT-006, 60 TEXT-007,
        115 TEXT-012, 122 TEXT-008.
 ULINE.
*-----------------------------------------------------------------------

*---------------------------- GET_STATUSES -----------------------------
*  This routine will retrieve all statuses for a particular WBS element
*-----------------------------------------------------------------------
FORM GET_STATUSES.

 CALL FUNCTION 'AIP9_STATUS_READ'                  "Retrieve Status Info
       EXPORTING
            I_OBJNR = PRPS-OBJNR
            I_SPRAS = SY-LANGU
       IMPORTING
            E_SYSST = STATUS
       EXCEPTIONS
            OTHERS = 1.

ENDFORM.                    " GET STATUSES
