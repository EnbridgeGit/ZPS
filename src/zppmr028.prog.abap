REPORT ZPPMR028 NO STANDARD PAGE HEADING LINE-SIZE 132
                LINE-COUNT 65 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    ZPPMR028
*   AUTHOR:     MOHAMMAD T. KHAN
*   DATE:       Dec, 2001.
*
*   Description:
*   The purpose of this program is to list capital expenditures by
*   project and asset number.
************************************************************************

TABLES:   T001,                         " Company codes
          JEST,                         " Object status
          JCDS,                         " Object status change documents
          TJ02T,                        " Object status description
          PRPS,                         " WBS element master data
          COSS,                         " CO object: internal postings
          COSP,                         " CO object: external postings
          COBRA,                        " Settlement rules by object nbr
          COBRB.                        " Settlement rules by object nbr

RANGES:   STATUS        FOR JEST-STAT.                  " status range

DATA:     STATVAL       LIKE JCDS-STAT,                 " status value
          STATDTE       LIKE JCDS-UDATE,                " status eff dte
          STATIME       LIKE JCDS-UTIME,                " status eff tme
          WRKAMT        LIKE COSS-WKG001,               " work amount
          TOTSUB        LIKE COSS-WKG001,               " sub ass total
          TOTMAIN       LIKE COSS-WKG001,               " main ass total
          TOTCLAS       LIKE COSS-WKG001,               " class total
          TOTELEM       LIKE COSS-WKG001,               " cost ele total
          TOTREPT       LIKE COSS-WKG001,               " report total
          RANLN1        LIKE COBRB-ANLN1,               " rpt asset
          RANLN2        LIKE COBRB-ANLN2,               " rpt sub asset
          RPOST1        LIKE PRPS-POST1,                " rpt WBS desc
          RBZDAT        LIKE COBRA-BZDAT,               " rpt asset date
          RTXT04        LIKE TJ02T-TXT04,               " rpt status
          SETTLED(1)    TYPE C,                         " settlement ind
          W_PROJ(7),
          W_PSPID       LIKE PROJ-PSPID,
          W_ASTNR       LIKE PROJ-ASTNR,
          WRK_TEXT(41).

DATA:     BEGIN OF IPRPS OCCURS 1000,
            OBJNR       LIKE PRPS-OBJNR,                " object number
            POSID       LIKE PRPS-POSID,                " project id
            POST1       LIKE PRPS-POST1,                " description
            TXT04       LIKE TJ02T-TXT04,               " status
          END OF IPRPS.

DATA:     BEGIN OF REPTAB OCCURS 1000,
            CLASS(5)    TYPE C,                         " asset class
            ASTNR(2)    TYPE C,                         " App. number
            ANLN1       LIKE COBRB-ANLN1,               " main asset
            ANLN2       LIKE COBRB-ANLN2,               " sub asset
            POSID       LIKE PRPS-POSID,                " project id
            PROZS       LIKE COBRB-PROZS,               " allocation %
            POST1       LIKE PRPS-POST1,                " description
            TXT04       LIKE TJ02T-TXT04,               " status
            BZDAT       LIKE COBRA-BZDAT,               " asset value dt
            AMOUNT      LIKE COSS-WKG001,               " capitalize amt
          END OF REPTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 15(40) TEXT-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 6(15) TEXT-022.
SELECTION-SCREEN POSITION 23.
PARAMETERS: P_DETAIL RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN COMMENT 31(22) TEXT-023.
SELECTION-SCREEN POSITION 55.
PARAMETERS: P_SUMMRY RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
PARAMETERS: P_COMP        LIKE PRPS-PBUKR OBLIGATORY
                          DEFAULT 'UGL'.
PARAMETERS: P_YEAR        LIKE COSS-GJAHR OBLIGATORY
                          DEFAULT SY-DATUM(4).
SELECT-OPTIONS: S_VERSN   FOR COSP-VERSN
                          DEFAULT '0'.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
SELECT-OPTIONS: S_POSID   FOR PRPS-POSID.
SELECT-OPTIONS: S_VERNR   FOR PRPS-VERNR.
SELECT-OPTIONS: S_TXT04   FOR TJ02T-TXT04
                NO INTERVALS.

SELECTION-SCREEN ULINE.
SELECT-OPTIONS S_KSTAR    FOR COSS-KSTAR
                DEFAULT '0000491001' TO '0000491002'.

SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
******************************************************************
* initialize
REFRESH: IPRPS, REPTAB.
CLEAR:   IPRPS, REPTAB.

*To print the variants
CALL FUNCTION 'SET_PRINT_PARAMETERS'
     EXPORTING
         COVER_PAGE = 'X'.

* set up the printing of report headers
TOP-OF-PAGE.
  IF P_SUMMRY = 'X'.
     MOVE TEXT-020 TO WRK_TEXT.
  ELSE.
     MOVE TEXT-003 TO WRK_TEXT.
  ENDIF.
  FORMAT INTENSIFIED OFF.
  ULINE.

  WRITE:   /01  SY-VLINE, TEXT-RPT, SY-REPID
          , 057 T001-BUTXT
          , 120 TEXT-002,  SY-PAGNO
          , 132 SY-VLINE.

  WRITE:   /01 SY-VLINE
          , TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID
          , 046 WRK_TEXT
          , 105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT
          , 132 SY-VLINE.

 IF SY-PAGNO > 1.
  WRITE:   /01 SY-VLINE
         , 003 TEXT-004
         ,     REPTAB-CLASS
         , 132 SY-VLINE.
 ENDIF.
  ULINE.
 IF P_DETAIL = 'X'.
  WRITE:   /02 TEXT-005
         , 016 TEXT-016
         , 022 TEXT-021
         , 026 TEXT-006
         , 033 TEXT-007
         , 049 TEXT-008
         , 094 TEXT-009
         , 101 TEXT-010
         , 111 TEXT-011.
  PERFORM SHOWVLINE.
  ULINE.
 ENDIF.


* Extract required data
START-OF-SELECTION.

* Get the company name description
  SELECT SINGLE * FROM T001
  WHERE BUKRS EQ P_COMP.

* Convert the status literals to table value
  CLEAR TJ02T.
  SELECT * FROM TJ02T
  WHERE  TXT04 IN S_TXT04
  AND    SPRAS EQ 'E'.
    STATUS-LOW    = TJ02T-ISTAT.
    STATUS-HIGH   = SPACE.
    STATUS-OPTION = 'EQ'.
    STATUS-SIGN   = 'I'.
    APPEND STATUS.
    CLEAR STATUS.
  ENDSELECT.

* -------------------------------------------------------------------- *
* Get the projects/wbs elements with a status defined in the table above

  SELECT * FROM PRPS
  WHERE POSID IN S_POSID
  AND   VERNR IN S_VERNR
  AND   PBUKR EQ P_COMP
  AND   BELKZ EQ 'X'.

    STATVAL = SPACE.
    STATDTE = SPACE.
    SELECT * FROM JEST
    WHERE OBJNR EQ PRPS-OBJNR
    AND   INACT EQ SPACE
    AND   STAT  IN STATUS.

      SELECT SINGLE * FROM JCDS
      WHERE OBJNR EQ PRPS-OBJNR
      AND   STAT  EQ JEST-STAT
      AND   CHGNR EQ JEST-CHGNR.

      IF JCDS-UDATE > STATDTE.
        STATVAL = JCDS-STAT.
        STATDTE = JCDS-UDATE.
      ENDIF.

    ENDSELECT.

    IF STATVAL <> SPACE.
      IPRPS-OBJNR = PRPS-OBJNR.
      IPRPS-POSID = PRPS-POSID.
      IPRPS-POST1 = PRPS-POST1.
      CLEAR TJ02T.
      SELECT SINGLE * FROM TJ02T
      WHERE  ISTAT EQ STATVAL
      AND    SPRAS EQ 'E'.
      IPRPS-TXT04 = TJ02T-TXT04.
      APPEND IPRPS.
      CLEAR IPRPS.
    ENDIF.

  ENDSELECT.

* -------------------------------------------------------------------- *
* Now get the remaining information required for the report

  LOOP AT IPRPS.

    CLEAR COBRA.
    SELECT SINGLE * FROM COBRA
    WHERE OBJNR EQ IPRPS-OBJNR.

    CLEAR COBRB.
    SETTLED = 'N'.
    SELECT * FROM COBRB
    WHERE OBJNR EQ IPRPS-OBJNR
    AND   KONTY EQ 'AN'                                  " fixed asset
    AND   PERBZ EQ 'GES'.                                " full setlmnt
      SETTLED       = 'Y'.
      REPTAB-CLASS  = COBRB-ANLN1(5).
      REPTAB-ANLN1  = COBRB-ANLN1.
      REPTAB-ANLN2  = COBRB-ANLN2.
      REPTAB-PROZS  = COBRB-PROZS.
      REPTAB-POSID  = IPRPS-POSID.
      REPTAB-POST1  = IPRPS-POST1.
      REPTAB-TXT04  = IPRPS-TXT04.
      REPTAB-BZDAT  = COBRA-BZDAT.
      PERFORM GET_APPLICANT_NUMBER.
      PERFORM GET_CO_AMOUNTS.
    ENDSELECT.

    IF SETTLED = 'N'.
      REPTAB-POSID  = IPRPS-POSID.
      REPTAB-POST1  = IPRPS-POST1.
      REPTAB-TXT04  = IPRPS-TXT04.
      REPTAB-BZDAT  = COBRA-BZDAT.
      PERFORM GET_APPLICANT_NUMBER.
      PERFORM GET_CO_AMOUNTS.
    ENDIF.
    CLEAR REPTAB.

  ENDLOOP.

* -------------------------------------------------------------------- *
* Sort the report table
  SORT REPTAB BY CLASS ANLN1 ANLN2 ASTNR POSID.

* -------------------------------------------------------------------- *
* Output the report
  LOOP AT REPTAB.
    IF P_SUMMRY = 'X'.
      AT NEW CLASS.                                       " asset class
        RANLN1 = REPTAB-ANLN1.
        RANLN2 = REPTAB-ANLN2.
      ENDAT.
      AT NEW ANLN1.                                       " main asset
        RANLN1 = REPTAB-ANLN1.
        RANLN2 = REPTAB-ANLN2.
      ENDAT.
      AT NEW ANLN2.                                       " sub asset
        RANLN2 = REPTAB-ANLN2.
      ENDAT.
      RPOST1 = REPTAB-POST1.
      RTXT04 = REPTAB-TXT04.
      RBZDAT = REPTAB-BZDAT.
     AT END OF PROZS.                                     " project with
        SUM.                                              " unique pct
        TOTSUB = TOTSUB + REPTAB-AMOUNT.
        CLEAR RANLN1.
        CLEAR RANLN2.
      ENDAT.
      AT END OF ANLN2.                                    " sub asset
        TOTMAIN = TOTMAIN + TOTSUB.
        CLEAR TOTSUB.
      ENDAT.
      AT END OF ANLN1.                                    " main asset
        TOTCLAS = TOTCLAS + TOTMAIN.
        CLEAR TOTMAIN.
      ENDAT.
      AT END OF CLASS.                                    " asset class
        PERFORM WRITE_CLASS_TOTAL.
        TOTREPT = TOTREPT + TOTCLAS.
        CLEAR TOTCLAS.
      ENDAT.
      AT LAST.                                            " final total
        PERFORM WRITE_REPORT_TOTAL.
      ENDAT.
    ENDIF.
  ENDLOOP.
  IF P_DETAIL = 'X'.
    PERFORM WRITE_DETAIL_REPORT.
  ENDIF.


END-OF-SELECTION.

************************************************************************
*  This section reads the COSS and COSP table for the amounts and
*  outputs one record to the internal table per cost element.
************************************************************************

FORM GET_CO_AMOUNTS.
  CLEAR WRKAMT.
  SELECT * FROM COSS                            " internal postings
  WHERE  OBJNR EQ IPRPS-OBJNR
  AND    GJAHR EQ P_YEAR
  AND    KSTAR NOT IN S_KSTAR
  AND    VERSN IN S_VERSN
  AND    WRTTP EQ '04'.
    ADD  COSS-WKG001 FROM 1 TO 16 GIVING WRKAMT.

    IF SETTLED = 'N'.
      REPTAB-AMOUNT = WRKAMT.
      APPEND REPTAB.
    ELSE.
      REPTAB-AMOUNT = ( WRKAMT * COBRB-PROZS ) / 100.
      APPEND REPTAB.
    ENDIF.
  ENDSELECT.

  CLEAR WRKAMT.
  SELECT * FROM COSP                            " external postings
  WHERE  OBJNR EQ IPRPS-OBJNR
  AND    GJAHR EQ P_YEAR
  AND    KSTAR NOT IN S_KSTAR
  AND    VERSN IN S_VERSN
  AND    WRTTP EQ '04'.
    ADD  COSP-WKG001 FROM 1 TO 16 GIVING WRKAMT.

    IF SETTLED = 'N'.
      REPTAB-AMOUNT = WRKAMT.
      APPEND REPTAB.
    ELSE.
      REPTAB-AMOUNT = ( WRKAMT * COBRB-PROZS ) / 100.
      APPEND REPTAB.
    ENDIF.
  ENDSELECT.
ENDFORM.

************************************************************************
*  Listed below are subroutines used by the program.
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  WRITE_DETAIL_REPORT
*&---------------------------------------------------------------------*
FORM WRITE_DETAIL_REPORT.
  LOOP AT REPTAB.

*First write asset class summary

    AT NEW CLASS.                                      " asset class
      RANLN1 = REPTAB-ANLN1.
      RANLN2 = REPTAB-ANLN2.
    ENDAT.
    AT NEW ANLN1.                                      " main asset
      RANLN1 = REPTAB-ANLN1.
      RANLN2 = REPTAB-ANLN2.
    ENDAT.
    AT NEW ANLN2.                                      " sub asset
      RANLN2 = REPTAB-ANLN2.
    ENDAT.
    RPOST1 = REPTAB-POST1.
    RTXT04 = REPTAB-TXT04.
    RBZDAT = REPTAB-BZDAT.
    AT END OF PROZS.                                   " project with
      SUM.                                             " unique pct
      TOTSUB = TOTSUB + REPTAB-AMOUNT.
      CLEAR RANLN1.
      CLEAR RANLN2.
    ENDAT.
    AT END OF ANLN2.                                   " sub asset
      TOTMAIN = TOTMAIN + TOTSUB.
      CLEAR TOTSUB.
    ENDAT.
    AT END OF ANLN1.                                   " main asset
      TOTCLAS = TOTCLAS + TOTMAIN.
      CLEAR TOTMAIN.
    ENDAT.
    AT END OF CLASS.                                   " asset class
      PERFORM WRITE_CLASS_TOTAL.
       TOTREPT = TOTREPT + TOTCLAS.
      CLEAR TOTCLAS.
    ENDAT.
    AT LAST.                                           " final total
      PERFORM WRITE_REPORT_TOTAL.
    ENDAT.
  ENDLOOP.
  NEW-PAGE.
* now write details
  CLEAR TOTREPT.
  LOOP AT REPTAB.
    AT NEW CLASS.                                      " asset class
      RANLN1 = REPTAB-ANLN1.
      RANLN2 = REPTAB-ANLN2.
      NEW-PAGE.
    ENDAT.
    AT NEW ANLN1.                                      " main asset
      RANLN1 = REPTAB-ANLN1.
      RANLN2 = REPTAB-ANLN2.
    ENDAT.
    AT NEW ANLN2.                                      " sub asset
      RANLN2 = REPTAB-ANLN2.
    ENDAT.
    RPOST1 = REPTAB-POST1.
    RTXT04 = REPTAB-TXT04.
    RBZDAT = REPTAB-BZDAT.
    AT END OF PROZS.                                   " project with
      SUM.                                             " unique pct
      PERFORM WRITE_DETAIL.
      TOTSUB = TOTSUB + REPTAB-AMOUNT.
      CLEAR RANLN1.
      CLEAR RANLN2.
    ENDAT.
    AT END OF ANLN2.                                   " sub asset
      PERFORM WRITE_SUB_TOTAL.
      TOTMAIN = TOTMAIN + TOTSUB.
      CLEAR TOTSUB.
    ENDAT.
    AT END OF ANLN1.                                   " main asset
      PERFORM WRITE_MAIN_TOTAL.
      TOTCLAS = TOTCLAS + TOTMAIN.
      CLEAR TOTMAIN.
    ENDAT.
    AT END OF CLASS.                                   " asset class
      PERFORM WRITE_CLASS_TOTAL.
      TOTREPT = TOTREPT + TOTCLAS.
      CLEAR TOTCLAS.
    ENDAT.
  ENDLOOP.
  PERFORM WRITE_REPORT_TOTAL.

ENDFORM.                    " WRITE_DETAIL_REPORT

*---------------------------------------------------------------------*
*       FORM WRITE_DETAIL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_DETAIL.
  WRITE: /     RANLN1         UNDER TEXT-005
         ,     RANLN2         UNDER TEXT-016
         ,     REPTAB-ASTNR   UNDER TEXT-021
         ,     REPTAB-PROZS   UNDER TEXT-006
         ,     REPTAB-POSID   UNDER TEXT-007
         ,     RPOST1         UNDER TEXT-008
         ,     RTXT04         UNDER TEXT-009
         ,     RBZDAT         UNDER TEXT-010
         ,     REPTAB-AMOUNT  UNDER TEXT-011.
  PERFORM SHOWVLINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_SUB_TOTAL                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_SUB_TOTAL.
  WRITE:   /01 SY-VLINE
         , 015 SY-VLINE.
  ULINE AT 16.
  WRITE:   /01 SY-VLINE
         , 015 SY-VLINE
         ,     TEXT-017       UNDER TEXT-016
         ,     RANLN2
         ,     TOTSUB         UNDER TEXT-011
         , 112 SY-VLINE
         , 132 SY-VLINE.
  WRITE:   /01 SY-VLINE.
  ULINE AT 15.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_MAIN_TOTAL                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_MAIN_TOTAL.
  WRITE:   /01 SY-VLINE
         ,     TEXT-018       UNDER TEXT-005
         ,     REPTAB-ANLN1
         ,     TOTMAIN        UNDER TEXT-011
         , 112 SY-VLINE
         , 132 SY-VLINE.
  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_CLASS_TOTAL                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_CLASS_TOTAL.
  WRITE:   /01 SY-VLINE
         ,     TEXT-012       UNDER TEXT-005
         ,     REPTAB-CLASS
         , 111 TOTCLAS
         , 112 SY-VLINE
         , 132 SY-VLINE.
  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_REPORT_TOTAL                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_REPORT_TOTAL.
  ULINE.
  WRITE:   /01 SY-VLINE
         ,     TEXT-013       UNDER TEXT-005
         , 111 TOTREPT
         , 112 SY-VLINE
         , 132 SY-VLINE.
  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ShowVline                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SHOWVLINE.
  WRITE:   001 SY-VLINE
         , 015 SY-VLINE
         , 021 SY-VLINE
         , 025 SY-VLINE
         , 032 SY-VLINE
         , 047 SY-VLINE
*         , 053 SY-VLINE
         , 092 SY-VLINE
         , 099 SY-VLINE
         , 112 SY-VLINE
         , 132 SY-VLINE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SHOWVLINE2
*&---------------------------------------------------------------------*

FORM SHOWVLINE2.
  WRITE:   001 SY-VLINE
         , 112 SY-VLINE
         , 132 SY-VLINE.

ENDFORM.                    " SHOWVLINE2

*&---------------------------------------------------------------------*
*&      Form  GET_APPLICANT_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_APPLICANT_NUMBER.
 DATA: A_ASTNR LIKE PROJ-ASTNR.
 IF IPRPS-POSID+0(7) <> W_PROJ.
    CLEAR W_ASTNR.
    MOVE IPRPS-POSID+0(7) TO W_PROJ.
    CONCATENATE W_PROJ '0000' INTO W_PSPID.

     SELECT  ASTNR INTO A_ASTNR FROM PROJ
       WHERE PSPID = W_PSPID.

       MOVE A_ASTNR TO W_ASTNR.
     ENDSELECT.
 ENDIF.
    MOVE W_ASTNR+6(2) TO REPTAB-ASTNR.


ENDFORM.                    " GET_APPLICANT_NUMBER


