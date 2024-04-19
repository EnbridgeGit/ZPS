REPORT ZPPMI002 NO STANDARD PAGE HEADING LINE-SIZE 114
                LINE-COUNT 65 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    ZPPMI002
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       December 1997.
*
*   The purpose of this program is to "write-up" the accumulated
*   depreciation amounts which are contained in the 9000 series of
*   WBS elements.  This is a monthly process where the will process
*   the WBS elements when there status is changed to 'Technically
*   Complete' (TECO).  Once all the required information is available,
*   a BDC session will be created to post the amounts to their
*   respective main asset.
* 98/12/16 mdemeest #619 Additional variants to handle CGO & UGl as
*                        a merged company
* 98/08/20 Janet Reid - renaming bdc session
************************************************************************

TABLES:   T001,                         " company codes
          JEST,                         " object status
          JCDS,                         " object status change documents
          PRPS,                         " WBS element master data
          COBK,                         " CO object: document header
          COSP,                         " CO object: external postings
          COSS,                         " CO object: internal postings
          Z9000.                        " WBS to asset xref

RANGES:   YEAR_RNG      FOR COEP-GJAHR,                 " year range
          PERIOD_RNG    FOR COEP-PERIO.                 " period range

DATA:     TBLDAT        LIKE SY-DATUM,                  " reformat date
          TBUDAT        LIKE SY-DATUM,                  " reformat date
          SUM_RPT(1)    VALUE 'N',                      " sumary rpt ind
          ZBLDAT(10),                                   " reformat date
          ZBUDAT(10),                                   " reformat date
          PROJECT(9),                                   " rpt project
          WBS_ELE(4),                                   " rpt WBS elemnt
          WBS_DSC(40),                                  " rpt WBS desc
          BDC_AMT(13),                                  " BDC amount
          DATEFROM      LIKE SY-DATUM,                  " TECO from date
          WRK_AMT1      LIKE COSS-WKG001,               " work amount
          WRK_AMT2      LIKE COSS-WKG001,               " work amount
          DIVID         TYPE P DECIMALS 2,              " divide field
          RMNDR         TYPE P DECIMALS 2,              " remainder
          TEXTMSG(30)   TYPE C.

DATA:     BEGIN OF EXTTAB OCCURS 2000,
            PBUKR       LIKE PRPS-PBUKR,                " company code
            OBJNR       LIKE PRPS-OBJNR,                " WBS object nbr
            POST1       LIKE PRPS-POST1,                " WBS descripton
            WBS(4),                                     " WBS element
            POSID       LIKE PRPS-POSID,                " project id
            BLDAT       LIKE COBK-BLDAT,                " transaction dt
            ANLN1       LIKE ANLA-ANLN1,                " main asset
            ANLN2       LIKE ANLA-ANLN2,                " sub asset
            GJAHR       LIKE ANBZ-GJAHR,                " status period
            PERID       LIKE ANBZ-PERID,                " status period
          END OF EXTTAB.

DATA:     BEGIN OF WBSTAB OCCURS 2000,
            PBUKR       LIKE PRPS-PBUKR,                " company code
            OBJNR       LIKE PRPS-OBJNR,                " WBS object nbr
            POST1       LIKE PRPS-POST1,                " WBS descripton
            WBS(4),                                     " WBS element
            POSID       LIKE PRPS-POSID,                " project id
*           bldat       like cobk-bldat,                " transaction dt
            ANLN1       LIKE ANLA-ANLN1,                " main asset
            ANLN2       LIKE ANLA-ANLN2,                " sub asset
            WRITUP      LIKE COSS-WKG001,               " Writeup amount
          END OF WBSTAB.

DATA:     BEGIN OF UPDTAB OCCURS 500,
            PBUKR       LIKE PRPS-PBUKR,                " company code
            ANLN1       LIKE ANLA-ANLN1,                " main asset
            ANLN2       LIKE ANLA-ANLN2,                " sub asset
            WRITUP      LIKE COSS-WKG001,               " Writeup amount
          END OF UPDTAB.

DATA:     BEGIN OF BDCDATA OCCURS 100.
            INCLUDE STRUCTURE BDCDATA.
DATA:     END OF BDCDATA.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 23(35) TEXT-003.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(60) TEXT-022.
   SELECTION-SCREEN END OF LINE.

   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(29) TEXT-017.
      PARAMETERS: P_RPT       AS CHECKBOX DEFAULT 'X'.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
 SELECT-OPTIONS: S_PBUKR  FOR PRPS-PBUKR OBLIGATORY
                          NO INTERVALS DEFAULT 'UGL'.
     PARAMETERS: P_YEAR   LIKE COSS-GJAHR OBLIGATORY
                          MODIF ID ABC DEFAULT SY-DATUM(4),
                 P_PERIOD LIKE ANBZ-PERID OBLIGATORY
                          MODIF ID ABC DEFAULT SY-DATUM+4(2),
                 P_TECODT LIKE JCDS-UDATE OBLIGATORY
                          MODIF ID ABC DEFAULT SY-DATUM,
                 P_ASSTGP(2)     OBLIGATORY.
 SELECT-OPTIONS: S_VERSN  FOR COSP-VERSN DEFAULT '0',
                 S_VERNR  FOR PRPS-VERNR.
 SELECTION-SCREEN ULINE.
 SELECT-OPTIONS  S_KSTAR  FOR COSP-KSTAR
                          DEFAULT '491001' TO '491002'.
SELECTION-SCREEN END OF BLOCK BOX2.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: EXTTAB, WBSTAB, UPDTAB, BDCDATA.
CLEAR:   EXTTAB, WBSTAB, UPDTAB, BDCDATA.
IF P_PERIOD = '01'.
  CONCATENATE P_YEAR P_PERIOD '15' INTO DATEFROM.
ELSE.
  CONCATENATE P_YEAR P_PERIOD '01' INTO DATEFROM.
ENDIF.

AT SELECTION-SCREEN ON P_ASSTGP.    "#619 - Asset Group must be 98 or 99
  IF P_ASSTGP = '98' OR P_ASSTGP = '99'.
     TEXTMSG = TEXT-021.
     IF P_ASSTGP = '98'.
        TEXTMSG+10(6) = '303200'.
     ELSE.
        TEXTMSG+10(6) = '303201'.
     ENDIF.
  ELSE.
    MESSAGE E100 WITH 'Group asset must be 98 or 99'.
  ENDIF.

* set default period as 1 less than current month and display on screen
AT SELECTION-SCREEN OUTPUT.
  P_PERIOD = P_PERIOD - 1.
  IF P_PERIOD = 0.
    P_YEAR    = P_YEAR - 1.
    P_PERIOD  = 12.
  ENDIF.
  IF P_PERIOD <> 12.
    PERFORM GET_POST_DATE.
    P_TECODT = TBUDAT.
  ENDIF.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    MODIFY SCREEN.
  ENDLOOP.

* set up the printing of report headers with correct company name
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.

* -------------------------------------------------------------------- *
* get the lowest level 9000 series WBS elements with an active
* status of I0045 (TECO - Technically Complete)

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Reading 9000 series WBS elements'
       EXCEPTIONS
            OTHERS = 1.

  SELECT * FROM PRPS
  WHERE PBUKR IN S_PBUKR
  AND   BELKZ EQ 'X'
  AND   VERNR IN S_VERNR.                                         "#619

    CHECK PRPS-POSID+7(1) = '9'.

    SELECT SINGLE * FROM JEST
    WHERE OBJNR EQ PRPS-OBJNR
    AND   INACT EQ SPACE
    AND   STAT  EQ 'I0045'.

    IF SY-SUBRC = 0.

      SELECT SINGLE * FROM JCDS
      WHERE OBJNR EQ PRPS-OBJNR
      AND   STAT  EQ JEST-STAT
      AND   CHGNR EQ JEST-CHGNR.

      IF JCDS-UDATE LE P_TECODT.
        EXTTAB-PBUKR     = PRPS-PBUKR.
        EXTTAB-OBJNR     = PRPS-OBJNR.
        EXTTAB-WBS       = PRPS-POSID+7(4).
        EXTTAB-POST1     = PRPS-POST1.
        EXTTAB-POSID     = PRPS-POSID.
*       if  jcds-udate     > datefrom
*       and jcds-udate     < p_tecodt.
*         exttab-gjahr     = p_year.
*         exttab-perid     = p_period.
*       else.
          EXTTAB-GJAHR     = JCDS-UDATE(4).
          EXTTAB-PERID     = JCDS-UDATE+4(2).
*       endif.
        APPEND EXTTAB.
        CLEAR EXTTAB.
      ENDIF.
    ENDIF.

  ENDSELECT.

  COMMIT WORK.                                    " release buffers

* -------------------------------------------------------------------- *
* now determine the asset to which the WBS element settles and save the
* information.  If the WBS element is not found in the xref table then
* delete the element as no further processing is required.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Reading assets where WBS element settles'
       EXCEPTIONS
            OTHERS = 1.

  LOOP AT EXTTAB.

    SELECT SINGLE * FROM Z9000
    WHERE WBSEL EQ EXTTAB-WBS.
    IF SY-SUBRC = 0.
      EXTTAB-ANLN1  = Z9000-ANLN1.
      EXTTAB-ANLN1+5(2) = P_ASSTGP.          "#619 Union vs Centra split
      EXTTAB-ANLN2  = '0000'.
      MODIFY EXTTAB.
    ELSE.
      DELETE EXTTAB.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.                                    " release buffers

* -------------------------------------------------------------------- *
* get all associated costs for each WBS element depending on when the
* WBS element received status of TECO.  If in the current period, then
* all the costs that the element has incurred to-date must be extracted
* otherwise just the current period data is required.  The assumption
* is that if the element has been TECO'd in the current period, then no
* costs have yet been written-up.  Otherwise, all costs to date have
* been processed and only the current period is required.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Getting all accumulated depreciation costs'
       EXCEPTIONS
            OTHERS = 1.

  LOOP AT EXTTAB.

    WRK_AMT2 = 0.
*   if exttab-gjahr   = p_year                    " determine year and
*   and exttab-perid  = p_period.                 " period range for amt
      YEAR_RNG-LOW    = '1900'.
      YEAR_RNG-HIGH   = P_YEAR.
      PERIOD_RNG-LOW  = '1'.
      PERIOD_RNG-HIGH = '12'.
*   else.
*     year_rng-low    = p_year.
*     year_rng-high   = p_year.
*     period_rng-low  = p_period.
*     period_rng-high = p_period.
*   endif.

    SELECT * FROM COSP                            " external postings
    WHERE  OBJNR EQ EXTTAB-OBJNR
    AND    GJAHR GE YEAR_RNG-LOW
    AND    GJAHR LE YEAR_RNG-HIGH
    AND    NOT KSTAR IN S_KSTAR
    AND    VERSN IN S_VERSN
    AND    WRTTP EQ '04'.

      ADD COSP-WKG001 FROM PERIOD_RNG-LOW
                        TO PERIOD_RNG-HIGH
                    GIVING WRK_AMT1.
      WRK_AMT2 = WRK_AMT2 + WRK_AMT1.

    ENDSELECT.

    SELECT * FROM COSS                            " internal postings
    WHERE  OBJNR EQ EXTTAB-OBJNR
    AND    GJAHR GE YEAR_RNG-LOW
    AND    GJAHR LE YEAR_RNG-HIGH
    AND    NOT KSTAR IN S_KSTAR
    AND    VERSN IN S_VERSN
    AND    WRTTP EQ '04'.

      ADD COSS-WKG001 FROM PERIOD_RNG-LOW
                        TO PERIOD_RNG-HIGH
                    GIVING WRK_AMT1.
      WRK_AMT2 = WRK_AMT2 + WRK_AMT1.

    ENDSELECT.

    IF WRK_AMT2 <> 0.

      MOVE-CORRESPONDING EXTTAB TO WBSTAB.
*     wbstab-bldat  = cobk-bldat.
      WBSTAB-WRITUP = WRK_AMT2.
      APPEND WBSTAB.

      UPDTAB-PBUKR  = WBSTAB-PBUKR.
      UPDTAB-ANLN1  = WBSTAB-ANLN1.
      UPDTAB-ANLN2  = WBSTAB-ANLN2.
      UPDTAB-WRITUP = WBSTAB-WRITUP.
      APPEND UPDTAB.

    ENDIF.

  ENDLOOP.

  REFRESH EXTTAB.                                 " clear table
  COMMIT WORK.                                    " release buffers

* -------------------------------------------------------------------- *
* sort the table for the report
  SORT WBSTAB BY PBUKR WBS POSID ANLN1 ANLN2.

* -------------------------------------------------------------------- *
* Process the table one last time to output the report lines

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Creating the accumulated depreciation report'
       EXCEPTIONS
            OTHERS = 1.
  LOOP AT WBSTAB.
    AT NEW PBUKR.
      NEW-PAGE.
    ENDAT.
    AT NEW WBS.
      WBS_ELE = WBSTAB-WBS.
      WBS_DSC = WBSTAB-POST1.
    ENDAT.
    AT NEW POSID.
      CONCATENATE WBSTAB-POSID+(2)  '-'
                  WBSTAB-POSID+2(2) '-'
                  WBSTAB-POSID+4(3) INTO PROJECT.
    ENDAT.
    AT END OF ANLN2.
      SUM.
      IF WBSTAB-WRITUP <> 0.
        PERFORM WRITE_DETAIL.
        WBS_ELE = SPACE.
        WBS_DSC = SPACE.
        PROJECT = SPACE.
      ENDIF.
    ENDAT.
    AT END OF WBS.
      SUM.
      IF WBSTAB-WRITUP <> 0.
        PERFORM WRITE_WBS_TOTAL.
      ENDIF.
    ENDAT.
    AT END OF PBUKR.
      SUM.
      PERFORM WRITE_COMP_TOTAL.
    ENDAT.
    AT LAST.
      SUM.
      PERFORM WRITE_RPT_TOTAL.
    ENDAT.
  ENDLOOP.

* -------------------------------------------------------------------- *
* Now create the BDC, if required, from a table created earlier, which
* only contains data that is to be posted.  Also create summary llines
* of amounts that were posted (by asset).
  COMMIT WORK.                                    " release buffers
  SORT UPDTAB BY PBUKR ANLN1 ANLN2.
  SUM_RPT = 'Y'.
  NEW-PAGE.
  IF P_RPT = SPACE.
    PERFORM OPEN_BATCH_SESSION.
    PERFORM GET_POST_DATE.
    LOOP AT UPDTAB.
      AT NEW PBUKR.
        NEW-PAGE.
      ENDAT.
      AT END OF ANLN2.
        SUM.
        IF UPDTAB-WRITUP <> 0.
          PERFORM WRITE_SUM_DETAIL.
          PERFORM POST_WRITUP_AMT.
        ENDIF.
      ENDAT.
      AT LAST.
        SUM.
        PERFORM WRITE_SUM_TOTAL.
      ENDAT.
    ENDLOOP.
    PERFORM CLOSE_BATCH_SESSION.
  ELSE.
    LOOP AT UPDTAB.
      AT END OF ANLN2.
        SUM.
        IF UPDTAB-WRITUP <> 0.
          PERFORM WRITE_SUM_DETAIL.
        ENDIF.
      ENDAT.
      AT LAST.
        SUM.
        PERFORM WRITE_SUM_TOTAL.
      ENDAT.
    ENDLOOP.
  ENDIF.

END-OF-SELECTION.

************************************************************************
*  Listed below are subroutines used by the program.
************************************************************************

FORM WRITE_HEADER.
     IF SUM_RPT = 'N'.
       SELECT SINGLE * FROM T001
       WHERE  BUKRS EQ WBSTAB-PBUKR.
     ELSE.
       SELECT SINGLE * FROM T001
       WHERE  BUKRS EQ UPDTAB-PBUKR.
     ENDIF.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE: /01 SY-VLINE, 03 TEXT-RPT, SY-REPID, 50 T001-BUTXT,
             85 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT, 114 SY-VLINE.
     WRITE: /01 SY-VLINE, TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
             42 TEXT-003, TEXT-PGE UNDER TEXT-DTE, SY-PAGNO,
            114 SY-VLINE.
     WRITE: /01 SY-VLINE, 114 SY-VLINE, TEXT-VER UNDER TEXT-CLT,
                S_VERSN+3(3).
     IF SUM_RPT = 'N'.
       WRITE: 048 TEXT-004
            ,     P_PERIOD
            ,     P_YEAR.
     ELSE.
       WRITE: 003 TEXT-018
            , 048 TEXT-004
            ,     P_PERIOD
            ,     P_YEAR.
     ENDIF.
     ULINE.
     WRITE:   /03 TEXT-006
            , 051 TEXT-007
            , 063 TEXT-008
            , 076 TEXT-009
            , 089 TEXT-010
            , 093 TEXT-011.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

FORM WRITE_DETAIL.
     WRITE: /     WBS_ELE        UNDER TEXT-006
            ,     WBS_DSC
            ,     PROJECT        UNDER TEXT-007
*           ,     wbstab-bldat   under text-008
            ,     WBSTAB-ANLN1   UNDER TEXT-009
            ,     WBSTAB-ANLN2   UNDER TEXT-010
            ,     WBSTAB-WRITUP  UNDER TEXT-011.
     PERFORM SHOWVLINE.
ENDFORM.

FORM WRITE_SUM_DETAIL.
     WRITE: /     UPDTAB-ANLN1   UNDER TEXT-009
            ,     UPDTAB-ANLN2   UNDER TEXT-010
            ,     UPDTAB-WRITUP  UNDER TEXT-011.
     PERFORM SHOWVLINE_SUM.
     WRITE:    /  SY-VLINE.
     ULINE AT 61.
ENDFORM.

FORM WRITE_WBS_TOTAL.
     FORMAT INTENSIFIED OFF.
     WRITE:    /  SY-VLINE.
     ULINE AT 49.
     WRITE:   /01 SY-VLINE
            ,     TEXT-012       UNDER WBS_DSC
            ,     WBSTAB-WRITUP  UNDER TEXT-011
            , 114 SY-VLINE.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_COMP_TOTAL.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            ,     TEXT-013       UNDER WBS_DSC
            ,     WBSTAB-WRITUP  UNDER TEXT-011
            , 114 SY-VLINE.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_RPT_TOTAL.
     FORMAT INTENSIFIED OFF.
     ULINE.
     ULINE.
     WRITE:   /01 SY-VLINE
            ,     TEXT-014       UNDER WBS_DSC
            ,     WBSTAB-WRITUP  UNDER TEXT-011
            , 114 SY-VLINE.
     ULINE.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_SUM_TOTAL.
     FORMAT INTENSIFIED OFF.
     ULINE.
     ULINE.
     WRITE:   /01 SY-VLINE
            ,     TEXT-019       UNDER WBS_DSC
            ,     UPDTAB-WRITUP  UNDER TEXT-011
            , 114 SY-VLINE.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 049 SY-VLINE
            , 061 SY-VLINE
            , 074 SY-VLINE
            , 094 SY-VLINE
            , 114 SY-VLINE.
ENDFORM.

FORM SHOWVLINE_SUM.
     WRITE:   001 SY-VLINE
            , 061 SY-VLINE
            , 074 SY-VLINE
            , 094 SY-VLINE
            , 114 SY-VLINE.
ENDFORM.

************************************************************************
*  This routine calculates the posting date as the last day of the
*  previous month.
************************************************************************

FORM GET_POST_DATE.

DATA: PST_YY(4) TYPE N,
      PST_MM(2) TYPE N,
      PST_DD(2) TYPE N.
  PST_YY = SY-DATUM+0(4).
  PST_MM = SY-DATUM+4(2).
  PST_DD = SY-DATUM+6(2).
  SUBTRACT 1 FROM PST_MM.
  IF PST_MM = 0.
    SUBTRACT 1 FROM PST_YY.
    MOVE 12 TO PST_MM.
  ENDIF.
* Determine last day of month
  CONCATENATE PST_YY PST_MM PST_DD INTO TBUDAT.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
               DAY_IN = TBUDAT
         IMPORTING
               LAST_DAY_OF_MONTH = TBUDAT
         EXCEPTIONS
                DAY_IN_NO_DATE = 1.

  TBLDAT = SY-DATUM.
* concatenate pst_yy pst_mm pst_dd into tbudat.
  WRITE TBLDAT TO ZBLDAT DD/MM/YYYY.
  WRITE TBUDAT TO ZBUDAT DD/MM/YYYY.

ENDFORM.

************************************************************************
*  Listed below is the subroutine which creates the update transactions
*  to post the accumulated depreciation amounts to the group asset as
*  a write-up.
************************************************************************

FORM POST_WRITUP_AMT.

  PERFORM SCREEN_HEADER   USING 'SAPMA01B'     '0100' 'X'.
  PERFORM SCREEN_FIELD    USING 'ANBZ-BUKRS'   UPDTAB-PBUKR.
  PERFORM SCREEN_FIELD    USING 'ANBZ-ANLN1'   UPDTAB-ANLN1.
  PERFORM SCREEN_FIELD    USING 'ANBZ-ANLN2'   '0000'.
  PERFORM SCREEN_FIELD    USING 'ANEK-BLDAT'   ZBLDAT.
  PERFORM SCREEN_FIELD    USING 'ANEK-BUDAT'   ZBUDAT.
  PERFORM SCREEN_FIELD    USING 'ANBZ-PERID'   P_PERIOD.
  PERFORM SCREEN_FIELD    USING 'ANBZ-BWASL'   '700'.

  BDC_AMT = UPDTAB-WRITUP.
  PERFORM SCREEN_HEADER   USING 'SAPMA01B'     '0140' 'X'.
  PERFORM SCREEN_FIELD    USING 'ANBZ-BZDAT'   ZBUDAT.
  PERFORM SCREEN_FIELD    USING 'ANBZ-NAFAV'   BDC_AMT.
  PERFORM SCREEN_FIELD    USING 'ANEK-SGTXT'   TEXTMSG.            "#619
*                                'CLEAR G/L 303200 TO RESERVES'.   "#619
  PERFORM SCREEN_FIELD    USING 'RA01B-BLART'  'AA'.

  PERFORM SCREEN_HEADER   USING 'SAPMA01B'     '0140' 'X'.
  PERFORM SCREEN_FIELD    USING 'BDC_OKCODE'   '/11'.            " save

  PERFORM INSERT_SESSION.
  CLEAR BDCDATA.
  REFRESH BDCDATA.

ENDFORM.

************************************************************************
*  Listed below are subroutines to open, close and process BDC data
************************************************************************

FORM OPEN_BATCH_SESSION.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = 'ZAM_ACCUMDEP'
            KEEP              = 'X'
            USER              = SY-UNAME
       EXCEPTIONS
            GROUP_INVALID     = 1
            GROUP_IS_LOCKED   = 2
            HOLDDATE_INVALID  = 3
            INTERNAL_ERROR    = 4
            QUEUE_ERROR       = 5
            RUNNING           = 6
            SYSTEM_LOCK_ERROR = 7
            USER_INVALID      = 8.
  IF SY-SUBRC <> 0.
    MESSAGE E699 WITH 'Could not open BDC session'.
  ENDIF.
ENDFORM.

FORM CLOSE_BATCH_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN
            QUEUE_ERROR.
  IF SY-SUBRC <> 0.
    MESSAGE E699 WITH 'Could not close the BDC session'.
  ENDIF.
ENDFORM.

FORM INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'ABZU'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    MESSAGE E699 WITH 'Could not insert to the BDC session'.
  ENDIF.
ENDFORM.

FORM Screen_Header using program screen indicator.
     clear BDCData.
     BDCData-program             = program.
     BDCData-dynpro              = screen.
     BDCData-dynbegin            = indicator.
     append BDCData.
ENDFORM.

FORM Screen_Field using fnam fval.
     clear BDCData.
     BDCData-fnam                = fnam.
     BDCData-fval                = fval.
     append BDCData.
ENDFORM.
