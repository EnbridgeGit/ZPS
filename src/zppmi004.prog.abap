REPORT zppmi004 NO STANDARD PAGE HEADING LINE-SIZE 114
                LINE-COUNT 65 MESSAGE-ID pp.

************************************************************************
*
*   PROGRAM:    ZPPMI004
*   PROGRAMMER: M DeMeester
*   CLIENT:     Union Gas
*   DATE:       September 1999.
************************************************************************

*   The purpose of this program is to "write-up" the accumulated
*   depreciation amounts which are contained in the 9000 series of
*   WBS elements.  This is a monthly process.
*
************************************************************************
*
* 99/09/03 mdemeest #495 Copied ZPPMI002 as a base.
*
* 00/05/16 mokhan   #737 Summry Report corrections.
*
* 00/07/10 mokhan   #--- Note in the selection screen. These changes are
*                        made as per email from Terry Laframboise on
*                        2000/07/07.
*
* 00/09/22 mokhan  #816 Sort in order of document number
*
* 00/11/13 mdemeest #--- Changed month from selection to parameter
*                        so that tvarv could be used
*
* 01/11/15 cmccoy  - issue #912 modify to reference Company Code

* 2006/01/03 mdemeest _____ Use parameter to determine posting date
*
* 2009/11/30 gymana TR582 Non-Lead ledger accumulated depreciation
*                         Create additional NL journal entries for each
*                         regulated assets. Added 2nd NL detail report
************************************************************************

TABLES:  z9000,           "WBS to asset xref
         bkpf,            "Accounting Document Header
         bseg,            "Accounting Document Detail
         prps,            "WBS element master data
         t001,            " company codes
         tka3a.           " Account Assignment

DATA:     tbudat          LIKE sy-datum,                " reformat date
          tbldat          LIKE sy-datum,                " reformat date
          zbudat(10),                                   " reformat date
          zbldat(10),                                   " reformat date
*         sum_rpt(1)    value 'N',                      " sumary rpt ind
*         project(9),                                   " rpt project
*         wbs_ele(4),                                   " rpt WBS elemnt
*         wbs_dsc(40),                                  " rpt WBS desc
          bdc_amt(13),                                  " BDC amount
          textmsg(30)   TYPE c,
          ln_cntr       TYPE I VALUE 0.


DATA: BEGIN OF exttab OCCURS 2000,
        bukrs       LIKE bkpf-bukrs,                " company code
        belnr       LIKE bkpf-belnr,                " document #
        gjahr       LIKE bkpf-gjahr,                        " year
        monat       LIKE bkpf-monat,                " posting month
        wbs(4),                                     " WBS element
        poski       LIKE prps-poski,                        " WBS
        posid       LIKE prps-posid,                " project id
        anln1       LIKE anla-anln1,                " main asset
        anln2       LIKE anla-anln2,                        " sub asset
        amt         LIKE cosp-wkg001,               " Total amts
        post1       LIKE prps-post1,                " WBS descripton
      END OF exttab.

DATA: BEGIN OF rpttab OCCURS 2000,
        bukrs       LIKE bkpf-bukrs,                " company code
        wbs(4),                                     " WBS element
        poski       LIKE prps-poski,                        " WBS
        posid       LIKE prps-posid,                " project id
        belnr       LIKE bkpf-belnr,                " document #
        gjahr       LIKE bkpf-gjahr,                        " year
        monat       LIKE bkpf-monat,                " posting month
        anln1       LIKE anla-anln1,                " main asset
        anln2       LIKE anla-anln2,                        " sub asset
        amt(10)     TYPE p DECIMALS 2,              " Total amts
        post1       LIKE prps-post1,                " WBS descripton
      END OF rpttab.

DATA: BEGIN OF updtab OCCURS 500,
        bukrs       LIKE prps-pbukr,                " company code
        anln1       LIKE anla-anln1,                " main asset
        anln2       LIKE anla-anln2,                        " sub asset
        amt(10)     TYPE p DECIMALS 2,              " Total amts
      END OF updtab.

DATA:   wa_amt(15)  type c,
        nl_amt      like updtab-amt,
        wa_dbcr(2)  type c,
        wa_txt(20)  type c.

DATA: BEGIN OF bdcdata OCCURS 100.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

************************************************************************
*   Beginning of selection screen

*SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 23(35) text-003.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK box1.
*                                                "start of changes ---
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-125.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(62) text-022.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(62) text-126.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) text-127.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK box2.
*                                                "End   of changes ---

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(29) text-017.
PARAMETERS: p_rpt       AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME.
PARAMETERS:
            p_group   like bgr00-group default 'ZAM_ABCOSTS',
            p_bukrs   LIKE bkpf-bukrs  OBLIGATORY MEMORY ID buk,
            p_hkont   LIKE bseg-hkont  OBLIGATORY DEFAULT '0000303200',
            p_year    LIKE sy-datum(4) OBLIGATORY DEFAULT sy-datum(4),
            p_mth     LIKE anbz-perid  OBLIGATORY.
*SELECT-OPTIONS: S_MTH FOR SY-DATUM+4(2) OBLIGATORY
*                                MODIF ID ABC DEFAULT '01' TO '12',
SELECT-OPTIONS: s_vernr FOR prps-vernr OBLIGATORY.
PARAMETERS: p_year1    LIKE sy-datum(4) OBLIGATORY
                                MODIF ID abc DEFAULT sy-datum(4),
            p_period   LIKE anbz-perid OBLIGATORY
                                MODIF ID abc DEFAULT sy-datum+4(2),
            p_asstgp(2)         OBLIGATORY DEFAULT '99'.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN END OF BLOCK box4.

selection-screen begin of block box5 with frame title text-500.
parameters: p_group1   like bgr00-group default 'ZAM_ACSTIFRS',
            p_tcode    like bkpf-tcode  default 'FB01L',
            p_blart    like bkpf-blart obligatory default 'IF',
            p_xblnr    like bkpf-xblnr obligatory default sy-datum,
            p_bktxt    like bkpf-bktxt obligatory
                                    default 'NL-ZPPMI004-',
            p_hkont1   like bseg-hkont obligatory default '0000319100'.
selection-screen end of block box5.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: exttab, updtab, bdcdata.
CLEAR:   exttab, updtab, bdcdata.

at selection-screen output.
  concatenate 'PstMth_' p_year1 '/' p_period into p_xblnr.


AT SELECTION-SCREEN ON p_asstgp.    "#619 -Asset Group must be 98 or 99
  IF p_asstgp = '98' OR p_asstgp = '99'.
    textmsg = text-021.            "CLEAR GL/L xxxxxx to RESERVES
    IF p_asstgp = '98'.            "CGO
      textmsg+10(6) = '303201'.
    ELSE.
      textmsg+10(6) = '303200'.   "UGL
    ENDIF.
  ELSE.
    MESSAGE e100 WITH 'Group asset must be 98 or 99'.
  ENDIF.

* set up the printing of report headers with correct company name
* Commented out to accommodate 2nd report.   gymana TR582
*TOP-OF-PAGE.
*  PERFORM write_header.

START-OF-SELECTION.

* -------------------------------------------------------------------- *
* Select all Settlement Documents for specified year/month
* -------------------------------------------------------------------- *

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Reading Document Header (BKPF) for Settled WBS'
    EXCEPTIONS
      OTHERS = 1.

  SELECT bukrs belnr gjahr monat INTO exttab FROM bkpf
    WHERE bukrs = p_bukrs
      AND gjahr = p_year
      AND monat = p_mth.
*   and blart = 'PJ'.
    APPEND exttab.
  ENDSELECT.

  COMMIT WORK.

* -------------------------------------------------------------------- *
* Select all Settlement Document Details for above list                *
* -------------------------------------------------------------------- *

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Reading Document Details (BSEG) for Settled WBS'
    EXCEPTIONS
      OTHERS = 1.

  LOOP AT exttab.
    SELECT * FROM bseg
      WHERE bukrs = exttab-bukrs
        AND belnr = exttab-belnr
        AND gjahr = exttab-gjahr
        AND hkont = p_hkont.
    ENDSELECT.
    IF sy-subrc = '0'.
      MOVE bseg-zuonr+10(4) TO exttab-wbs.
*   move bseg-zuonr(14)   to exttab-posid.
      MOVE bseg-zuonr(2)    TO exttab-posid.      "Remove the '-'
      MOVE bseg-zuonr+3(2)  TO exttab-posid+2(2).
      MOVE bseg-zuonr+6(3)  TO exttab-posid+4(3).
      MOVE bseg-zuonr+10(4) TO exttab-posid+7(4).
      MOVE bseg-zuonr       TO exttab-poski.

      IF bseg-shkzg = 'S'.
        exttab-amt = bseg-dmbtr.
      ELSE.
        exttab-amt = bseg-dmbtr * -1.
      ENDIF.
      MODIFY exttab.
    ELSE.
      DELETE exttab.
    ENDIF.
  ENDLOOP.

  COMMIT WORK.

* -------------------------------------------------------------------- *
* Ensure all WBS selected are in Z9000 (User Maintained Table          *
* -------------------------------------------------------------------- *

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Ensure selected WBS in Z9000 table'
    EXCEPTIONS
      OTHERS = 1.

  LOOP AT exttab.
    SELECT * FROM z9000
      WHERE wbsel = exttab-wbs
      AND bukrs = exttab-bukrs.              "cmccoy issue #912
    ENDSELECT.

    IF sy-subrc = '0'.
      exttab-anln1 = z9000-anln1.
      exttab-anln1+5(2) = p_asstgp.          "#619 Union vs Centra split
      exttab-anln2  = '0000'.
      MODIFY exttab.
* else.
*    delete exttab.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.

* -------------------------------------------------------------------- *
* Remove all WBS elements with no dollars to be posted                 *
* -------------------------------------------------------------------- *
  LOOP AT exttab.
    IF exttab-amt = 0.
      DELETE exttab.
    ENDIF.
  ENDLOOP.

* -------------------------------------------------------------------- *
* Find each WBS element's description from table PRPS                  *
* -------------------------------------------------------------------- *
  LOOP AT exttab.
    SELECT SINGLE * FROM prps
      WHERE poski = exttab-poski
        AND vernr IN s_vernr.
    IF sy-subrc = 0.
      exttab-post1 = prps-post1.
      MODIFY exttab.
* else.
*    delete exttab.
    ENDIF.
  ENDLOOP.

  IF p_rpt = 'X'.
    PERFORM display_report.
    PERFORM display_summary_report.
    PERFORM display_nonledger_report.
  ELSE.
    PERFORM display_report.
    PERFORM display_summary_report.
    PERFORM display_nonledger_report.
    PERFORM create_bdc.
    perform create_bdc_nl.
  ENDIF.

END-OF-SELECTION.

*****************************  SUBROUTINES  ****************************

* ---------------------------- DISPLAY REPORT  ----------------------- *
* Write report                                                         *
* -------------------------------------------------------------------- *
FORM display_report.
  LOOP AT exttab.
    MOVE-CORRESPONDING exttab TO rpttab.
    IF exttab-wbs   = space.                                " 816
      rpttab-poski = space.                                 " 816
      rpttab-posid = space.                                 " 816
    ENDIF.                                                  " 816
    APPEND rpttab.
  ENDLOOP.

*SORT RPTTAB BY BUKRS WBS POSID.                              " 816
  SORT rpttab BY bukrs wbs posid belnr.                     " 816

  LOOP AT rpttab.
    AT NEW bukrs.
      NEW-PAGE.
      ln_cntr = 0.
      perform write_header.
    ENDAT.

    IF ln_cntr >= 62.
      NEW-PAGE.
      ln_cntr = 0.
      PERFORM write_header.
    endif.

    AT NEW wbs.
      PERFORM showvline.
      WRITE: rpttab-wbs UNDER text-006.      "WBS Element
      ln_cntr = ln_cntr + 1.
    ENDAT.

    PERFORM showvline.

    IF rpttab-wbs = space.
      WRITE: (10) rpttab-belnr    UNDER text-007,
               rpttab-anln1 UNDER text-009, rpttab-anln2 UNDER text-010,
            (16) rpttab-amt   UNDER text-011.
    ELSE.
      WRITE: (10) rpttab-posid(7) UNDER text-007
                                  USING EDIT MASK '__-__-___',
                  rpttab-anln1 UNDER text-009,
                  rpttab-anln2 UNDER text-010,
             (16) rpttab-amt   UNDER text-011,
               10 rpttab-post1(30).
    ENDIF.
    ln_cntr = ln_cntr + 1.

    AT END OF wbs.
      SUM.
      PERFORM showvline_sum.
      WRITE: (16) rpttab-amt UNDER text-011, 10 text-012.
      ULINE.
      ln_cntr = ln_cntr + 2.
    ENDAT.

    AT LAST.
      SUM.
      PERFORM showvline_total.
      WRITE: text-013 UNDER text-006,
             (16) rpttab-amt UNDER text-011.
      ULINE.
      ln_cntr = ln_cntr + 4.
*      move rpttab-amt to wa_amt.
    ENDAT.

  ENDLOOP.

ENDFORM.                    "display_report
* ---------------------------- DISPLAY_SUMMARY_REPORT----------------- *
* Write report                                                         *
* -------------------------------------------------------------------- *
FORM display_summary_report.

* summarizes project amounts by asset number

  LOOP AT exttab.
    IF exttab-anln1 <> space.
      MOVE-CORRESPONDING exttab TO updtab.
      COLLECT updtab.
    ENDIF.
  ENDLOOP.

  SORT updtab BY bukrs anln1 anln2.

  LOOP AT updtab.
    AT NEW bukrs.
      NEW-PAGE.
      ln_cntr = 0.
      PERFORM write_header.
    ENDAT.

    IF ln_cntr >= 62.
      NEW-PAGE.
      ln_cntr = 0.
      PERFORM write_header.
    ENDIF.

* at end of anln1.                       #737 start of changes
*   sum.
*   perform showvline.
*   write:  rpttab-anln1 under text-009, rpttab-anln2 under text-010,
*      (16) rpttab-amt   under text-011.
*   uline.
* endat.

    PERFORM showvline.
    WRITE:  updtab-anln1 UNDER text-009, updtab-anln2 UNDER text-010,
       (16) updtab-amt   UNDER text-011.
    ULINE.
    ln_cntr = ln_cntr + 2.

    AT LAST.
      SUM.
      PERFORM showvline.
      WRITE: (16) updtab-amt  UNDER text-011, text-013 UNDER text-006.
*   write: (16) rpttab-amt  under text-011, text-013 under text-006.
      ULINE.
      ln_cntr = ln_cntr + 2.
    ENDAT.

*                                        #737 End of changes
  ENDLOOP.

ENDFORM.                    "display_summary_report

* -------------------------- DISPLAY_NONLEDGER_REPORT----------------- *
* Write report                                                         *
* -------------------------------------------------------------------- *
FORM display_nonledger_report.

* Display Journal entries for Non-Ledger Assets

  LOOP AT updtab.
    AT NEW bukrs.
      NEW-PAGE.
      ln_cntr = 0.
      PERFORM write_nl_header.
    ENDAT.

    IF ln_cntr >= 62.
      NEW-PAGE.
      ln_cntr = 0.
      PERFORM write_nl_header.
    ENDIF.

    IF updtab-amt > 0.
      MOVE '50' to wa_dbcr.
      nl_amt = updtab-amt.
    ELSE.
      MOVE '40' to wa_dbcr.
      nl_amt = updtab-amt * -1.
    ENDIF.
    CONCATENATE p_bktxt updtab-anln1 into wa_txt.
    PERFORM showvline_nl.
    WRITE:  wa_dbcr         UNDER text-024,
            p_hkont         UNDER text-025,
            (16) nl_amt     UNDER text-026,
            wa_txt          UNDER text-027,
            (16) updtab-amt UNDER text-028.
    ULINE.
    ln_cntr = ln_cntr + 2.

    IF updtab-amt > 0.
      MOVE '40' to wa_dbcr.
      nl_amt = updtab-amt.
    ELSE.
      MOVE '50' to wa_dbcr.
      nl_amt = updtab-amt * -1.
    ENDIF.
    CONCATENATE p_bktxt updtab-anln1 into wa_txt.
    PERFORM showvline_nl.
    WRITE:  wa_dbcr         UNDER text-024,
            p_hkont1        UNDER text-025,
            (16) nl_amt     UNDER text-026,
            wa_txt          UNDER text-027.
    ULINE.
    ln_cntr = ln_cntr + 2.

    AT LAST.
      SUM.
      PERFORM showvline_nl.
      WRITE: text-013        UNDER text-024,
             (16) updtab-amt UNDER text-028.
      ULINE.
    ENDAT.

  ENDLOOP.

ENDFORM.                    "display_nonledger_report

* ------------------------  CREATE_BDC  ------------------------------ *
* Create BDC session, if required.                                     *
* -------------------------------------------------------------------- *
FORM create_bdc.

  PERFORM open_batch_session using p_group.

  SORT updtab BY bukrs anln1 anln2.
  PERFORM get_post_date.

  LOOP AT updtab.
    PERFORM post_writup_amt.
  ENDLOOP.
  PERFORM close_batch_session.

ENDFORM.                    "create_bdc

*--------------------------------------------------------------------- *
*  Listed below are subroutines to open, close and process BDC data
*--------------------------------------------------------------------- *

FORM open_batch_session using bdcsessionname.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = bdcsessionname  "'ZAM_ACCUMDEP'
      keep              = 'X'
      user              = sy-uname
    EXCEPTIONS
      group_invalid     = 1
      group_is_locked   = 2
      holddate_invalid  = 3
      internal_error    = 4
      queue_error       = 5
      running           = 6
      system_lock_error = 7
      user_invalid      = 8.
  IF sy-subrc <> 0.
    MESSAGE e699 WITH 'Could not open BDC session'.
  ENDIF.
ENDFORM.                    "open_batch_session

*---------------------------------------------------------------------*
*       FORM CLOSE_BATCH_SESSION                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM close_batch_session.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc <> 0.
    MESSAGE e699 WITH 'Could not close the BDC session'.
  ENDIF.
ENDFORM.                    "close_batch_session

*---------------------------------------------------------------------*
*       FORM INSERT_SESSION                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM insert_session using transcode.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = transcode          "'ABZU'
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0.
    MESSAGE e699 WITH 'Could not insert to the BDC session'.
  ENDIF.
ENDFORM.                    "insert_session

*---------------------------------------------------------------------*
*       FORM Screen_Header                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  program                                                       *
*  -->  screen                                                        *
*  -->  indicator                                                     *
*---------------------------------------------------------------------*
FORM screen_header USING program screen indicator.
  CLEAR bdcdata.
  bdcdata-program             = program.
  bdcdata-dynpro              = screen.
  bdcdata-dynbegin            = indicator.
  APPEND bdcdata.
ENDFORM.                    "screen_header

*---------------------------------------------------------------------*
*       FORM Screen_Field                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  fnam                                                          *
*  -->  fval                                                          *
*---------------------------------------------------------------------*
FORM screen_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam                = fnam.
  bdcdata-fval                = fval.
  APPEND bdcdata.
ENDFORM.                    "screen_field

*--------------------------  WRITE_HEADER  --------------------------- *
FORM write_header.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  FORMAT INTENSIFIED OFF.
  ULINE.

  WRITE: /01 sy-vline, 03 text-rpt, sy-repid, 40 t001-butxt,
          85 text-dte, sy-datum, text-amp, sy-uzeit, 114 sy-vline.
  if p_bukrs = 'EMP'.
  ELSEIF p_asstgp = '98'.
    WRITE:  60 text-101.
  ELSEIF p_asstgp = '99'.
    WRITE: 60 text-100.
  ENDIF.

  WRITE: /01 sy-vline, text-clt UNDER text-rpt, sy-mandt, sy-sysid,
          42 text-003, text-pge UNDER text-dte, sy-pagno,
          114 sy-vline.

  WRITE: /01 sy-vline, text-004 UNDER text-003, p_period, p_year,
                                                114 sy-vline.

  ULINE.
  PERFORM showvline.
  WRITE:     03 text-006
          , 050 text-007
          , 063 text-008
          , 075 text-009
          , 088 text-010
          , 094 text-011.
  FORMAT INTENSIFIED ON.
  ULINE.
  ln_cntr = ln_cntr + 7.
ENDFORM.                    "write_header

*------------------------  WRITE_NL_HEADER  -------------------------- *
FORM write_NL_header.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  FORMAT INTENSIFIED OFF.
  ULINE.

  WRITE: /01 sy-vline, 03 text-rpt, sy-repid, 40 t001-butxt,
          85 text-dte, sy-datum, text-amp, sy-uzeit, 114 sy-vline.
  if p_bukrs = 'EMP'.
  ELSEIF p_asstgp = '98'.
    WRITE:  60 text-101.
  ELSEIF p_asstgp = '99'.
    WRITE: 60 text-100.
  ENDIF.

  WRITE: /01 sy-vline, text-clt UNDER text-rpt, sy-mandt, sy-sysid,
          42 text-023, text-pge UNDER text-dte, sy-pagno,
          114 sy-vline.

  WRITE: /01 sy-vline, text-004 UNDER text-023, p_period, p_year,
                                                114 sy-vline.

  ULINE.
  PERFORM showvline_nl.
  WRITE:     03 text-024
          , 011 text-025
          , 024 text-026
          , 044 text-027
          , 093 text-028.
  FORMAT INTENSIFIED ON.
  ULINE.
  ln_cntr = ln_cntr + 7.
ENDFORM.                    "write_NL header

*---------------------------------------------------------------------*
*       FORM ShowVline                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM showvline.
  WRITE: /001 sy-vline, 009 sy-vline, 049 sy-vline, 061 sy-vline,
          074 sy-vline, 093 sy-vline, 114 sy-vline.
ENDFORM.                    "showvline

*---------------------------------------------------------------------*
*       FORM SHOWVLINE_SUM                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM showvline_sum.
  WRITE: /001 sy-vline, 093 sy-vline, 114 sy-vline.
  WRITE: 009 sy-uline(120).
  WRITE: /001 sy-vline, 093 sy-vline, 114 sy-vline.
ENDFORM.                    "showvline_sum

*---------------------------------------------------------------------*
*       FORM SHOWVLINE_TOTAL                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM showvline_total.
  SKIP 1.
  ULINE.
  WRITE: /001 sy-vline, 093 sy-vline, 114 sy-vline.
ENDFORM.                    "showvline_total

*---------------------------------------------------------------------*
*       FORM ShowVline_NL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM showvline_nl.
  WRITE: /001 sy-vline, 009 sy-vline, 022 sy-vline, 041 sy-vline,
          090 sy-vline, 114 sy-vline.
ENDFORM.                    "showvline

************************************************************************
*  This routine calculates the posting date as the last day of the
*  previous month.
************************************************************************

FORM get_post_date.

*----------------------------------------------------------------------
* 2006/01/03 mdemeest Use parameter to determine posting date
*----------------------------------------------------------------------
*  DATA: pst_yy(4) TYPE n,
*        pst_mm(2) TYPE n,
*        pst_dd(2) TYPE n.
*  pst_yy = sy-datum+0(4).
*  pst_mm = sy-datum+4(2).
*  pst_dd = sy-datum+6(2).
*  SUBTRACT 1 FROM pst_mm.
*  IF pst_mm = 0.
*    SUBTRACT 1 FROM pst_yy.
*    MOVE 12 TO pst_mm.
*  ENDIF.
* Determine last day of month
*  CONCATENATE pst_yy pst_mm pst_dd INTO tbudat.
  concatenate p_year1 p_period sy-datum+6(2) into tbudat.
*----------------------------------------------------------------------
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = tbudat
    IMPORTING
      last_day_of_month = tbudat
    EXCEPTIONS
      day_in_no_date    = 1.

  tbldat = sy-datum.
*concatenate pst_yy pst_mm pst_dd into tbudat.
  WRITE tbldat TO zbldat DD/MM/YYYY.
  WRITE tbudat TO zbudat DD/MM/YYYY.

ENDFORM.                    "get_post_date

************************************************************************
*  Listed below is the subroutine which creates the update transactions
*  to post the accumulated depreciation amounts to the group asset as
*  a write-up.
************************************************************************

FORM post_writup_amt.

  PERFORM screen_header   USING 'SAPMA01B'     '0100' 'X'.
  PERFORM screen_field    USING 'ANBZ-BUKRS'   updtab-bukrs.
  PERFORM screen_field    USING 'ANBZ-ANLN1'   updtab-anln1.
  PERFORM screen_field    USING 'ANBZ-ANLN2'   '0000'.
  PERFORM screen_field    USING 'ANEK-BLDAT'   zbldat.
  PERFORM screen_field    USING 'ANEK-BUDAT'   zbudat.
  PERFORM screen_field    USING 'ANBZ-PERID'   p_period.
  PERFORM screen_field    USING 'ANBZ-BWASL'   '702'.

  bdc_amt = updtab-amt.
  PERFORM screen_header   USING 'SAPMA01B'     '0140' 'X'.
  PERFORM screen_field    USING 'ANBZ-BZDAT'   zbudat.
  PERFORM screen_field    USING 'ANBZ-NAFAV'   bdc_amt.
  PERFORM screen_field    USING 'ANEK-SGTXT'   textmsg.     "#619
*                                'CLEAR G/L 303200 TO RESERVES'.   "#619
  PERFORM screen_field    USING 'RA01B-BLART'  'AA'.

  PERFORM screen_header   USING 'SAPMA01B'     '0140' 'X'.
  PERFORM screen_field    USING 'BDC_OKCODE'   '/11'.       " save

  PERFORM insert_session using 'ABZU'.
  CLEAR bdcdata.
  REFRESH bdcdata.

ENDFORM.                    "post_writup_amt

*&---------------------------------------------------------------------*
*&      Form  create_bdc_nl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_bdc_nl.
  PERFORM open_batch_session using p_group1.

  LOOP AT updtab.
    PERFORM post_nl_ledger.
    PERFORM insert_session using p_tcode.
    REFRESH bdcdata.
  ENDLOOP.

  PERFORM close_batch_session.
endform.                    "create_bdc_nl

*------------------------------------------------------------
* TRANSACTION - FB01L
*------------------------------------------------------------
form post_nl_ledger.
* Screen 0100
  PERFORM screen_header   USING 'SAPMF05A'     '0100' 'X'.
  perform screen_field    using 'BDC_CURSOR'   'RF05A-NEWKO'.
  perform screen_field    using 'BDC_OKCODE'   '/00'.
  PERFORM screen_field    USING 'BKPF-BLDAT'   zbldat.
  perform screen_field    using 'BKPF-BLART'   p_blart.
  perform screen_field    using 'BKPF-BUKRS'   p_bukrs.
  perform screen_field    using 'BKPF-BUDAT'   zbudat.
  perform screen_field    using 'BKPF-MONAT'   p_period.
  perform screen_field    using 'BKPF-WAERS'   'CAD'.     "check
  perform screen_field    using 'BKPF-LDGRP'   'NL'.      "check
  perform screen_field    using 'BKPF-XBLNR'   p_xblnr.
  CONCATENATE p_bktxt updtab-anln1 into wa_txt.
  perform screen_field    using 'BKPF-BKTXT'   wa_txt.
* Posting keys screen 100

  if updtab-amt > 0.
    perform screen_field    using 'RF05A-NEWBS'  '50'.
  else.
    perform screen_field    using 'RF05A-NEWBS'  '40'.
  endif.

  perform screen_field    using 'RF05A-NEWKO'  p_hkont.
  perform screen_field    using 'BDC_SUBSCR'
            'SAPMF05A                                1300APPL_SUB_T'.
  perform screen_field    using 'BDC_SUBSCR'
            'SAPLSEXM                                0200APPL_SUB'.

* Screen 0300 - side #1
  PERFORM screen_header   USING 'SAPMF05A'     '0300' 'X'.
  move ABS( updtab-amt ) to wa_amt.
  perform screen_field    USING 'BSEG-WRBTR'   wa_amt.
  perform screen_field    USING 'BSEG-SGTXT'   wa_txt.

* Coding block
  perform screen_header   using 'SAPLKACB'     '0002' 'X'.
  select single * from tka3a
     where bukrs = p_bukrs
       and kstar = p_hkont.
  if sy-subrc = '0'.
    PERFORM screen_field USING 'COBL-KOSTL'  tka3a-kostl.
  endif.
  perform screen_field    using 'BDC_OKCODE'   '/8'.

* Screen 0300
  PERFORM screen_header   USING 'SAPMF05A'     '0300' 'X'.

  if updtab-amt > 0.
    perform screen_field    using 'RF05A-NEWBS'  '40'.
  else.
    perform screen_field    using 'RF05A-NEWBS'  '50'.
  endif.

  perform screen_field    USING 'RF05A-NEWKO'   p_hkont1.
  perform screen_field    USING 'BDC_SUBSCR'
            'SAPLKACB                                0001BLOCK'.
*  perform screen_field    USING 'DKACB-FMORE'   'X'.

*---------------------------------------------------------------------
*.
  PERFORM screen_header   USING 'SAPLKACB' '0002' 'X'.
  perform screen_field    USING 'BDC_OKCODE'   '/8'.

* Screen 300
  PERFORM SCREEN_HEADER   USING 'SAPMF05A'     '0300' 'X'.
  perform screen_field    USING 'BSEG-WRBTR'   wa_amt.

  perform screen_header   using 'SAPLKACB'     '0002' 'X'.
  perform screen_field    using 'BDC_OKCODE'   '/8'.

  PERFORM screen_header   USING 'SAPMF05A'     '0300' 'X'.
  perform screen_field    USING 'BDC_OKCODE'   '/11'.

  perform screen_header   using 'SAPLKACB'     '0002' 'X'.
  perform screen_field    USING 'BDC_OKCODE'   '/8'.

endform.                    "post_nl_ledger
