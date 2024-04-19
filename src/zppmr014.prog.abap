REPORT zppmr014 NO STANDARD PAGE HEADING LINE-SIZE 132
                LINE-COUNT 65 MESSAGE-ID pp.

************************************************************************
*
*   PROGRAM:    ZPPMR014
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       May 1998.
*
*   MODIFIED: December 18 2012 by Mohammad Khan
*   Make changes to apply settlement rules.     SDP35713
*
*   MODIFIED: October 2001 by Cathy McCoy
*
*   The purpose of this program is to list capital expenditures by
*   project and asset number.  The report is in cost element, asset
*   class and asset sequence with totals at the end of each sub asset.
*   main asset, asset class, cost element and at report end.  The
*   report lists the asset main and asset sub numbers, the project and
*   its description, the status, the asset value date and the amount
*   capitalized.  The selection screen for the report contains the
*   company code, year, version, WBS element, division status and
*   cost element.
*   Modifications have removed harcoding from Status and added checkbox
*   for the option of outputting Summary for Asset Class Only. Also the
*   summary for the detail report now writen at beginning of report.
************************************************************************

TABLES:   t001,                         " company codes
          jest,                         " object status
          jcds,                         " object status change documents
          tj02t,                        " object status description
          prps,                         " WBS element master data
          coss,                         " CO object: internal postings
          cosp,                         " CO object: external postings
          cobra,                        " Settlement rules by object nbr
          cobrb.                        " Settlement rules by object nbr

RANGES:   status        FOR jest-stat.                  " status range

DATA:     statval       LIKE jcds-stat,                 " status value
          statdte       LIKE jcds-udate,                " status eff dte
          statime       LIKE jcds-utime,                " status eff tme
          wrkamt        LIKE coss-wkg001,               " work amount
          totsub        LIKE coss-wkg001,               " sub ass total
          totmain       LIKE coss-wkg001,               " main ass total
          totclas       LIKE coss-wkg001,               " class total
          totelem       LIKE coss-wkg001,               " cost ele total
          totrept       LIKE coss-wkg001,               " report total
          ranln1        LIKE cobrb-anln1,                   " rpt asset
          ranln2        LIKE cobrb-anln2,               " rpt sub asset
          rpost1        LIKE prps-post1,                " rpt WBS desc
          rbzdat        LIKE cobra-bzdat,               " rpt asset date
          rtxt04        LIKE tj02t-txt04,               " rpt status
          settled(1)    TYPE c.                         " settlement ind

DATA:     BEGIN OF iprps OCCURS 1000,
            objnr       LIKE prps-objnr,                " object number
            posid       LIKE prps-posid,                " project id
            post1       LIKE prps-post1,                " description
            txt04       LIKE tj02t-txt04,                   " status
          END OF iprps.

DATA:     BEGIN OF reptab OCCURS 1000,
            kstar       LIKE coss-kstar,                " cost element
            class(5)    TYPE c,                         " asset class
            anln1       LIKE cobrb-anln1,               " main asset
            anln2       LIKE cobrb-anln2,                   " sub asset
            posid       LIKE prps-posid,                " project id
            prozs       LIKE cobrb-prozs,               " allocation %
            post1       LIKE prps-post1,                " description
            txt04       LIKE tj02t-txt04,                   " status
            bzdat       LIKE cobra-bzdat,               " asset value dt
            amount      LIKE coss-wkg001,               " capitalize amt
          END OF reptab.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 20(40) text-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
PARAMETERS: p_comp        LIKE prps-pbukr OBLIGATORY
                          DEFAULT 'UGL'.
PARAMETERS: p_year        LIKE coss-gjahr OBLIGATORY
                          DEFAULT sy-datum(4).
SELECT-OPTIONS: s_versn   FOR cosp-versn
                          DEFAULT '0'.
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME.
SELECT-OPTIONS: s_posid   FOR prps-posid.
SELECT-OPTIONS: s_vernr   FOR prps-vernr.
SELECT-OPTIONS: s_txt04   FOR tj02t-txt04
                NO INTERVALS.

PARAMETERS: p_class AS CHECKBOX.
SELECTION-SCREEN ULINE.
SELECT-OPTIONS s_kstar    FOR coss-kstar.
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN END OF BLOCK box.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: iprps, reptab.
CLEAR:   iprps, reptab.

* set up the printing of report headers
TOP-OF-PAGE.
if p_class = 'X'.
    PERFORM write_header2.
    else.
    perform write_header.
    endif.


* extract required data
START-OF-SELECTION.

* get the company name description
  SELECT SINGLE * FROM t001
  WHERE bukrs EQ p_comp.

* convert the status literals to table value
  CLEAR tj02t.
  SELECT * FROM tj02t
  WHERE  txt04 IN s_txt04
  AND    spras EQ 'E'.
    status-low    = tj02t-istat.
    status-high   = space.
    status-option = 'EQ'.
    status-sign   = 'I'.
    APPEND status.
    CLEAR status.
  ENDSELECT.

* -------------------------------------------------------------------- *
* get the projects/wbs elements with a status defined in the table above

  SELECT * FROM prps
  WHERE posid IN s_posid
  AND   vernr IN s_vernr
  AND   pbukr EQ p_comp
  AND   belkz EQ 'X'.

    statval = space.
    statdte = space.
    SELECT * FROM jest
    WHERE objnr EQ prps-objnr
    AND   inact EQ space
    AND   stat  IN status.

      SELECT SINGLE * FROM jcds
      WHERE objnr EQ prps-objnr
      AND   stat  EQ jest-stat
      AND   chgnr EQ jest-chgnr.

      IF jcds-udate > statdte.
        statval = jcds-stat.
        statdte = jcds-udate.
      ENDIF.

    ENDSELECT.

    IF statval <> space.
      iprps-objnr = prps-objnr.
      iprps-posid = prps-posid.
      iprps-post1 = prps-post1.
      CLEAR tj02t.
      SELECT SINGLE * FROM tj02t
      WHERE  istat EQ statval
      AND    spras EQ 'E'.
      iprps-txt04 = tj02t-txt04.
      APPEND iprps.
      CLEAR iprps.
    ENDIF.

  ENDSELECT.

* -------------------------------------------------------------------- *
* now get the remaining information required for the report

  LOOP AT iprps.

    CLEAR cobra.
    SELECT SINGLE * FROM cobra
    WHERE objnr EQ iprps-objnr.

    CLEAR cobrb.
    settled = 'N'.
    SELECT * FROM cobrb
    WHERE objnr EQ iprps-objnr
    AND   konty EQ 'AN'                                  " fixed asset
    AND   perbz EQ 'GES'                                 " full setlmnt
    AND   GABJA LE P_YEAR                                "SDP35713
    AND   GBISJ GE P_YEAR.                               "SDP35713

      settled       = 'Y'.
      reptab-class  = cobrb-anln1(5).
      reptab-anln1  = cobrb-anln1.
      reptab-anln2  = cobrb-anln2.
      reptab-prozs  = cobrb-prozs.
      reptab-posid  = iprps-posid.
      reptab-post1  = iprps-post1.
      reptab-txt04  = iprps-txt04.
      reptab-bzdat  = cobra-bzdat.
      PERFORM get_co_amounts.
    ENDSELECT.

    IF settled = 'N'.
      reptab-posid  = iprps-posid.
      reptab-post1  = iprps-post1.
      reptab-txt04  = iprps-txt04.
      reptab-bzdat  = cobra-bzdat.
      PERFORM get_co_amounts.
    ENDIF.
    CLEAR reptab.

  ENDLOOP.

* -------------------------------------------------------------------- *
* sort the report table
  SORT reptab BY kstar class anln1 anln2 posid.

* -------------------------------------------------------------------- *
* output the report
  LOOP AT reptab.
    IF p_class = 'X'.
     AT NEW kstar.                                        " cost element
        ranln1 = reptab-anln1.
        ranln2 = reptab-anln2.
      ENDAT.
      AT NEW class.                                        " asset class
        ranln1 = reptab-anln1.
        ranln2 = reptab-anln2.
      ENDAT.
      AT NEW anln1.                                        " main asset
        ranln1 = reptab-anln1.
        ranln2 = reptab-anln2.
      ENDAT.
      AT NEW anln2.                                         " sub asset
        ranln2 = reptab-anln2.
      ENDAT.
      rpost1 = reptab-post1.
      rtxt04 = reptab-txt04.
      rbzdat = reptab-bzdat.
     AT END OF prozs.                                     " project with
        SUM.                                               " unique pct
        totsub = totsub + reptab-amount.
        CLEAR ranln1.
        CLEAR ranln2.
      ENDAT.
      AT END OF anln2.                                      " sub asset
        totmain = totmain + totsub.
        CLEAR totsub.
      ENDAT.
      AT END OF anln1.                                     " main asset
        totclas = totclas + totmain.
        CLEAR totmain.
      ENDAT.
      AT END OF class.                                     " asset class
        PERFORM write_class_total.
        totelem = totelem + totclas.
        CLEAR totclas.
      ENDAT.
     AT END OF kstar.                                     " cost element
        totrept = totrept + totelem.
        CLEAR totelem.
      ENDAT.
      AT LAST.                                             " final total
        PERFORM write_report_total.
      ENDAT.
          ENDIF.
        ENDLOOP.
if p_class = ' '.
perform write_detail_report.
endif.


END-OF-SELECTION.

************************************************************************
*  This section reads the COSS and COSP table for the amounts and
*  outputs one record to the internal table per cost element.
************************************************************************

FORM get_co_amounts.
  CLEAR wrkamt.
  SELECT * FROM coss                            " internal postings
  WHERE  objnr EQ iprps-objnr
  AND    gjahr EQ p_year
  AND    kstar IN s_kstar
  AND    versn IN s_versn
  AND    wrttp EQ '04'.
    wrkamt = coss-wkg001 + coss-wkg002 + coss-wkg003 + coss-wkg004
           + coss-wkg005 + coss-wkg006 + coss-wkg007 + coss-wkg008
           + coss-wkg009 + coss-wkg010 + coss-wkg011 + coss-wkg012
           + coss-wkg013 + coss-wkg014 + coss-wkg015 + coss-wkg016.
    reptab-kstar = coss-kstar.
    IF settled = 'N'.
      reptab-amount = wrkamt.
      APPEND reptab.
    ELSE.
      reptab-amount = ( wrkamt * cobrb-prozs ) / 100.
      APPEND reptab.
    ENDIF.
  ENDSELECT.

  CLEAR wrkamt.
  SELECT * FROM cosp                            " external postings
  WHERE  objnr EQ iprps-objnr
  AND    gjahr EQ p_year
  AND    kstar IN s_kstar
  AND    versn IN s_versn
  AND    wrttp EQ '04'.
    wrkamt = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 + cosp-wkg004
           + cosp-wkg005 + cosp-wkg006 + cosp-wkg007 + cosp-wkg008
           + cosp-wkg009 + cosp-wkg010 + cosp-wkg011 + cosp-wkg012
           + cosp-wkg013 + cosp-wkg014 + cosp-wkg015 + cosp-wkg016.
    reptab-kstar = cosp-kstar.
    IF settled = 'N'.
      reptab-amount = wrkamt.
      APPEND reptab.
    ELSE.
      reptab-amount = ( wrkamt * cobrb-prozs ) / 100.
      APPEND reptab.
    ENDIF.
  ENDSELECT.
ENDFORM.

************************************************************************
*  Listed below are subroutines used by the program.
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  WRITE_DETAIL_REPORT
*&---------------------------------------------------------------------*
FORM write_detail_report.
  LOOP AT reptab.

*first write asset class summary

    AT NEW kstar.                                       " cost element
      ranln1 = reptab-anln1.
      ranln2 = reptab-anln2.
    ENDAT.
    AT NEW class.                                      " asset class
      ranln1 = reptab-anln1.
      ranln2 = reptab-anln2.
    ENDAT.
    AT NEW anln1.                                        " main asset
      ranln1 = reptab-anln1.
      ranln2 = reptab-anln2.
    ENDAT.
    AT NEW anln2.                                           " sub asset
      ranln2 = reptab-anln2.
    ENDAT.
    rpost1 = reptab-post1.
    rtxt04 = reptab-txt04.
    rbzdat = reptab-bzdat.
    AT END OF prozs.                                  " project with
      SUM.                                              " unique pct
      totsub = totsub + reptab-amount.
      CLEAR ranln1.
      CLEAR ranln2.
    ENDAT.
    AT END OF anln2.                                        " sub asset
      totmain = totmain + totsub.
      CLEAR totsub.
    ENDAT.
    AT END OF anln1.                                     " main asset
      totclas = totclas + totmain.
      CLEAR totmain.
    ENDAT.
    AT END OF class.                               " asset class
      PERFORM write_class_total.
      totelem = totelem + totclas.
      CLEAR totclas.
    ENDAT.
    AT END OF kstar.                                   " cost element
      totrept = totrept + totelem.
      CLEAR totelem.
    ENDAT.
    AT LAST.                                           " final total
      PERFORM write_report_total.
    ENDAT.
  ENDLOOP.
  NEW-PAGE.
* now write details
clear totrept.
  LOOP AT reptab.
    AT NEW kstar.                                        " cost element
      ranln1 = reptab-anln1.
      ranln2 = reptab-anln2.
      NEW-PAGE.

    ENDAT.
    AT NEW class.                                        " asset class
      ranln1 = reptab-anln1.
      ranln2 = reptab-anln2.
      NEW-PAGE.
    ENDAT.
    AT NEW anln1.                                        " main asset
      ranln1 = reptab-anln1.
      ranln2 = reptab-anln2.
    ENDAT.
    AT NEW anln2.                                           " sub asset
      ranln2 = reptab-anln2.
    ENDAT.
    rpost1 = reptab-post1.
    rtxt04 = reptab-txt04.
    rbzdat = reptab-bzdat.
    AT END OF prozs.                                     " project with
      SUM.                                               " unique pct
      PERFORM write_detail.
      totsub = totsub + reptab-amount.
      CLEAR ranln1.
      CLEAR ranln2.
    ENDAT.
    AT END OF anln2.                                        " sub asset
      PERFORM write_sub_total.
      totmain = totmain + totsub.
      CLEAR totsub.
    ENDAT.
    AT END OF anln1.                                     " main asset
      PERFORM write_main_total.
      totclas = totclas + totmain.
      CLEAR totmain.
    ENDAT.
    AT END OF class.                                     " asset class
      PERFORM write_class_total.
      totelem = totelem + totclas.
      CLEAR totclas.
    ENDAT.
    AT END OF kstar.                                     " cost element
      PERFORM write_element_total.
      totrept = totrept + totelem.
      CLEAR totelem.
    ENDAT.
  ENDLOOP.
      PERFORM write_report_total.

ENDFORM.                    " WRITE_DETAIL_REPORT

*&----------------------------------------------------------------------
*&      FORM WRITE_HEADER
*&----------------------------------------------------------------------

FORM write_header.
  FORMAT INTENSIFIED OFF.
  ULINE.
  WRITE:   /01 sy-vline
         , 003 sy-datum
         , 057 t001-butxt
         , 120 text-002,  sy-pagno
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         ,     sy-uzeit UNDER sy-datum
         , 046 text-003
         ,     sy-repid UNDER text-002
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         , 003 text-014
         ,     reptab-kstar
         ,     text-015 UNDER text-002
         ,     sy-mandt
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         , 003 text-004
         ,     reptab-class
         , 132 sy-vline.
  ULINE.
  WRITE:   /03 text-005
         , 018 text-016
         , 025 text-006
         , 034 text-007
         , 051 text-008
         , 094 text-009
         , 101 text-010
         , 111 text-011.
  PERFORM showvline.
  FORMAT INTENSIFIED ON.
  ULINE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER2
*&---------------------------------------------------------------------*
FORM write_header2.
  FORMAT INTENSIFIED OFF.
  ULINE.
  WRITE:   /01 sy-vline
         , 003 sy-datum
         , 057 t001-butxt
         , 120 text-002,  sy-pagno
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         ,     sy-uzeit UNDER sy-datum
         , 046 text-020
         ,     sy-repid UNDER text-002
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         ,     text-015 UNDER text-002
         ,     sy-mandt
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         , 132 sy-vline.
  ULINE.
  WRITE:   /03 text-004
         , 111 text-011.
  PERFORM showvline2.
  ULINE.
  FORMAT INTENSIFIED ON.

ENDFORM.                    " WRITE_HEADER2

*---------------------------------------------------------------------*
*       FORM WRITE_DETAIL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_detail.
  WRITE: /     ranln1         UNDER text-005
         ,     ranln2         UNDER text-016
         ,     reptab-prozs   UNDER text-006
         ,     reptab-posid   UNDER text-007
         ,     rpost1         UNDER text-008
         ,     rtxt04         UNDER text-009
         ,     rbzdat         UNDER text-010
         ,     reptab-amount  UNDER text-011.
  PERFORM showvline.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_SUB_TOTAL                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_sub_total.
  WRITE:   /01 sy-vline
         , 016 sy-vline.
  ULINE AT 23.
  WRITE:   /01 sy-vline
         , 016 sy-vline
         ,     text-017       UNDER text-016
         ,     ranln2
         ,     totsub         UNDER text-011
         , 112 sy-vline
         , 132 sy-vline.
  WRITE:   /01 sy-vline.
  ULINE AT 16.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_MAIN_TOTAL                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_main_total.
  WRITE:   /01 sy-vline
         ,     text-018       UNDER text-005
         ,     reptab-anln1
         ,     totmain        UNDER text-011
         , 112 sy-vline
         , 132 sy-vline.
  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_CLASS_TOTAL                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_class_total.
  WRITE:   /01 sy-vline
         ,     text-012       UNDER text-005
         ,     reptab-class
         ,     totclas        UNDER text-011
         , 112 sy-vline
         , 132 sy-vline.
  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_ELEMENT_TOTAL                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_element_total.
  WRITE:   /01 sy-vline
         ,     text-019       UNDER text-005
         ,     reptab-kstar
         ,     totelem        UNDER text-011
         , 112 sy-vline
         , 132 sy-vline.
  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_REPORT_TOTAL                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_report_total.
  ULINE.
  WRITE:   /01 sy-vline
         ,     text-013       UNDER text-005
         ,     totrept        UNDER text-011
         , 112 sy-vline
         , 132 sy-vline.
  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ShowVline                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM showvline.
  WRITE:   001 sy-vline
         , 016 sy-vline
         , 023 sy-vline
         , 032 sy-vline
         , 049 sy-vline
         , 092 sy-vline
         , 099 sy-vline
         , 112 sy-vline
         , 132 sy-vline.
ENDFORM.

************************************************************************
*  This is the end, my freind
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  SHOWVLINE2
*&---------------------------------------------------------------------*

FORM showvline2.
  WRITE:   001 sy-vline
         , 112 sy-vline
         , 132 sy-vline.

ENDFORM.                    " SHOWVLINE2
