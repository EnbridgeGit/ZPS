REPORT zppmr029 NO STANDARD PAGE HEADING LINE-COUNT 58
                                         LINE-SIZE  80
                                         MESSAGE-ID zs.
*
************************************************************************
*  AUTHOR:      MOHAMMAD KHAN.
*  DATE:        FEBRUARY 2002.
*  Description:
*     - The purpose of this program is to produce Allowance for Funds
*        Used During Construction (AFUDC) Report By Project.
*
*
*Changes:
*
*Issue: Date:   By:            Description:
*
* SDP58326      GYMANA         Adding secondary cost element range
* 2014/06/20                   710000 - 719999 to IDC calculation
*
* SDP75178      GYMANA         Remove version check from primary and
* 2014/10/03                   secondary cost actual amount logic.
*                              Separated Planned & Actual logic for
*                              primary cost.
*
* SDP64824      SAHMAD         Data for selection screen version only.
* SDP52669 29/7/2013 M. Khan   Book the IDCs to 324102 instead of 410101.
*                              Calculate IDC for Primary Cost element range
*                              ( 410000 - 499999 and 830000 - 830009 ).
*
* 4.7  15/5/06  Mohammad Khan  4.7 Upgrade - change screen fields for
*                              BDC session.
*
* 4.6c 24/6/04  Mohammad Khan  4.6c Upgrade - change screen number for
*                              BDC session.
*
* 1003  7/5/03  Mohammad Khan  Exclude projects without interest profile
*                              & make changes to calculate and post IDC
*                              for any year i.e. before or after the
*                              current year.
*
* 955  16/5/02  Mohammad Khan  Select 480199 cost element only and
*      2/10/02                 Use Call Transaction CJ40 to post the
*                              calculated IDC and do the Total up
*                              after posting.
*                        NOTE: This program will show an error and will
*not proceed further if a WBS element is found in Closed status. This
*logic has been added at the request of the user.
*
*************************************************************************
TABLES: proj,             "Project Table
        prps,             "Work Breakdown Structure-Elements Master Data
        cosp,             "CO Object: Primary cost
        coss,             "CO Object: Secondary cost
        tbp1c.            "Budget/Plan Profile            - Issue 1003

FIELD-SYMBOLS: <fa>, <fb>, <wb>.

DATA: ls_t001   TYPE t001,
      ls_month  TYPE t247,
      lt_month  LIKE TABLE OF ls_month.

DATA:
  BEGIN OF ptab OCCURS 0,
        poski LIKE prps-poski,
        erdat LIKE proj-erdat,
        post1 LIKE proj-post1,
        post2 LIKE prps-post1,
        objnr LIKE prps-objnr,
        plakz LIKE prps-plakz,
        belkz LIKE prps-belkz,
        zschm LIKE prps-zschm,         "Issue 1003
  END OF ptab.

DATA:
  BEGIN OF ctab OCCURS 0,
        wkg001        LIKE cosp-wkg001,
        wkg002        LIKE cosp-wkg001,
        wkg003        LIKE cosp-wkg001,
        wkg004        LIKE cosp-wkg001,
        wkg005        LIKE cosp-wkg001,
        wkg006        LIKE cosp-wkg001,
        wkg007        LIKE cosp-wkg001,
        wkg008        LIKE cosp-wkg001,
        wkg009        LIKE cosp-wkg001,
        wkg010        LIKE cosp-wkg001,
        wkg011        LIKE cosp-wkg001,
        wkg012        LIKE cosp-wkg001,
        poski         LIKE prps-poski,          "Project + WBS
        wbsseq(2)     TYPE n,                   "Issue Log 955
        carryover     LIKE cosp-wkg001,         "Carry over $
        post1         LIKE proj-post1,
        post2         LIKE prps-post1,
  END OF ctab.

*   Issue Log 955 changes
DATA:
  BEGIN OF bdcitab OCCURS 0,
     idc01  TYPE p,
     idc02  TYPE p,
     idc03  TYPE p,
     idc04  TYPE p,
     idc05  TYPE p,
     idc06  TYPE p,
     idc07  TYPE p,
     idc08  TYPE p,
     idc09  TYPE p,
     idc10  TYPE p,
     idc11  TYPE p,
     idc12  TYPE p,
     poski  LIKE  prps-poski,
     wbsseq(2) TYPE n,
  END OF bdcitab.

DATA:
  BEGIN OF dtab OCCURS 0,
     poski  LIKE  prps-poski,
     year   LIKE  cosp-gjahr,
     plact(6),
     dollar(10) TYPE p,
  END OF dtab.

DATA: BEGIN OF bdcdata OCCURS 0.      "BDC Table
        INCLUDE STRUCTURE bdcdata.    "Structure used for BDC Table
DATA: END OF bdcdata.

DATA:   syear         LIKE cosp-gjahr,          "Year Proj.Created
        curr_year     LIKE cosp-gjahr,          "Current Year
        amt           LIKE cosp-wkg001,
        amt2          LIKE cosp-wkg001,
        amt_plan      LIKE cosp-wkg001,
        amt_work      LIKE cosp-wkg001,
        amt_carry     LIKE cosp-wkg001,
        wkgall        LIKE cosp-wkg001,
        wbscarry      LIKE cosp-wkg001,
        printidc(10)  TYPE p,
        printcash(10) TYPE p,
        totalidc(10)  TYPE p,
        totalcash(10) TYPE p,
        w_index       LIKE sy-index,
        w_poski(14)    TYPE c,
*$        W_POSKI(9)    TYPE C,
*        LAST_POSKI(9) TYPE C,
        mrate(4)      TYPE p DECIMALS 5,        "Monthly Rate
        hrate(4)      TYPE p DECIMALS 5,        "Half Month Rate
        aa(3)         TYPE n,
        recno(1)      TYPE n,
        infield(6)    TYPE c,
        field_name    LIKE bdcdata-fnam,
        zsubrc        TYPE i,                  "return code
        postcurr(10)  TYPE n,
        wcount(2)     TYPE n,
        wposki(9)     TYPE c,
        status(40)    TYPE c,               "All valid status of object
        dstat_flag    TYPE c,
        w_start       TYPE tbp1c-y_start,   "Issue 1003
        scrn_year     LIKE cosp-gjahr,      "Issue 1003
        minus_year    LIKE cosp-gjahr,      "Issue 1003
        plus_year     LIKE cosp-gjahr,      "Issue 1003
        w_icon(4)     TYPE c.               "Issue 1003


RANGES: in_kstar FOR cosp-kstar,
        ex_kstar FOR cosp-kstar.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-000.

PARAMETERS:     p_ccode   LIKE prps-pbukr   DEFAULT 'UGL'. "CompanyCode
SELECT-OPTIONS: s_pspid   FOR  proj-pspid   OBLIGATORY.    "Projects
PARAMETERS:     p_gjahr   LIKE cosp-gjahr   OBLIGATORY,    "Plan Year
                p_versn   LIKE cosp-versn   DEFAULT '000', "Plan Versn
                p_month   LIKE t247-mnr     OBLIGATORY,    "Plan Month
                p_rate(3) TYPE p DECIMALS 2 OBLIGATORY.    "Plan Rate

SELECTION-SCREEN END OF BLOCK box.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-017.
PARAMETERS: bdcflag  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box2.


*------------------------ INITIALIZATION.   ------------------------

INITIALIZATION.
  in_kstar-sign   = 'I'.               " Include Cost element for Plan
  in_kstar-option = 'BT'.
  in_kstar-low    = '0000410000'.
  in_kstar-high   = '0000499999'.
  APPEND in_kstar.
  CLEAR  in_kstar.

* SDP58326 - EAM - GYMANA
  IN_KSTAR-SIGN   = 'I'.                " Include Cost element for Plan
  IN_KSTAR-OPTION = 'BT'.
  IN_KSTAR-LOW    = '0000710000'.
  IN_KSTAR-HIGH   = '0000719999'.
  APPEND IN_KSTAR.
  CLEAR  IN_KSTAR.

  ex_kstar-sign   = 'I'.               " Exclude Cost element for Actual
  ex_kstar-option = 'BT'.
  ex_kstar-low    = '0000491001'.
  ex_kstar-high   = '0000491003'.
  APPEND ex_kstar.
  CLEAR  ex_kstar.

  MOVE sy-datum+0(4) TO curr_year.

*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.

*Get Company Name
  SELECT SINGLE *
    FROM t001
    INTO CORRESPONDING FIELDS OF ls_t001
    WHERE bukrs = p_ccode.

*Get Month Names
  SELECT * FROM t247
    INTO CORRESPONDING FIELDS OF TABLE lt_month
    WHERE spras = 'EN'.

  SELECT SINGLE y_start                               "Issue 1003
    INTO w_start                                      "Issue 1003
    FROM tbp1c                                        "Issue 1003
   WHERE profil = 'UGLCGO' AND                        "Issue 1003
         applik = 'P'      AND                        "Issue 1003
         wrttp  = '01'.                               "Issue 1003
  scrn_year = sy-datum+0(4).                          "Issue 1003
  scrn_year = scrn_year + w_start.                    "Issue 1003
  IF p_gjahr < scrn_year.                             "Issue 1003
    minus_year = scrn_year - p_gjahr.                "Issue 1003
    MOVE 'YA-' TO w_icon.                            "Issue 1003
  ELSEIF p_gjahr > scrn_year.                         "Issue 1003
    plus_year = p_gjahr - scrn_year.                 "Issue 1003
    MOVE 'YA+' TO w_icon.                            "Issue 1003
  ENDIF.                                              "Issue 1003

  SELECT prps~poski proj~erdat proj~post1 prps~post1
         prps~objnr prps~plakz prps~belkz prps~zschm  "Issue 1003
         INTO TABLE ptab
         FROM ( proj
                  INNER JOIN prps
                  ON proj~pspnr   = prps~psphi )
         WHERE  proj~pspid IN s_pspid
           AND proj~vbukr = p_ccode
           AND prps~loevm <> 'X'.                     "Not flagged deleted

  IF ptab[] IS INITIAL.
    WRITE: /1 'NO DATA SELECTED'.
    STOP.
  ENDIF.

  CLEAR: wcount, wposki.                           "Issue Log 955
  LOOP AT ptab.
    IF ptab-poski+0(9) <> wposki.            "Issue Log 955
      MOVE ptab-poski+0(9) TO wposki.       "Issue Log 955
      wcount = 1.                           "Issue Log 955
    ELSE.
      wcount = wcount + 1.
    ENDIF.

    IF ptab-zschm <> space AND                  "Issue 1003
     ( ptab-plakz = 'X' OR ptab-belkz = 'X' ).
      MOVE ptab-erdat+0(4) TO syear.
      PERFORM get_carry_over_amount.
      IF amt_work = 0 AND amt_carry = 0.
      ELSE.
        PERFORM check_closed_status.
        IF dstat_flag <> 'Y'.
          MOVE ptab-poski TO ctab-poski.
          MOVE ptab-post1 TO ctab-post1.
          MOVE ptab-post2 TO ctab-post2.
          MOVE amt_carry  TO ctab-carryover.
          MOVE wcount     TO ctab-wbsseq.      "Issue Log 955
          APPEND ctab.
        ENDIF.
      ENDIF.
      CLEAR ctab.
    ENDIF.
  ENDLOOP.


  PERFORM calculate_idc.

  IF bdcflag = 'X'.
    IF bdcitab[] IS INITIAL.
      MESSAGE i368 WITH 'No IDC Updates to Perform.'.
    ELSE.
      PERFORM update_database.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
*  FORM GET_CARRY_OVER_AMOUNT
*-----------------------------------------------------------------------
FORM get_carry_over_amount.

  CLEAR: amt_plan, amt_carry, amt, amt_work, amt2.

* Primary Cost - Plan Amounts only                            SDP75178
  SELECT * FROM cosp                                         "SDP75178
     WHERE objnr = ptab-objnr              "Matching objects  SDP75178
       AND gjahr BETWEEN syear AND p_gjahr "Fiscal Year       SDP75178
       AND versn = p_versn                 "Version           SDP75178
*       OR    versn = '000' )                                 SDP75178
       AND wrttp = '01'                    "Plan amounts      SDP75178
       AND beknz IN ('S','H','L').         "Debit/Credit      SDP75178
                                                             "SDP75178
    IF sy-subrc = '0'.                                       "SDP75178
      ADD  cosp-wkg001 FROM 1 TO 12 GIVING amt.              "SDP75178
*           IF COSP-KSTAR = '0000480199'.                    "SDP52669
      IF cosp-kstar <> '0000324102'.                         "SDP52669
        IF cosp-kstar IN in_kstar.                           "SDP52669
          IF cosp-gjahr = p_gjahr.
            CLEAR: w_index.
            DO 12 TIMES.
              w_index = w_index + 1.
              ASSIGN COMPONENT w_index OF STRUCTURE ctab TO <fb>.
              ADD cosp-wkg001 FROM w_index TO w_index GIVING amt2.
              ADD amt2 TO <fb>.
              ADD amt2 TO amt_work.
              CLEAR amt2.
            ENDDO.
          ELSE.
            IF cosp-versn =  p_versn   AND
               cosp-gjahr => curr_year AND
               cosp-kstar IN in_kstar.
              amt_carry  = amt_carry + amt.
              PERFORM carryover_detail USING 'PLAN  ' cosp-gjahr.
              CLEAR amt.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDSELECT.

* Primary Cost - Actual Amounts only                         "SDP75178
  SELECT * FROM cosp                                         "SDP75178
     WHERE objnr = ptab-objnr              "Matching objects  SDP75178
       AND gjahr BETWEEN syear AND p_gjahr "Fiscal Year       SDP75178
*       AND versn = p_versn                "Version           SDP75178
       AND wrttp = '04'                    "Actuals           SDP75178
       AND beknz IN ('S','H','L').         "Debit/Credit      SDP75178
                                                             "SDP75178
    IF sy-subrc = '0'.                                       "SDP75178
      ADD  cosp-wkg001 FROM 1 TO 12 GIVING amt.              "SDP75178
      IF cosp-versn = '000'      AND                         "SDP75178
         cosp-gjahr < curr_year  AND                         "SDP75178
         NOT cosp-kstar IN ex_kstar.                         "SDP75178
          amt_carry  = amt_carry + amt.                      "SDP75178
          PERFORM carryover_detail USING 'ACTUAL' cosp-gjahr."SDP75178
          CLEAR amt.                                         "SDP75178
      ENDIF.                                                 "SDP75178
    ENDIF.                                                   "SDP75178

  ENDSELECT.

* Secondary Cost - Planned amount only
  SELECT * FROM coss
    WHERE objnr = ptab-objnr                      "Matching Objects
      AND gjahr BETWEEN syear AND p_gjahr    "Fiscal Year selected
      AND versn = p_versn  "versn = '000'
      AND wrttp = '01'              "Plan and Actuals
      AND NOT kstar IN ex_kstar.
    IF sy-subrc = 0.
      ADD  coss-wkg001 FROM 1 TO 12 GIVING amt.
      IF coss-gjahr = p_gjahr.
        CLEAR: w_index.
        DO 12 TIMES.
          w_index = w_index + 1.
          ASSIGN COMPONENT w_index OF STRUCTURE ctab TO <fb>.
          ADD coss-wkg001 FROM w_index TO w_index GIVING amt2.
          ADD amt2 TO <fb>.
          ADD amt2 TO amt_work.
          CLEAR amt2.
        ENDDO.
      ELSE.
        IF coss-versn =  p_versn   AND
           coss-gjahr => curr_year.
          amt_carry  = amt_carry + amt.
          PERFORM carryover_detail USING 'PLAN  ' coss-gjahr.
          CLEAR amt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDSELECT.

* Secondary Cost - Actual amount only
  SELECT * FROM coss
    WHERE objnr = ptab-objnr       "Matching Objects
      AND gjahr < curr_year        "Fiscal Year
      AND gjahr => syear           "Fiscal Year
*      AND versn = p_versn  "versn = '000'                    SDP75178
      AND wrttp = '04'             "Record with Actuals
      AND NOT kstar IN ex_kstar.

    IF sy-subrc = 0.
      ADD coss-wkg001 FROM 1 TO 12 GIVING amt.
      amt_carry = amt_carry + amt.
      PERFORM carryover_detail  USING 'ACTUAL' coss-gjahr.
      CLEAR amt.
    ENDIF.

  ENDSELECT.

ENDFORM.                    "GET_CARRY_OVER_AMOUNT

*-----------------------------------------------------------------------
*  FORM CALCULATE_IDC
*-----------------------------------------------------------------------

FORM calculate_idc.

  CLEAR: mrate, hrate.
  mrate = p_rate / ( 100 * 12 ).
  hrate = p_rate / ( 100 * 12 * 2 ).

  SORT dtab BY poski year.

  LOOP AT ctab.
    CLEAR: wbscarry, printidc, printcash, wkgall.
    wbscarry = ctab-carryover * mrate.
    w_index = 0.
    DO 12 TIMES.
      w_index = w_index + 1.
      ASSIGN COMPONENT w_index OF STRUCTURE ctab TO <fb>.

      IF sy-index > p_month.
        printidc  = 0.
        MOVE <fb> TO printcash.
      ELSE.
        IF <fb> = 0.
          printidc  = ( ctab-carryover * mrate ) + wkgall * mrate.
*        PRINTIDC  = ( CTAB-CARRYOVER * 1 / 100 ) + WKGALL * MRATE.
          printcash = 0.
        ELSE.
          printidc = wbscarry + <fb> * hrate + wkgall * mrate.
          MOVE <fb> TO printcash.
          wkgall = wkgall + <fb>.
        ENDIF.
      ENDIF.
      IF bdcflag = 'X'.                                 "Issue Log 955
        ASSIGN COMPONENT w_index OF STRUCTURE bdcitab TO <wb>.
        MOVE printidc    TO  <wb>.                     "Issue Log 955
        MOVE ctab-poski  TO  bdcitab-poski.            "Issue Log 955
        MOVE ctab-wbsseq TO  bdcitab-wbsseq.           "Issue Log 955
      ENDIF.                                            "Issue Log 955
      PERFORM write_detail_line.
    ENDDO.
    APPEND bdcitab.                                    "Issue Log 955
    CLEAR bdcitab.                                     "Issue Log 955

    PERFORM write_total_line.

  ENDLOOP.

ENDFORM.                    "CALCULATE_IDC

*-----------------------------------------------------------------------
*  FORM CARRYOVER_DETAIL.
*-----------------------------------------------------------------------
FORM carryover_detail USING value(parm1) coyear.

  dtab-poski = ptab-poski.
  dtab-year  = coyear.
  dtab-plact = parm1.
  dtab-dollar = amt.
  COLLECT dtab.
  CLEAR dtab.

ENDFORM.                    "CARRYOVER_DETAIL

*-----------------------------------------------------------------------
*  FORM WRITE_DETAIL_LINE.
*-----------------------------------------------------------------------
FORM write_detail_line.

  IF w_index = 1.
    NEW-PAGE.
  ENDIF.
  SKIP.
  READ TABLE lt_month INTO ls_month WITH KEY mnr = w_index BINARY SEARCH.
  IF sy-subrc EQ 0.
    WRITE: ls_month-ktx UNDER text-008.
  ENDIF.

  WRITE: printcash UNDER text-009, printidc UNDER text-010.
  totalidc = totalidc + printidc.
  totalcash = totalcash + printcash.

ENDFORM.                    "WRITE_DETAIL_LINE

*-----------------------------------------------------------------------
*  FORM WRITE_TOTAL_LINE.
*-----------------------------------------------------------------------
FORM write_total_line.

  SKIP.
  WRITE:    text-012 UNDER text-009,
            text-012 UNDER text-010.
  SKIP.
  WRITE: /5 text-022, totalcash UNDER text-009,
            totalidc UNDER text-010.
  CLEAR: totalcash, totalidc.

ENDFORM.                    "WRITE_TOTAL_LINE

*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-rpt, sy-repid,                          "Report Id
         30 ls_t001-butxt,                                  "Company Name
         65 text-dte, sy-datum.                          "Date
  WRITE: / text-clt UNDER text-rpt, sy-mandt, sy-sysid,  "Client
         20 text-001,                                    "Report Title
         65 text-amp, sy-uzeit.                          "Date
  ULINE.
  SKIP.

  WRITE: /2 text-016, 28 ctab-post1.
  WRITE: /2 text-002, 28 ctab-poski.
  WRITE: /2 text-003, 28 ctab-post2.
  WRITE: /2 text-004, 28 p_gjahr.
  WRITE: /2 text-005, 28 p_versn.
  WRITE: /2 text-006, 28 p_month.
  WRITE: /2 text-007, 27 p_rate.
  ULINE.
*    SKIP.

*    DATA RECNO(1) TYPE N VALUE 0.
  WRITE: /27 text-030.
  WRITE: /27 text-029.
  SKIP.
  WRITE: /2 text-031, 9 text-032, 28 text-033,
         45 text-034, 52 text-035, 71 text-036.
  WRITE: /2 text-014, 9 text-014, 29 text-014,
         45 text-014, 52 text-014, 72 text-014.

  CLEAR: recno.
  LOOP AT dtab.
    IF dtab-poski = ctab-poski.
      IF recno = 0.
        MOVE 1 TO recno.
        WRITE: / dtab-year   UNDER text-031,
                 dtab-plact  UNDER text-032,
                 15 dtab-dollar.
      ELSE.
        MOVE 0 TO recno.
        WRITE:   dtab-year   UNDER text-034,
                 dtab-plact  UNDER text-035,
                 58 dtab-dollar.
      ENDIF.
    ENDIF.
  ENDLOOP.
  ULINE.
  SKIP.

  WRITE: /05 text-008, 35 text-009, 60 text-010.
  WRITE: /   text-014 UNDER text-008, text-015 UNDER text-009,
             text-015 UNDER text-010.
  SKIP.
  WRITE: /17 text-011, ctab-carryover UNDER text-009.
  WRITE: /   text-012 UNDER text-009.

*&---------------------------------------------------------------------*
*&      Form  CHECK_CLOSED_STATUS
*&---------------------------------------------------------------------*
FORM check_closed_status.
  DATA: w_mesg(65) TYPE c.
  CLEAR: status, dstat_flag.

  CALL FUNCTION 'AIP9_STATUS_READ'                  "Retrieve Status Info
         EXPORTING
              i_objnr = ptab-objnr
              i_spras = sy-langu
         IMPORTING
              e_sysst = status
         EXCEPTIONS
              OTHERS = 1.

  IF status CS 'CLSD'.
    CONCATENATE 'CLSD Status Found-Change Status For.'
                 ptab-poski INTO w_mesg.
    MESSAGE a019 WITH w_mesg.
  ELSEIF status CS 'TECO'.
    CONCATENATE 'TECO Status Found-Change Status For.'
                 ptab-poski INTO w_mesg.
    MESSAGE a019 WITH w_mesg.
  ELSEIF status CS 'DLIN'.
    dstat_flag = 'Y'.
  ENDIF.

ENDFORM.                    "CHECK_CLOSED_STATUS
*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC_SESSION
*&---------------------------------------------------------------------*
FORM update_database.
  LOOP AT bdcitab.
    REFRESH bdcdata.
*$     MOVE BDCITAB-POSKI+0(9) TO W_POSKI.
    MOVE bdcitab-poski TO w_poski.
    PERFORM screen_header USING 'SAPMKBUD' '0200' 'X'.
    PERFORM screen_field  USING 'PRPS-POSID'  w_poski.
*$      PERFORM SCREEN_FIELD  USING 'PROJ-PSPID'  W_POSKI.
    PERFORM screen_field  USING 'BPDY-VERSN'  p_versn.
    PERFORM screen_field  USING 'BDC_OKCODE' '/5'.
*     PERFORM SCREEN_HEADER USING 'SAPLKBPP' '310' 'X'.  "4.6c upgrade
    PERFORM screen_header USING 'SAPLKBPP' '320' 'X'.  "4.6c upgrade

    IF minus_year <> '0000'.                               "Issue 1003
      DO minus_year TIMES.                                "Issue 1003
        PERFORM screen_field  USING 'BDC_OKCODE' 'YA-'.  "Issue 1003
      ENDDO.                                              "Issue 1003
    ENDIF.                                                 "Issue 1003

    IF plus_year <> '0000'.                                "Issue 1003
      DO plus_year TIMES.                                 "Issue 1003
        PERFORM screen_field  USING 'BDC_OKCODE' 'YA+'.  "Issue 1003
      ENDDO.                                              "Issue 1003
    ENDIF.                                                 "Issue 1003

    CLEAR field_name.
*$     CONCATENATE 'RCJ_MARKL-MARK(' BDCITAB-WBSSEQ ')' INTO FIELD_NAME.
*     PERFORM SCREEN_HEADER USING 'SAPLKBPP' '310' 'X'.  "4.6c upgrade
    PERFORM screen_header USING 'SAPLKBPP' '320' 'X'.  "4.6c upgrade
*$      PERFORM SCREEN_FIELD  USING  FIELD_NAME 'X'.
    PERFORM screen_field  USING 'BDC_OKCODE' 'KAPR'.
    PERFORM screen_header USING 'SAPLKPP2' '112' 'X'.

*     PERFORM SCREEN_FIELD  USING 'BDC_CURSOR' 'BDC03(03)'. "4.7 Upgrade
*     PERFORM SCREEN_FIELD  USING 'BDC_CURSOR' 'Z-BDC01(03)'. "SDP52669
    PERFORM screen_field  USING 'BDC_CURSOR' 'Z-BDC01(14)'. "SDP52669

    PERFORM screen_field  USING 'BDC_OKCODE' '/6'.

    PERFORM screen_header USING 'SAPLKPP2' '110' 'X'.
    CLEAR: wcount.
    DO 12 TIMES.
      CLEAR field_name.
      wcount = wcount + 1.
      ASSIGN COMPONENT wcount OF STRUCTURE bdcitab TO <wb>.
      MOVE <wb> TO postcurr.
*         CONCATENATE 'BDC03(' WCOUNT ')' INTO FIELD_NAME.  "4.7 Upgrade
      CONCATENATE 'Z-BDC03(' wcount ')' INTO field_name. "4.7 Upgrade
      PERFORM screen_field  USING field_name  postcurr.
    ENDDO.
    PERFORM screen_field  USING 'BDC_OKCODE' 'CBUC'.        "Save

*     PERFORM SCREEN_HEADER USING 'SAPLKBPP' '310' 'X'.  "4.6c upgrade
    PERFORM screen_header USING 'SAPLKBPP' '320' 'X'.  "4.6c upgrade

    PERFORM screen_field  USING 'BDC_OKCODE' 'SYNC'.      "Total Up
    PERFORM screen_header USING 'SAPLKBPP' '0705' 'X'.

    PERFORM screen_field  USING 'BDC_OKCODE' '/5'.
*     PERFORM SCREEN_HEADER USING 'SAPLKBPP' '310' 'X'.  "4.6c upgrade
    PERFORM screen_header USING 'SAPLKBPP' '320' 'X'.  "4.6c upgrade
    PERFORM screen_field  USING 'BDC_OKCODE' 'SAVE'.      "Save

    PERFORM execute.
    IF zsubrc <> 0.
      MESSAGE e368 WITH 'Error in updating IDC.'.
*      ELSE.
*         MESSAGE I368 WITH 'IDC Update - Successful.'.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " CREATE_BDC_SESSION


*----------------------------------------------------------------------
* form screen_header ... create BDC header entry using parameters
*                        (1) program   ...  screen program
*                        (2) screen    ...  screen number
*                        (3) indicator ...  indicator
*----------------------------------------------------------------------
FORM screen_header USING program screen indicator.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = screen.
  bdcdata-dynbegin = indicator.
  APPEND bdcdata.
ENDFORM.                    "SCREEN_HEADER

*----------------------------------------------------------------------
* form screen_field ... create BDC entry using parameters
*                       (1) fnam ... field name to fill
*                       (2) fval ... value to fill in
*----------------------------------------------------------------------
FORM screen_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "SCREEN_FIELD

*----------------------------------------------------------------------
* form execute ... execute transaction OAV8 using
*                  (1) bdcdata as data source
*                  (2) in no display mode
*                  (3) in synchronous update mode
*----------------------------------------------------------------------
FORM execute.
  CALL TRANSACTION 'CJ40' USING bdcdata         "Data source
                        MODE 'E'         "E - Display Screen if Error
*                         MODE 'A'         "A - Display Screen
*                         MODE 'N'         "N - Don't display screen
                        UPDATE 'S'.           "S - synchronous update
  MOVE sy-subrc TO zsubrc.
ENDFORM.                    "EXECUTE
*----------------------------------------------------------------------
