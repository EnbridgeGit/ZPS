report zppmr007 no standard page heading line-count 58 line-size 170
                message-id zp.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        February 1998
*  Description:
*     - The purpose of this program is to produce Summary of Capital
*       Expenditures Report by Project Type, Priority Code and
*       Div/Dept.
************************************************************************
* 2014/09/05 gymana SDP73514 - Change selection screen to allow multiple
*                              company codes
* 10/04/07 Mohammad TR 813 - Changes for secondry cost element
* 10/01/19 lritchie TR 582 - add cost element on selection screen
* 09/09/29 mokhan #663 Apply the Authorization check
* 99/03/17 mdemeest #--- Eliminate zero lines
* 99/01/26 md7140 #624 Change Commitment Calculation
* 98/10/21 omning #--- Nancy Gilligan
*                      - add version number to headers for planned value
*                      - changed budget & actuals to select only '000'
* 98/03/23 md7140 #--- Change in template definition
* 98/03/10 md7140 Added additional sort field for TOTAL NEW BUSINESS
*                 and TOTAL REPLACEMENTS (distgrp)
* 98/02/19 md7140 Copied ZPPMR001 to use as template
************************************************************************

tables: bpja, cosp, coss, csku, jest, proj, prps, t001, t247, tcj1t,
        tcn7t,
        cobra.

data:
   begin of wa  occurs 0,
      prart         like prps-prart,          "Project Type
      pspri         like prps-pspri,          "Priority Type
      posid         like prps-posid,          "Project Number
      objnr         like prps-objnr,          "Object Number
      psphi         like prps-psphi,
      plakz         like prps-plakz,          "Planning element
      stufe         like prps-stufe,          "Level Number
      projactual(8)   type p decimals 2,           "Actual Amount
      projlocked(8)   type p decimals 2,           "Locked budget
      priorcommit(8)  type p decimals 2,
      prtypcommit(8)  type p decimals 2,
      deptcommit(8)   type p decimals 2,
      comcommit(8)    type p decimals 2,
      distcommit(8)    type p decimals 2,
      remplan(8)      type p decimals 2,           "Remaining Plan
      priorremplan(8) type p decimals 2,
      prtypremplan(8) type p decimals 2,
      deptremplan(8)  type p decimals 2,
      comremplan(8)   type p decimals 2,
      distremplan(8)   type p decimals 2,
      annplan(8)      type p decimals 2,
   end of wa.

data:
    begin of big_table occurs 10000,
       prart         like prps-prart,           "Project Type
       distgrp(1)    type c,                    "WA for subtotalling
       pspri         like prps-pspri,           "Priority Type
       dept(2),                                 "Div/Dept
       project       like prps-posid,           "Project (7 digits)
       psphi         like prps-psphi,           "Internal Project No.
*      posid         like prps-posid,           "WBS Element
*      objnr         like prps-objnr,           "Object Number
       post1         like proj-post1,           "Project Description
       verna          like proj-verna,          "Dept Name
       actual(8)     type p decimals 2,           "Actual Amount
       plan(8)     type p decimals 2,           "Plan Amount
       variance(8) type p decimals 2,           "Variance (actual-plan)
       locked(8)   type p decimals 2,           "Locked budget
       commit(8)   type p decimals 2,           "Total Committed
       annplan(8)  type p decimals 2,           "Annual Plan
*      remplan(8)  type p decimals 2,           "Remaining Plan
       uncommit(8) type p decimals 2,           "Uncommited Budget
       bzdat       type i,                      "Acquisition Value Date
*      BZDAT(4)    TYPE I,        " TR663       "Acquisition Value Date
   end of big_table.


data:   value         like cosp-wkg001,
        annvalue      like cosp-wkg001,
        begvalue      like cosp-wkg001,
        endvalue      like cosp-wkg001,
        actual          like cosp-wkg001,
        plan          like cosp-wkg001,
        commit        like cosp-wkg001,
        annplan       like cosp-wkg001,
        remplan       like cosp-wkg001,
        variance type p decimals 2,              "Variance (actual-plan)
        post1         like proj-post1,
        p_month(2),
        p_begnme(9),
        p_endnme(9),
        a_begmth(2)     type c,
        a_endmth(2)     type c,
        locked        like bpja-wtjhv,
        uncommit      like bpja-wtjhv,
        verna         like proj-verna,
        bzdat(1)      type c.

ranges: s_bukrs for t001t-bukrs,                            "TR663
        s_kostl for proj-kostl,                             "TR663
        s_ccgrp for rgsb4-setnr,                            "TR663
        s_aufnr for bseg-aufnr,                             "TR663
        s_pspnr for proj-pspnr,                             "TR663
        s_nplnr for bseg-nplnr,                             "TR663
        s_werks for prps-werks,                             "TR663
        s_ekorg for ekko-ekorg.                             "TR663
*----------------------  Selection Screen  -----------------------------
selection-screen begin of block box with frame title text-000.
select-options  s_ccode for t001-bukrs default 'UGL'.        "SDP73514
*parameters: p_ccode like t001t-bukrs  default 'UGL' obligatory, "TR663
*PARAMETERS: P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL',           "TR663
PARAMETERS: p_vers  like cosp-versn default '0',  "Plan version
            p_fyear like cosp-gjahr default sy-datum+0(4),
            p_begmth(2)         default sy-datum+4(2),
            p_endmth(2) memory id xyz.
select-options:  s_pspid for proj-pspid,                 "Project
                 s_vernr for proj-vernr,                 "Division
                 sprart for prps-prart,                  "Project Type
                 spspri for prps-pspri.                  "Priority Type
select-options   s_kstar for coss-kstar.        "Cost element 2010/01/19
selection-screen skip 2.
parameter: p_check as checkbox.
selection-screen end of block box.
*-----------------------------------------------------------------------
a_begmth = p_begmth.
if p_endmth = space.
  a_endmth = p_begmth - 0.
else.
  a_endmth = p_endmth - 0.
endif.
*Authorization
s_bukrs-sign   = 'I'.
s_bukrs-option = 'EQ'.
s_bukrs-low    = s_ccode-low.                               "SDP73514
s_bukrs-high   = s_CCODE-high.                              "SDP73514
append s_bukrs.
clear  s_bukrs.

include zfico_auth_check_include.                           "TR663
include znonfico_auth_check_include.                        "TR663

start-of-selection.
  perform fico_authorization_check.                         "TR663
  perform nonfico_authorization_check.                      "TR663

  select * from proj
    where pspid in s_pspid
      and vernr in s_vernr.

    select prart pspri posid objnr psphi plakz stufe
      into wa from prps
      where pbukr in s_ccode       "Company Code               SDP73514
        and pkokr = '10'           "Controlling area
        and psphi = proj-pspnr
        and loevm <> 'X'
        and prart in sprart
        and pspri in spspri.
      if wa-posid+5(2) co '1234567890'.     "Eliminates templates
        perform get_fiscal_data.
      endif.
    endselect.
  endselect.                             "PROJ

  select single * from t001                    "Get Company Name
    where bukrs in s_ccode.                                  "SDP73514

  select single * from t247                   "Get Month Name(Begin)
    where spras = 'E'
      and mnr = p_begmth.
  p_begnme = t247-ltx.
  p_endnme = space.
  if p_endmth <> space.
    select single * from t247                "Get Month Name(End)
      where spras = 'E'
        and mnr = p_endmth.
    p_endnme = t247-ltx.
  endif.

  if sy-subrc eq 0.
    perform remove_zero_rows.
    perform display_table.
  endif.


*-----------------------------------------------------------------------

top-of-page.
  format intensified off.
  write: /1 text-rpt, sy-repid,                          "Report Id
         80 t001-butxt,                                  "Company Name
        140 text-dte, sy-datum, text-amp, sy-uzeit.      "Date/Time
  write: / text-clt under text-rpt, sy-mandt, sy-sysid,  "Client
        70 text-003,                                     "Report Title
           text-pge under text-dte, sy-pagno.            "Page Number
  write: / text-vrs under text-rpt, p_vers,              "Version
        75 text-004, p_fyear.                            "Fiscal Year
  if p_endmth = space.                                   "Time Frame
    write: /80 p_begnme.
  else.
    write: /70 p_begnme, text-024, p_endnme, text-025.
  endif.
  format intensified on.
  perform print_headings.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*------------------------ REMOVE_ZERO_ROWS -----------------------------
*  Delete rows with zeros
*-----------------------------------------------------------------------
form remove_zero_rows.
  loop at big_table.
    delete big_table where plan    = 0
                       and actual  = 0
                       and locked  = 0
                       and annplan = 0.
  endloop.
endform.                    "REMOVE_ZERO_ROWS

*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
form display_table.

  bzdat = 'N'.
  sort big_table by prart distgrp pspri dept project.
  loop at big_table.
    move big_table-post1   to post1.
    move big_table-verna   to verna.
    add big_table-actual   to wa-projactual.
    add big_table-locked   to wa-projlocked.
    add big_table-annplan  to wa-annplan.
    if big_table-bzdat <> 0.                                "#624
      bzdat = 'Y'.                                          "#624
    endif.
*  perform big_table_dump.                "Dump for problem solving

    at new prart.                          "Change in Project Type
      new-page.
      perform heading_project_type.
    endat.

    at new pspri.                           "Change in Priority Type
      perform heading_priority_type.
    endat.

    at new dept.                            "Change in Div/Dept
      if p_check = 'X'.
        perform print_vert.
        write: 6 verna.
      endif.
    endat.

    at end of project.                      "PROJECT TOTALS

      if bzdat = 'Y'.                       "if activation date     #624
      else.
        if wa-projactual = 0.
          wa-projactual = wa-projlocked.
        elseif wa-projlocked = 0.
*        continue
        elseif wa-projlocked > wa-projactual.
          wa-projactual = wa-projlocked.
        endif.
      endif.

      wa-remplan = wa-annplan - wa-projactual.
      sum.
      if p_check = 'X'.
*    if  big_table-plan <> 0 or          "Eliminate lines with all zeros
*        big_table-actual <> 0 or
*        big_table-locked <> 0 or
*        big_table-annplan <> 0.
        perform print_vert.
        write: (9) big_table-project under text-005,        "Proj#
            (26) post1 under text-007,             "Project Desc
        (16) big_table-plan     under text-008,     "YTD Plan
        (16) big_table-actual   under text-010,     "Actuals
        (16) big_table-variance under text-011,     "Variance(act-plan)
        (16) big_table-locked   under text-013,     "Appr Budget Comt
        (16) wa-projactual      under text-015,     "Total Commitment
        (16) big_table-annplan  under text-016,     "Annual Plan
        (16) wa-remplan         under text-017,     "Remaining Plan
             bzdat              under text-041.
*    endif.
      endif.
      bzdat = 'N'.
      add wa-projactual       to wa-deptcommit.
      add wa-remplan          to wa-deptremplan.
      clear: wa-projlocked, wa-projactual, wa-remplan, wa-annplan.
    endat.

    at end of dept.                              "Div/Dept Total
      sum.
      perform print_vert.
      write: 6 text-020, verna,
      (16) big_table-plan      under text-008,   "YTD Plan
      (16) big_table-actual    under text-010,   "Actuals
      (16) big_table-variance  under text-011,   "Variance(actual-plan
      (16) big_table-locked    under text-013,   "Appr Budget Comt
      (16) wa-deptcommit       under text-015,   "Total Commitment
      (16) big_table-annplan   under text-016,   "Annual Plan
      (16) wa-deptremplan      under text-017.   "Remaining Plan
      format color col_background.
      add wa-deptcommit       to wa-priorcommit.
      add wa-deptremplan      to wa-priorremplan.
      clear:  wa-deptcommit, wa-deptremplan.
    endat.

    at end of pspri.
      sum.
      perform print_vert.
      write: 44(126) sy-uline.
      perform print_vert.
      format color col_positive.
      write: 4 text-020, (34) tcn7t-ktext,            "Total Priority
       (16) big_table-plan     under text-008,    "YTD Plan
       (16) big_table-actual    under text-010,   "Actuals
       (16) big_table-variance  under text-011,   "Variance(actual-plan
       (16) big_table-locked    under text-013,   "Appr Budget Comt
       (16) wa-priorcommit       under text-015,   "Total Commitment
       (16) big_table-annplan   under text-016,   "Annual Plan
       (16) wa-priorremplan     under text-017.   "Remaining Plan
      format color col_background.
      perform print_vert.
      write: 44(126) sy-uline.
*   add wa-priorcommit       to wa-prtypcommit.
*   add wa-priorremplan      to wa-prtypremplan.
      add wa-priorcommit       to wa-distcommit.
      add wa-priorremplan      to wa-distremplan.
      clear: wa-priorcommit, wa-priorremplan.
    endat.

    at end of distgrp.
      if big_table-distgrp <> 'Z'.
        sum.
        perform print_vert.
        write: 1(170) sy-uline.
        perform print_vert.
        if big_table-distgrp = '1'.
          write: 2 text-033.           "TOTAL NEW BUSINESS
        elseif big_table-distgrp = '2'. "TOTAL ADDITIONS
          write: 2 text-035.
        elseif big_table-distgrp = '3'. "TOTAL REPLACEMENTS
          write: 2 text-034.
        endif.
        write:
         (16) big_table-plan     under text-008,    "YTD Plan
         (16) big_table-actual    under text-010,   "Actuals
         (16) big_table-variance  under text-011,  "Variance(actual-plan
         (16) big_table-locked    under text-013,   "Appr Budget Comt
         (16) wa-distcommit      under text-015,   "Total Commitment
         (16) big_table-annplan   under text-016,   "Annual Plan
         (16) wa-distremplan     under text-017.   "Remaining Plan
        uline.
      endif.
      add wa-distcommit to wa-prtypcommit.
      add wa-distremplan to wa-prtypremplan.
      clear: wa-distcommit, wa-distremplan.
    endat.

    at end of prart.
      sum.
      perform print_vert.
      write: 1(170) sy-uline.
      perform print_vert.
      format color col_total.
      write:  2 text-020, (35) tcj1t-pratx,       "Total Project Type
       (16) big_table-plan     under text-008,    "YTD Plan
       (16) big_table-actual    under text-010,   "Actuals
       (16) big_table-variance  under text-011,   "Variance(actual-plan
       (16) big_table-locked    under text-013,   "Appr Budget Comt
       (16) wa-prtypcommit      under text-015,   "Total Commitment
       (16) big_table-annplan   under text-016,   "Annual Plan
       (16) wa-prtypremplan     under text-017.   "Remaining Plan
      uline.
      format color col_background.
*   perform print_vert.
      add wa-prtypcommit       to wa-comcommit.
      add wa-prtypremplan      to wa-comremplan.
      clear: wa-prtypcommit, wa-prtypremplan.
    endat.

    at last.
      new-page.
      sum.
      perform print_vert.
      write: 2 text-029,
       (16) big_table-plan      under text-008,   "YTD Plan
       (16) big_table-actual    under text-010,   "Actuals
       (16) big_table-variance  under text-011,   "Variance(actual-plan
       (16) big_table-locked    under text-013,   "Appr Budget Comt
       (16) wa-comcommit        under text-015,   "Total Commitment
       (16) big_table-annplan   under text-016,   "Annual Plan
       (16) wa-comremplan       under text-017.   "Remaining Plan
      uline.
      write: /.
      write: /80 text-028.
    endat.
  endloop.
endform.                    "DISPLAY_TABLE

*----------------------- PRINT_HEADINGS -------------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
form print_headings.
  format color 2 on.
  write: /1(170) sy-uline.
  perform print_vert.
  write:  45 text-008, 79 text-011,
          96 text-013, 113 text-015, 130 text-016, 147 text-017.
  perform print_vert.
  write: 7 text-005, 18 text-007,
         text-009 under text-008, 62 text-010,
         text-012 under text-011, text-014 under text-013,
         text-032 under text-015, text-009 under text-016,
         text-009 under text-017, 164 text-041.
  perform print_vert.
* write in versions numbers:
  write: text-040 under text-008, p_vers,           "ytd plan
         text-040 under text-010, '0',              "actuals
         text-040 under text-016, p_vers,           "annual plan
         text-040 under text-017, p_vers.           "annual plan
  uline.
  perform print_vert.
  format color 2 off.
endform.                    "PRINT_HEADINGS

*--------------------------  PRINT_VERT  -------------------------------
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
form print_vert.
  write:  /1 sy-vline, 44 sy-vline, 61 sy-vline, 78 sy-vline,
          95 sy-vline, 112 sy-vline, 129 sy-vline, 146 sy-vline,
         163 sy-vline, 170 sy-vline.
endform.                    "PRINT_VERT

*&---------------------------------------------------------------------*
*&      Form  HEADING_DEPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form heading_dept.
  perform print_vert.
  format color col_positive.
  write: 2 text-023, verna.
  format color col_background.
endform.                    "HEADING_DEPT

*&---------------------------------------------------------------------*
*&      Form  HEADING_PROJECT_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form heading_project_type.
  select single * from tcj1t
      where langu = 'E' and
            prart = big_table-prart.
  perform print_vert.
  format color col_total.
  write: 2 tcj1t-pratx.
  format color col_background.
endform.                    "HEADING_PROJECT_TYPE

*&---------------------------------------------------------------------*
*&      Form  HEADING_PRIORITY_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form heading_priority_type.
  select single * from tcn7t
    where langu = 'E'
      and nprio = big_table-pspri.
  format color col_positive.
  if sy-subrc <> 0.
    perform print_vert.
    write: 4 text-021, big_table-pspri, text-022.
  else.
    perform print_vert.
    write: 4 tcn7t-ktext.
  endif.
  format color col_background.
endform.                    "HEADING_PRIORITY_TYPE

*&---------------------------------------------------------------------*
*&      Form  BIG_TABLE_DUMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form big_table_dump.
*    write: /2 big_table-posid,
  write: /111 big_table-plan,                "YTD Plan
              129 big_table-actual,          "Actuals
              147 big_table-variance,        "Variance(actual - plan
              165 big_table-locked,          "Appr Budget Comt
              183 big_table-variance,        "Total Commitment
              183 big_table-commit,          "Total Commitment
              201 big_table-annplan,         "Annual Plan
              219 wa-remplan,                "Remaining Plan
              237 big_table-uncommit.        "Uncommitted Budget
endform.                    "BIG_TABLE_DUMP
*&---------------------------------------------------------------------*
*&      Form  GET_FISCAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_fiscal_data.
*write: / wa-prart, wa-pspri, wa-posid, wa-objnr, wa-psphi, wa-plakz.
* COST TOTALS - External Postings
  select * from cosp
           where objnr = wa-objnr          "Matching objects
             and gjahr = p_fyear           "Fiscal Year selected
             and ( versn = p_vers            "Version
                 or versn = '000' )                             "omning
             and wrttp in ('01','04')      "Record with actuals & plans
             and kstar in s_kstar          "Cost element      2010/01/19
             and beknz in ('S','H','L').   "Debit/Credit Indicator
    add cosp-wkg001 from a_begmth to a_endmth giving value.
    add cosp-wkg001 from 1 to 12 giving annvalue.

    case cosp-wrttp.
      when '01'.
        if cosp-versn = p_vers.                           "omning
          plan = plan + value.
          annplan = annplan + annvalue.                     "998/05/01
        endif.                                            "omning
        clear: value, annvalue.
      when '04'.
        if cosp-versn = '000'.                            "omning
          actual = actual + value.
        endif.                                            "omning
        clear: value.
    endcase.
  endselect.


* COST TOTALS - Internal Postings
  select * from coss
           where objnr = wa-objnr          "Matching objects
             and gjahr = p_fyear           "Fiscal Year selected
             and ( versn = p_vers            "Version
                 or versn = '000' )                             "omning
             and kstar in s_kstar         "Cost element   2010/01/19
             and wrttp in ('01','04').    "TR813 Record with actual/plan
*             and wrttp in ('04').        "TR813 "Record with actuals
*             and beknz in ('S','H','L'). "Debit/Credit Indicator
    add coss-wkg001 from a_begmth to a_endmth giving value.
    add coss-wkg001 from 1 to 12 giving annvalue.   "TR813

    case coss-wrttp.
      when '01'.                           "TR813
        if coss-versn = p_vers.            "TR813         "omning
          plan = plan + value.             "TR813
          annplan = annplan + annvalue.    "TR813           "998/05/01
        endif.                             "TR813         "omning
        clear: value, annvalue.            "TR813

      when '04'.
        if coss-versn = '000'.                            "omning
          actual = actual + value.
        endif.                                            "omning
        clear value.
    endcase.
  endselect.
* End of COST TOTALS - Internal Postings

  if  wa-stufe = '1'.
    perform get_budget_data.
  endif.

  perform build_table.
  clear: plan, actual, variance, locked, commit, annplan, remplan,
         uncommit.
endform.                    "GET_FISCAL_DATA

*-----------------------------------------------------------------------
*     FORM GET_BUDGET_DATA
*-----------------------------------------------------------------------
*   Description:
*   - The routine is used to calculate the budgeted values
*   check bjpa-wrttp for budget(41) vs plan(01)
*   check jest-stat  for status:  I0001 ==> created
*                                 I0002 ==> released?
*                                 I0060 ==> planned
*                                 I0063 ==> locked
*                                 I0067 ==> budgeted
*                                 I0076 ==> deleted
*-----------------------------------------------------------------------
form get_budget_data.
  select * from bpja                 "Budget Info
      where objnr = wa-objnr
        and wrttp in ('41')
*          and wrttp in ('01', '41')                   "998/05/01
        and gjahr = p_fyear
        and versn = '000'.                               "omning
    select * from jest
       where objnr = wa-objnr
       and inact <> 'X'
       and stat in ('I0060','I0067','I0076')        "md7140 97/10/06
       order by stat.
      case bpja-wrttp.
        when '01'.
*               case jest-stat.
*                when 'I0060'.                             "98/05/01
*                   annplan = bpja-wtjhr.                  "Planned
*                endcase.
        when '41'.
          case jest-stat.
            when 'I0067'.
              locked = bpja-wtjhr.         "Unlocked 97/10/06
            when 'I0076'.
              clear: locked, uncommit.       "Deleted
          endcase.
      endcase.
    endselect.

  endselect.
endform.                    "GET_BUDGET_DATA
*-----------------------------------------------------------------------
*     FORM BUILD_TABLE
*-----------------------------------------------------------------------
*   Description:
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
form build_table.
  clear: big_table-plan,    big_table-actual, big_table-variance,
         big_table-locked,  big_table-commit, big_table-annplan,
         big_table-uncommit.
  move 'Z'               to big_table-distgrp.
  if wa-prart between '01' and '05'.
    if wa-pspri co  '1234HJ'.
*    if wa-pspri co  '1234'.
      move '1'         to big_table-distgrp.
    elseif wa-pspri co '58'.
      move '2'         to big_table-distgrp.
    elseif wa-pspri co '79CDE'.
      move '3'         to big_table-distgrp.
    endif.
  endif.
*    move wa-posid          to big_table-posid.
  move wa-posid(7)       to big_table-project.
  move wa-posid(2)       to big_table-dept.
  move wa-prart          to big_table-prart.
  move wa-pspri          to big_table-pspri.
  move wa-psphi          to big_table-psphi.
  move proj-post1        to big_table-post1.
  move proj-verna        to big_table-verna.
*    move wa-objnr          to big_table-objnr.
  move plan              to big_table-plan.
  move actual            to big_table-actual.
  variance = plan - actual.
  move variance          to big_table-variance.

  move locked           to big_table-locked.

  move annplan           to big_table-annplan.
  big_table-uncommit = uncommit.

  big_table-bzdat = 0.
  select single * from cobra
    where objnr = wa-objnr
      and bzdat <> '00000000'.
  if sy-subrc = '0'.
    big_table-bzdat = 1.
  endif.

  collect big_table.
endform.                    "BUILD_TABLE
