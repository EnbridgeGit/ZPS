REPORT ZPPSR002 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        December 1997
*  Description:
*     - The purpose of this program is to produce Summary of Capital
*       Expenditures Report by Project Type
************************************************************************
* 98/03/23 md7140 #--- Change in template definition
* 97/12/16 md7140 #289 Summary of Capital Expenditures Report
************************************************************************

*ables: coss, csku, jest,  t247,
*       tcn7t,
TABLES: PROJ,                          "Project definition
        PRPS,                          "WBS Element
        COSP,                          "Cost Totals - External Postings
        BPJA,                          "Budget
        TCJ1T,                         "Project Types table
        T001.                          "Company Name Table


DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       VERNR         LIKE PROJ-VERNR,  "Div/Dept
       PSPID         LIKE PROJ-PSPID,  "Project
       PSPNR         LIKE PROJ-PSPNR,  "Project
       POST1         LIKE PROJ-POST1,  "Project Description
       APPPL(5)      TYPE P,           "Approved Capital Plan
       INCPL(5)      TYPE P,           "Incremental Plan
       YTDAC(5)      TYPE P,           "YTD Actuals
       TOTCM(5)      TYPE P,           "Total Committed
       TOTPL(5)      TYPE P,           "Total Plan
       REMPL(5)      TYPE P,           "Remaining Plan
       YTDVCO(5)     TYPE P,           "Variance Ytd Act Committed
       YTDVTPL(5)    TYPE P,           "Variance Ytd Act Total Plan
       YTDPL(5)      TYPE P,           "YTD Plan - versions 000 & 010
       YTDVPL(5)     TYPE P,           "Variance YTD Plan

   END OF BIG_TABLE.


DATA:   P_BEGNME(9),
        P_ENDNME(9),
        BMTH(2)       TYPE C,
        EMTH(2)       TYPE C,
        VALUE         LIKE COSP-WKG001,
        VERSN         LIKE COSP-VERSN,
        WRTTP         LIKE COSP-VERSN,
        OBJNR         LIKE COSP-OBJNR,
        APPPL         LIKE COSP-WKG001,
        INCPL         LIKE COSP-WKG001,
        YTDAC         LIKE COSP-WKG001,
        TOTCM         LIKE COSP-WKG001,
        REMPL         LIKE COSP-WKG001,
        YTDVCO        LIKE COSP-WKG001,
        YTDVTPL       LIKE COSP-WKG001,
        YTDPLA        LIKE COSP-WKG001,
        YTDPLB        LIKE COSP-WKG001,
        YTDPL         LIKE COSP-WKG001.
*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:     P_CCODE LIKE T001T-BUKRS DEFAULT 'UGL' OBLIGATORY,
                P_VERS  LIKE COSP-VERSN DEFAULT '0',  "Plan version
                P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
                P_BMTH(2)    DEFAULT SY-DATUM+4(2) OBLIGATORY,
                P_EMTH(2)    DEFAULT '12'.
SELECT-OPTIONS: S_VERNR FOR PROJ-VERNR,
                S_PSPID FOR PROJ-PSPID.
PARAMETERS:     P_DOLLAR LIKE COSP-WKG001.
SELECTION-SCREEN END OF BLOCK BOX.

* End of selection screen
*-----------------------------------------------------------------------

IF  P_EMTH = SPACE.
  P_EMTH = P_BMTH.
ENDIF.

START-OF-SELECTION.
  SELECT * FROM PROJ
   WHERE VBUKR = P_CCODE               "Company Code
     AND VERNR IN S_VERNR              "Division
     AND PSPID IN S_PSPID              "Project Number
     AND LOEVM <> 'X'.                 "Not Deleted
*   if proj-pspid(7) co '1234567890'."No templates
    IF PROJ-PSPID+5(2) CO '1234567890'."No templates           98/03/23
      CLEAR BIG_TABLE.
      SELECT * FROM PRPS
        WHERE PSPHI = PROJ-PSPNR
          AND LOEVM <> 'X'.            "Not deleted
        PERFORM GET_COSP USING PRPS-OBJNR '001' '01' APPPL SY-SUBRC  "#1
                 P_BMTH P_EMTH.
        PERFORM GET_COSP USING PRPS-OBJNR '010' '01' INCPL SY-SUBRC  "#2
                 P_BMTH P_EMTH.
        PERFORM GET_COSP USING PRPS-OBJNR '000' '01' YTDAC SY-SUBRC  "#6
                      1 P_EMTH.
        PERFORM GET_COSP USING PRPS-OBJNR '000' '01' YTDPLA SY-SUBRC "#9
                      1 P_EMTH.
        PERFORM GET_COSP USING PRPS-OBJNR '010' '01' YTDPLB SY-SUBRC "#9
                      1 P_EMTH.
*                 Total all amounts for project
        BIG_TABLE-APPPL = BIG_TABLE-APPPL + APPPL.
        BIG_TABLE-INCPL = BIG_TABLE-INCPL + INCPL.
        BIG_TABLE-YTDAC = BIG_TABLE-YTDAC + YTDAC.
        BIG_TABLE-YTDPL = BIG_TABLE-YTDPL + YTDPLA + YTDPLB.
        IF PRPS-STUFE = '1'.
          PERFORM GET_BPJA.
          BIG_TABLE-TOTCM = BIG_TABLE-TOTCM + TOTCM.
        ENDIF.
      ENDSELECT.                       "End of PRPS
      PERFORM BUILD_TABLE.
      APPEND BIG_TABLE.
    ENDIF.                             "End of No Templates
  ENDSELECT.
  PERFORM DISPLAY_TABLE.

*------------------------  GET_COSP  ----------------------------------*
* Calculates value dependent on version, value type.                   *
* Parameters are versn (version), wrttp (value type ie. Planning,Actual)
*   value (calculated amount being passed back),                       *
*   sy-subrc (return code being passed back).                          *
*   bmth - beginning month, emth - ending month                        *
*----------------------------------------------------------------------*
FORM GET_COSP USING OBJNR VERSN WRTTP VALUE SY-SUBRC BMTH EMTH.
  CLEAR: VALUE, SY-SUBRC.
  SELECT * FROM COSP
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_FYEAR
      AND VERSN = VERSN
      AND WRTTP = WRTTP.
    ADD COSP-WKG001 FROM BMTH TO EMTH GIVING VALUE.
  ENDSELECT.
ENDFORM.

*------------------------  GET_BPJA  ----------------------------------*
* Calculates budget                                                    *
*---------------------------------------------------------------------*
FORM GET_BPJA.
  CLEAR: TOTCM.
  SELECT * FROM BPJA
    WHERE OBJNR = PRPS-OBJNR
      AND GJAHR = P_FYEAR
      AND VERSN = '000'
      AND WRTTP = '04'.
    MOVE BPJA-WLJHR TO TOTCM.
  ENDSELECT.
ENDFORM.

*-----------------------  BUILD_TABLE  ---------------------------------
FORM BUILD_TABLE.
  MOVE PROJ-VERNR     TO BIG_TABLE-VERNR.
  MOVE PROJ-PSPNR     TO BIG_TABLE-PSPNR.
  MOVE PROJ-PSPID     TO BIG_TABLE-PSPID.
  MOVE PROJ-POST1     TO BIG_TABLE-POST1.
  BIG_TABLE-TOTPL = BIG_TABLE-APPPL + BIG_TABLE-INCPL.

* move totcm          to big_table-totcm.               "Total Committed
* if actuals > totcm.                                   "Fix this
*    big_table-totcm = actuals.
* endif.
  BIG_TABLE-REMPL = BIG_TABLE-TOTPL - BIG_TABLE-TOTCM.

  BIG_TABLE-YTDVCO  = BIG_TABLE-TOTCM  - YTDAC.
  BIG_TABLE-YTDVTPL =  BIG_TABLE-TOTPL - BIG_TABLE-YTDAC.
  BIG_TABLE-YTDVPL  = BIG_TABLE-YTDPL - BIG_TABLE-YTDAC.
ENDFORM.

*----------------------------  DISPLAY_TABLE ---------------------------
FORM DISPLAY_TABLE.
* sort big_table by vernr pspnr.
  SORT BIG_TABLE BY VERNR PSPID.
  LOOP AT BIG_TABLE.
    AT NEW VERNR.                      "Division Break
      PERFORM GET_COMPANY_NAME.
      NEW-PAGE.
      PERFORM PRINT_VERT.
      WRITE: BIG_TABLE-VERNR UNDER TEXT-025.
    ENDAT.

    PERFORM PRINT_VERT.
    WRITE:  BIG_TABLE-PSPID  UNDER TEXT-026,
            BIG_TABLE-POST1  UNDER TEXT-027,
            BIG_TABLE-APPPL  UNDER TEXT-005,
            BIG_TABLE-INCPL  UNDER TEXT-008,
            BIG_TABLE-TOTPL  UNDER TEXT-009,
            BIG_TABLE-TOTCM  UNDER TEXT-010,
            BIG_TABLE-REMPL  UNDER TEXT-012,
            BIG_TABLE-YTDAC  UNDER TEXT-013,
            BIG_TABLE-YTDPL  UNDER TEXT-018.

    IF BIG_TABLE-YTDVCO < 0.
      FORMAT COLOR COL_NEGATIVE INVERSE ON.
    ENDIF.
    WRITE: BIG_TABLE-YTDVCO  UNDER TEXT-014.
    FORMAT COLOR COL_BACKGROUND.

    IF BIG_TABLE-YTDVTPL < 0.
      FORMAT COLOR COL_NEGATIVE INVERSE ON.
    ENDIF.
    WRITE: BIG_TABLE-YTDVTPL UNDER TEXT-016.
    FORMAT COLOR COL_BACKGROUND.

    IF BIG_TABLE-YTDVPL < 0.
      FORMAT COLOR COL_NEGATIVE INVERSE ON.
    ENDIF.
    WRITE: BIG_TABLE-YTDVPL  UNDER TEXT-020.
    FORMAT COLOR COL_BACKGROUND.

  ENDLOOP.
ENDFORM.

*---------------------------- GET_COMPANY_NAME -------------------------
* find company name.
*-----------------------------------------------------------------------
FORM GET_COMPANY_NAME.
  SELECT SINGLE * FROM T001            "Get Company Name
       WHERE BUKRS = P_CCODE.
ENDFORM.

*   select single * from t247                   "Get Month Name(Begin)
*       where spras = 'E' and
*            mnr = p_begmth.
*  p_begnme = t247-ltx.
*  p_endnme = space.
*   if p_endmth <> space.
*      select single * from t247                "Get Month Name(End)
*             where spras = 'E' and
*                   mnr = p_endmth.
*      p_endnme = t247-ltx.
*  endif.

*   if sy-subrc eq 0.
*    perform display_table.
*  endif.


*-----------------------------------------------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: /1 TEXT-RPT, SY-REPID,        "Report Id
          70 T001-BUTXT,               "Company Name
        140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT,            "Client
          60 TEXT-003,                 "Report Title
          TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "Page Number
  WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS.              "Version
  IF P_EMTH = SPACE.                   "Time Frame
    WRITE: /110 P_BMTH.
* else.
*   write: /100 p_begnme, text-024, p_endnme, text-025.
  ENDIF.
  FORMAT INTENSIFIED ON.
  PERFORM PRINT_HEADINGS.


*-----------------------------------------------------------------------
*     FORM PRINT_HEADINGS
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
  FORMAT COLOR 2 ON.
  WRITE: /1(170) SY-ULINE.
  PERFORM PRINT_VERT.
  WRITE:  2 TEXT-025,
         61 TEXT-005,  72 TEXT-008,  83 TEXT-009,  94 TEXT-010,
        105 TEXT-012, 116 TEXT-013, 127 TEXT-014, 138 TEXT-016,
        149 TEXT-018, 160 TEXT-020.
  PERFORM PRINT_VERT.
  WRITE: 10 TEXT-026, 20 TEXT-027.
  WRITE: TEXT-006 UNDER TEXT-005, TEXT-007 UNDER TEXT-008,
         TEXT-007 UNDER TEXT-009, TEXT-011 UNDER TEXT-010,
         TEXT-007 UNDER TEXT-012, TEXT-024 UNDER TEXT-013,
         TEXT-015 UNDER TEXT-014, TEXT-015 UNDER TEXT-016,
         TEXT-019 UNDER TEXT-018, TEXT-015 UNDER TEXT-020.
  PERFORM PRINT_VERT.
  WRITE: TEXT-007 UNDER TEXT-005, TEXT-011 UNDER TEXT-014,
         TEXT-017 UNDER TEXT-016, TEXT-021 UNDER TEXT-020.
  ULINE.
  FORMAT COLOR COL_BACKGROUND.

ENDFORM.

*------------------------  PRINT_VERT  ---------------------------------
*  This procedure is used to print vertical lines to separate one
*  column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:  /1 SY-VLINE,
          60 SY-VLINE,  71 SY-VLINE,  82 SY-VLINE,  93 SY-VLINE,
         104 SY-VLINE, 115 SY-VLINE, 126 SY-VLINE, 137 SY-VLINE,
         148 SY-VLINE, 159 SY-VLINE, 170 SY-VLINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM HEADING_DEPT                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM HEADING_DEPT.
*      perform print_vert.
*      format color col_positive.
*      write: 2 text-023, verna.
*      format color col_background.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM HEADING_PROJECT_TYPE                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM HEADING_PROJECT_TYPE.
*      select single * from tcj1t
*          where langu = 'E' and
*                prart = big_table-prart.
*          perform print_vert.
*          format color col_total.
*          write: 20 tcj1t-pratx.
*          format color col_background.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM HEADING_PRIORITY_TYPE                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM HEADING_PRIORITY_TYPE.
*    select single * from tcn7t
*           where langu = 'E' and
*                 nprio = big_table-pspri.
*           format color col_group.
*           if  sy-subrc <> 0.
*               perform print_vert.
*               write: 40 text-021, big_table-pspri, text-022.
*           else.
*               perform print_vert.
*               write: 40 tcn7t-ktext.
*           endif.
*           format color col_background.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BIG_TABLE_DUMP                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BIG_TABLE_DUMP.
*     write: /2 big_table-posid,
*                 111 big_table-plan,            "YTD Plan
*                 129 big_table-actual,          "Actuals
*                147 big_table-variance,        "Variance(actual - plan
*                 165 big_table-locked,          "Appr Budget Comt
*                 183 big_table-variance,        "Total Commitment
*                 183 big_table-commit,          "Total Commitment
*                 201 big_table-annplan,         "Annual Plan
*                 219 wa-remplan,                "Remaining Plan
*                 237 big_table-uncommit.        "Uncommitted Budget
ENDFORM.
*---------------------------------------------------------------------*
*       FORM GET_FISCAL_DATA                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_FISCAL_DATA.
*write: / wa-prart, wa-pspri, wa-posid, wa-objnr, wa-psphi, wa-plakz.
* COST TOTALS - External Postings
*   select * from cosp
*            where objnr = wa-objnr          "Matching objects
*              and gjahr = p_fyear           "Fiscal Year selected
*              and versn = p_vers            "Version
*             and wrttp in ('01','04')      "Record with actuals & plans
*             and beknz in ('S','H','L').    "Debit/Credit Indicator
*              and kstar in skstar.          "Cost Element/Cost Group
*   add cosp-wkg001 from p_bmth to p_emth giving value.

*      case cosp-wrttp.
*           when '01'.
*                plan = plan + value.
*                clear value.
*           when '04'.
*                actual = actual + value.
*                clear value.
*      endcase.
*  endselect.


* COST TOTALS - Internal Postings
*  select * from coss
*           where objnr = wa-objnr          "Matching objects
*             and gjahr = p_fyear           "Fiscal Year selected
*             and versn = p_vers            "Version
*             and wrttp in ('04')           "Record with actuals
*             and beknz in ('S','H','L').   "Debit/Credit Indicator
*             and kstar in skstar.          "Cost Element/Cost Group
*   add coss-wkg001 from a_begmth to a_endmth giving value.

*      case coss-wrttp.
*          when '04'.
*               actual = actual + value.
*               clear value.
*     endcase.
*  endselect.
* End of COST TOTALS - Internal Postings

*  if  wa-stufe = '1'.
*     perform get_budget_data.
* endif.

*  perform build_table.
* clear: plan, actual, variance, locked, commit, annplan, remplan,
*        uncommit.
ENDFORM.

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
FORM GET_BUDGET_DATA.
*    select * from bpja                 "Budget Info
*        where objnr = wa-objnr
*          and wrttp in ('01', '41')
*          and gjahr = p_fyear
*          and versn = p_vers.
*       select * from jest
*          where objnr = wa-objnr
*          and inact <> 'X'
*          and stat in ('I0060','I0067','I0076')        "md7140 97/10/06
*          order by stat.
*         case bpja-wrttp.
*            when '01'.
*              case jest-stat.
*              when 'I0060'.
*                 annplan = bpja-wtjhr.                  "Planned
*              endcase.
*             when '41'.
*               case jest-stat.
*               when 'I0067'.
*                  locked = bpja-wtjhr.         "Unlocked 97/10/06
*              when 'I0076'.
*                 clear: annplan, locked, uncommit.       "Deleted
*              endcase.
*         endcase.
*       endselect.
*
*  endselect.
ENDFORM.
