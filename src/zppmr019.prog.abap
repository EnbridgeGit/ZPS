REPORT zppmr019 NO STANDARD PAGE HEADING LINE-SIZE 150
                LINE-COUNT 58 MESSAGE-ID pp.

************************************************************************
*
*   PROGRAM:    zppmr019
*   PROGRAMMER: M. Khan
*   CLIENT:     Union Gas
*   DATE:       May 2000.
*
*   The purpose of this program is to produce a list of major projects.
*
************************************************************************
* CHANGES
* Date       Issue        By        Description
* 2004/06/03  1065  Mohammad Khan   Add finish date in the report.
*
************************************************************************

TABLES: prps,               " WBS element master data
        proj,               " Project definition
        coss,               " CO object: internal postings
        cosp,               " CO object: external postings
        bpja,               " Totals record for annual total
        cabn,               " field characteristics
        ausp,               " characteristics value
        t001.               " Company code

DATA: BEGIN OF big_table OCCURS 0,
        dept(2),                                  "Division/Department
        pspri         LIKE prps-pspri,            "Proj. Priority Code
        prart         LIKE prps-prart,            "Project Type Code
        psphi         LIKE prps-psphi,            "Project No. internal
        poski         LIKE prps-poski,            "Project No. external
        pctrl(6),                                 "Project Control
        major,                                    "Major Proj. Indicator
        plsez         LIKE proj-plsez,          "Finish Date -Issue 1065
        post1         LIKE proj-post1,            "Project Description
        wkgnnp(8)     TYPE p DECIMALS 2,          "Plan Amount
        wljhr(8)      TYPE p DECIMALS 2,          "Budget Amount
        wkgnna(10)     TYPE p DECIMALS 2,          "Actual Amount
     END OF big_table.
DATA: BEGIN OF small_table OCCURS 0,
        dept(2),                                  "Division/Department
        pspri         LIKE prps-pspri,            "Proj. Priority Code
        prart         LIKE prps-prart,            "Project Type Code
        psphi         LIKE prps-psphi,            "Project No. internal
        pctrl(6),                                 "Project Control
        major,                                    "Major Proj. Indicator
        plsez         LIKE proj-plsez,          "Finish Date -Issue 1065
        post1         LIKE proj-post1,            "Project Description
        wkgnnp(8)     TYPE p DECIMALS 2,          "Plan Amount
        wljhr(8)      TYPE p DECIMALS 2,          "Budget Amount
        wkgnna(10)     TYPE p DECIMALS 2,          "Actual Amount
     END OF small_table.

*DATA small_table LIKE big_table OCCURS 0 WITH HEADER LINE.
DATA proj_data  LIKE proj OCCURS 0 WITH HEADER LINE.

DATA: amt(10)            TYPE p DECIMALS 2 VALUE 0,
      amt_plan(8)       TYPE p DECIMALS 2 VALUE 0,
      amt_budget(8)     TYPE p DECIMALS 2 VALUE 0,
      amt_actual(10)     TYPE p DECIMALS 2 VALUE 0,
      prev_psphi        LIKE prps-psphi VALUE 0,
      head_print(1)     VALUE 'Y',
      prev_major,
      prev_pctrl(6),
*                                     "Req. fields for Proj ctr & Major
      g_atinn           LIKE cabn-atinn,
      g_atinn_pc        LIKE cabn-atinn,
      g_atinn_mp        LIKE cabn-atinn,
      proj_control(6),                                "Project control
      major_proj,                                     "Major project
      object            LIKE ausp-objek,
      charic            LIKE cabn-atnam,
      gv_poski type prps-poski.


DATA: BEGIN OF warea,
        prart LIKE prps-prart,
        pspri LIKE prps-pspri,
        posid LIKE prps-posid,
        objnr LIKE prps-objnr,
        psphi LIKE prps-psphi,
        poski LIKE prps-poski,
        stufe LIKE prps-stufe,
      END OF warea.

DATA: BEGIN OF char_tab OCCURS 0.
        INCLUDE STRUCTURE ausp.
DATA: END OF char_tab.
*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK boxa WITH FRAME TITLE text-117.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(62) text-118.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) text-119.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(62) text-120.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK boxa.

SELECTION-SCREEN BEGIN OF BLOCK boxb WITH FRAME TITLE text-000.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     p_rpt1 RADIOBUTTON GROUP rept.
SELECTION-SCREEN COMMENT 3(77) text-115.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:   p_rpt2 RADIOBUTTON GROUP rept.
SELECTION-SCREEN COMMENT 3(75) text-116.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK boxb.


SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.

PARAMETERS:
   p_fyear LIKE cosp-gjahr DEFAULT sy-datum+0(4),        "Fiscal Year
   p_ccode LIKE prps-pbukr DEFAULT 'UGL',                "Company code
   p_vers  LIKE cosp-versn DEFAULT '0',                 "Budget Version
   p_check AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
SELECT-OPTIONS:
        s_amt    FOR cosp-wkg001 DEFAULT 200000 TO 100000000,
        s_vernr  FOR proj-vernr,                         "Division
        s_pspri  FOR prps-pspri,                         "Priority
        s_prart  FOR prps-prart,                         "Project Type
        s_atnam  FOR cabn-atnam,                         "Proj. control
        s_pspid  FOR proj-pspid.                         "Project Number

SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-003.
SELECT-OPTIONS:
        s_pspidx   FOR proj-pspid no-DISPLAY,                        "exclude proj.
        s_kstar    FOR coss-kstar                         "Cost element
                   DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-004.
PARAMETERS:     p_rprt RADIOBUTTON GROUP rbcr,            "PRINT REPORT
                p_file RADIOBUTTON GROUP rbcr.            "EXCEL FILE
SELECTION-SCREEN END OF BLOCK box4.

*-----------------------  END of SELECTION SCREEN-----------------------
************************************************************************
*-------------------------  START-OF-SELECTION -------------------------
*AT SELECTION-SCREEN.

*IF P_RPT2 = 'X'.
**  submit zppmr020 via selection-screen and return.
*   SUBMIT ZPPMR020
*     WITH  P_FYEAR = P_FYEAR
*     WITH  P_CCODE = P_CCODE
*     WITH  P_VERS  = P_VERS
*     WITH  P_CHECK = P_CHECK
*     WITH  P_FILE  = P_FILE
*     WITH  S_KSTAR IN S_KSTAR
*     WITH  S_PSPIDX IN S_PSPIDX
*     AND   RETURN.
*     STOP.
*ENDIF.
*
START-OF-SELECTION.

  IF p_rpt2 = 'X'.
    IF sy-batch = 'X'.
      SUBMIT zppmr020 TO SAP-SPOOL     "Changes for Background Printing
      IMMEDIATELY ' '
      KEEP IN SPOOL 'X'
       WITH  p_fyear = p_fyear
       WITH  p_ccode = p_ccode
       WITH  p_vers  = p_vers
       WITH  p_check = p_check
       WITH  p_file  = p_file
       WITH  s_kstar  IN s_kstar
       WITH  s_pspidx IN s_pspidx
       AND   RETURN.
    ELSE.
      SUBMIT zppmr020
       WITH  p_fyear = p_fyear
       WITH  p_ccode = p_ccode
       WITH  p_vers  = p_vers
       WITH  p_check = p_check
       WITH  p_file  = p_file
       WITH  s_kstar  IN s_kstar
       WITH  s_pspidx IN s_pspidx
        AND   RETURN.
    ENDIF.

    STOP.
  ENDIF.


  SELECT SINGLE * FROM t001                "Company Code
    WHERE bukrs = p_ccode.

  IF p_rprt EQ 'X'.
    PERFORM print_variant.
  ENDIF.

  MOVE 'PROJECT_CNTR_NUMBER'  TO  charic.    "Characteristics required
  PERFORM get_atinn.
  MOVE g_atinn      TO   g_atinn_pc.

  MOVE 'MAJORPROJECT'         TO  charic.    "Characteristics required
  PERFORM get_atinn.
  MOVE g_atinn      TO   g_atinn_mp.
  PERFORM build_ausp.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Reading project data'
    EXCEPTIONS
      OTHERS = 1.

  SELECT * FROM proj INTO TABLE proj_data
    WHERE pspid IN s_pspid
      AND vernr IN s_vernr.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Reading PRPS data'
    EXCEPTIONS
      OTHERS = 1.

  LOOP AT proj_data.
    IF s_pspidx IS NOT INITIAL.
      CHECK proj_data-pspid IN s_pspidx.
    ENDIF.
    PERFORM select_prps.
*     IF S_PSPIDX IS INITIAL.
*        PERFORM SELECT_PRPS.
*     ELSEIF NOT ( PROJ_DATA-PSPID IN S_PSPIDX ).
*        PERFORM SELECT_PRPS.
*     ENDIF.
  ENDLOOP.

  SORT big_table BY dept pspri prart psphi
              pctrl DESCENDING major DESCENDING.

  LOOP AT big_table.
    IF big_table-psphi <> prev_psphi.
      prev_psphi = big_table-psphi.
      prev_pctrl = big_table-pctrl.
      prev_major = big_table-major.
    ENDIF.
    MOVE-CORRESPONDING big_table TO small_table.
    small_table-pctrl = prev_pctrl.
    small_table-major = prev_major.
    COLLECT small_table.
    CLEAR   small_table.
  ENDLOOP.

  LOOP AT small_table.                        "Eliminate unwanted rows
    IF NOT ( small_table-pctrl IN s_atnam ).
      DELETE small_table INDEX sy-tabix.
      CONTINUE.

    ELSEIF  p_check = 'X'             AND
            small_table-wkgnnp  = 0   AND
            small_table-wljhr   = 0   AND
            small_table-wkgnna  = 0.
      DELETE small_table INDEX sy-tabix.
      CONTINUE.

    ELSEIF  NOT small_table-wkgnnp  IN  s_amt    AND
            NOT small_table-wljhr   IN  s_amt    AND
            NOT small_table-wkgnna  IN  s_amt.
      DELETE small_table INDEX sy-tabix.
      CONTINUE.
    ENDIF.
  ENDLOOP.
  CLEAR small_table.

  SORT small_table BY dept pspri prart psphi.
  LOOP AT small_table.
    clear gv_poski.
    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
      EXPORTING
        input         = small_table-psphi
     IMPORTING
       OUTPUT        =  gv_poski
*       PSELT         =
              .

    WRITE:/   small_table-dept  UNDER text-107, "003,
              small_table-pspri UNDER text-009,
              small_table-prart UNDER text-010,
              gv_poski          UNDER text-006,
              small_table-post1 UNDER text-007,
              small_table-pctrl UNDER text-011,
              small_table-major UNDER text-008,
              small_table-plsez UNDER text-015.    "Issue 1065
    PERFORM write_amounts.
    AT END OF pspri.
      SUM.
      WRITE: /.
      WRITE: /93 sy-uline.
      WRITE: /2 text-025.
      PERFORM write_amounts.
      WRITE: /93 sy-uline.
    ENDAT.

    AT END OF dept.
      SUM.
      WRITE: /2 text-024.
      PERFORM write_amounts.
      WRITE: /93 sy-uline.
    ENDAT.

    AT LAST.
      SUM.
      WRITE: /2 text-026.
      PERFORM write_amounts.
      WRITE: /.
    ENDAT.

  ENDLOOP.

END-OF-SELECTION.

************************************************************************
*                       SUBROUTINES
************************************************************************

*---------------------  BUILD_TABLE ------------------------------------
FORM build_table.
  big_table-dept    = warea-posid(2).
  big_table-pspri   = warea-pspri.
  big_table-psphi   = warea-psphi.
  big_table-prart   = warea-prart.
  big_table-post1   = proj_data-post1.
  big_table-plsez   = proj_data-plsez.               "Issue 1065
  big_table-wkgnnp  = amt_plan.                             "Plan Amount
  big_table-wkgnna  = amt_actual.                           "Actual Amou
  big_table-wljhr   = amt_budget.                           "Budget Amou
  object = warea-objnr.
*  WRITE warea-psphi TO big_table-poski.
  big_table-poski = warea-poski.
  PERFORM find_characteristic.
  big_table-pctrl   = proj_control.
  big_table-major   = major_proj.

  APPEND big_table.
  CLEAR: big_table, amt_actual, amt_plan, amt_budget, amt.
ENDFORM.                    "BUILD_TABLE

*---------------------  SELECT_PRPS  -----------------------------------

FORM select_prps.

  SELECT prart pspri posid objnr psphi poski stufe
    INTO warea FROM prps
         WHERE pbukr = p_ccode
           AND pkokr = '10'
           AND psphi  = proj_data-pspnr
           AND loevm <> 'X'
           AND prart IN s_prart
           AND pspri IN s_pspri.
*           AND ( PLAKZ = 'X' OR BELKZ = 'X' OR STUFE = 1 )   "new
    IF warea-posid+5(2) CO '1234567890'.   "Eliminates Templates
      PERFORM get_amounts.
      PERFORM build_table.
    ENDIF.
  ENDSELECT.
ENDFORM.                    "SELECT_PRPS

*---------------------  WRITE_AMOUNTS-----------------------------------
FORM write_amounts.

  IF small_table-wkgnnp <  0.
    FORMAT COLOR COL_NEGATIVE INVERSE ON.
    WRITE:  small_table-wkgnnp DECIMALS 0 UNDER text-012.
    FORMAT COLOR OFF INVERSE OFF.
  ELSE.
    WRITE:  small_table-wkgnnp DECIMALS 0 UNDER text-012.
  ENDIF.
  IF small_table-wljhr <  0.
    FORMAT COLOR COL_NEGATIVE INVERSE ON.
    WRITE:  small_table-wljhr DECIMALS 0 UNDER text-014.
    FORMAT COLOR OFF INVERSE OFF.
  ELSE.
    WRITE:  small_table-wljhr DECIMALS 0 UNDER text-014.
  ENDIF.
  IF small_table-wkgnna <  0.
    FORMAT COLOR COL_NEGATIVE INVERSE ON.
    WRITE: small_table-wkgnna DECIMALS 0 UNDER text-016.
    FORMAT COLOR OFF INVERSE OFF.
  ELSE.
    WRITE: small_table-wkgnna DECIMALS 0 UNDER text-016.
  ENDIF.
ENDFORM.                    "WRITE_AMOUNTS

*---------------------  GET_AMOUNTS ------------------------------------
FORM get_amounts.
* COST TOTALS - External Postings

  SELECT * FROM cosp
    WHERE objnr = warea-objnr
      AND gjahr = p_fyear                              "Fiscal Year
      AND ( versn = p_vers                             "Version
       OR   versn = '000' )
      AND wrttp IN ('01', '04')                        "Plan and Actuals
      AND kstar IN  s_kstar
      AND beknz IN ('S', 'H', 'L').               "Debit/Credit Indicator
    IF sy-subrc = '0'.
      CLEAR amt.
      ADD  cosp-wkg001 FROM 1 TO 12 GIVING amt.
      CASE cosp-wrttp.
        WHEN '01'.
          IF cosp-versn = p_vers.
            amt_plan = amt_plan + amt.
          ENDIF.
        WHEN '04'.
          IF cosp-versn = '000'.
            amt_actual = amt_actual + amt.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDSELECT.
*
* COST TOTALS - Internal Postings

  SELECT * FROM coss
    WHERE objnr = warea-objnr
      AND gjahr = p_fyear                              "Fiscal Year
     AND ( versn = p_vers                             "Version
      OR   versn = '000' )
     AND kstar IN  s_kstar
      AND wrttp IN ('01' , '04').               "Plan/Actuals
    IF sy-subrc = '0'.
      CLEAR amt.
      ADD  coss-wkg001 FROM 1 TO 12 GIVING amt.
      CASE coss-wrttp.
        WHEN '01'.
          IF coss-versn = p_vers.
            amt_plan = amt_plan + amt.
          ENDIF.
        WHEN '04'.
          IF coss-versn = '000'.
            amt_actual = amt_actual + amt.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDSELECT.
  IF warea-stufe = '1'.
    PERFORM budget_amounts.
  ENDIF.
ENDFORM.                    "GET_AMOUNTS
*
*---------------------  BUDGET_AMOUNTS ---------------------------------
FORM budget_amounts.
  SELECT * FROM bpja                                  "Budget Info
       WHERE objnr = warea-objnr
         AND wrttp = '41'
         AND gjahr = p_fyear
         AND versn = '000'. "P_VERS.                          "Version
    IF sy-subrc = '0'.
      ADD  bpja-wljhr TO amt_budget.
    ENDIF.
  ENDSELECT.
*I0060   E     PLCS  Planned costs
*I0067   E     BUDG  Budgeted
*I0076   E     DLFL  Deletion Flag
ENDFORM.                    "BUDGET_AMOUNTS

*-----------------------  GET_ATINN  -----------------------------------
* Routine used to get the internal character number for project control
*-----------------------------------------------------------------------
FORM get_atinn.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
    EXPORTING
      class_type                  = '014'
      feature_neutral_name        = charic
    IMPORTING
      feature_id                  = g_atinn
    EXCEPTIONS
      invalid_class_type          = 1
      missing_feature_information = 2
      no_feature_found            = 3
      no_feature_valid            = 4
      no_language                 = 5
      OTHERS                      = 6.
  IF sy-subrc NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTICS OF', charic.
    WRITE: /.
  ENDIF.
ENDFORM.                    "GET_ATINN

*---------------------  BUILD_AUSP -------------------------------------
FORM build_ausp.

  REFRESH char_tab.
  SELECT * FROM ausp INTO TABLE char_tab
         WHERE ( atinn = g_atinn_pc OR
                 atinn = g_atinn_mp ) AND
                 mafid = 'O'          AND
                 klart = '014'.
  SORT char_tab BY objek atinn.
ENDFORM.                    "BUILD_AUSP

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine to get the value of the project control & major indicator
*-----------------------------------------------------------------------
FORM find_characteristic.

  CLEAR: proj_control, major_proj.
  READ TABLE char_tab WITH KEY objek = warea-objnr
                               atinn = g_atinn_pc  BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE char_tab-atwrt+0(6) TO proj_control.
  ENDIF.

  READ TABLE char_tab WITH KEY objek = warea-objnr
                               atinn = g_atinn_mp  BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE char_tab-atwrt+0(6) TO major_proj.
  ENDIF.

ENDFORM.                    "FIND_CHARACTERISTIC
*------------------- PRINT VARIANT -------------------------------------

FORM print_variant.

  WRITE: /20 text-100, 50 text-101.
  WRITE: / sy-uline(19) UNDER text-100.

  WRITE: / text-102 UNDER  text-100,
           p_fyear  UNDER  text-101.

  WRITE: / text-103 UNDER  text-100,
           p_ccode  UNDER  text-101.

  WRITE: / text-104 UNDER  text-100,
           p_vers   UNDER  text-101.

  WRITE: / text-106 UNDER  text-100.
*           p_amt    under  text-101.
  LOOP AT s_amt.
    IF s_amt-option = 'EQ'.
      WRITE: s_amt-option UNDER text-101, s_amt-low.
    ELSE.
      WRITE: s_amt-option UNDER text-101, s_amt-low, s_amt-high.
    ENDIF.
    SKIP.
  ENDLOOP.


*  WRITE: / text-107 UNDER text-100.
WRITE: / 'Div.' UNDER text-100.
  LOOP AT s_vernr.
    IF s_vernr-option = 'EQ'.
      WRITE: s_vernr-option UNDER text-101, s_vernr-low.
    ELSE.
      WRITE: s_vernr-option UNDER text-101, s_vernr-low, s_vernr-high.
    ENDIF.
    SKIP.
  ENDLOOP.

  WRITE: / text-105 UNDER text-100.
  LOOP AT s_pspri.
    IF s_pspri-option = 'EQ'.
      WRITE: s_pspri-option UNDER text-101, s_pspri-low.
    ELSE.
      WRITE: s_pspri-option UNDER text-101, s_pspri-low, s_pspri-high.
    ENDIF.
    SKIP.
  ENDLOOP.

  WRITE: / text-109 UNDER text-100.
  LOOP AT s_prart.
    IF s_prart-option = 'EQ'.
      WRITE: s_prart-option UNDER text-101, s_prart-low.
    ELSE.
      WRITE: s_prart-option UNDER text-101, s_prart-low, s_prart-high.
    ENDIF.
    SKIP.
  ENDLOOP.

  WRITE: / 'Control' UNDER text-100.
  LOOP AT s_atnam.
    IF s_atnam-option = 'EQ'.
      WRITE: s_atnam-option UNDER text-101, s_atnam-low.
    ELSE.
      WRITE: s_atnam-option UNDER text-101, s_atnam-low, s_atnam-high.
    ENDIF.
    SKIP.
  ENDLOOP.

WRITE: / 'Project' UNDER text-100.
  LOOP AT s_pspid.
    IF s_pspid-option = 'EQ'.
      WRITE: s_pspid-option UNDER text-101, s_pspid-low.
    ELSE.
      WRITE: s_pspid-option UNDER text-101, s_pspid-low, s_pspid-high.
    ENDIF.
    SKIP.
  ENDLOOP.

  SKIP.
  WRITE: / text-112 UNDER text-100.
  WRITE: / 'Project' UNDER text-100.
  LOOP AT s_pspidx.
    IF s_pspidx-option = 'EQ'.
      WRITE: s_pspidx-option UNDER text-101, s_pspidx-low.
    ELSE.
      WRITE: s_pspidx-option UNDER text-101, s_pspidx-low,
                                              s_pspidx-high.
    ENDIF.
    SKIP.
  ENDLOOP.

  WRITE: / text-111 UNDER text-100.
  LOOP AT s_kstar.
    IF s_kstar-option = 'EQ'.
      WRITE:  s_kstar-option UNDER text-101, s_kstar-low.
    ELSE.
      WRITE:  s_kstar-option UNDER text-101, s_kstar-low,
                                       s_kstar-high.
    ENDIF.
    SKIP.
  ENDLOOP.

*  NEW-PAGE.
ENDFORM.                    "PRINT_VARIANT
*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
  IF head_print = 'Y'.
    WRITE: /1 text-rpt, sy-repid,  60 t001-butxt,             "Company
          103 text-dte, sy-datum, text-amp, sy-uzeit.
    WRITE: / text-clt UNDER text-rpt, sy-sysid, sy-mandt,  "Title
           50 text-005,
             text-pge UNDER text-dte, sy-pagno UNDER sy-datum.
    WRITE: / text-ver UNDER text-rpt, p_vers,
           60 text-tl2, p_fyear.                           "Fiscal Year
    ULINE.
    WRITE: 1  text-107, 6 text-009, 9 text-010, 12 text-006,
           29 text-007, 72 text-011, 80 text-008, 82 text-015,
           95 text-012, 113 text-014, 130 text-016.
    ULINE.
    IF p_file = 'X'.
      head_print = 'N'.
    ENDIF.
  ENDIF.
*------------------------- End of Report Header ------------------------
