REPORT zppmr020 NO STANDARD PAGE HEADING LINE-SIZE 140
                LINE-COUNT 58 MESSAGE-ID pp.

************************************************************************
*
*   PROGRAM:    zppmr020
*   PROGRAMMER: M. Khan
*   CLIENT:     Union Gas
*   DATE:       June 2000.
*
*   This program query report for major projects.
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
*        POSKI         LIKE PRPS-POSKI,            "Project No. external
        post1         LIKE proj-post1,            "Project Description
        plsez         LIKE proj-plsez,         "Finish Date   Issue 1065
        pctrl(6),                                 "Project Control
        wkgnnp(7)     TYPE p DECIMALS 2,          "Plan Amount
        wljhr(7)      TYPE p DECIMALS 2,          "Budget Amount
        wkgnna(7)     TYPE p DECIMALS 2,          "Actual Amount
     END OF big_table.

DATA: amt(8)            TYPE p DECIMALS 2 VALUE 0,
      amt_plan(8)       TYPE p DECIMALS 2 VALUE 0,
      amt_budget(8)     TYPE p DECIMALS 2 VALUE 0,
      amt_actual(8)     TYPE p DECIMALS 2 VALUE 0,
      prev_posid        LIKE prps-posid   VALUE space,
      head_print(1)     VALUE 'Y',
*                                     "Req. fields for Major Project
      g_atinn           LIKE cabn-atinn,
      g_atinn_pc        LIKE cabn-atinn,
      g_atinn_mp        LIKE cabn-atinn,
      proj_control(6),                                "Project control
      major_proj,                                     "Major project
      object            LIKE ausp-objek,
      charic            LIKE cabn-atnam,
      gv_poski TYPE prps-poski.

DATA: BEGIN OF char_tab OCCURS 0.                     "Major project
        INCLUDE STRUCTURE ausp.
DATA: END OF char_tab.
DATA: BEGIN OF char_tabpc OCCURS 0.                   "Project control
        INCLUDE STRUCTURE ausp.
DATA: END OF char_tabpc.
DATA: BEGIN OF prps_data OCCURS 0,
        poski LIKE prps-poski,
        psphi LIKE prps-psphi,
        prart LIKE prps-prart,
        pspri LIKE prps-pspri,
        posid LIKE prps-posid,
        objnr LIKE prps-objnr,
        stufe LIKE prps-stufe,
      END OF prps_data.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-nt1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(62) text-nt2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-001.

PARAMETERS:
   p_fyear LIKE cosp-gjahr,                              "Fiscal Year
   p_ccode LIKE prps-pbukr,                              "Company code
   p_vers  LIKE cosp-versn,                              "Budget Version
   p_check AS CHECKBOX.                               "for zero suppress

SELECT-OPTIONS:
   s_objnr FOR prps-objnr,                            "Dummy Selection
   s_psphi FOR prps-psphi.                            "Dummy Selection
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-002.
SELECT-OPTIONS:
        s_pspidx   FOR proj-pspid,                        "exclude proj.
        s_kstar    FOR coss-kstar.                         "Cost element

SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-004.
PARAMETERS:     p_rprt RADIOBUTTON GROUP rbcr,            "PRINT REPORT
                p_file RADIOBUTTON GROUP rbcr.            "EXCEL FILE
SELECTION-SCREEN END OF BLOCK box4.

*-----------------------  END of SELECTION SCREEN-----------------------
************************************************************************
*-------------------------  START-OF-SELECTION -------------------------
AT SELECTION-SCREEN.

START-OF-SELECTION.

  SELECT SINGLE * FROM t001                    "Company Code
    WHERE bukrs = p_ccode.

  MOVE 'PROJECT_CNTR_NUMBER'  TO  charic.    "Characteristics required
  PERFORM get_atinn.
  MOVE g_atinn      TO   g_atinn_pc.


  MOVE 'MAJORPROJECT'         TO  charic.    "Characteristics required
  PERFORM get_atinn.
  MOVE g_atinn      TO   g_atinn_mp.
  PERFORM build_ausp_majproj.
  PERFORM build_ausp_pcontrol.

  REFRESH s_objnr.
  LOOP AT char_tab.
    s_objnr-sign   = 'I'.
    s_objnr-option = 'EQ'.
    s_objnr-low = char_tab-objek.
    APPEND s_objnr.
    CLEAR  s_objnr.
  ENDLOOP.

  REFRESH prps_data.

  SELECT psphi prart pspri posid objnr stufe INTO CORRESPONDING FIELDS OF
         TABLE prps_data FROM prps
     WHERE objnr IN s_objnr
       AND pbukr = p_ccode
       AND pkokr = '10'
*      AND STUFE = 1
       AND loevm <> 'X'.

  REFRESH s_psphi.
  LOOP AT prps_data.
    IF prps_data-posid+5(2) CO '1234567890'.     "Eliminates Templates
      s_psphi-sign   = 'I'.
      s_psphi-option = 'EQ'.
      s_psphi-low = prps_data-psphi.
      APPEND s_psphi.
      CLEAR  s_psphi.
    ELSE.
      DELETE prps_data.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  REFRESH prps_data.
  SELECT poski psphi prart pspri posid objnr stufe INTO CORRESPONDING FIELDS OF
         TABLE prps_data FROM prps
     WHERE psphi IN s_psphi
       AND pbukr = p_ccode
*      AND ( PLAKZ = 'X' OR BELKZ = 'X' OR STUFE = 1 )
       AND loevm <> 'X'.

  SORT prps_data BY  psphi prart pspri posid stufe objnr.

  LOOP AT prps_data.
    IF prev_posid <> prps_data-posid.
      prev_posid  = prps_data-posid.
      SELECT SINGLE * FROM proj
        WHERE pspnr = prps_data-psphi.
    ENDIF.
    IF s_pspidx[] IS NOT INITIAL.
      CHECK proj-pspid IN s_pspidx.
    ENDIF.
    IF prps_data-stufe = 1.
      PERFORM find_characteristic.
    ENDIF.

    PERFORM get_amounts.
    IF p_check = 'X'.
      IF amt_plan EQ 0  AND  amt_actual EQ 0  AND  amt_budget EQ 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    PERFORM build_table.
  ENDLOOP.

  SORT big_table BY dept pspri prart psphi.

  LOOP AT big_table.
    CLEAR gv_poski.
    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
      EXPORTING
        input  = big_table-psphi
      IMPORTING
        output = gv_poski.

    WRITE:/   big_table-dept  UNDER text-003,
              big_table-pspri UNDER text-009,
              big_table-prart UNDER text-010,
              gv_poski        UNDER text-006,
*                BIG_TABLE-PSPHI UNDER TEXT-006,
*                BIG_TABLE-POSKI UNDER TEXT-006,
              big_table-post1 UNDER text-007,
              big_table-plsez UNDER text-015,    "Issue 1065
              big_table-pctrl UNDER text-011.
    PERFORM write_amounts.
    AT END OF pspri.
      SUM.
      WRITE: /.
      WRITE: /94 sy-uline.
      WRITE: /2 text-025.
      PERFORM write_amounts.
      WRITE: /94 sy-uline.
    ENDAT.

    AT END OF dept.
      SUM.
      WRITE: /2 text-024.
      PERFORM write_amounts.
      WRITE: /94 sy-uline.
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
  big_table-dept    = prps_data-posid(2).
  big_table-pspri   = prps_data-pspri.
  big_table-prart   = prps_data-prart.
  big_table-psphi   = prps_data-psphi.
*    BIG_TABLE-POSKI   = PRPS_DATA-POSKI.
  big_table-post1   = proj-post1.
  big_table-plsez   = proj-plsez.
  big_table-wkgnnp  = amt_plan.                           "Plan Amount
  big_table-wkgnna  = amt_actual.                         "Actual Amou
  big_table-wljhr   = amt_budget.                         "Budget Amou
  big_table-pctrl   = proj_control.

  COLLECT big_table.
  CLEAR: big_table, amt_actual, amt_plan, amt_budget, amt.
ENDFORM.                    "BUILD_TABLE

*---------------------  SELECT_PRPS ------------------------------------

*REFRESH PRPS_DATA.
FORM select_prps.
  SELECT psphi prart pspri posid objnr stufe INTO CORRESPONDING FIELDS OF
         TABLE prps_data FROM prps
     WHERE objnr IN s_objnr
       AND pbukr = p_ccode
       AND stufe = 1
       AND loevm <> 'X'.
ENDFORM.                    "SELECT_PRPS
*---------------------  WRITE_AMOUNTS-----------------------------------
FORM write_amounts.

  IF big_table-wkgnnp <  0.
    FORMAT COLOR COL_NEGATIVE INVERSE ON.
    WRITE:  big_table-wkgnnp DECIMALS 0 UNDER text-012.
    FORMAT COLOR OFF INVERSE OFF.
  ELSE.
    WRITE:  big_table-wkgnnp DECIMALS 0 UNDER text-012.
  ENDIF.
  IF big_table-wljhr <  0.
    FORMAT COLOR COL_NEGATIVE INVERSE ON.
    WRITE:  big_table-wljhr DECIMALS 0 UNDER text-014.
    FORMAT COLOR OFF INVERSE OFF.
  ELSE.
    WRITE:  big_table-wljhr DECIMALS 0 UNDER text-014.
  ENDIF.
  IF big_table-wkgnna <  0.
    FORMAT COLOR COL_NEGATIVE INVERSE ON.
    WRITE: big_table-wkgnna DECIMALS 0 UNDER text-016.
    FORMAT COLOR OFF INVERSE OFF.
  ELSE.
    WRITE: big_table-wkgnna DECIMALS 0 UNDER text-016.
  ENDIF.
ENDFORM.                    "WRITE_AMOUNTS

*---------------------  GET_AMOUNTS ------------------------------------
FORM get_amounts.
* COST TOTALS - External Postings

  SELECT * FROM cosp
    WHERE objnr = prps_data-objnr
      AND gjahr = p_fyear                              "Fiscal Year
      AND ( versn = p_vers                             "Version
       OR   versn = '000' )
      AND wrttp IN ('01', '04')                        "Plan and Actuals
      AND kstar IN  s_kstar
      AND beknz IN ('S', 'H', 'L').               "Debit/Credit Indicator
    IF sy-subrc = '0'.
      ADD  cosp-wkg001 FROM 1 TO 12 GIVING amt.
      CASE cosp-wrttp.
        WHEN '01'.
          IF cosp-versn = p_vers.
            amt_plan = amt_plan + amt.
          ENDIF.
          CLEAR amt.
        WHEN '04'.
          IF cosp-versn = '000'.
            amt_actual = amt_actual + amt.
          ENDIF.
          CLEAR amt.
      ENDCASE.
    ENDIF.
  ENDSELECT.
*
* COST TOTALS - Internal Postings

  SELECT * FROM coss
    WHERE objnr = prps_data-objnr
      AND gjahr = p_fyear                              "Fiscal Year
      AND ( versn = p_vers                             "Version
       OR   versn = '000' )
      AND kstar IN  s_kstar
      AND wrttp IN ('01' , '04').                                 "Actuals/PLAN
    IF sy-subrc = '0'.
      ADD  coss-wkg001 FROM 1 TO 12 GIVING amt.
      CASE coss-wrttp.
        WHEN '04'.
          IF coss-versn = '000'.
            amt_actual = amt_actual + amt.
          ENDIF.
          CLEAR amt.
        WHEN '01'.
          IF coss-versn = p_vers.
            amt_plan = amt_plan + amt.
          ENDIF.
          CLEAR amt.
      ENDCASE.
    ENDIF.
  ENDSELECT.
  IF prps_data-stufe = '1'.
    PERFORM budget_amounts.
  ENDIF.
ENDFORM.                    "GET_AMOUNTS
*
*---------------------  BUDGET_AMOUNTS ---------------------------------
FORM budget_amounts.
  SELECT * FROM bpja                                  "Budget Info
       WHERE objnr = prps_data-objnr
         AND wrttp = '41'
         AND gjahr = p_fyear
         AND versn = '000'. "P_VERS.                          "Version
    IF sy-subrc = '0'.
      ADD  bpja-wljhr TO amt_budget.
    ENDIF.
  ENDSELECT.

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

*---------------------  BUILD_AUSP_MAJPROJ------------------------------
FORM build_ausp_majproj.

  REFRESH char_tab.
  SELECT * FROM ausp INTO TABLE char_tab
         WHERE   atinn = g_atinn_mp   AND
*                    ATWRT = 'Y'          AND "4.6C Change
                 mafid = 'O'          AND
                 klart = '014'.
  SORT char_tab BY atwrt.                  "4.6C Changes
  DELETE char_tab WHERE atwrt <> 'Y'.      "4.6C Changes
  SORT char_tab BY objek.
ENDFORM.                    "BUILD_AUSP_MAJPROJ

*---------------------  BUILD_AUSP_PCONTROL ----------------------------
FORM build_ausp_pcontrol.
  REFRESH char_tabpc.
  SELECT * FROM ausp INTO TABLE char_tabpc
         WHERE   atinn = g_atinn_pc   AND
                 mafid = 'O'          AND
                 klart = '014'.
  SORT char_tabpc BY objek.
ENDFORM.                    "BUILD_AUSP_PCONTROL

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine to get the value of the project control & major indicator
*-----------------------------------------------------------------------
FORM find_characteristic.
  CLEAR: proj_control.
  READ TABLE char_tabpc WITH KEY objek = prps_data-objnr
                               atinn = g_atinn_pc  BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE char_tabpc-atwrt+0(6) TO proj_control.
  ENDIF.

ENDFORM.                    "FIND_CHARACTERISTIC
*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
  IF head_print = 'Y'.
    WRITE: /1 text-rpt, sy-repid,  60 t001-butxt,             "Company
          103 text-dte, sy-datum, text-amp, sy-uzeit.
    WRITE: / text-clt UNDER text-rpt, sy-mandt UNDER sy-repid, sy-sysid,
           60 text-005,
             text-pge UNDER text-dte, sy-pagno UNDER sy-datum.
    WRITE: / text-ver UNDER text-rpt, p_vers,
           60 text-tl2, p_fyear.                           "Fiscal Year
    ULINE.
*   WRITE: 1  TEXT-003, 6 TEXT-009, 15 TEXT-010, 20 TEXT-006,
*          31 TEXT-007, 76 TEXT-011, 89 TEXT-012,
*          103 TEXT-014, 117 TEXT-016.               "Issue 1065
    WRITE: 1  text-003, 5 text-009, 9 text-010, 13 text-006,
           30 text-007, 74 text-011, 84 text-015, 95 text-012,
           109 text-014, 123 text-016.
    ULINE.
    IF p_file = 'X'.
      head_print = 'N'.
    ENDIF.
  ENDIF.
*------------------------- End of Report Header ------------------------
