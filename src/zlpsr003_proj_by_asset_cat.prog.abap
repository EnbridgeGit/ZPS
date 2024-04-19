*&---------------------------------------------------------------------*
*& Report  ZLPSR003_PROJ_BY_ASSET_CAT
*&
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Glenn Ymana                                            *
*  Date:        February 2017.                                         *
*  Issue Log:   ACR-3377                                               *
*  Description:                                                        *
*     - The purpose of this program is to produce the report:          *
*       Totals By Project and Asset Category.                          *
*                                                                      *
*                                                                      *
************************************************************************
*Changes:                                                              *
*Date     Track By       Description                                   *
*-------- ----- -------- ----------------------------------------------*
************************************************************************
*&---------------------------------------------------------------------*

REPORT  zlpsr003_proj_by_asset_cat.
TYPE-POOLS: slis.

TABLES:   proj,        "Project Definition
          prps,        "WBS (Work Breakdown Structure)
          cosp,        "CO Object:Cost total for External Postings
          coss,        "CO Object:Cost total for Internal Postings
          ausp,        "
          cawn,        "Characteristics values table
          cawnt.       "Characteristics values text table       "ACR3377

DATA: BEGIN OF itaball OCCURS 2,
      pspid     LIKE  proj-pspid,
      post1     LIKE  proj-post1,
      psphi     LIKE  prps-psphi,
      wrttp     LIKE  cosp-wrttp,
      wkg001    LIKE  cosp-wkg001,
      wkg002    LIKE  cosp-wkg001,
      wkg003    LIKE  cosp-wkg001,
      wkg004    LIKE  cosp-wkg001,
      wkg005    LIKE  cosp-wkg001,
      wkg006    LIKE  cosp-wkg001,
      wkg007    LIKE  cosp-wkg001,
      wkg008    LIKE  cosp-wkg001,
      wkg009    LIKE  cosp-wkg001,
      wkg010    LIKE  cosp-wkg001,
      wkg011    LIKE  cosp-wkg001,
      wkg012    LIKE  cosp-wkg001,
      END OF itaball.

DATA: BEGIN OF reptab OCCURS 1,
      asset_catg LIKE cawnt-atwtb,        "Asset Category       "ACR3377
      budg_catg LIKE  ausp-atwrt,         "Budject Category     "ACR3377
      pspid     LIKE  proj-pspid,         "Project Definition
      post1     LIKE  proj-post1,         "Project Description
      actual$   LIKE  coss-wkg001,        "Actual $
      plan$     LIKE  coss-wkg001,        "Plan $
      variance$ LIKE  coss-wkg001,        "Variance $
      ann_plan$ LIKE  coss-wkg001,        "Total Annual Plan $
      rem_plan$ LIKE  coss-wkg001,        "Remaining Plan $
      p_lkhood(6),                        "Plan Likelyhood  (L)
      p_conseq(6),                        "Plan Consequence (C)
      p_lbyc(6),                          "Plan L x C
      p_riskrank(6),                      "Plan Risk Rank
      a_lkhood(6),                        "Actual Likelyhood  (L)
      a_conseq(6),                        "Actual Consequence (C)
      a_lbyc(6),                          "Actual L x C
      a_riskrank(6),                      "Actual Risk Rank
      END OF reptab.

DATA: BEGIN OF char_tab OCCURS 0,
        objek LIKE ausp-objek,
        atinn LIKE ausp-atinn,
        atwrt LIKE ausp-atwrt,
      END OF char_tab.

DATA: BEGIN OF objectab OCCURS 0,
        objek LIKE ausp-objek,
        pcntrl LIKE ausp-atwrt,
        p_lkhood LIKE ausp-atwrt,
        p_conseq LIKE ausp-atwrt,
        a_lkhood LIKE ausp-atwrt,
        a_conseq LIKE ausp-atwrt,
      END OF objectab.

DATA: g_atinn          LIKE cabn-atinn,
      g_atinn_pc       LIKE cabn-atinn,
      g_atinn_plh      LIKE cabn-atinn,
      g_atinn_pco      LIKE cabn-atinn,
      g_atinn_alh      LIKE cabn-atinn,
      g_atinn_aco      LIKE cabn-atinn,
      g_atinn_all      LIKE cabn-atinn,
      charic           LIKE cabn-atnam,
      plan_ind         VALUE space,
      w_category       LIKE ausp-atwrt,
      w_asset_cat      LIKE cawnt-atwtb,
      w_psphi          LIKE prps-psphi,
      w_p_likelyhood   LIKE reptab-a_lkhood,
      w_a_likelyhood   LIKE reptab-a_lkhood,
      w_p_consequence  LIKE reptab-a_conseq,
      w_a_consequence  LIKE reptab-a_conseq,
      w_p_lbyc(6), "   LIKE REPTAB-LBYC,
      w_a_lbyc(6), "   LIKE REPTAB-LBYC,
      ww_lbyc          TYPE i,
      w_p_riskrank     LIKE reptab-a_riskrank,
      w_a_riskrank     LIKE reptab-a_riskrank.

DATA:
    w_head01(60)  TYPE c,
    w_head02(60)  TYPE c,
    w_head03(60)  TYPE c,
    w_head04(60)  TYPE c,
    w_headpl(37) TYPE c VALUE 'Abbreviations: "PL" -Plan Likelihood,',
    w_headpc(22) TYPE c VALUE '"PC" -Plan Consequence',
    w_headal(25) TYPE c VALUE '"AL" -Actual Likelihood, ',
    w_headac(25) TYPE c VALUE '"AC" -Actual Consequence',
    es_variant    LIKE disvariant,
    is_variant    LIKE disvariant.

RANGES: r_charic    FOR cabn-atnam.


***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK box1.
PARAMETERS:
  p_vbukr LIKE proj-vbukr DEFAULT 'UGL',                       "Company
  p_fyear LIKE cosp-gjahr DEFAULT sy-datum+0(4) OBLIGATORY,       "Year
  p_versn LIKE cosp-versn DEFAULT 000 OBLIGATORY.              "Version

SELECT-OPTIONS:
  s_categ  FOR cawnt-atwtb,                            "Asset Category
  s_period FOR sy-datum+4(2) DEFAULT '01',                      "Period
  s_vernr  FOR prps-vernr,                                    "Division
  s_prart  FOR prps-prart,                                "Project Type
  s_pspri  FOR prps-pspri,                                "Priority
  s_kstar  FOR coss-kstar,                                "Cost element
  s_pspid  FOR proj-pspid.                                     "Project
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: ex_kstar    FOR coss-kstar.
*                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN SKIP.
PARAMETERS pvariant LIKE disvariant-variant.           "Display Variant
SELECTION-SCREEN END OF BLOCK box.

***********************************************************************
*                  SELECT DISPLAY VARIANT                             *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvariant.
  is_variant-report = 'ZLPSR001_PROJ_BY_BUGT_CATEG_AA'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = is_variant
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     IT_DEFAULT_FIELDCAT =
      i_save              = 'A'
    IMPORTING
*     E_EXIT              =
      es_variant          = es_variant
    EXCEPTIONS
      not_found           = 1
      program_error       = 2
      OTHERS              = 3.
  pvariant = es_variant-variant.
***********************************************************************
*                     START OF SELECTION                              *
***********************************************************************
START-OF-SELECTION.
  PERFORM get_project_and_dollars_info.
  IF NOT itaball[] IS INITIAL.
    PERFORM setup_for_proj_cntrl_number.
    PERFORM build_report_table.
    PERFORM display_alv_grid_data.
  ENDIF.

***********************************************************************
*                 GET_PROJECT_AND_DOLLARS_INFO                        *
***********************************************************************
FORM get_project_and_dollars_info.

  SELECT proj~pspid  proj~post1  prps~psphi  cosp~wrttp
         cosp~wkg001 cosp~wkg002 cosp~wkg003 cosp~wkg004 cosp~wkg005
         cosp~wkg006 cosp~wkg007 cosp~wkg008 cosp~wkg009 cosp~wkg010
         cosp~wkg011 cosp~wkg012

    APPENDING TABLE itaball
    FROM ( ( proj
             INNER JOIN prps
             ON proj~pspnr = prps~psphi )
             INNER JOIN cosp
             ON prps~objnr = cosp~objnr )

   WHERE proj~pspid IN s_pspid
     AND proj~vernr IN s_vernr
     AND proj~vbukr = p_vbukr
     AND prps~prart IN s_prart
     AND prps~pspri IN s_pspri
     AND ( prps~plakz = 'X' OR prps~belkz = 'X' )
     AND prps~loevm <> 'X'
     AND cosp~gjahr = p_fyear
     AND cosp~wrttp  IN ('01','04')        "Record with actuals & plans
     AND ( cosp~versn =  p_versn             "Version
        OR cosp~versn   = '000' )
     AND cosp~kstar IN s_kstar
     AND cosp~kstar NOT IN ex_kstar        "Cost Element (not required)
     AND cosp~beknz  IN ('S','H','L').     "Debit/Credit Indicator

  SELECT proj~pspid  proj~post1  prps~psphi  coss~wrttp
         coss~wkg001 coss~wkg002 coss~wkg003 coss~wkg004 coss~wkg005
         coss~wkg006 coss~wkg007 coss~wkg008 coss~wkg009 coss~wkg010
         coss~wkg011 coss~wkg012

    APPENDING TABLE itaball
    FROM ( ( proj
             INNER JOIN prps
             ON proj~pspnr = prps~psphi )
             INNER JOIN coss
             ON prps~objnr = coss~objnr )

   WHERE proj~pspid IN s_pspid
     AND proj~vernr IN s_vernr
     AND proj~vbukr = p_vbukr
     AND prps~prart IN s_prart
     AND prps~pspri IN s_pspri
     AND ( prps~plakz = 'X' OR prps~belkz = 'X' )
     AND prps~loevm <> 'X'
     AND coss~gjahr = p_fyear
     AND coss~wrttp  IN ('01','04')        "Record with actuals & plans
     AND ( coss~versn =  p_versn           "Version
        OR coss~versn   = '000' )
     AND coss~kstar IN s_kstar
     AND coss~kstar NOT IN ex_kstar        "Cost Element (not required)
     AND coss~beknz  IN ('S','H','L').     "Debit/Credit Indicator

  SORT itaball BY pspid post1 psphi.
ENDFORM.                    "GET_PROJECT_AND_DOLLARS_INFO

***********************************************************************
*                           BUILD_REPORT_TABLE                        *
***********************************************************************
FORM build_report_table.

  LOOP AT itaball.

    MOVE: itaball-pspid TO reptab-pspid,
          itaball-post1 TO reptab-post1,
          itaball-psphi TO w_psphi.
    IF itaball-wrttp = '01'.
      ADD itaball-wkg001 FROM s_period-low TO s_period-high GIVING
                                                     reptab-plan$.
      ADD itaball-wkg001 FROM 1 TO 12 GIVING reptab-ann_plan$.
      reptab-rem_plan$ = reptab-ann_plan$ - reptab-plan$.
      reptab-variance$ = reptab-variance$ - reptab-plan$.
    ELSE.
      ADD itaball-wkg001 FROM s_period-low TO s_period-high GIVING
                                                    reptab-actual$.
      reptab-variance$ = reptab-variance$ + reptab-actual$.
    ENDIF.
    AT NEW pspid.
      CLEAR: w_category, w_asset_cat, w_p_likelyhood,         "ACR3377
             w_a_likelyhood, w_p_consequence, w_a_consequence,
             w_p_lbyc, w_a_lbyc, w_p_riskrank, w_a_riskrank.
      PERFORM get_budget_category.
      PERFORM get_asset_category.                             "ACR3377
    ENDAT.

    IF w_asset_cat IN s_categ OR s_categ = space.             "ACR3377
      MOVE w_asset_cat     TO reptab-asset_catg.              "ACR3377
      MOVE w_category      TO reptab-budg_catg.               "ACR3377
      MOVE w_p_likelyhood  TO reptab-p_lkhood.
      MOVE w_p_consequence TO reptab-p_conseq.
      MOVE w_p_lbyc        TO reptab-p_lbyc.
      MOVE w_p_riskrank    TO reptab-p_riskrank.
      MOVE w_a_likelyhood  TO reptab-a_lkhood.
      MOVE w_a_consequence TO reptab-a_conseq.
      MOVE w_a_lbyc        TO reptab-a_lbyc.
      MOVE w_a_riskrank    TO reptab-a_riskrank.
      COLLECT reptab.
    ENDIF.
    CLEAR   reptab.
  ENDLOOP.
  SORT reptab BY asset_catg pspid.

ENDFORM.                    "BUILD_REPORT_TABLE
***********************************************************************
**                 SETUP_FOR_PROJ_CNTRL_NUMBER.                       *
***********************************************************************
*
FORM setup_for_proj_cntrl_number.
  PERFORM get_charic_together.
  REFRESH char_tab.
  LOOP AT r_charic.
*#MOVE 'PROJ_CNTRL_NUMBER'  TO  CHARIC.    "Characteristics required
    MOVE r_charic-low TO charic.
    PERFORM get_atinn.
    MOVE g_atinn      TO   g_atinn_all.
    CASE charic.
      WHEN 'PROJ_CNTRL_NUMBER'.
        MOVE g_atinn   TO   g_atinn_pc.    "Project Control
*         WHEN 'TEST_TEST_2'.
*              MOVE G_ATINN   TO   G_ATINN_PLH.   "Plan Likely hood.
*         WHEN 'TEST_TEST_3'.
*              MOVE G_ATINN   TO   G_ATINN_PCO.   "Plan Consequence
*         WHEN 'TEST_TEST_4'.
*              MOVE G_ATINN   TO   G_ATINN_ALH.   "Actual Likely hood.
*         WHEN 'TEST_TEST_5'.
*              MOVE G_ATINN   TO   G_ATINN_ACO.   "Actual Consequence
*
      WHEN 'LIKELIHOOD_PLANNED'.
        MOVE g_atinn   TO   g_atinn_plh.   "Plan Likely hood.
      WHEN 'CONSEQUENCE_PLANNED'.
        MOVE g_atinn   TO   g_atinn_pco.   "Plan Consequence
      WHEN 'LIKELIHOOD_ACTUAL'.
        MOVE g_atinn   TO   g_atinn_alh.   "Actual Likely hood.
      WHEN 'CONSEQUENCE_ACTUAL'.
        MOVE g_atinn   TO   g_atinn_aco.   "Actual Consequence

      WHEN OTHERS.
    ENDCASE.

    PERFORM build_ausp USING g_atinn_all.
    CLEAR: charic, g_atinn, g_atinn_all.
  ENDLOOP.
  SORT char_tab BY objek atinn atwrt .
  PERFORM build_object_table.
ENDFORM.                    "SETUP_FOR_PROJ_CNTRL_NUMBER
*
***********************************************************************
*                          GET_ATINN                                  *
***********************************************************************
*This routine gets the internal character number for Budget Category

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
*
***********************************************************************
*                        BUILD_AUSP                                   *
***********************************************************************
FORM build_ausp USING atinn LIKE cabn-atinn.

  SELECT objek atinn atwrt FROM ausp
  APPENDING TABLE char_tab
    WHERE   atinn = g_atinn_all   AND
           mafid = 'O'           AND
           klart = '014'.
ENDFORM.                    "BUILD_AUSP
*
***********************************************************************
*                       BUILD_OBJECT_TABLE.                           *
***********************************************************************
FORM build_object_table.

  LOOP AT char_tab.

    MOVE char_tab-objek  TO  objectab-objek.
    CASE char_tab-atinn.
      WHEN g_atinn_pc.
        MOVE char_tab-atwrt  TO  objectab-pcntrl.
      WHEN g_atinn_plh.
        IF objectab-pcntrl = 'RISK BASED'.
          MOVE char_tab-atwrt TO objectab-p_lkhood.
        ENDIF.
      WHEN g_atinn_pco.
        IF objectab-pcntrl = 'RISK BASED'.
          MOVE char_tab-atwrt TO objectab-p_conseq.
        ENDIF.
      WHEN g_atinn_alh.
        IF objectab-pcntrl = 'RISK BASED'.
          MOVE char_tab-atwrt TO objectab-a_lkhood.
        ENDIF.
      WHEN g_atinn_aco.
        IF objectab-pcntrl = 'RISK BASED'.
          MOVE char_tab-atwrt TO objectab-a_conseq.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    AT END OF objek.
      APPEND objectab.
      CLEAR  objectab.
    ENDAT.
  ENDLOOP.
  SORT objectab BY objek pcntrl.

ENDFORM.                    "BUILD_OBJECT_TABLE
***********************************************************************
*                       GET_CHARIC_TOGETHER                           *
***********************************************************************
FORM get_charic_together.

  r_charic-low  = 'PROJ_CNTRL_NUMBER'.
  r_charic-high   = space.
  r_charic-option = 'EQ'.
  r_charic-sign   = 'I'.
  APPEND r_charic.
  r_charic-low  = 'LIKELIHOOD_PLANNED'.
  APPEND r_charic.
  r_charic-low  = 'CONSEQUENCE_PLANNED'.
  APPEND r_charic.
  r_charic-low  = 'LIKELIHOOD_ACTUAL'.
  APPEND r_charic.
  r_charic-low  = 'CONSEQUENCE_ACTUAL'.
  APPEND r_charic.
  CLEAR  r_charic.

ENDFORM.                    "GET_CHARIC_TOGETHER
***********************************************************************
*                     GET_BUDGET_CATEGORY                             *
***********************************************************************
*Routine to get the value of the project control/budget category
FORM get_budget_category.
  DATA: w_objnr LIKE prps-objnr.

  SELECT SINGLE objnr INTO w_objnr
    FROM prps
   WHERE psphi = w_psphi
     AND stufe = '1'.

  READ TABLE objectab WITH KEY objek  = w_objnr BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE objectab-pcntrl TO w_category.
    IF objectab-pcntrl = 'RISK BASED'.
      MOVE objectab-p_lkhood TO w_p_likelyhood.
      MOVE objectab-p_conseq TO w_p_consequence.
      MOVE objectab-a_lkhood TO w_a_likelyhood.
      MOVE objectab-a_conseq TO w_a_consequence.
      IF objectab-p_lkhood <> space AND objectab-p_conseq <> space.
        w_p_lbyc = objectab-p_lkhood * objectab-p_conseq.
        MOVE w_p_lbyc TO ww_lbyc.
        IF ww_lbyc BETWEEN 1 AND  3.
          MOVE 'IV' TO w_p_riskrank.
        ELSEIF ww_lbyc BETWEEN 4 AND  9.
          MOVE 'III' TO w_p_riskrank.
        ELSEIF ww_lbyc BETWEEN 10 AND  16.
          MOVE 'II' TO w_p_riskrank.
        ELSEIF ww_lbyc > 16.
          MOVE 'I' TO w_p_riskrank.
        ENDIF.
      ENDIF.
      IF objectab-a_lkhood <> space AND objectab-a_conseq <> space.
        w_a_lbyc = objectab-a_lkhood * objectab-a_conseq.
        MOVE w_a_lbyc TO ww_lbyc.
        IF ww_lbyc BETWEEN 1 AND  4.
          MOVE 'IV' TO w_a_riskrank.
        ELSEIF ww_lbyc BETWEEN 5 AND  9.
          MOVE 'III' TO w_a_riskrank.
        ELSEIF ww_lbyc BETWEEN 10 AND  16.
          MOVE 'II' TO w_a_riskrank.
        ELSEIF ww_lbyc > 16.
          MOVE 'I' TO w_a_riskrank.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "GET_BUDGET_CATEGORY
***********************************************************************
*                     GET_ASSET_CATEGORY                              *
***********************************************************************
*Routine to get the value of the project control/asset         "ACR3377
* category                                                     "ACR3377
FORM get_asset_category.
  DATA wa_ausp TYPE ausp.

  DATA: w_objnr LIKE prps-objnr,
        w_atinn LIKE ausp-atinn,
        w_atwtb LIKE cawnt-atwtb.

  SELECT SINGLE objnr INTO w_objnr
    FROM prps
   WHERE psphi = w_psphi
     AND stufe = '1'.

  READ TABLE objectab WITH KEY objek  = w_objnr BINARY SEARCH.
  IF sy-subrc EQ 0.

     MOVE '0000000581' TO w_atinn.
     CLEAR: wa_ausp, w_atwtb.

     SELECT * FROM ausp
        INTO CORRESPONDING FIELDS OF wa_ausp
      WHERE objek = w_objnr
        AND atinn = w_atinn.
     ENDSELECT.

     IF sy-subrc EQ 0.
        SELECT atwtb INTO w_atwtb
          FROM cawnt
         WHERE atinn = wa_ausp-atinn
           AND atzhl = wa_ausp-atwrt.
        ENDSELECT.
     ENDIF.

    MOVE w_atwtb TO w_asset_cat.
  ENDIF.

ENDFORM.                    "GET_ASSET_CATEGORY             ACR3377
*
***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
FORM display_alv_grid_data.

  DATA: fieldcat TYPE slis_t_fieldcat_alv,
        fc_str   TYPE slis_fieldcat_alv,
        layout   TYPE slis_layout_alv,
        title    TYPE lvc_title,
        repid    LIKE sy-repid,
        variant  LIKE disvariant,
        sort     TYPE slis_t_sortinfo_alv,
        sort_str TYPE slis_sortinfo_alv.

  CONCATENATE text-004 s_period-low text-005 s_period-high text-006
              text-007 p_fyear text-006 text-008 p_versn
              INTO w_head01 SEPARATED BY space.

  MOVE text-clt  TO w_head02+0(7).
  MOVE sy-sysid  TO w_head02+8(5).
  MOVE sy-mandt  TO w_head02+14(4).
  MOVE text-dte  TO w_head02+21(5).
  WRITE sy-datum TO w_head02+27(10).
  MOVE text-tme  TO w_head02+40(5).
  WRITE sy-uzeit TO w_head02+46(10).
  CONCATENATE w_headpl w_headpc INTO w_head03 SEPARATED BY space.
  CONCATENATE w_headal w_headac INTO w_head04 SEPARATED BY space.
  repid = sy-repid.
  layout-colwidth_optimize = 'X'.
*  LAYOUT-COUNTFNAME = 'PSPID'.
  layout-zebra = 'X'.
  variant-report = repid.
  variant-variant = pvariant.
  REFRESH fieldcat.
  CLEAR:  fieldcat, fc_str.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repid
      i_internal_tabname     = 'REPTAB'
      i_inclname             = repid
    CHANGING
      ct_fieldcat            = fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
* update field catalog (hide/reposition/etc)
  LOOP AT fieldcat INTO fc_str.

    CASE fc_str-fieldname.
      WHEN 'ASSET_CATG'.
        fc_str-seltext_l = text-c01.          "Alternative col header
        fc_str-ddictxt = 'L'.
      WHEN 'BUDG_CATG'.
        fc_str-seltext_l = text-c15.          "Alternative col header
        fc_str-ddictxt = 'L'.
      WHEN 'ACTUAL$'.
        fc_str-seltext_l = text-c02.          "Alternative col header
        fc_str-ddictxt = 'L'.
        fc_str-do_sum  = 'X'.                 "Do Sum
      WHEN 'PLAN$'.
        fc_str-seltext_l = text-c03.          "Alternative col header
        fc_str-ddictxt = 'L'.
        fc_str-do_sum  = 'X'.                 "Do Sum
      WHEN 'VARIANCE$'.
        fc_str-seltext_l = text-c04.          "Alternative col header
        fc_str-ddictxt = 'L'.
        fc_str-do_sum  = 'X'.                 "Do Sum
      WHEN 'ANN_PLAN$'.
        fc_str-seltext_l = text-c05.          "Alternative col header
        fc_str-ddictxt = 'L'.
        fc_str-do_sum  = 'X'.                 "Do Sum
      WHEN 'REM_PLAN$'.
        fc_str-seltext_l = text-c06.          "Alternative col header
        fc_str-ddictxt = 'L'.                 "Use Large system text
        fc_str-do_sum  = 'X'.                 "Do Sum
      WHEN 'P_LKHOOD'.
        fc_str-seltext_l = text-c07.          "Alternative col header
*          FC_STR-DDICTXT = 'L'.                 "Use Large system text
*          FC_STR-DO_SUM  = 'X'.                 "Do Sum
      WHEN 'P_CONSEQ'.
        fc_str-seltext_l = text-c08.          "Alternative col header
      WHEN 'P_LBYC'.
        fc_str-seltext_l = text-c09.          "Alternative col header
        fc_str-no_zero = 'X'.                 "No ZERO
      WHEN 'P_RISKRANK'.
        fc_str-seltext_l = text-c10.          "Alternative col header
      WHEN 'A_LKHOOD'.
        fc_str-seltext_l = text-c11.          "Alternative col header
*          FC_STR-DDICTXT = 'L'.                 "Use Large system text
*          FC_STR-DO_SUM  = 'X'.                 "Do Sum
      WHEN 'A_CONSEQ'.
        fc_str-seltext_l = text-c12.          "Alternative col header
      WHEN 'A_LBYC'.
        fc_str-seltext_l = text-c13.          "Alternative col header
        fc_str-no_zero = 'X'.                 "No ZERO
      WHEN 'A_RISKRANK'.
        fc_str-seltext_l = text-c14.          "Alternative col header
      WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
    ENDCASE.

    MODIFY fieldcat FROM fc_str.
  ENDLOOP.

* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat             = fieldcat
      is_layout               = layout
      i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
      i_callback_program      = repid
      i_save                  = 'A'
      is_variant              = variant
      it_sort                 = sort
*     I_GRID_TITLE            = TITLE
*     I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
      t_outtab                = reptab
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
ENDFORM.                    "DISPLAY_ALV_GRID_DATA

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.

*1- Heading Line: Type H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.             "sy-title.
  APPEND ls_line TO lt_top_of_page.

*2- Heading Line: Type H
  CLEAR ls_line.
  ls_line-typ   = 'S'.
  ls_line-key   = ''.
  ls_line-info = w_head01.
  APPEND ls_line TO lt_top_of_page.

*3- Action Line:  Type A
  CLEAR ls_line.
  ls_line-typ   = 'S'.
  ls_line-key   = ''.
  ls_line-info = w_head02.
  APPEND ls_line TO lt_top_of_page.

*4- Action Line:  Type A
  CLEAR ls_line.
  ls_line-typ   = 'A'.
  ls_line-key   = ''.
  ls_line-info = w_head03.
  APPEND ls_line TO lt_top_of_page.

*5- Action Line:  Type A
  CLEAR ls_line.
  ls_line-typ   = 'A'.
  ls_line-key   = ''.
  ls_line-info = w_head04.
  APPEND ls_line TO lt_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.
ENDFORM.                    "ALV_TOP_OF_PAGE
