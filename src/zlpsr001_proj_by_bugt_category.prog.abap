*&---------------------------------------------------------------------*
*& Report  ZLPSR001_PROJ_BY_BUGT_CATEGORY
*&
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        MAY 2011.                                              *
*  Issue Log:   TR932                                                  *
*  Description:                                                        *
*     - The purpose of this program is to produce the report:          *
*       Totals By Project and Budget Category.                         *
*                                                                      *
*                                                                      *
************************************************************************
*Changes:                                                              *
*Date     Track By       Description                                   *
*-------- ----- -------- ----------------------------------------------*
* 2012/04/18 - mkhan  - Qxxx - Added 8 new columns in the report and   *
*                       calculations for Plan & Actual Risk Rank.      *
*                                                                      *
************************************************************************
*&---------------------------------------------------------------------*

REPORT  ZLPSR001_PROJ_BY_BUGT_CATEGORY.
TYPE-POOLS: SLIS.

TABLES:   PROJ,             "Project Definition
          PRPS,             "WBS (Work Breakdown Structure)
          COSP,             "CO Object:Cost total for External Postings
          COSS.             "CO Object:Cost total for Internal Postings

DATA: BEGIN OF ITABALL OCCURS 2,
      PSPID     LIKE  PROJ-PSPID,
      POST1     LIKE  PROJ-POST1,
      PSPHI     LIKE  PRPS-PSPHI,
      WRTTP     LIKE  COSP-WRTTP,
      WKG001    LIKE  COSP-WKG001,
      WKG002    LIKE  COSP-WKG001,
      WKG003    LIKE  COSP-WKG001,
      WKG004    LIKE  COSP-WKG001,
      WKG005    LIKE  COSP-WKG001,
      WKG006    LIKE  COSP-WKG001,
      WKG007    LIKE  COSP-WKG001,
      WKG008    LIKE  COSP-WKG001,
      WKG009    LIKE  COSP-WKG001,
      WKG010    LIKE  COSP-WKG001,
      WKG011    LIKE  COSP-WKG001,
      WKG012    LIKE  COSP-WKG001,
      END OF ITABALL.

DATA: BEGIN OF REPTAB OCCURS 1,
      BUGT_CATG LIKE  AUSP-ATWRT,         "Budject Category
      PSPID     LIKE  PROJ-PSPID,         "Project Definition
      POST1     LIKE  PROJ-POST1,         "Project Description
      ACTUAL$   LIKE  COSS-WKG001,        "Actual $
      PLAN$     LIKE  COSS-WKG001,        "Plan $
      VARIANCE$ LIKE  COSS-WKG001,        "Variance $
      ANN_PLAN$ LIKE  COSS-WKG001,        "Total Annual Plan $
      REM_PLAN$ LIKE  COSS-WKG001,        "Remaining Plan $
      P_LKHOOD(6),                        "Plan Likelyhood  (L)
      P_CONSEQ(6),                        "Plan Consequence (C)
      P_LBYC(6),                          "Plan L x C
      P_RISKRANK(6),                      "Plan Risk Rank
      A_LKHOOD(6),                        "Actual Likelyhood  (L)
      A_CONSEQ(6),                        "Actual Consequence (C)
      A_LBYC(6),                          "Actual L x C
      A_RISKRANK(6),                      "Actual Risk Rank
      END OF REPTAB.

DATA: BEGIN OF CHAR_TAB OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATINN LIKE AUSP-ATINN,
        ATWRT LIKE AUSP-ATWRT,
      END OF CHAR_TAB.

DATA: BEGIN OF OBJECTAB OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        PCNTRL LIKE AUSP-ATWRT,
        P_LKHOOD LIKE AUSP-ATWRT,
        P_CONSEQ LIKE AUSP-ATWRT,
        A_LKHOOD LIKE AUSP-ATWRT,
        A_CONSEQ LIKE AUSP-ATWRT,
      END OF OBJECTAB.

DATA: G_ATINN          LIKE CABN-ATINN,
      G_ATINN_PC       LIKE CABN-ATINN,
      G_ATINN_PLH      LIKE CABN-ATINN,
      G_ATINN_PCO      LIKE CABN-ATINN,
      G_ATINN_ALH      LIKE CABN-ATINN,
      G_ATINN_ACO      LIKE CABN-ATINN,
      G_ATINN_ALL      LIKE CABN-ATINN,
      CHARIC           LIKE CABN-ATNAM,
      PLAN_IND         VALUE SPACE,
      W_CATEGORY       LIKE AUSP-ATWRT,
      W_PSPHI          LIKE PRPS-PSPHI,
      W_P_LIKELYHOOD   LIKE REPTAB-A_LKHOOD,
      W_A_LIKELYHOOD   LIKE REPTAB-A_LKHOOD,
      W_P_CONSEQUENCE  LIKE REPTAB-A_CONSEQ,
      W_A_CONSEQUENCE  LIKE REPTAB-A_CONSEQ,
      W_P_LBYC(6), "   LIKE REPTAB-LBYC,
      W_A_LBYC(6), "   LIKE REPTAB-LBYC,
      WW_LBYC          TYPE I,
      W_P_RISKRANK     LIKE REPTAB-A_RISKRANK,
      W_A_RISKRANK     LIKE REPTAB-A_RISKRANK.

DATA:
    W_HEAD01(60)  TYPE C,
    W_HEAD02(60)  TYPE C,
    W_HEAD03(60)  TYPE C,
    W_HEAD04(60)  TYPE C,
    W_HEADPL(37) TYPE C VALUE 'Abbreviations: "PL" -Plan Likelihood,',
    W_HEADPC(22) TYPE C VALUE '"PC" -Plan Consequence',
    W_HEADAL(25) TYPE C VALUE '"AL" -Actual Likelihood, ',
    W_HEADAC(25) TYPE C VALUE '"AC" -Actual Consequence',
    ES_VARIANT    LIKE DISVARIANT,
    IS_VARIANT    LIKE DISVARIANT.

RANGES: R_CHARIC    FOR CABN-ATNAM.


***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1.
PARAMETERS:
  P_VBUKR LIKE PROJ-VBUKR DEFAULT 'UGL',                       "Company
  P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY,       "Year
  P_VERSN LIKE COSP-VERSN DEFAULT 000 OBLIGATORY,              "Version
  P_CATEG(25) TYPE C.                                  "Budget Category

SELECT-OPTIONS:
  S_PERIOD FOR SY-DATUM+4(2) DEFAULT '01',                      "Period
  S_VERNR  FOR PRPS-VERNR,                                    "Division
  S_PRART  FOR PRPS-PRART,                                "Project Type
  S_PSPRI  FOR PRPS-PSPRI,                                "Priority
  S_KSTAR  FOR COSS-KSTAR,                                "Cost element
  S_PSPID  FOR PROJ-PSPID.                                     "Project
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: EX_KSTAR    FOR COSS-KSTAR
                            DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN SKIP.
PARAMETERS PVARIANT LIKE DISVARIANT-VARIANT.           "Display Variant
SELECTION-SCREEN END OF BLOCK BOX.

***********************************************************************
*                  SELECT DISPLAY VARIANT                             *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZLPSR001_PROJ_BY_BUGT_CATEG_AA'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = IS_VARIANT
*           I_TABNAME_HEADER    =
*           I_TABNAME_ITEM      =
*           IT_DEFAULT_FIELDCAT =
            I_SAVE              = 'A'
        IMPORTING
*           E_EXIT              =
            ES_VARIANT          = ES_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1
            PROGRAM_ERROR       = 2
            OTHERS              = 3.
  PVARIANT = ES_VARIANT-VARIANT.
***********************************************************************
*                     START OF SELECTION                              *
***********************************************************************
START-OF-SELECTION.
  PERFORM GET_PROJECT_AND_DOLLARS_INFO.
  IF NOT ITABALL[] IS INITIAL.
     PERFORM SETUP_FOR_PROJ_CNTRL_NUMBER.
     PERFORM BUILD_REPORT_TABLE.
     PERFORM DISPLAY_ALV_GRID_DATA.
  ENDIF.

***********************************************************************
*                 GET_PROJECT_AND_DOLLARS_INFO                        *
***********************************************************************
FORM GET_PROJECT_AND_DOLLARS_INFO.

SELECT PROJ~PSPID  PROJ~POST1  PRPS~PSPHI  COSP~WRTTP
       COSP~WKG001 COSP~WKG002 COSP~WKG003 COSP~WKG004 COSP~WKG005
       COSP~WKG006 COSP~WKG007 COSP~WKG008 COSP~WKG009 COSP~WKG010
       COSP~WKG011 COSP~WKG012

  APPENDING TABLE ITABALL
  FROM ( ( PROJ
           INNER JOIN PRPS
           ON PROJ~PSPNR = PRPS~PSPHI )
           INNER JOIN COSP
           ON PRPS~OBJNR = COSP~OBJNR )

 WHERE PROJ~PSPID IN S_PSPID
   AND PROJ~VERNR IN S_VERNR
   AND PROJ~VBUKR = P_VBUKR
   AND PRPS~PRART IN S_PRART
   AND PRPS~PSPRI IN S_PSPRI
   AND ( PRPS~PLAKZ = 'X' OR PRPS~BELKZ = 'X' )
   AND PRPS~LOEVM <> 'X'
   AND COSP~GJAHR = P_FYEAR
   AND COSP~WRTTP  IN ('01','04')          "Record with actuals & plans
   AND ( COSP~VERSN =  P_VERSN             "Version
      OR COSP~VERSN   = '000' )
   AND COSP~KSTAR IN S_KSTAR
   AND COSP~KSTAR NOT IN EX_KSTAR          "Cost Element (not required)
   AND COSP~BEKNZ  IN ('S','H','L').       "Debit/Credit Indicator

SELECT PROJ~PSPID  PROJ~POST1  PRPS~PSPHI  COSS~WRTTP
       COSS~WKG001 COSS~WKG002 COSS~WKG003 COSS~WKG004 COSS~WKG005
       COSS~WKG006 COSS~WKG007 COSS~WKG008 COSS~WKG009 COSS~WKG010
       COSS~WKG011 COSS~WKG012

  APPENDING TABLE ITABALL
  FROM ( ( PROJ
           INNER JOIN PRPS
           ON PROJ~PSPNR = PRPS~PSPHI )
           INNER JOIN COSS
           ON PRPS~OBJNR = COSS~OBJNR )

 WHERE PROJ~PSPID IN S_PSPID
   AND PROJ~VERNR IN S_VERNR
   AND PROJ~VBUKR = P_VBUKR
   AND PRPS~PRART IN S_PRART
   AND PRPS~PSPRI IN S_PSPRI
   AND ( PRPS~PLAKZ = 'X' OR PRPS~BELKZ = 'X' )
   AND PRPS~LOEVM <> 'X'
   AND COSS~GJAHR = P_FYEAR
   AND COSS~WRTTP  IN ('01','04')          "Record with actuals & plans
   AND ( COSS~VERSN =  P_VERSN              "Version
      OR COSS~VERSN   = '000' )
   AND COSS~KSTAR IN S_KSTAR
   AND COSS~KSTAR NOT IN EX_KSTAR          "Cost Element (not required)
   AND COSS~BEKNZ  IN ('S','H','L').        "Debit/Credit Indicator

   SORT ITABALL BY PSPID POST1 PSPHI.
ENDFORM.

***********************************************************************
*                           BUILD_REPORT_TABLE                        *
***********************************************************************
FORM BUILD_REPORT_TABLE.

LOOP AT ITABALL.

     MOVE: ITABALL-PSPID TO REPTAB-PSPID,
           ITABALL-POST1 TO REPTAB-POST1,
           ITABALL-PSPHI TO W_PSPHI.
     IF ITABALL-WRTTP = '01'.
        ADD ITABALL-WKG001 FROM S_PERIOD-LOW TO S_PERIOD-HIGH GIVING
                                                       REPTAB-PLAN$.
        ADD ITABALL-WKG001 FROM 1 TO 12 GIVING REPTAB-ANN_PLAN$.
        REPTAB-REM_PLAN$ = REPTAB-ANN_PLAN$ - REPTAB-PLAN$.
        REPTAB-VARIANCE$ = REPTAB-VARIANCE$ - REPTAB-PLAN$.
     ELSE.
        ADD ITABALL-WKG001 FROM S_PERIOD-LOW TO S_PERIOD-HIGH GIVING
                                                      REPTAB-ACTUAL$.
        REPTAB-VARIANCE$ = REPTAB-VARIANCE$ + REPTAB-ACTUAL$.
     ENDIF.
     AT NEW PSPID.
        CLEAR: W_CATEGORY, W_P_LIKELYHOOD, W_A_LIKELYHOOD,
               W_P_CONSEQUENCE, W_A_CONSEQUENCE,
               W_P_LBYC, W_A_LBYC, W_P_RISKRANK, W_A_RISKRANK.
        PERFORM GET_BUDGET_CATEGORY.
     ENDAT.

     IF P_CATEG = W_CATEGORY  OR P_CATEG = SPACE.
       MOVE W_CATEGORY    TO REPTAB-BUGT_CATG.
       MOVE W_P_LIKELYHOOD  TO REPTAB-P_LKHOOD.
       MOVE W_P_CONSEQUENCE TO REPTAB-P_CONSEQ.
       MOVE W_P_LBYC        TO REPTAB-P_LBYC.
       MOVE W_P_RISKRANK    TO REPTAB-P_RISKRANK.
       MOVE W_A_LIKELYHOOD  TO REPTAB-A_LKHOOD.
       MOVE W_A_CONSEQUENCE TO REPTAB-A_CONSEQ.
       MOVE W_A_LBYC        TO REPTAB-A_LBYC.
       MOVE W_A_RISKRANK    TO REPTAB-A_RISKRANK.
       COLLECT REPTAB.
     ENDIF.
     CLEAR   REPTAB.
ENDLOOP.
     SORT REPTAB BY BUGT_CATG PSPID.

ENDFORM.
***********************************************************************
**                 SETUP_FOR_PROJ_CNTRL_NUMBER.                       *
***********************************************************************
*
FORM SETUP_FOR_PROJ_CNTRL_NUMBER.
  PERFORM GET_CHARIC_TOGETHER.
  REFRESH CHAR_TAB.
  LOOP AT R_CHARIC.
*#MOVE 'PROJ_CNTRL_NUMBER'  TO  CHARIC.    "Characteristics required
       MOVE R_CHARIC-LOW TO CHARIC.
       PERFORM GET_ATINN.
       MOVE G_ATINN      TO   G_ATINN_ALL.
       CASE CHARIC.
         WHEN 'PROJ_CNTRL_NUMBER'.
              MOVE G_ATINN   TO   G_ATINN_PC.    "Project Control
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
              MOVE G_ATINN   TO   G_ATINN_PLH.   "Plan Likely hood.
         WHEN 'CONSEQUENCE_PLANNED'.
              MOVE G_ATINN   TO   G_ATINN_PCO.   "Plan Consequence
         WHEN 'LIKELIHOOD_ACTUAL'.
              MOVE G_ATINN   TO   G_ATINN_ALH.   "Actual Likely hood.
         WHEN 'CONSEQUENCE_ACTUAL'.
              MOVE G_ATINN   TO   G_ATINN_ACO.   "Actual Consequence

         WHEN OTHERS.
       ENDCASE.

       PERFORM BUILD_AUSP USING G_ATINN_ALL.
       CLEAR: CHARIC, G_ATINN, G_ATINN_ALL.
  ENDLOOP.
       SORT CHAR_TAB BY OBJEK ATINN ATWRT .
       PERFORM BUILD_OBJECT_TABLE.
ENDFORM.
*
***********************************************************************
*                          GET_ATINN                                  *
***********************************************************************
*This routine gets the internal character number for Budget Category

FORM GET_ATINN.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
            CLASS_TYPE                  = '014'
            FEATURE_NEUTRAL_NAME        = CHARIC
       IMPORTING
            FEATURE_ID                  = G_ATINN
       EXCEPTIONS
            INVALID_CLASS_TYPE          = 1
            MISSING_FEATURE_INFORMATION = 2
            NO_FEATURE_FOUND            = 3
            NO_FEATURE_VALID            = 4
            NO_LANGUAGE                 = 5
            OTHERS                      = 6.
  IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTICS OF', CHARIC.
    WRITE: /.
  ENDIF.
ENDFORM.
*
***********************************************************************
*                        BUILD_AUSP                                   *
***********************************************************************
FORM BUILD_AUSP USING ATINN LIKE CABN-ATINN.

     SELECT OBJEK ATINN ATWRT FROM AUSP
     APPENDING TABLE CHAR_TAB
       WHERE   ATINN = G_ATINN_ALL   AND
              MAFID = 'O'           AND
              KLART = '014'.
ENDFORM.
*
***********************************************************************
*                       BUILD_OBJECT_TABLE.                           *
***********************************************************************
FORM BUILD_OBJECT_TABLE.

   LOOP AT CHAR_TAB.

        MOVE CHAR_TAB-OBJEK  TO  OBJECTAB-OBJEK.
        CASE CHAR_TAB-ATINN.
          WHEN G_ATINN_PC.
               MOVE CHAR_TAB-ATWRT  TO  OBJECTAB-PCNTRL.
          WHEN G_ATINN_PLH.
               IF OBJECTAB-PCNTRL = 'RISK BASED'.
                  MOVE CHAR_TAB-ATWRT TO OBJECTAB-P_LKHOOD.
               ENDIF.
          WHEN G_ATINN_PCO.
               IF OBJECTAB-PCNTRL = 'RISK BASED'.
                  MOVE CHAR_TAB-ATWRT TO OBJECTAB-P_CONSEQ.
               ENDIF.
          WHEN G_ATINN_ALH.
               IF OBJECTAB-PCNTRL = 'RISK BASED'.
                  MOVE CHAR_TAB-ATWRT TO OBJECTAB-A_LKHOOD.
               ENDIF.
          WHEN G_ATINN_ACO.
               IF OBJECTAB-PCNTRL = 'RISK BASED'.
                  MOVE CHAR_TAB-ATWRT TO OBJECTAB-A_CONSEQ.
               ENDIF.
          WHEN OTHERS.
        ENDCASE.

     AT END OF OBJEK.
        APPEND OBJECTAB.
        CLEAR  OBJECTAB.
     ENDAT.
   ENDLOOP.
   SORT OBJECTAB BY OBJEK PCNTRL.

ENDFORM.
***********************************************************************
*                       GET_CHARIC_TOGETHER                           *
***********************************************************************
FORM GET_CHARIC_TOGETHER.

    R_CHARIC-LOW  = 'PROJ_CNTRL_NUMBER'.
    R_CHARIC-HIGH   = SPACE.
    R_CHARIC-OPTION = 'EQ'.
    R_CHARIC-SIGN   = 'I'.
    APPEND R_CHARIC.
    R_CHARIC-LOW  = 'LIKELIHOOD_PLANNED'.
    APPEND R_CHARIC.
    R_CHARIC-LOW  = 'CONSEQUENCE_PLANNED'.
    APPEND R_CHARIC.
    R_CHARIC-LOW  = 'LIKELIHOOD_ACTUAL'.
    APPEND R_CHARIC.
    R_CHARIC-LOW  = 'CONSEQUENCE_ACTUAL'.
    APPEND R_CHARIC.
    CLEAR  R_CHARIC.

ENDFORM.
***********************************************************************
*                     GET_BUDGET_CATEGORY                             *
***********************************************************************
*Routine to get the value of the project control/budget category
FORM GET_BUDGET_CATEGORY.
DATA: W_OBJNR LIKE PRPS-OBJNR.

  SELECT SINGLE OBJNR INTO W_OBJNR
    FROM PRPS
   WHERE PSPHI = W_PSPHI
     AND STUFE = '1'.

 READ TABLE OBJECTAB WITH KEY OBJEK  = W_OBJNR BINARY SEARCH.
  IF SY-SUBRC EQ 0.
      MOVE OBJECTAB-PCNTRL TO W_CATEGORY.
    IF OBJECTAB-PCNTRL = 'RISK BASED'.
       MOVE OBJECTAB-P_LKHOOD TO W_P_LIKELYHOOD.
       MOVE OBJECTAB-P_CONSEQ TO W_P_CONSEQUENCE.
       MOVE OBJECTAB-A_LKHOOD TO W_A_LIKELYHOOD.
       MOVE OBJECTAB-A_CONSEQ TO W_A_CONSEQUENCE.
       IF OBJECTAB-P_LKHOOD <> SPACE AND OBJECTAB-P_CONSEQ <> SPACE.
          W_P_LBYC = OBJECTAB-P_LKHOOD * OBJECTAB-P_CONSEQ.
          MOVE W_P_LBYC TO WW_LBYC.
          IF WW_LBYC BETWEEN 1 AND  3.
             MOVE 'IV' TO W_P_RISKRANK.
          ELSEIF WW_LBYC BETWEEN 4 AND  9.
             MOVE 'III' TO W_P_RISKRANK.
          ELSEIF WW_LBYC BETWEEN 10 AND  16.
             MOVE 'II' TO W_P_RISKRANK.
          ELSEIF WW_LBYC > 16.
             MOVE 'I' TO W_P_RISKRANK.
          ENDIF.
       ENDIF.
       IF OBJECTAB-A_LKHOOD <> SPACE AND OBJECTAB-A_CONSEQ <> SPACE.
          W_A_LBYC = OBJECTAB-A_LKHOOD * OBJECTAB-A_CONSEQ.
          MOVE W_A_LBYC TO WW_LBYC.
          IF WW_LBYC BETWEEN 1 AND  4.
             MOVE 'IV' TO W_A_RISKRANK.
          ELSEIF WW_LBYC BETWEEN 5 AND  9.
             MOVE 'III' TO W_A_RISKRANK.
          ELSEIF WW_LBYC BETWEEN 10 AND  16.
             MOVE 'II' TO W_A_RISKRANK.
          ELSEIF WW_LBYC > 16.
             MOVE 'I' TO W_A_RISKRANK.
          ENDIF.
       ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*
***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

CONCATENATE TEXT-004 S_PERIOD-LOW TEXT-005 S_PERIOD-HIGH TEXT-006
            TEXT-007 P_FYEAR TEXT-006 TEXT-008 P_VERSN
            INTO W_HEAD01 SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  CONCATENATE W_HEADPL W_HEADPC INTO W_HEAD03 SEPARATED BY SPACE.
  CONCATENATE W_HEADAL W_HEADAC INTO W_HEAD04 SEPARATED BY SPACE.
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  LAYOUT-COUNTFNAME = 'PSPID'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.
  CLEAR:  FIELDCAT, FC_STR.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'REPTAB'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.
* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'BUGT_CATG'.
          FC_STR-SELTEXT_L = TEXT-C01.          "Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'ACTUAL$'.
          FC_STR-SELTEXT_L = TEXT-C02.          "Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 "Do Sum
     WHEN 'PLAN$'.
          FC_STR-SELTEXT_L = TEXT-C03.          "Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 "Do Sum
     WHEN 'VARIANCE$'.
          FC_STR-SELTEXT_L = TEXT-C04.          "Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 "Do Sum
     WHEN 'ANN_PLAN$'.
          FC_STR-SELTEXT_L = TEXT-C05.          "Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 "Do Sum
     WHEN 'REM_PLAN$'.
          FC_STR-SELTEXT_L = TEXT-C06.          "Alternative col header
          FC_STR-DDICTXT = 'L'.                 "Use Large system text
          FC_STR-DO_SUM  = 'X'.                 "Do Sum
     WHEN 'P_LKHOOD'.
          FC_STR-SELTEXT_L = TEXT-C07.          "Alternative col header
*          FC_STR-DDICTXT = 'L'.                 "Use Large system text
*          FC_STR-DO_SUM  = 'X'.                 "Do Sum
     WHEN 'P_CONSEQ'.
          FC_STR-SELTEXT_L = TEXT-C08.          "Alternative col header
     WHEN 'P_LBYC'.
          FC_STR-SELTEXT_L = TEXT-C09.          "Alternative col header
          FC_STR-NO_ZERO = 'X'.                 "No ZERO
     WHEN 'P_RISKRANK'.
          FC_STR-SELTEXT_L = TEXT-C10.          "Alternative col header
     WHEN 'A_LKHOOD'.
          FC_STR-SELTEXT_L = TEXT-C11.          "Alternative col header
*          FC_STR-DDICTXT = 'L'.                 "Use Large system text
*          FC_STR-DO_SUM  = 'X'.                 "Do Sum
     WHEN 'A_CONSEQ'.
          FC_STR-SELTEXT_L = TEXT-C12.          "Alternative col header
     WHEN 'A_LBYC'.
          FC_STR-SELTEXT_L = TEXT-C13.          "Alternative col header
          FC_STR-NO_ZERO = 'X'.                 "No ZERO
     WHEN 'A_RISKRANK'.
          FC_STR-SELTEXT_L = TEXT-C14.          "Alternative col header
     WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = REPTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.
ENDFORM.

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- Action Line:  Type A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*4- Action Line:  Type A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD03.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*5- Action Line:  Type A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD04.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
        IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
