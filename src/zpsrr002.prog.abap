REPORT ZPSRR002. "NO STANDARD PAGE HEADING LINE-COUNT 64 LINE-SIZE 255.
TYPE-POOLS: SLIS.

********************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                          *
* Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)      *
*       Date: March 13, 1997                                       *
* Request ID:                                                      *
*                                                                  *
* The following program will generate a Cash Flow Report sorted    *
* by division within project type.  This program will break on     *
* the project types and will calculate all wbs elements total      *
* related to their project number.  Monthly totals will be         *
* displayed a long with a total plan at the end.                   *
************************************************************************
* 10/04/19 mokhan #813 Costing sheet (Secondary cost element) changes. *
* 07/05/25 TR#146  M Khan    Programs changes for ALV Display          *
*                                                                      *
*----------------------------------------------------------------------*
* 99/03/26 mdemeest EMAIL Fix TOTALLING - Sometimes doesn't work       *
*                                                                      *
************************************************************************
TABLES: COSP,  "CO Object:  Cost Totals - External Postings
        COSS,  "Secondary cost                             "TR813
        PROJ,  "Project Definition
        PRPS,  "WBS (Work Breakdown Structure) Element Master Data
        TCJ1T. "Project Types

*                                              "TR813 Start change
*DATA:  PLAN     LIKE COSP-KSTAR VALUE '0000480199',  "Plan Cost Element
*       IDC      LIKE COSP-KSTAR VALUE '0000324102',  "IDC  Cost Element
*       PROCEEDS LIKE COSP-KSTAR VALUE '0000430025',  "Proc Cost Element
*                                              "TR813 End change
DATA:   FIRST(2) TYPE N VALUE '01',                   "Month periods
        LAST(2)  TYPE N VALUE '12',                   "Month Periods
        PL_AMT   LIKE COSP-WKG001.                    "Calculation field

* Table for control-break processing.
DATA:  BEGIN OF MAINTAB OCCURS 10000,
         PRATX  LIKE TCJ1T-PRATX,                     "Type - Descriptio
         DIV(2) TYPE C,                               "Division break
         WBS    LIKE PRPS-POSID,                      "WBS element
         POST1  LIKE PROJ-POST1,                      "Proj Description
* Monthly values set a side for Planning cost elements
         PLAN01(6) TYPE P,                           "Jan
         PLAN02(6) TYPE P,                           "Feb
         PLAN03(6) TYPE P,                           "Mar
         PLAN04(6) TYPE P,                           "Apr
         PLAN05(6) TYPE P,                           "May
         PLAN06(6) TYPE P,                           "Jun
         PLAN07(6) TYPE P,                           "Jul
         PLAN08(6) TYPE P,                           "Aug
         PLAN09(6) TYPE P,                           "Sep
         PLAN10(6) TYPE P,                           "Oct
         PLAN11(6) TYPE P,                           "Nov
         PLAN12(6) TYPE P,                           "Dec
         PLNTOT    TYPE P,                           "TOTAL PLAN

   END OF MAINTAB.

DATA:   W_HEAD01(50)  TYPE C,
        W_HEAD02(50)  TYPE C,
        W_HEAD03(60)  TYPE C,
        ES_VARIANT    LIKE DISVARIANT,
        IS_VARIANT    LIKE DISVARIANT.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-101.
RANGES: TMPVAR FOR COSP-KSTAR.               "Range used for checkboxes
PARAMETERS: P_VBUKR LIKE PROJ-VBUKR MODIF ID ABC OBLIGATORY     "Company
                                    DEFAULT 'UGL'.
SELECT-OPTIONS:
            S_VERNR FOR PROJ-VERNR MODIF ID ABC,              "Division
            S_PROJ FOR PRPS-POSID MODIF ID ABC,               "Projects
            S_TYPE FOR PRPS-PRART MODIF ID ABC                "Type
              DEFAULT '01'.
PARAMETERS: P_VERSN LIKE TKA09-VERSN OBLIGATORY DEFAULT '000' "Version
                    MODIF ID ABC,
            P_YEAR  LIKE COSP-GJAHR DEFAULT SY-DATUM(4)       "Year
                    OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK BOX2.

*START of TR813 changes
*SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-106.
*SELECTION-SCREEN SKIP.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT: 1(10) TEXT-027 MODIF ID ABC.
*PARAMETERS: P_PLAN AS CHECKBOX DEFAULT 'X' MODIF ID ABC. "Plan checkbox
*SELECTION-SCREEN COMMENT: 30(7) TEXT-028 MODIF ID ABC.
*PARAMETERS: P_IDC  AS CHECKBOX DEFAULT 'X' MODIF ID ABC. "IDC checkbox
*SELECTION-SCREEN COMMENT: 55(10) TEXT-029 MODIF ID ABC.
*PARAMETERS: P_PROC AS CHECKBOX DEFAULT 'X' MODIF ID ABC. "Proceeds cbox
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-106.
SELECT-OPTIONS:
 PL_KSTAR FOR COSP-KSTAR DEFAULT '0000480199',                "Planing
 CS_KSTAR FOR COSP-KSTAR DEFAULT '0000830000' TO '0000830099', "Csheet
 ID_KSTAR FOR COSP-KSTAR DEFAULT '0000324102',                 "I.D.C
 PR_KSTAR FOR COSP-KSTAR DEFAULT '0000430025'.              "Proceeds
SELECTION-SCREEN END OF BLOCK BOX4.
*END of TR813 changes
SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME TITLE TEXT-004.
PARAMETERS:      PVARIANT LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BOX5.

SELECTION-SCREEN END OF BLOCK BOX1.

************************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZPSRR002'.
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

START-OF-SELECTION.
 PERFORM INITIALIZE.
 PERFORM GET_PROJ.
 PERFORM DISPLAY_ALV_GRID_DATA.
END-OF-SELECTION.

* This routine initializes the internal tables.
FORM INITIALIZE.
 REFRESH: MAINTAB, TMPVAR.
 CLEAR:   MAINTAB, TMPVAR.

*START of TR813 changes
* TMPVAR-SIGN = 'I'.                      "Assigment of checkboxes
* TMPVAR-OPTION = 'EQ'.
* IF P_PLAN = 'X'.
*    TMPVAR-LOW = PLAN.
*    APPEND TMPVAR.
* ENDIF.
* IF P_IDC = 'X'.
*    TMPVAR-LOW = IDC.
*    APPEND TMPVAR.
* ENDIF.
* IF P_PROC = 'X'.
*    TMPVAR-LOW = PROCEEDS.
*    APPEND TMPVAR.
* ENDIF.

 IF NOT PL_KSTAR[] IS INITIAL.
    TMPVAR[] = PL_KSTAR[].
 ENDIF.

 IF NOT CS_KSTAR[] IS INITIAL.
    LOOP AT CS_KSTAR INTO TMPVAR.
         APPEND TMPVAR.
    ENDLOOP.
 ENDIF.

 IF NOT ID_KSTAR[] IS INITIAL.
    LOOP AT ID_KSTAR INTO TMPVAR.
         APPEND TMPVAR.
    ENDLOOP.
 ENDIF.

 IF NOT PR_KSTAR[] IS INITIAL.
    LOOP AT PR_KSTAR INTO TMPVAR.
         APPEND TMPVAR.
    ENDLOOP.
 ENDIF.
*END of TR813 changes

ENDFORM.
*--------------------- GET_PROJ ----------------------------------------
*This routine gathers the project description.
*-----------------------------------------------------------------------
* GET_PROJ and GET_DATA were combined and rewritten when company code
* was added to selection variant.                               98/03/25
*-----------------------------------------------------------------------
FORM GET_PROJ.
 SELECT * FROM PROJ
    WHERE PSPID IN S_PROJ
      AND VBUKR = P_VBUKR
      AND VERNR IN S_VERNR.
  IF PROJ-PSPID+5(2) CO '0123456789'.              "Get rid of templates
    SELECT * FROM PRPS
       WHERE PSPHI = PROJ-PSPNR
         AND PRART IN S_TYPE.
       PERFORM GET_DATA.
    ENDSELECT.
   ENDIF.                                          "Template end
  ENDSELECT.
ENDFORM.

* This routine gathers the cost element, values, version and projects.
FORM GET_DATA.
  SELECT * FROM COSP WHERE OBJNR = PRPS-OBJNR
     AND GJAHR = P_YEAR AND VERSN = P_VERSN AND WRTTP = '01'
     AND KSTAR IN TMPVAR.
     SELECT SINGLE * FROM TCJ1T WHERE PRART = PRPS-PRART
         AND LANGU = 'E'.
*     PERFORM POP_PLAN.                                     "TR813
     PERFORM POP_PLAN_COSP.                                 "TR813
   ENDSELECT.

*START of TR813 changes
  SELECT * FROM COSS WHERE OBJNR = PRPS-OBJNR
     AND GJAHR = P_YEAR AND VERSN = P_VERSN AND WRTTP = '01'
     AND KSTAR IN TMPVAR.
     SELECT SINGLE * FROM TCJ1T WHERE PRART = PRPS-PRART
         AND LANGU = 'E'.
     PERFORM POP_PLAN_COSS.
   ENDSELECT.

ENDFORM.
*END of TR813 changes


* This routine populates maintab table with planning values from COSP.
FORM POP_PLAN_COSP.
 CLEAR: MAINTAB.
 MOVE: TCJ1T-PRATX TO MAINTAB-PRATX,
       PRPS-POSID  TO MAINTAB-WBS.
       MAINTAB-WBS+7 = '0000'.    "Turns WBS into projects
 MOVE  PROJ-POST1     TO MAINTAB-POST1.
 MOVE  PRPS-POSID(2)  TO MAINTAB-DIV.
 MOVE: COSP-WKG001 TO MAINTAB-PLAN01,
       COSP-WKG002 TO MAINTAB-PLAN02,
       COSP-WKG003 TO MAINTAB-PLAN03,
       COSP-WKG004 TO MAINTAB-PLAN04,
       COSP-WKG005 TO MAINTAB-PLAN05,
       COSP-WKG006 TO MAINTAB-PLAN06,
       COSP-WKG007 TO MAINTAB-PLAN07,
       COSP-WKG008 TO MAINTAB-PLAN08,
       COSP-WKG009 TO MAINTAB-PLAN09,
       COSP-WKG010 TO MAINTAB-PLAN10,
       COSP-WKG011 TO MAINTAB-PLAN11,
       COSP-WKG012 TO MAINTAB-PLAN12.
 ADD MAINTAB-PLAN01 FROM FIRST TO LAST GIVING MAINTAB-PLNTOT.
 IF MAINTAB-PLNTOT <> 0.
    COLLECT MAINTAB.
 ENDIF.
ENDFORM.

* This routine populates maintab table with planning values from COSS.
FORM POP_PLAN_COSS.
 CLEAR: MAINTAB.
 MOVE: TCJ1T-PRATX TO MAINTAB-PRATX,
       PRPS-POSID  TO MAINTAB-WBS.
       MAINTAB-WBS+7 = '0000'.    "Turns WBS into projects
 MOVE  PROJ-POST1     TO MAINTAB-POST1.
 MOVE  PRPS-POSID(2)  TO MAINTAB-DIV.
 MOVE: COSS-WKG001 TO MAINTAB-PLAN01,
       COSS-WKG002 TO MAINTAB-PLAN02,
       COSS-WKG003 TO MAINTAB-PLAN03,
       COSS-WKG004 TO MAINTAB-PLAN04,
       COSS-WKG005 TO MAINTAB-PLAN05,
       COSS-WKG006 TO MAINTAB-PLAN06,
       COSS-WKG007 TO MAINTAB-PLAN07,
       COSS-WKG008 TO MAINTAB-PLAN08,
       COSS-WKG009 TO MAINTAB-PLAN09,
       COSS-WKG010 TO MAINTAB-PLAN10,
       COSS-WKG011 TO MAINTAB-PLAN11,
       COSS-WKG012 TO MAINTAB-PLAN12.
 ADD MAINTAB-PLAN01 FROM FIRST TO LAST GIVING MAINTAB-PLNTOT.
 IF MAINTAB-PLNTOT <> 0.
    COLLECT MAINTAB.
 ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*               DISPLAY_ALV_GRID_DATA.                                 *
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

  SORT MAINTAB BY PRATX DIV WBS.

  CONCATENATE SY-REPID TEXT-002 INTO W_HEAD01 SEPARATED BY SPACE.
  CONCATENATE TEXT-003 P_VBUKR TEXT-CMA TEXT-FYR P_YEAR TEXT-CMA
              TEXT-VRS P_VERSN INTO W_HEAD02.

  MOVE TEXT-CLT  TO W_HEAD03+0(7).
  MOVE SY-SYSID  TO W_HEAD03+8(5).
  MOVE SY-MANDT  TO W_HEAD03+14(4).
  MOVE TEXT-DTE  TO W_HEAD03+21(5).
  WRITE SY-DATUM TO W_HEAD03+27(10).
  MOVE TEXT-TME  TO W_HEAD03+40(5).
  WRITE SY-UZEIT TO W_HEAD03+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'MAINTAB'
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
     WHEN 'PRATX'.
          FC_STR-SELTEXT_L = TEXT-C01.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'DIV'.
          FC_STR-SELTEXT_L = TEXT-C02.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'WBS'.
          FC_STR-SELTEXT_L = TEXT-C03.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'POST1'.
          FC_STR-SELTEXT_L = TEXT-C04.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'PLAN01'.
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN02'.
          FC_STR-SELTEXT_L = TEXT-C06.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN03'.
          FC_STR-SELTEXT_L = TEXT-C07.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN04'.
          FC_STR-SELTEXT_L = TEXT-C08.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN05'.
          FC_STR-SELTEXT_L = TEXT-C09.  " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN06'.
          FC_STR-SELTEXT_L = TEXT-C10.  " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN07'.
          FC_STR-SELTEXT_L = TEXT-C11.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN08'.
          FC_STR-SELTEXT_L = TEXT-C12.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN09'.
          FC_STR-SELTEXT_L = TEXT-C13.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN10'.
          FC_STR-KEY    = ' '.                  " Key columns -not first
          FC_STR-SELTEXT_L = TEXT-C14.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN11'.
          FC_STR-SELTEXT_L = TEXT-C15.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLAN12'.
          FC_STR-SELTEXT_L = TEXT-C16.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'PLNTOT'.
          FC_STR-SELTEXT_L = TEXT-C17.  " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
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
*          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = MAINTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- main heading
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO  = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- sub heading 1
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
  CLEAR LS_LINE.

*3  sub heading 2
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD03.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.


ENDFORM.
