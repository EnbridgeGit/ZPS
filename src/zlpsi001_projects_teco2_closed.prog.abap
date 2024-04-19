*&---------------------------------------------------------------------*
*& Report  ZLPSI001_PROJECTS_TECO2_CLOSED
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZLPSI001_PROJECTS_TECO2_CLOSED MESSAGE-ID ZS.
* NO STANDARD PAGE HEADING LINE-SIZE 110 LINE-COUNT 58 MESSAGE-ID ZS.
* Type-pool of ALV
TYPE-POOLS: SLIS.

***********************************************************************
*                                                                     *
*  Programmer: Mohammad Khan                                          *
*                                                                     *
*  Date:       Novenber, 2011.                                        *
*                                                                     *
*  Description: This program produces a listing of projects TECO for  *
*               a specified period through variants and if the user   *
*               choose to close projects, the program ZPPMI009 will be*
*               submitted automatically using output of this program. *
*               Program ZPPMI009 will create a BDC session to close   *
*               all these projects.                                   *
*                                                                     *
***********************************************************************
* CHANGES:                                                            *
*                                                                     *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
***********************************************************************
TABLES: PRPS,                         " WBS element table
        JCDS,                         " User Statuses
        JEST.                         " Individual Object Status

DATA:  BEGIN OF PTAB OCCURS 0,
         PBUKR  LIKE PRPS-PBUKR,      "Company Code
         VERNR  LIKE PRPS-VERNR,
         POSID  LIKE PRPS-POSID,      "Project Number
         PSPNR  LIKE PRPS-PSPNR,      "Project Number
         POST1  LIKE PRPS-POST1,      "Project Description
         ERDAT  LIKE PRPS-ERDAT,      "Proj. Creation Date
         UDATE  LIKE JCDS-UDATE,      "TECO Status create date
         OBJNR  LIKE AUSP-OBJEK, "PRPS-OBJNR,
         AUDIT  LIKE AUSP-ATWRT,      "Characterstic value for AUDITOR
       END OF PTAB.

DATA:  BEGIN OF PROJECTS_TAB OCCURS 0,
         SIGN(1)   TYPE C,
         OPTION(2) TYPE C,
         LOW   LIKE PROJ-PSPID,
         HIGH  LIKE PROJ-PSPID,
       END OF PROJECTS_TAB.

DATA: BEGIN OF PROJ_TAB OCCURS 0,
      PSPID LIKE PROJ-PSPID,
      END OF PROJ_TAB.

DATA: BEGIN OF BDCDATA OCCURS 0.      "BDC Table
        INCLUDE STRUCTURE BDCDATA.    "Structure used for BDC Table
DATA: END OF BDCDATA.

DATA: TOTREC          TYPE I,
      W_HEAD01(60)    TYPE C,
      W_HEAD02(60)    TYPE C,
      PRIOR_UDATE(10) TYPE C.
DATA: GT_AUSP TYPE TABLE OF AUSP.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
 PARAMETERS:     P_PBUKR LIKE PRPS-PBUKR DEFAULT 'UGL' OBLIGATORY,
                 P_UDATE LIKE JCDS-UDATE OBLIGATORY.  "TECO GoBackDate
 SELECT-OPTIONS: S_PSPNR FOR PRPS-PSPNR,      "Proj #
                 S_VERNR FOR PRPS-VERNR.      "Division
SELECTION-SCREEN SKIP 1.
 PARAMETERS:     P_BDC     AS   CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX1.

***********************************************************************
START-OF-SELECTION.
 PERFORM SELECT_DB_DATA.
 DESCRIBE TABLE PTAB LINES TOTREC.
 IF TOTREC > 0 AND P_BDC = 'X'.
       PERFORM BUILD_PROJ_CLOSE_TABLE.
       SUBMIT ZPPMI009
            WITH S_PROJ  IN PROJECTS_TAB
            WITH P_CLOSE = 'X'
            WITH  P_BZDAT = SY-DATUM
          AND RETURN.
       PERFORM DISPLAY_ALV_GRID_DATA.
 ELSEIF TOTREC > 0.
        PERFORM DISPLAY_ALV_GRID_DATA.
 ELSE.
    MESSAGE A019 WITH TEXT-005.
 ENDIF.

END-OF-SELECTION.
***********************************************************************
* This routine will get all equired data for the selected projects.
FORM SELECT_DB_DATA.

  CONSTANTS: LC_ATINN TYPE CABN-ATNAM VALUE 'AUDITOR'.
  DATA: LV_AUDITOR TYPE AUSP-ATINN,
        LS_AUSP    TYPE AUSP.

  CLEAR GT_AUSP.

  SELECT PRPS~PBUKR  PRPS~VERNR PRPS~POSID PRPS~PSPNR  PRPS~POST1
         PRPS~ERDAT  JCDS~UDATE PRPS~OBJNR
    INTO CORRESPONDING FIELDS OF TABLE PTAB
    FROM ( PRPS
           INNER JOIN JCDS
           ON JCDS~OBJNR  = PRPS~OBJNR )
  WHERE  PRPS~PSPNR  IN S_PSPNR           "Project Number
    AND  PRPS~PBUKR  =  P_PBUKR           "Company code
    AND  PRPS~VERNR  IN S_VERNR           "Division
    AND  PRPS~STUFE  =  1                 "Project Level
    AND  JCDS~UDATE  <= P_UDATE           "Go Back TECO Date
    AND  JCDS~STAT   = 'I0045'            "Status TECO
    AND NOT EXISTS ( SELECT *
                          FROM JEST
                         WHERE OBJNR = PRPS~OBJNR
                   AND ( JEST~STAT  = 'I0046' OR JEST~STAT  = 'I0002' )
                   AND JEST~INACT = ' ' ).

*                          FROM JCDS
*                         WHERE OBJNR = PRPS~OBJNR
*                   AND ( JCDS~STAT  = 'I0046' OR JCDS~STAT  = 'I0002' )
*                   AND JCDS~INACT = ' ' ).

  SORT PTAB BY PBUKR VERNR PSPNR UDATE.

  DELETE ADJACENT DUPLICATES                      " delete duplicates
         FROM PTAB COMPARING PSPNR. " POST1 ERDAT UDATE.
  IF PTAB[] IS NOT INITIAL.

     CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
         input         = LC_ATINN
      IMPORTING
         OUTPUT        = LV_AUDITOR.

     SELECT * FROM AUSP INTO TABLE GT_AUSP
       FOR ALL ENTRIES IN PTAB
       WHERE OBJEK = PTAB-OBJNR
         AND ATINN = LV_AUDITOR.
    LOOP AT PTAB.
         READ TABLE GT_AUSP INTO LS_AUSP
         WITH KEY OBJEK = PTAB-OBJNR
                  ATINN = LV_AUDITOR.
         CHECK SY-SUBRC = 0.
         PTAB-AUDIT = LS_AUSP-ATWRT.
         MODIFY PTAB.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&--------------------------------------------------------------------*
*&      Form  BUILD_TABLE
*&--------------------------------------------------------------------*
FORM BUILD_PROJ_CLOSE_TABLE.

SELECT PSPID FROM PROJ INTO TABLE PROJ_TAB
 FOR ALL ENTRIES IN PTAB
 WHERE PSPID = PTAB-POSID.

 SORT PROJ_TAB BY PSPID.

LOOP AT PTAB.
READ TABLE PROJ_TAB WITH KEY PSPID = PTAB-POSID BINARY SEARCH.
     IF SY-SUBRC = 0.
        PROJECTS_TAB-SIGN = 'I'.
        PROJECTS_TAB-OPTION = 'EQ'.
        PROJECTS_TAB-LOW = PROJ_TAB-PSPID.
        APPEND PROJECTS_TAB.
        ADD  1  TO  TOTREC.
        CLEAR: PROJECTS_TAB.
     ELSE.
        WRITE: /1 TEXT-007, PTAB-POSID.
*        WRITE: /1 'Project from PRPS not found in PROJ - PRPS-POSID=',
*                                                           PTAB-POSID.
*        WRITE: /1 'PRPS-POSID=', PTAB-POSID, 'not found in PROJ-PSPNR'.
     ENDIF.
ENDLOOP.
ENDFORM.                    " BUILD_TABLE

*&--------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&--------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

  CONCATENATE P_UDATE+0(4) '/' P_UDATE+4(2) '/' P_UDATE+6(2)
                                           INTO PRIOR_UDATE.

  CONCATENATE TEXT-100 PRIOR_UDATE INTO W_HEAD01 SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  VARIANT-REPORT = REPID.
  REFRESH FIELDCAT.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'PTAB'
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
     WHEN 'UDATE'.
          FC_STR-SELTEXT_L = TEXT-C01.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'PSPNR'.
           FC_STR-NO_OUT = 'X'.                " hide column
     WHEN 'AUDIT'.
           FC_STR-seltext_l = 'Auditor'.
           FC_STR-seltext_m = 'Auditor'.
           FC_STR-seltext_s = 'Auditor'.
     WHEN 'OBJNR'.
           FC_STR-TECH = 'X'.
     WHEN OTHERS.
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.
*
* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
*          I_SAVE       = 'A'
*          IS_VARIANT   = variant
*          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = PTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- SELECTION LINE: TYPE S
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
*  LS_LINE-KEY   = 'CLIENT: '.
  LS_LINE-INFO  = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
*  ls_line-typ  = 'S'.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.


ENDFORM.
*---------------------------------------------------------------------*
*               DISPLAY_EXCEL_SHEET.
*---------------------------------------------------------------------*
