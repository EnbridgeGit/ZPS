REPORT ZPPMR036.
************************************************************************
*  Author:      Mohammad T. Khan
*  Date:        May 2003.
*  Requestor:   Glen Burk
*  Description:
*     - The purpose of this program is to produce a report of mains
*       lengths (Budget and Actual).
************************************************************************
*  CHANGES:
*     Date    Issue Log        By       Description
*  ---------- ---------  -------------  -------------------------------
*  29/09/2004    1074    Mohammad Khan  1- Add AVD to variants and make
*                                          program changes accordingly.
*                                       2- Remove hard coded WBS element
*                                          and manage it through variant
*                                       3- Add status changed by column
*                                          in the report.
************************************************************************
TABLES:
        PROJ,                        "Project Definition
        PRPS,                        "WBS Element Master Data
        COBRA,                       "Asset Valuation Date
        COSP,                        "External Postings
        JEST,                        "Status of WBS Element
        JCDS,       "Issue 1074      "Change Doc.for System/User Status
        TJ02T.                       "System Status Texts

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_BUD_LGTH  LIKE CABN-ATINN,       "Budget Length Char
       G_ATINN_ACT_LGTH  LIKE CABN-ATINN.       "Actual Length Char

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.
*-----------------------------------------------------------------------

DATA: BEGIN OF ITAB OCCURS 0,
            POSID  LIKE PRPS-POSID,
            POST1  LIKE PRPS-POST1,
            OBJNR  LIKE PRPS-OBJNR,
            STATUS LIKE JEST-STAT,
      END OF ITAB.

DATA: BEGIN OF EXCLTAB OCCURS 0,
            PROJ(14)  TYPE C,
            POST1     LIKE PROJ-POST1,      "Issue 1074
            POST2     LIKE PRPS-POST1,      "Issue 1074
            STATUS    LIKE JEST-STAT,
            BZDAT(10) TYPE C,
            BLGTH     TYPE I,             "Budget Length
            ALGTH     TYPE I,             "Actual Length
            USNAM     LIKE JCDS-USNAM,    "Stat.Change by-Issue 1074
      END OF EXCLTAB.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5) TYPE C,
        DDIC_FIELD(5) TYPE C,
        KEY TYPE C,
      END OF PROT_HEADER.

DATA  ERRORTAB     LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
DATA: RETCODE      LIKE SY-SUBRC,
      W_REPTTL     LIKE SY-TITLE,          "Report Title
      W_OPTION(11) TYPE C VALUE 'START_EXCEL',
      W_HEAD01(40) TYPE C,
      W_HEAD02(25) TYPE C,
      AVD_FLAG     TYPE C.                "Issue 1074


*----------------------  SELECTION SCREEN  -----------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-000.
PARAMETER PBUKRS LIKE PRPS-PBUKR OBLIGATORY DEFAULT 'UGL'. "Company Code
SELECT-OPTIONS:  SVERNR  FOR  PRPS-VERNR,                 "Division
*                 SPSPHI  FOR  PRPS-PSPHI,           "Project issue 1074
                 SPOSID  FOR  PRPS-POSID.            "Project issue 1074
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 31(20) TEXT-003.
SELECTION-SCREEN COMMENT 56(20) TEXT-003.
SELECTION-SCREEN END   OF LINE.

SELECT-OPTIONS:  SWBS    FOR PRPS-POSID+7(4),            "WBS-Issue 1074
                 SGJAHR  FOR  COSP-GJAHR,                "FiscYr
                 SSTAT   FOR JEST-STAT,                  "Status
                 SBZDAT  FOR COBRA-BZDAT.                "AVD-Issue 1074

SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.        "Comment 1 on screen  "Issue 1074
SELECTION-SCREEN COMMENT 1(43) TEXT-ST4.                     "Issue 1074
SELECTION-SCREEN END OF LINE.                                "Issue 1074

SELECTION-SCREEN BEGIN OF LINE.        "Comment 2 on screen
SELECTION-SCREEN COMMENT 1(43) TEXT-ST3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.        "Comment 3 on screen
SELECTION-SCREEN COMMENT 1(43) TEXT-ST1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.        "Comment 4 on screen
SELECTION-SCREEN COMMENT 1(43) TEXT-ST2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-002.
PARAMETERS:     P_EXCL RADIOBUTTON GROUP RBCR,    "EXCEL SHEET
                P_RPRT RADIOBUTTON GROUP RBCR.    "PRINT REPORT
SELECTION-SCREEN END OF BLOCK BOX4.
SELECTION-SCREEN END OF BLOCK BOX1.

*-----------------------------------------------------------------------

START-OF-SELECTION.

  MOVE 'ACTUAL_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_ACT_LGTH.

  MOVE 'BUDGET_LENGTH'  TO CHARIC.
  PERFORM GET_ATINN.
  MOVE G_ATINN          TO G_ATINN_BUD_LGTH.

********************************************************************
*
  SELECT PRPS~POSID PRPS~POST1 PRPS~OBJNR JEST~STAT
    INTO (PRPS-POSID, PRPS-POST1, PRPS-OBJNR, JEST-STAT)
    FROM ( ( PRPS INNER JOIN JEST
               ON PRPS~OBJNR = JEST~OBJNR )
                  INNER JOIN COSP
               ON PRPS~OBJNR = COSP~OBJNR )
*   WHERE  PRPS~PSPHI IN SPSPHI               "Issue 1074
   WHERE  PRPS~POSID IN SPOSID                "Issue 1074
     AND  PRPS~VERNR IN SVERNR
     AND  PRPS~PBUKR = PBUKRS
     AND  JEST~STAT  IN SSTAT
     AND  JEST~INACT =  SPACE
     AND  COSP~GJAHR IN SGJAHR
     AND  COSP~WRTTP = '04'
     AND  COSP~KSTAR NOT IN ('0000491001', '0000491002').

     IF SY-SUBRC = 0.
        MOVE: PRPS-POSID TO ITAB-POSID,
             PRPS-POST1  TO ITAB-POST1,
             PRPS-OBJNR  TO ITAB-OBJNR,
             JEST-STAT   TO ITAB-STATUS.
        COLLECT ITAB.
        CLEAR ITAB.
     ENDIF.
   ENDSELECT.

  SORT ITAB BY POSID.
  LOOP AT ITAB.
*    IF  ( ITAB-POSID+7(4) BETWEEN '8411' AND '8430' OR    "Issue 1074
*          ITAB-POSID+7(4) BETWEEN '8451' AND '8459' OR    "Issue 1074
*          ITAB-POSID+7(4) BETWEEN '6411' AND '6430' OR    "Issue 1074
*          ITAB-POSID+7(4) BETWEEN '6451' AND '6459' OR    "Issue 1074
*          ITAB-POSID+7(4) BETWEEN '4601' AND '4620' OR    "Issue 1074
*          ITAB-POSID+7(4) BETWEEN '5411' AND '5430' OR    "Issue 1074
*          ITAB-POSID+7(4) BETWEEN '5451' AND '5459' ).    "Issue 1074

      IF  ITAB-POSID+7(4) IN SWBS.                         "Issue 1074
          CLEAR: EXCLTAB.
          CONCATENATE ITAB-POSID+0(2) '-' ITAB-POSID+2(2)
                                      '-' ITAB-POSID+4(3)
                                      '-' ITAB-POSID+7(4)
                                      INTO EXCLTAB-PROJ.
*         MOVE  ITAB-POST1  TO  EXCLTAB-POST1.            "Issue 1074
          PERFORM FORMAT_PROJ_DESCRIPTION.                "Issue 1074
          MOVE  ITAB-STATUS TO  EXCLTAB-STATUS.
          PERFORM GET_STATUS_TEXT.
          OBJECT = ITAB-OBJNR.
          PERFORM FIND_CHARACTERISTIC.
          PERFORM ASSET_VALUATION_DATE.                   "Issue 1074
          PERFORM GET_LAST_CHANGED_BY.                    "Issue 1074
          IF AVD_FLAG = 'Y'.
             APPEND EXCLTAB.
          ENDIF.
     ENDIF.
  ENDLOOP.

    PERFORM PROT_HEADER.
 IF SY-BATCH <> 'X'.
    CONCATENATE SY-REPID  '-'  TEXT-TTL INTO W_REPTTL
                SEPARATED BY SPACE.
    MOVE TEXT-016  TO W_HEAD01+0(5).
    WRITE SY-DATUM TO W_HEAD01+6(10).
    MOVE TEXT-017  TO W_HEAD01+24(5).
    WRITE SY-UZEIT TO W_HEAD01+30(10).

    MOVE TEXT-CLT  TO W_HEAD02+0(7).
    MOVE SY-MANDT  TO W_HEAD02+8(4).
    MOVE SY-SYSID  TO W_HEAD02+13(5).
 ENDIF.

    IF P_RPRT = 'X'.
       CLEAR W_OPTION.
       IF SY-BATCH = 'X'.
          CLEAR W_OPTION.
       ENDIF.
    ENDIF.

    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
          EXPORTING
            BASIC_LIST_TITLE        = W_REPTTL
            HEAD_LINE1              = W_HEAD01
            HEAD_LINE2              = W_HEAD02
            FILE_NAME               = SY-CPROG
            ADDITIONAL_OPTIONS      = W_OPTION
          IMPORTING
               RETURN_CODE          = RETCODE
          TABLES
               DATA_TAB             = EXCLTAB
               FIELDNAME_TAB        = PROT_HEADER
               ERROR_TAB            = ERRORTAB
          EXCEPTIONS
               DOWNLOAD_PROBLEM     = 1
               NO_DATA_TAB_ENTRIES  = 2
               TABLE_MISMATCH       = 3
               PRINT_PROBLEMS       = 4
               OTHERS               = 5.
  IF SY-SUBRC NE 0.
    WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFULL - REASON = ', SY-SUBRC.
  ENDIF.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*--------------------  FORMAT_PROJ_DESCRIPTION --------------Issue 1074*
************************************************************************
FORM FORMAT_PROJ_DESCRIPTION.

DATA: W_PSPID LIKE PROJ-PSPID.
      CONCATENATE ITAB-POSID+0(7) '0000' INTO W_PSPID.
     SELECT SINGLE POST1 INTO PROJ-POST1
       FROM PROJ
      WHERE PSPID  = W_PSPID.
      IF SY-SUBRC = 0.
         MOVE PROJ-POST1 TO EXCLTAB-POST1.
         MOVE ITAB-POST1 TO EXCLTAB-POST2.
      ENDIF.

ENDFORM.

************************************************************************
*---------------------  GET_LAST_CHANGED_BY -----------------Issue 1074*
************************************************************************
FORM GET_LAST_CHANGED_BY.

  SELECT SINGLE USNAM INTO JCDS-USNAM
    FROM JCDS
   WHERE OBJNR = ITAB-OBJNR
     AND STAT  = ITAB-STATUS
     AND INACT <> 'X'.

  IF SY-SUBRC = 0.
     MOVE JCDS-USNAM TO EXCLTAB-USNAM.
     CLEAR JCDS-USNAM.
  ENDIF.

ENDFORM.

************************************************************************
*--------------------  ASSET VALUATION DATE ----------------------------
************************************************************************
FORM ASSET_VALUATION_DATE.
  CLEAR AVD_FLAG.                                   "Issue 1074
  SELECT SINGLE * FROM COBRA           "Valuation Date
    WHERE OBJNR = ITAB-OBJNR
      AND BZDAT IN SBZDAT.                          "Issue 1074
  IF SY-SUBRC = '0'.
  CONCATENATE COBRA-BZDAT+0(4) '/' COBRA-BZDAT+4(2) '/'
              COBRA-BZDAT+6(2) INTO EXCLTAB-BZDAT.
  MOVE 'Y' TO AVD_FLAG.                             "Issue 1074
  ENDIF.
ENDFORM.

*-----------------------------------------  ----------------------------
*                          GET_STATUS_TEXT
*-----------------------------------------------------------------------
FORM GET_STATUS_TEXT.

  SELECT  SINGLE * FROM TJ02T
    WHERE ISTAT = ITAB-STATUS
      AND SPRAS = 'EN'.

    IF SY-SUBRC = '0'.
       MOVE TJ02T-TXT04 TO EXCLTAB-STATUS.
    ENDIF.
ENDFORM.


*-----------------------  GET_ATINN  -----------------------------------
* Routine used to get the internal character number for project control
*-----------------------------------------------------------------------
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
  ENDIF.
ENDFORM.

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine used to get the value of the project control number *
*-----------------------------------------------------------------------
FORM FIND_CHARACTERISTIC.
  REFRESH CHAR_TAB.
  CALL FUNCTION 'CLFM_SELECT_AUSP'
       EXPORTING
            MAFID     = 'O'
            CLASSTYPE = '014'
            OBJECT    = OBJECT
       TABLES
            EXP_AUSP  = CHAR_TAB
       EXCEPTIONS
            NO_VALUES = 1
            OTHERS    = 2.
* Character  values in "ATWRT', numeric values in"ATFLV".
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
*  actual length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_ACT_LGTH BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATFLV TO EXCLTAB-ALGTH.
    ENDIF.
*  budget length
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_BUD_LGTH BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATFLV TO EXCLTAB-BLGTH.
    ENDIF.
  ENDIF.
  CLEAR: OBJECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PROT_HEADER                                              *
*---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-CL1 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL2 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C03 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL3 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL4 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL5 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL6 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL7 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER

************************************************************************
