REPORT ZPPMR025 NO STANDARD PAGE HEADING LINE-SIZE 132
                LINE-COUNT 60 MESSAGE-ID ZS.
***********************************************************************
*                                                                     *
*  Programmer: Mohammad Khan                                          *
*                                                                     *
*        Date: APRIL, 2001.                                           *
*                                                                     *
*  Description: This could be one time programs that lists the        *
*               projects with the status selected in variants.        *
*               It takes status of the project definition.            *
***********************************************************************
* CHANGES:                                                            *
*                                                                     *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
***********************************************************************
TABLES: PROJ,                         " Project definition
        JEST,                         "
        T001.                         " Company code/name

DATA:  STATUS(40)    TYPE C,          "All valid status of object
       TOTREC(4)        TYPE N.

DATA:  BEGIN OF PTAB OCCURS 0,
         VERNR  LIKE PROJ-VERNR,      "Division
         PSPID  LIKE PROJ-PSPID,      "Project ID
         POST1  LIKE PROJ-POST1,      "Project Description
         STAT(40)  TYPE C,            "Project statuses
       END OF PTAB.

DATA:  BEGIN OF PROJECTS_TAB OCCURS 0,
         SIGN(1)   TYPE C,
         OPTION(2) TYPE C,
         LOW   LIKE PROJ-PSPID,
         HIGH  LIKE PROJ-PSPID,
       END OF PROJECTS_TAB.

DATA: BEGIN OF BDCDATA OCCURS 0.      "BDC Table
        INCLUDE STRUCTURE BDCDATA.    "Structure used for BDC Table
DATA: END OF BDCDATA.


SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
 PARAMETERS:     P_VBUKR LIKE PROJ-VBUKR DEFAULT 'UGL'.
 SELECT-OPTIONS: S_VERNR FOR PROJ-VERNR,
                 S_PSPID FOR PROJ-PSPID,
                 S_STATUS FOR JEST-STAT.
 SELECTION-SCREEN SKIP 1.
 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 1(66) TEXT-100.
 SELECTION-SCREEN END OF LINE.
 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 1(66) TEXT-101.
 SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
 PARAMETERS:     P_BDC     AS   CHECKBOX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(15) TEXT-015.
PARAMETERS:    P_UNLK   RADIOBUTTON GROUP OGRP.
SELECTION-SCREEN: COMMENT 25(15) TEXT-016.
PARAMETERS:    P_REL    RADIOBUTTON GROUP OGRP.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX4.


************************************************************************
AT SELECTION-SCREEN.

  LOOP AT S_STATUS.
    IF S_STATUS-OPTION <> 'EQ' AND P_BDC = 'X'.
       MESSAGE E019 WITH 'BDC session not valid with multiple statuses'.
       EXIT.
    ELSE.
    IF S_STATUS-LOW <> 'I0042' AND S_STATUS-LOW <> 'I0043'
                               AND P_BDC = 'X'.
       MESSAGE E019 WITH 'BDC session not valid with selected status'.
    ELSE.
    IF P_BDC = 'X' AND S_STATUS-LOW = 'I0042' AND P_REL <> 'X'.
      MESSAGE E019 WITH 'Invalid combination of current & BDC statuses'.
    ELSE.
    IF P_BDC = 'X' AND S_STATUS-LOW = 'I0043' AND P_UNLK <> 'X'.
      MESSAGE E019 WITH 'Invalid combination of current & BDC statuses'.
    ENDIF.
    ENDIF.
    ENDIF.
    ENDIF.

   ENDLOOP.


INITIALIZATION.
     CALL FUNCTION 'SET_PRINT_PARAMETERS'
          EXPORTING COVER_PAGE = 'X'.



START-OF-SELECTION.

 PERFORM PROCESS_PROJECTS.
 PERFORM GET_COMPANY_NAME.
 PERFORM PRINT_REPORT.
 IF TOTREC > 0.
    IF P_BDC = 'X'.
       IF P_UNLK = 'X'.
          SUBMIT ZPPMI009
            WITH S_PROJ  IN PROJECTS_TAB
            WITH P_UNLK = 'X'
            WITH  P_BZDAT = SY-DATUM
          AND RETURN.
       ELSE.
       IF P_REL = 'X'.
          SUBMIT ZPPMI009
            WITH S_PROJ  IN PROJECTS_TAB
            WITH P_REL = 'X'
            WITH  P_BZDAT = SY-DATUM
          AND RETURN.
       ENDIF.
       ENDIF.
    ENDIF.
 ELSE.
*    MESSAGE E019 WITH 'No Projects Selected For These Variants'.
    WRITE: /5 'NO PROJECTS SELECTED FOR THESE VARIANTS'.
 ENDIF.

END-OF-SELECTION.

* This routine will get all appropriate data for the selected projects.*
FORM PROCESS_PROJECTS.

SELECT * FROM PROJ WHERE VBUKR =  P_VBUKR         "Company Code
                      AND VERNR IN S_VERNR        "Division
                      AND PSPID IN S_PSPID        "Project Id
                      AND LOEVM <> 'X'            "Deleted indicator
               ORDER BY PSPID.
 IF SY-SUBRC = 0.
       SELECT SINGLE * FROM JEST       "Select Status
           WHERE OBJNR = PROJ-OBJNR
             AND  STAT IN S_STATUS
             AND INACT <> 'X'.         "Not Inactie
       IF SY-SUBRC = 0.
          PERFORM BUILD_TABLE.
       ENDIF.

 ENDIF.
 ENDSELECT.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  BUILD_TABLE
*&---------------------------------------------------------------------*
FORM BUILD_TABLE.
CLEAR STATUS.
PERFORM GET_PROJECT_STATUSES.
PTAB-VERNR = PROJ-VERNR.
PTAB-PSPID = PROJ-PSPID.
PTAB-POST1 = PROJ-POST1.
PTAB-STAT  = STATUS.
APPEND PTAB.

PROJECTS_TAB-SIGN = 'I'.
PROJECTS_TAB-OPTION = 'EQ'.
PROJECTS_TAB-LOW = PROJ-PSPID.
APPEND PROJECTS_TAB.
ADD  1  TO  TOTREC.
CLEAR: PTAB, PROJECTS_TAB.


CLEAR: PTAB.
ENDFORM.                    " BUILD_TABLE

*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
FORM PRINT_REPORT.
  LOOP AT PTAB.
    WRITE: /1 PTAB-VERNR, PTAB-PSPID UNDER TEXT-007, PTAB-POST1 UNDER
          TEXT-008, PTAB-STAT UNDER TEXT-009.
  ENDLOOP.
ENDFORM.                    " PRINT_REPORT



*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
   WRITE: /1 TEXT-RPT, SY-REPID,  40 T001-BUTXT,             "Company
         80 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
          30 TEXT-TTL,
             TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   ULINE.
   WRITE: 1  TEXT-006, 15 TEXT-007, 30 TEXT-008, 80 TEXT-009.
   ULINE.

*------------------------- End of Report Header ------------------------

*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_NAME
*&---------------------------------------------------------------------*
FORM GET_COMPANY_NAME.

SELECT SINGLE * FROM T001                "Company Code
  WHERE BUKRS = P_VBUKR.


ENDFORM.                    " GET_COMPANY_NAME

*&---------------------------------------------------------------------*
*&      Form  GET_PROJECT_STATUSES
*&---------------------------------------------------------------------*
FORM GET_PROJECT_STATUSES.

 CALL FUNCTION 'AIP9_STATUS_READ'                  "Retrieve Status Info
       EXPORTING
            I_OBJNR = PROJ-OBJNR
            I_SPRAS = SY-LANGU
       IMPORTING
            E_SYSST = STATUS
       EXCEPTIONS
            OTHERS = 1.

ENDFORM.                    " GET_PROJECT_STATUSES

