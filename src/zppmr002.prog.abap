REPORT ZPPMR002 NO STANDARD PAGE HEADING LINE-COUNT 44 LINE-SIZE 120
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        April 1997
*  Description:
*     - The purpose of this program is to produce Interest During
*       Construction Report by Company, Division, Date, Project.
************************************************************************
* 98/03/23 md7140 #--- Change in template definition
* 97/06/17 md7140 Dev.Req. DRPP0182
***** ***** ***** ***** ***** ****** ***** ***** ***** ***** ***** *****
* modified by Nancy Gilligan, OmniLogic  11/98
*   - added edit check of whether the "auditor" summarization field in *
*     level 1 of the WBS element is full  (do not include in report)   *
************************************************************************

TABLES:  COBRA, COSP, JEST, PROJ, PRPS, TJ02T, AUSP.
DATA: AUDIT_NUMBER LIKE CABN-ATINN.                              "OMNING
DATA: STATUS         LIKE JEST-STAT.           "Status
DATA: FLAG(3)  TYPE C.                           "Flag
DATA: FLAGX(3) TYPE C.                           "Flag
DATA: KSTAR          LIKE COSP-KSTAR.
DATA: BZDAT          LIKE COBRA-BZDAT.          "Valuation Date
DATA: STATUSDESC     LIKE TJ02T-TXT04.

DATA:
    BEGIN OF SAVE_TABLE OCCURS 10000,
       POSID         LIKE PRPS-POSID,           "Project Number
       BZDAT         LIKE COBRA-BZDAT,          "InService Date
       POST1         LIKE PRPS-POST1,           "Description
       PROJDESC      LIKE PROJ-POST1,           "Description
       STATUS        LIKE JEST-STAT,            "Status
   END OF SAVE_TABLE.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       DEPT(2),                                 "Div/Dept
       POSIDSRT      LIKE PROJ-PSPID,           "Project Number
       PROJDESC      LIKE PROJ-POST1,           "Description
       POSID         LIKE PRPS-POSID,           "Project Number
       BZDAT         LIKE COBRA-BZDAT,          "InService Date
       POST1         LIKE PRPS-POST1,           "Description
       STATUS        LIKE JEST-STAT,            "Status
   END OF BIG_TABLE.


*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN   BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:    SPBUKR   FOR PRPS-PBUKR.       "Company Code
PARAMETER:         PVERS    LIKE COSP-VERSN,      "Version
                   PKSTAR   LIKE COSP-KSTAR OBLIGATORY DEFAULT '491003'.
SELECT-OPTIONS:    S_PSPID  FOR PROJ-PSPID.       "Project
SELECTION-SCREEN   END OF BLOCK BOX.
*selection-screen: comment 1(37) text-030, comment 38(38) text-031.

* End of selection screen
*-----------------------------------------------------------------------

START-OF-SELECTION.
    SELECT * FROM PROJ
        WHERE VBUKR IN SPBUKR                  "Company Code
          AND PSPID IN S_PSPID.                "Project
      REFRESH SAVE_TABLE.
      CLEAR: FLAG, FLAGX, KSTAR.

* ONLY LOOK AT PROJECTS THAT HAVE NOT BEEN REVIEWED BY ASSET MANAGEMENT
  SELECT SINGLE * FROM PRPS WHERE POSID = PROJ-PSPID             "OMNING
                              AND STUFE = '1'.                   "OMNING
  IF SY-SUBRC = 0.                                               "OMNING
    PERFORM GET_NOT_AUDIT.                                       "OMNING

* CONTINUE PROCESSING WITH RELEVENT PROJECTS.                    "OMNING
  IF SY-SUBRC = 0.                                               "OMNING
    CONTINUE. " THE PROJECT HAS BEEN REVIEWED - DON'T CONSIDER IT OMNING
  ELSE.                                                          "OMNING
      PERFORM PROCESS_TABLES.

      LOOP AT SAVE_TABLE.
        IF  SAVE_TABLE-STATUS = 'I0045'.       "if WBS element
            FLAGX = 'yes'.                     "has a valuation date or
        ENDIF.                                 "status is TECO
        IF  SAVE_TABLE-BZDAT CA '123456789'.
            FLAGX = 'yes'.
        ENDIF.
      ENDLOOP.

      IF KSTAR = PKSTAR.
         FLAG = 'no '.
      ENDIF.

      IF  FLAG = 'yes' AND FLAGX = 'yes'.
          PERFORM BUILD_BIG_TABLE.
      ENDIF.
      ENDIF.                                                     "OMNING
    ENDIF.   "END OF PROJECT SELECTION                            OMNING
    ENDSELECT.

    PERFORM DISPLAY_TABLE.
    WRITE: / TEXT-014 UNDER TEXT-009.          "End of Report
*-----------------------------------------------------------------------

TOP-OF-PAGE.
FORMAT INTENSIFIED OFF.
WRITE: / TEXT-007, SY-REPID COLOR COL_GROUP INTENSIFIED OFF INVERSE ON,
      32 TEXT-003, 90 TEXT-001, SY-DATUM, TEXT-004, SY-UZEIT.
WRITE: / TEXT-005, SY-MANDT UNDER SY-REPID,                   "Client
                   TEXT-002 UNDER TEXT-001, SY-PAGNO.         "Page
WRITE: / TEXT-006, PVERS UNDER SY-REPID.                      "Version
WRITE: / TEXT-016 UNDER TEXT-007, SPBUKR+3(3).
ULINE.
FORMAT INTENSIFIED ON.
PERFORM PRINT_HEADINGS.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

  SORT BIG_TABLE BY DEPT POSIDSRT PROJDESC POSID BZDAT.
*  loop at big_table.
*   perform big_table_dump.
*  endloop.
  LOOP AT BIG_TABLE.
      AT NEW DEPT.
         NEW-PAGE.
      ENDAT.

      PERFORM PRINT_VERT.
      CLEAR STATUSDESC.
      SELECT SINGLE * FROM TJ02T
         WHERE ISTAT = BIG_TABLE-STATUS
           AND SPRAS = SY-LANGU.
         IF  SY-SUBRC = '0'.
             STATUSDESC = TJ02T-TXT04.
         ENDIF.
      WRITE:  BIG_TABLE-POSID    UNDER TEXT-011,      "Project Number
              BIG_TABLE-POST1    UNDER TEXT-012,      "Description
              BIG_TABLE-BZDAT    UNDER TEXT-009,      "Valuation Date
              STATUSDESC         UNDER TEXT-015.      "Status

      AT END OF POSIDSRT.
         PERFORM PRINT_VERT.
         ULINE: 13(107).
      ENDAT.

      AT END OF DEPT.
         ULINE.
      ENDAT.
  ENDLOOP.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_HEADINGS
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
   FORMAT COLOR 2 ON.
   WRITE: /1(120) SY-ULINE.
   PERFORM PRINT_VERT.
   WRITE:   4 TEXT-013, 39 TEXT-009.
   PERFORM PRINT_VERT.
   WRITE:               TEXT-010 UNDER TEXT-009,
           13 TEXT-011, 59 TEXT-015, 70 TEXT-012.
   WRITE: /1(120) SY-ULINE.
   FORMAT COLOR 2 OFF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_VERT
*-----------------------------------------------------------------------
*   Description:
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:  /1 SY-VLINE, 12 SY-VLINE, 38 SY-VLINE, 68 SY-VLINE,
          120 SY-VLINE, 57 SY-VLINE.
ENDFORM.

*--------------------  PROCESS_TABLES  ---------------------------------
*   This routine selects all PRPS row which require Interest
*   Calculation (PRPS-ZSCHM = 'CTRA/UN')
*-----------------------------------------------------------------------
FORM PROCESS_TABLES.

  SELECT * FROM PRPS                     "WBS elements require interest
    WHERE PSPHI = PROJ-PSPNR
    ORDER BY ZSCHM DESCENDING.           "Force 'cntr/un' to top
*     and zschm = 'CTRA/UN'.

   IF  PRPS-ZSCHM = 'CTRA/UN'.            "Determine if project should
       FLAG = 'yes'.                      "be reported.
   ENDIF.

   SELECT SINGLE * FROM COSP            "Interest already calculated
     WHERE OBJNR = PRPS-OBJNR
       AND VERSN = PVERS
       AND KSTAR = PKSTAR.

     IF  SY-SUBRC = '0'.
         KSTAR = COSP-KSTAR.
     ENDIF.

*     if prps-posid+4(3) co '1234567890'.          "No templates
       IF PRPS-POSID+5(2) CO '1234567890'.          "No templates
          IF PRPS-POSID+4(3) > '199'.               "001-199 excluded
            CLEAR STATUS.
            PERFORM GET_STATUS.
            CLEAR BZDAT.
            SELECT SINGLE * FROM COBRA             "Valuation Date
                WHERE OBJNR = PRPS-OBJNR.
            IF  SY-SUBRC = '0'.
                IF  COBRA-BZDAT CA '123456789'.
                    FLAGX = 'yes'.
                    BZDAT = COBRA-BZDAT.
                ENDIF.
            ENDIF.
            PERFORM BUILD_SAVE_TABLE.
         ENDIF.                                    "001-199 excluded
      ENDIF.                                       "no templates
  ENDSELECT.
ENDFORM.

FORM BUILD_BIG_TABLE.
    LOOP AT SAVE_TABLE.

      IF  SAVE_TABLE-STATUS <> 'I0076'.
      CLEAR BIG_TABLE.
      MOVE SAVE_TABLE-POSID(7) TO BIG_TABLE-POSIDSRT.   "Project
      MOVE SAVE_TABLE-POSID(2) TO BIG_TABLE-DEPT.       "Department
      MOVE SAVE_TABLE-POSID    TO BIG_TABLE-POSID.      "Project
      MOVE SAVE_TABLE-POST1    TO BIG_TABLE-POST1.      "Description
      MOVE SAVE_TABLE-BZDAT    TO BIG_TABLE-BZDAT.      "Valuation Date
      MOVE SAVE_TABLE-STATUS   TO BIG_TABLE-STATUS.     "Status
      MOVE SAVE_TABLE-PROJDESC TO BIG_TABLE-PROJDESC.    "Proj Desc
      APPEND BIG_TABLE.
      ENDIF.
    ENDLOOP.
ENDFORM.

*------------------------  GET_STATUS  ---------------------------------
*  Only RELEASED (I0002) or TECHNICALLY COMPLETE STATUS are acceptable
*-----------------------------------------------------------------------
FORM GET_STATUS.
   CLEAR STATUS.
   SELECT  * FROM JEST
       WHERE OBJNR = PRPS-OBJNR
         AND INACT <> 'X'                     "Ignore Inactives
       ORDER BY STAT.
       CASE JEST-STAT.
         WHEN 'I0002'.                        "Status Released
            MOVE JEST-STAT TO STATUS.
         WHEN 'I0045'.                        "Status Teco Complete
            MOVE 'yes'     TO FLAGX.
*           move 'yes'     to flag.
            MOVE JEST-STAT TO STATUS.
         WHEN 'I0076'.                        "Status Flag Deleted
            MOVE JEST-STAT TO STATUS.
       ENDCASE.
   ENDSELECT.
ENDFORM.
*---------------------  BUILD_SAVE_TABLE  ------------------------------
*   Description:
*   Store all info in SAVE_TABLE.  If FLAG is YES, then move to
*   all info to SAVE_TABLE
*-----------------------------------------------------------------------
FORM BUILD_SAVE_TABLE.
    MOVE PRPS-POSID      TO SAVE_TABLE-POSID.     "Project
    MOVE PRPS-POST1      TO SAVE_TABLE-POST1.     "Description
    MOVE PROJ-POST1      TO SAVE_TABLE-PROJDESC.  "Project Description
    MOVE BZDAT           TO SAVE_TABLE-BZDAT.     "Valuation Date
    MOVE STATUS          TO SAVE_TABLE-STATUS.    "Status
    APPEND SAVE_TABLE.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM BIG_TABLE_DUMP
*-----------------------------------------------------------------------
*   Description:
*   To view contents loop thru BIG_TABLE
*-----------------------------------------------------------------------
FORM BIG_TABLE_DUMP.
    WRITE: /1 BIG_TABLE-DEPT, 20 BIG_TABLE-POSID, 40 BIG_TABLE-BZDAT,
           60 '*', BIG_TABLE-POST1, '**', BIG_TABLE-PROJDESC.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_NOT_AUDIT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*  USE FUNCTION TO GET NUMBER OF CHARACTERISTIC ASSOCIATED WITH AUDITOR
*  AND THEN SEARCH THROUGH AUSP TO SEE IF THERE IS A VALUE IN THAT
*  CHARACTERISTIC.  IF SO, EXIT THE LOOP AND SELECT THE NEXT PROJECT
*  (DONE IN MAIN SECTION OF PROGRAM), IF NOT, CONTINUE PROCESSING.
*                                                                 OMNING
FORM GET_NOT_AUDIT.

CALL FUNCTION 'CTUT_FEATURE_CHECK'
     EXPORTING
          CLASS_TYPE                  = '014'
          FEATURE_NEUTRAL_NAME        = 'AUDITOR'
     IMPORTING
          FEATURE_ID                  = AUDIT_NUMBER
     EXCEPTIONS
          INVALID_CLASS_TYPE          = 1
          MISSING_FEATURE_INFORMATION = 2
          NO_FEATURE_FOUND            = 3
          NO_FEATURE_VALID            = 4
          NO_LANGUAGE                 = 5
          OTHERS                      = 6.

 IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTIC.'.
 ELSE.
    SELECT * FROM AUSP WHERE OBJEK = PRPS-OBJNR
                         AND ATINN = AUDIT_NUMBER
                         AND MAFID = 'O'
                         AND KLART = '014'.
    ENDSELECT.  " IF THIS IS SUCCESSFUL, THE PROJECT HAS BEEN REVIEWED
 ENDIF.

ENDFORM.                    " GET_NOT_AUDIT
