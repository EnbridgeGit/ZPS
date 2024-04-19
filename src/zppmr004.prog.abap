REPORT ZPPMR004 NO STANDARD PAGE HEADING LINE-COUNT 44 LINE-SIZE 120
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        October 1997
*  Description:
*     - The purpose of this program is to produce a report indicating
*       those projects that are eligible for Interest During
*       Construction.
************************************************************************
* 98/03/23 md7140 #--- Change in template definition
* 97/10/15 md7140 Dev.Req. DRPP0211
*
************************************************************************

TABLES:  COBRA, COSP, JEST, PROJ, PRPS, TJ02T.
DATA: STATUS         LIKE JEST-STAT.   "Status
DATA: FLAG(3)  TYPE C.                 "Flag
DATA: FLAGX(3) TYPE C.                 "Flag
DATA: FLAGY(3) TYPE C.                 "Flag
DATA: KSTAR          LIKE COSP-KSTAR.
DATA: BZDAT          LIKE COBRA-BZDAT. "Valuation Date
DATA: STATUSDESC     LIKE TJ02T-TXT04.
DATA:  VALUE         LIKE COSP-WKG001. "IDC amount
DATA:
    BEGIN OF SAVE_TABLE OCCURS 10000,
       POSID         LIKE PRPS-POSID,  "Project Number
       BZDAT         LIKE COBRA-BZDAT, "InService Date
       POST1         LIKE PRPS-POST1,  "Description
       PROJDESC      LIKE PROJ-POST1,  "Description
       STATUS        LIKE JEST-STAT,   "Status
       VALUE         LIKE COSP-WKG001, "IDC amount
       ZSCHM         LIKE PRPS-ZSCHM,  "Interest Profile
   END OF SAVE_TABLE.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       DEPT(2),                        "Div/Dept
       POSIDSRT      LIKE PROJ-PSPID,  "Project Number
       PROJDESC      LIKE PROJ-POST1,  "Description
       POSID         LIKE PRPS-POSID,  "Project Number
       BZDAT         LIKE COBRA-BZDAT, "InService Date
       POST1         LIKE PRPS-POST1,  "Description
       STATUS        LIKE JEST-STAT,   "Status
       VALUE         LIKE COSP-WKG001, "IDC amount
       ZSCHM         LIKE PRPS-ZSCHM,  "Interest Profile
   END OF BIG_TABLE.


*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN   BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:    SPBUKR   FOR PRPS-PBUKR.       "Company Code
PARAMETER:         PVERS    LIKE COSP-VERSN,      "Version
                   PKSTAR   LIKE COSP-KSTAR OBLIGATORY DEFAULT '491003'.
SELECT-OPTIONS:    S_PSPID  FOR PROJ-PSPID.       "Project
SELECTION-SCREEN   END OF BLOCK BOX.

*-----------------------  TOP-OF-PAGE  ---------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
 WRITE: / TEXT-007, SY-REPID COLOR COL_GROUP INTENSIFIED OFF INVERSE ON,
         32 TEXT-003, 90 TEXT-001, SY-DATUM, TEXT-004, SY-UZEIT.
  WRITE: / TEXT-005, SY-MANDT UNDER SY-REPID,                   "Client
                     TEXT-002 UNDER TEXT-001, SY-PAGNO.         "Page
  WRITE: / TEXT-006, PVERS UNDER SY-REPID.                      "Version
  ULINE.
  FORMAT INTENSIFIED ON.
  PERFORM PRINT_HEADINGS.

*------------------------  START-OF-SELECTION --------------------------

START-OF-SELECTION.
  SELECT * FROM PROJ
    WHERE VBUKR IN SPBUKR              "Company Code
      AND PSPID IN S_PSPID.            "Project
    REFRESH SAVE_TABLE.
    CLEAR: FLAG, FLAGX, KSTAR.

*   if proj-pspid+4(3) co '1234567890'."No templates
    IF PROJ-PSPID+5(2) CO '1234567890'."No templates         98/03/23
      IF PROJ-PSPID+4(3) > '199'.      "001-199 excluded

        PERFORM PROCESS_TABLES.
        CLEAR FLAGY.
        LOOP AT SAVE_TABLE.
          IF  SAVE_TABLE-ZSCHM = 'CTRA/UN' AND
              SAVE_TABLE-VALUE =  0        AND
              SAVE_TABLE-STATUS = 'I0046'.
            MOVE 'yes'              TO FLAGY.
          ENDIF.
        ENDLOOP.

        IF FLAG = 'yes' AND FLAGX = 'yes' AND FLAGY = 'yes'.
          PERFORM BUILD_BIG_TABLE.
        ENDIF.                         "End of Flag's test
      ENDIF.                           "End of 001-199 excluded
    ENDIF.                             "End of Template test
  ENDSELECT.

  PERFORM DISPLAY_TABLE.
  WRITE: / TEXT-014 UNDER TEXT-009.    "End of Report

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*--------------------  PROCESS_TABLES  ---------------------------------
*   This routine flags any project that has a WBS that requires
*   Interest During Calculation (PRPS-ZSCHM = 'CTRA/UN')
*-----------------------------------------------------------------------
FORM PROCESS_TABLES.

  SELECT * FROM PRPS                   "WBS elements require interest
    WHERE PSPHI = PROJ-PSPNR
    ORDER BY ZSCHM DESCENDING.         "Force 'ctra/un' to top

    IF  PRPS-ZSCHM = 'CTRA/UN'.        "Determine if project should
      FLAG = 'yes'.                    "be reported.
    ENDIF.

    CLEAR STATUS.
    PERFORM GET_STATUS.                "Flag CLOSED status WBS elements

    CLEAR: KSTAR, VALUE.
    SELECT SINGLE * FROM COSP          "Interest already calculated
      WHERE OBJNR = PRPS-OBJNR
        AND VERSN = PVERS
        AND KSTAR = PKSTAR.

    IF  SY-SUBRC = '0'.
      KSTAR = COSP-KSTAR.
      ADD COSP-WKG001 FROM 1 TO 12 GIVING VALUE.
    ENDIF.

    PERFORM BUILD_SAVE_TABLE.
  ENDSELECT.
ENDFORM.

*---------------------  BUILD_SAVE_TABLE  ------------------------------
*   Store all info in SAVE_TABLE.
*-----------------------------------------------------------------------
FORM BUILD_SAVE_TABLE.
  MOVE PRPS-POSID      TO SAVE_TABLE-POSID.     "Project
  MOVE PRPS-POST1      TO SAVE_TABLE-POST1.     "Description
  MOVE PROJ-POST1      TO SAVE_TABLE-PROJDESC.  "Project Description
  MOVE BZDAT           TO SAVE_TABLE-BZDAT.     "Valuation Date
  MOVE STATUS          TO SAVE_TABLE-STATUS.    "Status
  MOVE VALUE           TO SAVE_TABLE-VALUE.     "IDC Amount
  MOVE PRPS-ZSCHM      TO SAVE_TABLE-ZSCHM.     "Interest Profile
  APPEND SAVE_TABLE.
*   write: / save_table-posid, save_table-status, save_table-value,
*            flag, flagx.

ENDFORM.
*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

  SORT BIG_TABLE BY DEPT POSIDSRT PROJDESC POSID BZDAT.
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
       (10) BIG_TABLE-VALUE    UNDER TEXT-009,      "IDC Amount
            STATUSDESC         UNDER TEXT-015,      "Status
            BIG_TABLE-ZSCHM    UNDER TEXT-016.      "Interest Profile

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
  WRITE:   4 TEXT-013, 39 TEXT-009, 3 TEXT-016..
  PERFORM PRINT_VERT.
  WRITE:  TEXT-010 UNDER TEXT-009, TEXT-017 UNDER TEXT-016,
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


*---------------------------------------------------------------------*
*       FORM BUILD_BIG_TABLE                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
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
      MOVE SAVE_TABLE-VALUE    TO BIG_TABLE-VALUE.       "IDC Amount
      MOVE SAVE_TABLE-ZSCHM    TO BIG_TABLE-ZSCHM.       "Interest Prof.
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
        AND INACT <> 'X'               "Ignore Inactives
        AND STAT IN ('I0002', 'I0045', 'I0046')
      ORDER BY STAT.
    CASE JEST-STAT.
      WHEN 'I0046'.                    "Status Complete
        MOVE 'yes'     TO FLAGX.
        MOVE JEST-STAT TO STATUS.
      WHEN OTHERS.
        MOVE JEST-STAT TO STATUS.
    ENDCASE.
  ENDSELECT.
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
