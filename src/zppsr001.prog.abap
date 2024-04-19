REPORT ZPPSR001 NO STANDARD PAGE HEADING LINE-COUNT 64 LINE-SIZE 132.
********************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                          *
* Programmer: M DeMeester                                          *
*       Date: September 1997                                       *
* Request ID: DRPP0199                                             *
*                                                                  *
* The following program will generate a Project Summary Report     *
* by project control number within project type.  This program will*
* break on the project types and will calculate all wbs-elements   *
* total related to their project number.  A grand total is         *
* displayed for the total plan.                                    *
* Months selected are from Jan to Dec.                             *
* BUDGETING: level 1                                               *
* PLANNING & ACTUALS at any level (although usually the lowest     *
********************************************************************
* 10/01/19 lritchie TR 582 Add cost element to selection screen
* 01/10/11 mokhan Issue Log: 926
*                      Early Watch report recommends to improve the
*                      performance as it's taking a lot of time and
*                      load. Program is changed to not to call the
*                      function module to get the character field and
*                      look into the tables diectly that provide
*                      character fields.
* 98/04/09 md7140      Add month selection to variant (actuals)
* 98/03/23 md7140 #--- Change in template definition. Added company code
*                      selection
* 97/10/16 md7140 #--- Added COSS table
* 97/09/12 md7140 #--- Development Request DRPP0199                 *
***** ***** ***** ***** ****** ***** ***** ***** ***** ***** ***** *****
*  modified by Nancy Gilligan, OmniLogic Oct 21, 1998    D30K906198    *
*   (marked with OMNING)                                               *
*    - added version numbers to headers for planned values             *
*    - changed budget and actuals to select only version '000'         *
************************************************************************
TABLES: AUSB,  "Table plant status
        BPJA,  "Annual Budget Values
        CABN,  "Characteristics
        COSP,  "CO Object:  Cost Totals - External Postings
        COSS,  "CO Object:  Cost Totals - Internal Postings
        PROJ,  "Project Definition
        PRPS,  "WBS (Work Breakdown Structure) Element Master Data
        TCJ1T, "Project Types
        AUSP,
        T247.  "Month name
CONSTANTS: BLANKVAR(132) TYPE C VALUE ''.    "Writes blanks

DATA:  FLAGPRT(3)        TYPE C VALUE 'no',
       MNR              LIKE T247-MNR.

DATA:   FIRST(2) TYPE N VALUE '01',                   "Month periods
        LAST(2)  TYPE N VALUE '12',                   "Month Periods
        POST1    LIKE PROJ-POST1,                    "Description
        ATWRT    LIKE AUSP-ATWRT,                    "Project Control #
        WBS      LIKE PRPS-POSID,                   "WBS element
        PL_AMT   LIKE COSP-WKG001,
        ACT_AMT  LIKE COSP-WKG001,
        FMONTH   LIKE T247-LTX,
        LMONTH   LIKE T247-LTX.


* Table for control-break processing.
DATA:  BEGIN OF MAINTAB OCCURS 10000,
         PRATX    LIKE TCJ1T-PRATX,                   "Type - Descriptio
         ATWRT    LIKE AUSP-ATWRT,                    "Project Control #
         WBS      LIKE PRPS-POSID,                    "WBS element
         PRART    LIKE TCJ1T-PRART,                   "Project Type
         KSTAR    LIKE COSP-KSTAR,                    "Cost Element
         GJAHR    LIKE COSP-GJAHR,                    "Fiscal Year
         PL_AMT   LIKE COSP-WKG001,                   "Yearly Planned
         BUDG_AMT LIKE COSP-WKG001,                   "Yearly Budget
         ACT_AMT  LIKE COSP-WKG001,                   "Yearly Actual
         VAR_AMT  LIKE COSP-WKG001,                   "Yearly Variance
         POST1    LIKE PROJ-POST1,                    "Description
   END OF MAINTAB.

DATA: BEGIN OF WACOSP OCCURS 0.
      INCLUDE STRUCTURE COSP.
DATA: END OF WACOSP.

DATA: BEGIN OF WACOSS OCCURS 0.
      INCLUDE STRUCTURE COSS.
DATA: END OF WACOSS.

*DATA: BEGIN OF CHAR_TAB OCCURS 20.                      "Issuelog 926
*      INCLUDE STRUCTURE AUSP.                           "Issuelog 926
*DATA: END OF CHAR_TAB.                                  "Issuelog 926

DATA: BEGIN OF CHAR_TAB OCCURS 0,                        "Issuelog 926
         OBJEK LIKE AUSP-OBJEK,                          "Issuelog 926
         ATWRT LIKE AUSP-ATWRT,                          "Issuelog 926
       END OF CHAR_TAB.                                  "Issuelog 926

DATA:  OBJECT         LIKE AUSP-OBJEK,
       CHARIC         LIKE CABN-ATNAM,
       G_ATINN        LIKE CABN-ATINN,
       G_ATINN_NEW    LIKE CABN-ATINN,
       TMPVAL(2)      TYPE P,
       FLAG1(1)       TYPE C VALUE 'N'.

SELECTION-SCREEN BEGIN OF BLOCK BEGIN WITH FRAME.
PARAMETERS: P_VBUKR LIKE PROJ-VBUKR DEFAULT 'UGL' OBLIGATORY  "Company
                    MODIF ID ABC.
SELECT-OPTIONS:
            S_DIV   FOR PROJ-VERNR MODIF ID ABC,               "Div
            S_PROJ FOR PROJ-PSPID MODIF ID ABC,               "Projects
            S_TYPE FOR PRPS-PRART MODIF ID ABC                "Type
              DEFAULT '01',
            S_ATWRT FOR AUSP-ATWRT MODIF ID ABC.   "Project Control #
PARAMETERS: P_VERSN LIKE TKA09-VERSN OBLIGATORY DEFAULT '000' "Version
                    MODIF ID ABC,
            P_YEAR  LIKE COSP-GJAHR DEFAULT SY-DATUM(4)       "Year
                    OBLIGATORY MODIF ID ABC.
SELECT-OPTIONS:
            S_MTH   FOR SY-DATUM+4(2) OBLIGATORY DEFAULT '01'
                    MODIF ID ABC.
select-options s_kstar for cosp-kstar modif id abc.         "2010/01/19


SELECTION-SCREEN END OF BLOCK BEGIN.

* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON S_MTH.
 SELECT SINGLE * FROM T247         "Begin month name
   WHERE SPRAS = SY-LANGU
     AND  MNR = S_MTH+3(2).
   FMONTH = T247-LTX.

 CLEAR LMONTH.
 IF S_MTH+5(2) <> 0.
   SELECT * FROM T247
     WHERE SPRAS = SY-LANGU
       AND  MNR = S_MTH+5(2).
     LMONTH = T247-LTX.
    ENDSELECT.
 ENDIF.

 IF S_MTH+5(2) = 0.
    S_MTH+5(2) = S_MTH+3(2).
 ENDIF.
************************************************************************

START-OF-SELECTION.
 PERFORM INITIALIZE.

 PERFORM GET_ATINN.
*SORT CHAR_TAB BY ATINN.                                 "Issuelog 926

 PERFORM GET_DATA.
 PERFORM WRITE_DATA.
END-OF-SELECTION.

* This routine initializes the internal tables.
FORM INITIALIZE.
 REFRESH: MAINTAB.
 CLEAR:   MAINTAB.
 MOVE 'PROJECT_CNTR_NUMBER'  TO CHARIC.
ENDFORM.

*-------------------------- GET_DATA -----------------------------------
* This routine gathers the cost element, values, version and projects.
*-----------------------------------------------------------------------
FORM GET_DATA.

SELECT * FROM PROJ WHERE PSPID IN S_PROJ
                     AND VERNR IN S_DIV
                     AND VBUKR = P_VBUKR.
 CLEAR ATWRT.
*if proj-pspid+5(3) co '0123456789'.             "Template test
 IF PROJ-PSPID+5(2) CO '0123456789'.             "Template test 98/03/23
    SELECT * FROM PRPS WHERE PSPHI = PROJ-PSPNR
                         AND PRART IN S_TYPE
                         ORDER BY STUFE.
      CLEAR: MAINTAB.
      SELECT SINGLE * FROM TCJ1T WHERE PRART = PRPS-PRART
                                   AND LANGU = 'E'.
*        clear: pl_amt, act_amt.
      MOVE: PROJ-POST1 TO MAINTAB-POST1.
      MOVE: TCJ1T-PRART TO MAINTAB-PRART,
            TCJ1T-PRATX TO MAINTAB-PRATX,
            PRPS-POSID  TO MAINTAB-WBS.
            MAINTAB-WBS+7 = '0000'.             "Turns WBS into projects
* Only level 1 gets the characteristic value.  All other levels assume
* the same as level 1.
      IF PRPS-STUFE = '1'.
         CONCATENATE: 'PR' PRPS-PSPNR INTO OBJECT.
         PERFORM FIND_CHARACTERISTIC.
         ATWRT = PRPS-USR01. " ADDED BY AKMADASU
      ENDIF.                                  "End of Level 1 test

      MOVE ATWRT TO MAINTAB-ATWRT.

* Planning & actuals are picked up at whatever level they occur
      SELECT * INTO WACOSP FROM COSP                "COSP Table - Ext
        WHERE OBJNR = PRPS-OBJNR
          AND GJAHR = P_YEAR
          AND ( VERSN = P_VERSN
             OR VERSN = '000' )                                  "OMNING
          and kstar in s_kstar                                   "2010/01/18
          AND BEKNZ IN ('S', 'H', 'L')
          AND WRTTP IN ('01','04').
      IF SY-SUBRC = '0'.
         CASE WACOSP-WRTTP.
         WHEN '01'.
             IF WACOSP-VERSN = P_VERSN.                          "OMNING
               ADD WACOSP-WKG001 FROM FIRST TO LAST GIVING PL_AMT.
               MAINTAB-PL_AMT = MAINTAB-PL_AMT + PL_AMT.
             ENDIF.                                              "OMNING
         WHEN '04'.
         IF WACOSP-VERSN = '000'.                                "OMNING
         ADD WACOSP-WKG001 FROM S_MTH+3(2) TO S_MTH+5(2) GIVING ACT_AMT.
         MAINTAB-ACT_AMT = MAINTAB-ACT_AMT + ACT_AMT.
         ENDIF.                                                  "OMNING
         ENDCASE.
      ENDIF.
      ENDSELECT.                                  "End of COSP

      SELECT * INTO WACOSS FROM COSS              "COSS Table - Internal
        WHERE OBJNR = PRPS-OBJNR
          AND GJAHR = P_YEAR
          AND ( VERSN = P_VERSN OR VERSN = '000' )
          and kstar in s_kstar                                   "2010/01/18
*        and beknz in ('S', 'H', 'L')             "mdemeest 98/11/05
          AND WRTTP IN ('01','04').
      IF SY-SUBRC = '0'.
         CASE WACOSS-WRTTP.
         WHEN '01'.
             IF WACOSS-VERSN = P_VERSN.                          "OMNING
               ADD WACOSS-WKG001 FROM FIRST TO LAST GIVING PL_AMT.
               MAINTAB-PL_AMT = MAINTAB-PL_AMT + PL_AMT.
             ENDIF.                                              "OMNING
         WHEN '04'.
         IF WACOSS-VERSN = '000'.                                "OMNING
         ADD WACOSS-WKG001 FROM S_MTH+3(2) TO S_MTH+5(2) GIVING ACT_AMT.
            MAINTAB-ACT_AMT = MAINTAB-ACT_AMT + ACT_AMT.
         ENDIF.                                                  "OMNING
         ENDCASE.
      ENDIF.
   ENDSELECT.                                  "end COSP select
      MOVE: WACOSP-KSTAR  TO MAINTAB-KSTAR,
            WACOSP-GJAHR  TO MAINTAB-GJAHR.


    IF PRPS-STUFE = '1'.
         SELECT * FROM BPJA
           WHERE OBJNR = PRPS-OBJNR
             AND GJAHR = P_YEAR
             AND VERSN = '000'                                "omning
             AND WRTTP IN ('41').
          IF SY-SUBRC = '0'.
            MAINTAB-BUDG_AMT = BPJA-WLJHR.
          ENDIF.
         ENDSELECT.                                   "end BPJA select
       ENDIF.

     IF MAINTAB-PL_AMT NE 0 OR         "Ignore project if no info
        MAINTAB-ACT_AMT NE 0 OR
        MAINTAB-BUDG_AMT NE 0.
         IF MAINTAB-ATWRT IN S_ATWRT.
         MAINTAB-VAR_AMT = MAINTAB-BUDG_AMT - MAINTAB-ACT_AMT.
            APPEND MAINTAB.
         ENDIF.
     ENDIF.
   ENDSELECT.                                      "end PRPS select
  ENDIF.                                           "Template test end
ENDSELECT.                                         "end PROJ select
ENDFORM.
*-------------------------- WRITE_DATA ---------------------------------
* This routine process the data and writes the output.
*-----------------------------------------------------------------------
FORM WRITE_DATA.

 SORT MAINTAB BY PRATX ATWRT WBS.

*loop at maintab.
*write: / 'maintab', maintab-atwrt, maintab-wbs,
*         maintab-pl_amt, maintab-budg_amt, maintab-act_amt,
*         maintab-post1.
*endloop.

LOOP AT MAINTAB.
  AT NEW PRATX.                        "Control break - Project type
     NEW-PAGE.
  ENDAT.

  AT NEW ATWRT.                         "Control-break on control number
    FORMAT COLOR COL_TOTAL ON INTENSIFIED OFF.
    WRITE: /.                                                  "MD7140
    IF MAINTAB-ATWRT = SPACE.                         "Blank Control#
       WRITE: /13 TEXT-015, TEXT-023.
    ELSE.
       WRITE: /13 TEXT-015, MAINTAB-ATWRT, BLANKVAR(7).
    ENDIF.
    FORMAT COLOR 5 OFF.
    FORMAT COLOR 2 ON INTENSIFIED OFF.
    WRITE: BLANKVAR(86).
    FORMAT COLOR 2 OFF INTENSIFIED ON.
    FORMAT RESET.
 ENDAT.

  AT NEW WBS.                           "Control break - WBS/project
     WBS = MAINTAB-WBS.
  ENDAT.

  POST1 = MAINTAB-POST1.

 AT END OF WBS.
    SUM.
    WRITE: / WBS                  UNDER TEXT-008,   "Project Number
             POST1                UNDER TEXT-009,   "Description
            (14) MAINTAB-PL_AMT   UNDER TEXT-010,
            (14) MAINTAB-BUDG_AMT UNDER TEXT-011,
            (14) MAINTAB-ACT_AMT  UNDER TEXT-012.
    IF MAINTAB-VAR_AMT < 0.
       FORMAT COLOR COL_NEGATIVE INTENSIFIED ON INVERSE ON.
    ENDIF.

    WRITE: (14) MAINTAB-VAR_AMT  UNDER TEXT-003.     "Variance
    FORMAT RESET.
 ENDAT.

 AT END OF ATWRT.                         "Subtotal at Control Number
    SUM.
    FORMAT COLOR COL_TOTAL ON INTENSIFIED OFF.
    IF MAINTAB-ATWRT = SPACE.
       WRITE: /5 TEXT-016, 13 TEXT-015, TEXT-023.
    ELSE.
       WRITE: /5 TEXT-016, 13 TEXT-015, MAINTAB-ATWRT.
    ENDIF.

    WRITE:  (14) MAINTAB-PL_AMT   UNDER TEXT-010,
            (14) MAINTAB-BUDG_AMT UNDER TEXT-011,
            (14) MAINTAB-ACT_AMT  UNDER TEXT-012.
    IF MAINTAB-VAR_AMT < 0.
       FORMAT COLOR COL_NEGATIVE INTENSIFIED ON INVERSE ON.
    ENDIF.

    WRITE: (14) MAINTAB-VAR_AMT  UNDER TEXT-003.     "Variance
    FORMAT RESET.
 ENDAT.

 AT END OF PRATX.                         "Subtotal at Project Type
    SUM.

    RESERVE 6 LINES.
    SKIP 1.
    WRITE: /1 SY-ULINE.

    FORMAT COLOR COL_NEGATIVE ON INTENSIFIED ON.
    WRITE: /4 TEXT-013, 13 MAINTAB-PRATX, 43 SY-VLINE.

    WRITE:  (14) MAINTAB-PL_AMT   UNDER TEXT-010,
            (14) MAINTAB-BUDG_AMT UNDER TEXT-011,
            (14) MAINTAB-ACT_AMT  UNDER TEXT-012.
    IF MAINTAB-VAR_AMT < 0.
       FORMAT COLOR COL_NEGATIVE INTENSIFIED ON INVERSE ON.
    ENDIF.

    WRITE: (14) MAINTAB-VAR_AMT  UNDER TEXT-003.     "Variance
    FORMAT RESET.
    WRITE: /43 SY-VLINE, 44 SY-ULINE.
 ENDAT.

 AT LAST.                                 "Report Total
    SUM.

    FORMAT COLOR COL_POSITIVE ON INTENSIFIED ON.
    RESERVE 6 LINES.
    SKIP 1.
    WRITE: /43 SY-VLINE, 44 SY-ULINE.

    WRITE: /13 TEXT-013, 43 SY-VLINE.

    WRITE:  (14) MAINTAB-PL_AMT   UNDER TEXT-010,
            (14) MAINTAB-BUDG_AMT UNDER TEXT-011,
            (14) MAINTAB-ACT_AMT  UNDER TEXT-012.
    IF MAINTAB-VAR_AMT < 0.
       FORMAT COLOR COL_NEGATIVE INTENSIFIED ON INVERSE ON.
    ENDIF.

    WRITE: (14) MAINTAB-VAR_AMT  UNDER TEXT-003.     "Variance
    FORMAT RESET.
    WRITE: /43 SY-VLINE, 44 SY-ULINE.
 ENDAT.
 ENDLOOP.
ENDFORM.


*---------------------------  TOP-OF-PAGE  ----------------------------*
* This event is used to display the main headings.                     *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
 FORMAT COLOR COL_NORMAL ON.
 WRITE: /1 SY-ULINE.
 WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_NEGATIVE,
        50 TEXT-002,
       105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
 WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
          TEXT-005 UNDER TEXT-002,
          TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
 WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERSN, 65 P_YEAR.
 WRITE: / TEXT-021 UNDER TEXT-RPT, P_VBUKR.
 IF LMONTH = SPACE.
  WRITE: 64 FMONTH.
 ELSE.
  WRITE: 60 FMONTH, TEXT-022, LMONTH.
 ENDIF.
 ULINE: /1.
 FORMAT COLOR COL_NORMAL OFF.
 ULINE: /1.                                          "Project Type Box
 FORMAT COLOR COL_NEGATIVE ON.
 WRITE: /1 SY-VLINE, 2 TEXT-007, 13 MAINTAB-PRATX(22), 54 SY-VLINE.
 ULINE: /1(54).
 FORMAT COLOR COL_NEGATIVE OFF.

 FORMAT COLOR COL_GROUP ON.
 FORMAT INTENSIFIED OFF.
 WRITE: /1 TEXT-008, 13 TEXT-009, 54 TEXT-010, 73 TEXT-011,
        92 TEXT-012, 111 TEXT-001.
 WRITE: / TEXT-018 UNDER TEXT-010,
          TEXT-019 UNDER TEXT-011, TEXT-020 UNDER TEXT-012,
          TEXT-003 UNDER TEXT-001.
 FORMAT INTENSIFIED OFF.
 FORMAT COLOR COL_GROUP OFF.
 SKIP 1.

*-------------------------- GET_ATINN ----------------------------------
* Routine used to get the internal character number for project control
*-----------------------------------------------------------------------
FORM GET_ATINN.

  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
           CLASS_TYPE                   = '014'
           FEATURE_NEUTRAL_NAME         = CHARIC
       IMPORTING
           FEATURE_ID                   = G_ATINN
       EXCEPTIONS
           INVALID_CLASS_TYPE           = 1
           MISSING_FEATURE_INFORMATION  = 2
           NO_FEATURE_FOUND             = 3
           NO_FEATURE_VALID             = 4
           NO_LANGUAGE                  = 5
           OTHERS                       = 6.
 IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE CHARAC OF PROJECT_CNTR_NUMBER'.
 ELSE.
 MOVE G_ATINN                 TO G_ATINN_NEW.             "Issuelog 926
 PERFORM BUILD_AUSP.                                      "Issuelog 926
 ENDIF.
ENDFORM.

*---------------------  BUILD_AUSP -----------------------"Issuelog 926
FORM BUILD_AUSP.

     REFRESH CHAR_TAB.
     SELECT OBJEK ATWRT FROM AUSP INTO TABLE CHAR_TAB
            WHERE   ATINN = G_ATINN_NEW  AND
                    MAFID = 'O'          AND
                    KLART = '014'.
     SORT CHAR_TAB BY OBJEK.

ENDFORM.

*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine to get the value of the project control & major indicator
*-----------------------------------------------------------------------
FORM FIND_CHARACTERISTIC.

CLEAR: ATWRT.
  READ TABLE CHAR_TAB WITH KEY OBJEK = OBJECT BINARY SEARCH. "Issulog926
    IF SY-SUBRC EQ 0.
       MOVE CHAR_TAB-ATWRT    TO ATWRT.
    ELSE.
       MOVE '*******'          TO ATWRT.
    ENDIF.


ENDFORM.
