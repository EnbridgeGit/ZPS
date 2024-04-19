REPORT ZPPMR012 NO STANDARD PAGE HEADING LINE-COUNT 90 LINE-SIZE 120
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        April 1998
*  Description:
*     - The purpose of this program is to produce Summary of Capital
*       Expenditures Cost Element Report by Project Type, Priority
*       Code andDiv/dept. - selection is by Cost Element element
************************************************************************
* 98/04/23 md7140 #--- Created program - used ZPPMR010 for start
************************************************************************

TABLES: COSP, COSS, CSKU, JEST, PROJ, PRPS, T001, T247, TCJ1T,
        TCN7T.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       KSTAR         LIKE COSP-KSTAR,           "Cost Element
       PRART         LIKE PRPS-PRART,           "Project Type
       DISTGRP(1)    TYPE C,                    "WA for subtotalling
       PSPRI         LIKE PRPS-PSPRI,           "Priority Type
       VERNR         LIKE PRPS-VERNR,           "Div/dept
       PROJECT       LIKE PRPS-POSID,           "Project (7 digits)
       PSPHI         LIKE PRPS-PSPHI,           "Internal Project No.
       OBJNR         LIKE PRPS-OBJNR,           "Object Number
       POST1         LIKE PROJ-POST1,           "Project Description
       verna          like proj-verna,          "vernr Name
       ACTUAL(8)     TYPE P DECIMALS 2,         "Actual Amount
   END OF BIG_TABLE.


data:   value         like cosp-wkg001,
        actual          like cosp-wkg001,
        LTEXT         LIKE CSKU-LTEXT,
        POST1         LIKE PRPS-POST1,           "Temp - description
        POST2         LIKE PROJ-POST1,           "Temp - description
        VERNA         LIKE PROJ-VERNA,
       KSTAR         LIKE COSP-KSTAR,           "Cost Element
        P_BEGNME(9),
        P_ENDNME(9),
        BEGMTH(2)     TYPE C,
        ENDMTH(2)     TYPE C.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:     P_CCODE LIKE T001T-BUKRS DEFAULT 'UGL'.
SELECT-OPTIONS: SDIV    FOR PRPS-VERNR.                   "Division
PARAMETERS:     P_VERS  LIKE COSP-VERSN DEFAULT '0',      "Plan version
                P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM(4).
SELECT-OPTIONS: SMTH    FOR SY-DATUM+4(2) OBLIGATORY      "Months
                              DEFAULT SY-DATUM+4(2),
                SKSTAR  FOR COSP-KSTAR,                   "Cost elements
                SPRART  FOR PRPS-PRART,                   "Project Type
                SPSPRI  FOR PRPS-PSPRI.                   "Priority Type
SELECTION-SCREEN END OF BLOCK BOX.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-036.
PARAMETER: P_CHKLVL AS CHECKBOX,
           P_CHKPRJ AS CHECKBOX,
           P_CHKDET AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX2.
*------------------------  AT SELECTION-SCREEN  ------------------------

AT SELECTION-SCREEN.
  IF ( P_CHKLVL IS INITIAL ) AND
     ( P_CHKPRJ IS INITIAL ) AND
     ( P_CHKDET IS INITIAL ).
     MESSAGE E100 WITH 'Must select at least one report'.
  ENDIF.

AT SELECTION-SCREEN ON P_CCODE.
 SELECT SINGLE * FROM T001                        "Get Company Name
   WHERE BUKRS = P_CCODE.

AT SELECTION-SCREEN ON SMTH.
 SELECT SINGLE * FROM T247                        "Get Month Name(Begin)
   WHERE SPRAS = 'E'
   AND MNR = SMTH+3(2).
 P_BEGNME = T247-LTX.

 P_ENDNME = SPACE.
 IF SMTH+5(2) <> '00'.
    SELECT SINGLE * FROM T247                      "Get Month Name(End)
       WHERE SPRAS = 'E'
         AND MNR = SMTH+5(2).
         P_ENDNME = T247-LTX.
 ENDIF.
BEGMTH = SMTH+3(2).
ENDMTH = SMTH+5(2).
IF ENDMTH = '00'.
   ENDMTH = BEGMTH.
ENDIF.
*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.
PERFORM PRINT_VARIANT.
SELECT * FROM PROJ
  WHERE VBUKR = P_CCODE                    "Company Code
    AND VERNR IN SDIV
    AND LOEVM <> 'X'.
 IF PROJ-PSPID+5(2) CO '1234567890'.
   SELECT * FROM PRPS
     WHERE PSPHI = PROJ-PSPNR
       AND PBUKR = PROJ-VBUKR                "Company Code
       AND VERNR = PROJ-VERNR               "Division
       AND PKOKR = '10'                      "Controlling area
       AND LOEVM <> 'X'                      "Not flagged deleted
       AND PRART IN SPRART                   "Project type
       AND PSPRI IN SPSPRI                   "Priority type
       AND BELKZ = 'X'.                      "Lowest level
        PERFORM GET_FISCAL_DATA.
   ENDSELECT.                                "End of PRPS
 ENDIF.
 ENDSELECT.                                  "End of PROJ

 IF SY-SUBRC EQ 0.
    PERFORM DISPLAY_TABLE.
 ENDIF.


*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
 FORMAT INTENSIFIED OFF.
 WRITE: /1 TEXT-RPT, SY-REPID,                          "Report Id
        50 T001-BUTXT,                                  "Company Name
        93 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
 WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,  "Client
       33 TEXT-003,                                     "Report Title
          TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.            "Page Number
 WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS,              "Version
       47 TEXT-004, P_FYEAR.                            "Fiscal Year
 WRITE: / TEXT-037.                                 "Divisions Requested
 IF SDIV+3(8) = 0.
    WRITE: 11 TEXT-038.
 ELSEIF SDIV+11(8) = 0.
    WRITE: 11 SDIV+3(8).
 ELSE.
    WRITE: 11 SDIV+1(2), SDIV+3(8), SDIV+11(8).    "End divisions
 ENDIF.
 IF P_ENDNME = SPACE.                                   "Time Frame
    WRITE: 54 P_BEGNME.
 ELSE.
    WRITE: 42 P_BEGNME, TEXT-024, P_ENDNME, TEXT-025.
 ENDIF.
 FORMAT INTENSIFIED ON.
 PERFORM PRINT_HEADINGS.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*--------------------------  DISPLAY_TABLE  ----------------------------
* This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

 SORT BIG_TABLE BY KSTAR PRART DISTGRP PSPRI VERNR PROJECT.
 IF P_CHKLVL = 'X'.                "Summarized at wbs level
    NEW-PAGE.
    LOOP AT BIG_TABLE.

      AT END OF KSTAR.                              "WBS element
        SUM.
        PERFORM PRINT_VERT.
        PERFORM GET_KSTAR_DEFINITION.
        WRITE: 2 BIG_TABLE-KSTAR, LTEXT.
        PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT LAST.                                     "Company TOTAL
         SUM.
         ULINE.
         PERFORM PRINT_VERT.
         WRITE: 2 TEXT-029.
         PERFORM AMOUNT_TOTALS.
         ULINE.
         WRITE: /.
         WRITE: /40 TEXT-028.
      ENDAT.
   ENDLOOP.
 ENDIF.

 IF P_CHKPRJ = 'X'.                "Summarized at project type
    NEW-PAGE.
    LOOP AT BIG_TABLE.
      MOVE BIG_TABLE-VERNA    TO VERNA.

      AT NEW KSTAR.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        PERFORM GET_KSTAR_DEFINITION.
        WRITE: 2 BIG_TABLE-KSTAR, LTEXT.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT NEW PRART.
         FORMAT COLOR COL_POSITIVE.
         PERFORM GET_PROJECT_TYPE.
         PERFORM PRINT_VERT.
         WRITE: 7 TCJ1T-PRATX.
         FORMAT COLOR COL_POSITIVE.
      ENDAT.

      AT NEW PSPRI.
         FORMAT COLOR COL_HEADING.
         PERFORM GET_PRIORITY_TYPE.
         PERFORM PRINT_VERT.
         WRITE: 11 TCN7T-KTEXT.
         FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF VERNR.
         SUM.
         PERFORM PRINT_VERT.
         WRITE: 13 VERNA.
         PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF PSPRI.
         SUM.
         FORMAT COLOR COL_HEADING.
         PERFORM GET_PRIORITY_TYPE.
         PERFORM PRINT_VERT.
         WRITE: 11 TCN7T-KTEXT.
         PERFORM AMOUNT_TOTALS.
         FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF DISTGRP.
        IF BIG_TABLE-DISTGRP <> 'Z'.
          SUM.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
          PERFORM PRINT_VERT.
          IF BIG_TABLE-DISTGRP = '1'.
             WRITE: 9 TEXT-033.           "TOTAL NEW BUSINESS
          ELSEIF BIG_TABLE-DISTGRP = '2'. "TOTAL ADDITIONS
             WRITE: 9 TEXT-035.
          ELSEIF BIG_TABLE-DISTGRP = '3'. "TOTAL REPLACEMENTS
             WRITE: 9 TEXT-034.
          ENDIF.
          PERFORM AMOUNT_TOTALS.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
        ENDIF.
      ENDAT.

      AT END OF PRART.             "Summarized at project level
         SUM.
         PERFORM GET_PROJECT_TYPE.
         FORMAT COLOR COL_POSITIVE.
         PERFORM PRINT_VERT.
         WRITE: 7 TCJ1T-PRATX(35).
         FORMAT COLOR COL_BACKGROUND.
         PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF KSTAR.             "Summarized at WBS element
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 81(89) SY-ULINE.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        WRITE: 2 BIG_TABLE-KSTAR.
        PERFORM AMOUNT_TOTALS.
        FORMAT COLOR COL_BACKGROUND.
        ULINE.
      ENDAT.

      AT LAST.                                     "Company TOTAL
         SUM.
         ULINE.
         PERFORM PRINT_VERT.
         WRITE: 2 TEXT-029.
         PERFORM AMOUNT_TOTALS.
         ULINE.
         WRITE: /.
         WRITE: /40 TEXT-028.
      ENDAT.
   ENDLOOP.
 ENDIF.

 IF P_CHKDET = 'X'.                "Summarized at project type
    NEW-PAGE.
    LOOP AT BIG_TABLE.
      MOVE BIG_TABLE-POST1    TO POST2.
      MOVE BIG_TABLE-VERNA    TO VERNA.

      AT NEW KSTAR.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        PERFORM GET_KSTAR_DEFINITION.
        WRITE: 2 BIG_TABLE-KSTAR, LTEXT.
        FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT NEW PRART.
         FORMAT COLOR COL_POSITIVE.
         PERFORM GET_PROJECT_TYPE.
         PERFORM PRINT_VERT.
         WRITE: 7 TCJ1T-PRATX(35).
         FORMAT COLOR COL_POSITIVE.
      ENDAT.

      AT NEW PSPRI.
         FORMAT COLOR COL_HEADING.
         PERFORM GET_PRIORITY_TYPE.
         PERFORM PRINT_VERT.
         WRITE: 11 TCN7T-KTEXT.
         FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF PROJECT.

         SUM.
         PERFORM PRINT_VERT.
         WRITE: 15 BIG_TABLE-PROJECT(10), POST2.
         PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF VERNR.
         SUM.
         PERFORM PRINT_VERT.
         WRITE: 40(80) SY-ULINE.
         PERFORM PRINT_VERT.
         WRITE: 13 VERNA.
         PERFORM AMOUNT_TOTALS.
         PERFORM PRINT_VERT.
         WRITE: 40(80) SY-ULINE.
      ENDAT.

      AT END OF PSPRI.
         SUM.
         FORMAT COLOR COL_HEADING.
         PERFORM GET_PRIORITY_TYPE.
         PERFORM PRINT_VERT.
         WRITE: 11 TCN7T-KTEXT.
         PERFORM AMOUNT_TOTALS.
         FORMAT COLOR COL_BACKGROUND.
      ENDAT.

      AT END OF DISTGRP.
        IF BIG_TABLE-DISTGRP <> 'Z'.
          SUM.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
          PERFORM PRINT_VERT.
          IF BIG_TABLE-DISTGRP = '1'.
             WRITE: 9 TEXT-033.           "TOTAL NEW BUSINESS
          ELSEIF BIG_TABLE-DISTGRP = '2'. "TOTAL ADDITIONS
             WRITE: 9 TEXT-035.
          ELSEIF BIG_TABLE-DISTGRP = '3'. "TOTAL REPLACEMENTS
             WRITE: 9 TEXT-034.
          ENDIF.
          PERFORM AMOUNT_TOTALS.
          PERFORM PRINT_VERT.
          WRITE: 9(161) SY-ULINE.
        ENDIF.
      ENDAT.

      AT END OF PRART.             "Summarized at project level
         SUM.
         PERFORM GET_PROJECT_TYPE.
         FORMAT COLOR COL_POSITIVE.
         PERFORM PRINT_VERT.
         WRITE: 7 TCJ1T-PRATX(35).
         FORMAT COLOR COL_BACKGROUND.
         PERFORM AMOUNT_TOTALS.
      ENDAT.

      AT END OF KSTAR.             "Summarized at Cost element
        SUM.
        PERFORM PRINT_VERT.
        WRITE: 81(89) SY-ULINE.
        PERFORM PRINT_VERT.
        FORMAT COLOR COL_GROUP.
        WRITE: 2 BIG_TABLE-KSTAR.
        PERFORM AMOUNT_TOTALS.
        FORMAT COLOR COL_BACKGROUND.
        ULINE.
      ENDAT.

      AT LAST.                                     "Company TOTAL
         SUM.
         ULINE.
         PERFORM PRINT_VERT.
         WRITE: 2 TEXT-029.
         PERFORM AMOUNT_TOTALS.
         ULINE.
         WRITE: /.
         WRITE: /40 TEXT-028.
      ENDAT.
   ENDLOOP.
 ENDIF.
ENDFORM.

*------------------------  AMOUNT_TOTALS  ------------------------------
FORM AMOUNT_TOTALS.
  WRITE: BIG_TABLE-ACTUAL     UNDER TEXT-010.   "Actuals
ENDFORM.
*----------------------- PRINT_HEADINGS -------------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
   FORMAT COLOR 2 ON.
   WRITE: /1(120) SY-ULINE.
   PERFORM PRINT_VERT.
   PERFORM PRINT_VERT.
   WRITE: 7 TEXT-005, 18 TEXT-007,
          81 TEXT-010.
   ULINE.
   PERFORM PRINT_VERT.
   FORMAT COLOR 2 OFF.
ENDFORM.

*--------------------------  PRINT_VERT  -------------------------------
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:  /1 SY-VLINE,  80 SY-VLINE,
          98 SY-VLINE.
ENDFORM.

form heading_vernr.
      PERFORM PRINT_VERT.
      FORMAT COLOR COL_POSITIVE.
      WRITE: 2 TEXT-023, VERNA.
      FORMAT COLOR COL_BACKGROUND.
ENDFORM.

FORM HEADING_PROJECT_TYPE.
      SELECT SINGLE * FROM TCJ1T
          WHERE LANGU = 'E' AND
                PRART = BIG_TABLE-PRART.
          PERFORM PRINT_VERT.
          FORMAT COLOR COL_TOTAL.
          WRITE: 4 TCJ1T-PRATX.
          FORMAT COLOR COL_BACKGROUND.
ENDFORM.

FORM GET_PROJECT_TYPE.
  SELECT SINGLE * FROM TCJ1T
      WHERE LANGU = 'E'
        AND PRART = BIG_TABLE-PRART.
ENDFORM.

FORM GET_PRIORITY_TYPE.
  SELECT SINGLE * FROM TCN7T
    WHERE LANGU = 'E'
      AND NPRIO = BIG_TABLE-PSPRI.
ENDFORM.

FORM HEADING_PRIORITY_TYPE.
  SELECT SINGLE * FROM TCN7T
    WHERE LANGU = 'E'
      AND NPRIO = BIG_TABLE-PSPRI.
    FORMAT COLOR COL_POSITIVE.
    IF SY-SUBRC <> 0.
       PERFORM PRINT_VERT.
       WRITE: 6 TEXT-021, BIG_TABLE-PSPRI, TEXT-022.
    ELSE.
       PERFORM PRINT_VERT.
       WRITE: 4 TCN7T-KTEXT.
    ENDIF.
    FORMAT COLOR COL_BACKGROUND.
ENDFORM.

FORM GET_FISCAL_DATA.
*write: / wa-prart, wa-pspri, wa-posid, wa-objnr, wa-psphi, wa-plakz.
* COST TOTALS - External Postings
 SELECT * FROM COSP
   WHERE OBJNR = PRPS-OBJNR          "Matching objects
     AND GJAHR = P_FYEAR             "Fiscal Year selected
     AND VERSN = P_VERS              "Version
     AND KSTAR IN SKSTAR             "Cost Element
     AND WRTTP IN ('04')             "Record with actuals
     AND BEKNZ IN ('S','H','L').     "Debit/Credit Indicator
     ADD COSP-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.
     MOVE COSP-KSTAR TO KSTAR.

       CASE COSP-WRTTP.
            WHEN '04'.
                 actual = actual + value.
                 CLEAR VALUE.
       ENDCASE.

   IF ACTUAL <> 0.
      PERFORM BUILD_TABLE.
      CLEAR: ACTUAL.
   ENDIF.
   ENDSELECT.


* COST TOTALS - Internal Postings
 SELECT * FROM COSS
   WHERE OBJNR = PRPS-OBJNR                     "Matching objects
     AND GJAHR = P_FYEAR                      "Fiscal Year selected
     AND VERSN = P_VERS                       "Version
     AND WRTTP IN ('04')                      "Record with actuals
     AND KSTAR IN SKSTAR.                     "Cost Element
*    and beknz in ('S','H','L').              "Debit/Credit Indicator
   ADD COSS-WKG001 FROM BEGMTH TO ENDMTH GIVING VALUE.
   MOVE COSS-KSTAR TO KSTAR.

   CASE COSS-WRTTP.
     WHEN '04'.
       ACTUAL = ACTUAL + VALUE.
       CLEAR VALUE.
   ENDCASE.
* End of COST TOTALS - Internal Postings

   IF ACTUAL <> 0.
      PERFORM BUILD_TABLE.
      CLEAR: ACTUAL.
   ENDIF.
 ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BUILD_TABLE
*-----------------------------------------------------------------------
*   Description:
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE.
    CLEAR: BIG_TABLE-ACTUAL.
    MOVE 'Z'               TO BIG_TABLE-DISTGRP.
    IF PRPS-PRART BETWEEN '01' AND '05'.
       IF PRPS-PSPRI CO  '1234'.
          MOVE '1'         TO BIG_TABLE-DISTGRP.
       ELSEIF PRPS-PSPRI CO '58'.
          MOVE '2'         TO BIG_TABLE-DISTGRP.
       ELSEIF PRPS-PSPRI CO '79CDE'.
          MOVE '3'         TO BIG_TABLE-DISTGRP.
       ENDIF.
    ENDIF.
    MOVE      KSTAR          TO BIG_TABLE-KSTAR.
    MOVE PRPS-POSID(7)       TO BIG_TABLE-PROJECT.
    MOVE PRPS-POSID(2)       TO BIG_TABLE-VERNR.
    MOVE PRPS-PRART          TO BIG_TABLE-PRART.
    MOVE PRPS-PSPRI          TO BIG_TABLE-PSPRI.
    MOVE PRPS-PSPHI          TO BIG_TABLE-PSPHI.
    MOVE PROJ-POST1          TO BIG_TABLE-POST1.
    MOVE PROJ-VERNA        TO BIG_TABLE-VERNA.
    MOVE PRPS-OBJNR        TO BIG_TABLE-OBJNR.
    MOVE ACTUAL            TO BIG_TABLE-ACTUAL.

*   if  big_table-actual <> 0.           "Eliminate lines with all zeros
        APPEND BIG_TABLE.
*   endif.
ENDFORM.

FORM GET_KSTAR_DEFINITION.
 CLEAR LTEXT.
 SELECT SINGLE * FROM CSKU
   WHERE SPRAS = SY-LANGU
     AND KTOPL = 'COAT'
     AND KSTAR = BIG_TABLE-KSTAR.
 IF SY-SUBRC = '0'.
    LTEXT = CSKU-LTEXT.
 ENDIF.
ENDFORM.

*-------------------- PRINT_VARIANT ------------------------------------
FORM PRINT_VARIANT.
WRITE: /20 TEXT-040, 40 TEXT-039.
WRITE: /.
WRITE: / TEXT-041 UNDER TEXT-040, P_CCODE UNDER TEXT-039.  "Company
WRITE: /.

WRITE: / TEXT-042 UNDER TEXT-040.                       "Divisions
LOOP AT SDIV.
  IF SDIV+1(2) = 'EQ'.
     WRITE: SDIV+1(2) UNDER TEXT-039, SDIV+3(8).
  ELSE.
     WRITE: SDIV+1(2) UNDER TEXT-039, SDIV+3(8), SDIV+11(8).
  ENDIF.
  WRITE: /.
ENDLOOP.

WRITE: / TEXT-043 UNDER TEXT-040, P_VERS UNDER TEXT-039.  "Version
WRITE: /.
WRITE: / TEXT-044 UNDER TEXT-040, P_FYEAR UNDER TEXT-039. "Fiscal Year
WRITE: /.

WRITE: / TEXT-045 UNDER TEXT-040.                         "Months
LOOP AT SMTH.
  IF SMTH+1(2) = 'EQ'.
     WRITE: SMTH+1(2) UNDER TEXT-039, SMTH+3(2).
  ELSE.
     WRITE: SMTH+1(2) UNDER TEXT-039, SMTH+3(2), SMTH+5(2).
  ENDIF.
  WRITE: /.
ENDLOOP.

WRITE: / TEXT-046 UNDER TEXT-040.                         "Cost Element
IF SKSTAR+1(2) = SPACE. WRITE: /. ENDIF.
LOOP AT SKSTAR.
  IF SKSTAR+1(2) = 'EQ'.
     WRITE: SKSTAR+1(2) UNDER TEXT-039, SKSTAR+3(10).
  ELSE.
     WRITE: SKSTAR+1(2) UNDER TEXT-039, SKSTAR+3(10), SKSTAR+13(10).
  ENDIF.
  WRITE: /.
ENDLOOP.

WRITE: / TEXT-047 UNDER TEXT-040.                         "Project Type
IF SPRART+1(2) = SPACE.
   WRITE: /.
ENDIF.
LOOP AT SPRART.
  IF SPRART+1(2) = 'EQ'.
     WRITE: SPRART+1(2) UNDER TEXT-039, SPRART+3(2).
  ELSE.
     WRITE: SPRART+1(2) UNDER TEXT-039, SPRART+3(2), SPRART+5(2).
  ENDIF.
  WRITE: /.
ENDLOOP.

WRITE: / TEXT-048 UNDER TEXT-040.                         "Priority Type
LOOP AT SPSPRI.
  IF SPSPRI+1(2) = 'EQ'.
     WRITE: SPSPRI+1(2) UNDER TEXT-039, SPSPRI+3(1).
  ELSE.
     WRITE: SPSPRI+1(2) UNDER TEXT-039, SPSPRI+3(1), SPSPRI+4(1).
  ENDIF.
  WRITE: /.
ENDLOOP.

ENDFORM.
