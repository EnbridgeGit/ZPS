REPORT ZPWBR004 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        January 1997
*  Description:
*     - The purpose of this program is to produce a summary by Project
*       Type broken down by WBS Element & Cost Element
************************************************************************
*  CHANGES/CORRECTIONS
* 98/03/23 md7140 #--- Change in template definition
* 98/01/07 md7140 #321 Changed BUILD_TABLE_COSS
* 97/10/06 md7140 #225 Changed Trans $(WTG0nn) to CO $(WKG0nn)
* 97/07/22 md7140 added project selection criteria for H. Allison
* 97/03/31 MD7140 Added COSS table for internal postings
* 97/02/19 MD7140 Added numeric check - Project templates have
*                 characters 99-99-xxx-999 and should not be included
************************************************************************

TABLES:   COSP, COSS, CSKA, CSKU, PRPS, T247, T001, TCJ1T, TCJ04.

DATA:
    BEGIN OF WA OCCURS 10000,
        OBJNR         LIKE PRPS-OBJNR, "Object Number
        PRART         LIKE PRPS-PRART, "Project Type
        POSID         LIKE PRPS-POSID, "WBS Element
        VERNR         LIKE PRPS-VERNR, "Responsibility
    END OF WA.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
        VERNR         LIKE PRPS-VERNR, "Responsibility
        PRART         LIKE PRPS-PRART, "Project Type
        KSTAR         LIKE COSP-KSTAR, "Cost Element
        OBJNR         LIKE PRPS-OBJNR, "Object Number
        POSID         LIKE PRPS-POSID, "WBS Element
        VALUE         LIKE COSP-WKG001,"Actual Amount
    END OF BIG_TABLE.
DATA:
    BEGIN OF BIG_TABLE2 OCCURS 10000,
*       vernr         like prps-vernr,           "Responsibility
        PRART         LIKE PRPS-PRART, "Project Type
        KSTAR         LIKE COSP-KSTAR, "Cost Element
        VALUE         LIKE COSP-WKG001,"Actual Amount
    END OF BIG_TABLE2.
DATA:
    BEGIN OF BIG_TABLE3 OCCURS 10000,
*       vernr         like prps-vernr,           "Responsibility
*       prart         like prps-prart,           "Project Type
        KSTAR         LIKE COSP-KSTAR, "Cost Element
        VALUE         LIKE COSP-WKG001,"Actual Amount
    END OF BIG_TABLE3.

DATA:   VALUE         LIKE COSP-WKG001,
        GROUP(9),
        LEN           TYPE I,
        SYMBOL(4)     TYPE C,
        START         TYPE I,
        SORT-SEQ(50)  TYPE C,
        DESC          LIKE CSKU-MCTXT. "Cost Element Desc


DATA: BEGIN OF NAME_TAB OCCURS 10.
        INCLUDE STRUCTURE RGSB4.
DATA: END OF NAME_TAB.

DATA:    BEGIN OF ALLCOST_TAB OCCURS 1000,
            COSTEL     LIKE    CSKA-KSTAR,
            DESCRIPT   LIKE    CSKU-LTEXT,
            GRNAME     LIKE    RGSB4-TITLE,
         END OF ALLCOST_TAB.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS: P_CCODE LIKE T001T-BUKRS DEFAULT 'UGL' OBLIGATORY,
            P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
            P_MONTH(2)              DEFAULT SY-DATUM+4(2).
SELECT-OPTIONS: S_VERNR FOR PRPS-VERNR,"Responsible Persons
                S_PRART FOR PRPS-PRART."Project Type
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECTION-SCREEN COMMENT 1(79) TEXT-017.
PARAMETERS:     P_COSTGR  LIKE    KKB0-KST.
SELECT-OPTIONS: S_KSTAR FOR COSP-KSTAR."Cost element
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECTION-SCREEN COMMENT 1(79) TEXT-018.
PARAMETERS: P_CHECK1 AS CHECKBOX,      "By Div/ProjType/Cost Element
            P_CHECK2 AS CHECKBOX,      "By ProjType/Cost Element
            P_CHECK3 AS CHECKBOX.      "By Cost Element
SELECTION-SCREEN END OF BLOCK BOX2.




* End of selection screen
*-----------------------------------------------------------------------

START-OF-SELECTION.
  PERFORM GET_COST_RANGE_NAME USING P_COSTGR.
  PERFORM GET_ALL_COST_EL.

  IF  S_KSTAR NE SPACE.
    PERFORM GET_ADDITIONAL_COST_EL.
  ENDIF.

  SORT ALLCOST_TAB BY COSTEL.

  PERFORM SELECT_ALL_RECORDS.

*---------------------------------------------------------------------*
*       FORM SELECT_ALL_RECORDS                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SELECT_ALL_RECORDS.
  SELECT OBJNR PRART POSID VERNR       "Select required values
      INTO WA
      FROM PRPS WHERE PBUKR = P_CCODE  "Company Code
                 AND PKOKR = '10'      "Controlling area
                 AND VERNR IN S_VERNR  "Responsible Person
                 AND PRART IN S_PRART  "Project Type
      ORDER BY PRART.                  "in Project Type order
    IF  WA-POSID+5(2) CO '1234567890'.                      "98/03/23
      SELECT * FROM COSP               "EXTERNAL POSTINGs
          WHERE OBJNR = WA-OBJNR  AND  "Matching objects
                GJAHR = P_FYEAR   AND  "Fiscal Year selected
                WRTTP = '04'.          "Record with actuals
*                     kstar in s_kstar.
*               perform calc_value.                                "#225
        CLEAR VALUE.                   "#225
        ADD COSP-WKG001 FROM 1 TO P_MONTH GIVING VALUE.    "#225
        PERFORM BUILD_TABLE.
      ENDSELECT.

      SELECT * FROM COSS               "INTERNAL POSTINGS
          WHERE OBJNR = WA-OBJNR  AND  "Matching objects
                GJAHR = P_FYEAR   AND  "Fiscal Year selected
                WRTTP = '04'.          "Record with actuals
*                     kstar in s_kstar.
*               perform calc_value_coss.                           "#225
        CLEAR VALUE.                   "#225
        ADD COSS-WKG001 FROM 1 TO P_MONTH GIVING VALUE.    "#225
        PERFORM BUILD_TABLE_COSS.
      ENDSELECT.
    ENDIF.

  ENDSELECT.

  IF SY-SUBRC EQ 0.
    LOOP AT BIG_TABLE.
      MOVE  BIG_TABLE-PRART TO BIG_TABLE2-PRART.
      MOVE: BIG_TABLE-KSTAR TO BIG_TABLE2-KSTAR,
            BIG_TABLE-KSTAR TO BIG_TABLE3-KSTAR.
      MOVE: BIG_TABLE-VALUE TO BIG_TABLE2-VALUE,
            BIG_TABLE-VALUE TO BIG_TABLE3-VALUE.
      APPEND: BIG_TABLE2, BIG_TABLE3.
    ENDLOOP.

    IF  P_CHECK1 = 'X'.
      NEW-PAGE.
      MOVE TEXT-019 TO SORT-SEQ.
      PERFORM DISPLAY_TABLE.      "By Division/Project Type/Cost Element
    ENDIF.
    IF  P_CHECK2 = 'X'.
      NEW-PAGE.
      MOVE TEXT-020 TO SORT-SEQ.
      PERFORM DISPLAY_TABLE2.          "By Project Type/Cost Element
    ENDIF.
    IF  P_CHECK3 = 'X'.
      NEW-PAGE.
      MOVE TEXT-021 TO SORT-SEQ.
      PERFORM DISPLAY_TABLE3.          "By Cost Element
    ENDIF.
  ENDIF.


ENDFORM.                               "end of select_all_records
*-----------------------------------------------------------------------

TOP-OF-PAGE.
  SELECT SINGLE * FROM T247            "Month Name
      WHERE SPRAS = 'E' AND
            MNR = P_MONTH.

  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-RPT, SY-REPID,
        30 TEXT-001,                   "Title
        90 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.           "Date/Time
  WRITE: / TEXT-CLT, SY-MANDT UNDER SY-REPID,
           TEXT-010 UNDER TEXT-001, 55 T247-LTX,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM. "Page
  WRITE: / SORT-SEQ UNDER TEXT-001.    "Type of Summary

  SELECT SINGLE * FROM T001            "Company Name
       WHERE BUKRS = P_CCODE.
  WRITE: /40 T001-BUTXT.

  ULINE.
  FORMAT INTENSIFIED ON.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*----------------------- BUILD_TABLE ----------------------------------
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE.

  LOOP AT ALLCOST_TAB
      WHERE COSTEL = COSP-KSTAR.
    MOVE WA-VERNR            TO BIG_TABLE-VERNR.
    MOVE WA-OBJNR            TO BIG_TABLE-OBJNR.
    MOVE WA-PRART            TO BIG_TABLE-PRART.
    MOVE WA-POSID            TO BIG_TABLE-POSID.
    MOVE COSP-KSTAR          TO BIG_TABLE-KSTAR.
    MOVE VALUE               TO BIG_TABLE-VALUE.
    APPEND BIG_TABLE.
    CLEAR BIG_TABLE.
  ENDLOOP.

ENDFORM.

*--------------------------  BUILD_TABLE_COSS --------------------------
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE_COSS.

  LOOP AT ALLCOST_TAB
      WHERE COSTEL = COSS-KSTAR.       "#321
*     where costel = cosp-kstar.                               "#321
    MOVE WA-OBJNR            TO BIG_TABLE-OBJNR.
    MOVE WA-PRART            TO BIG_TABLE-PRART.
    MOVE WA-POSID            TO BIG_TABLE-POSID.
    MOVE COSS-KSTAR          TO BIG_TABLE-KSTAR.
    MOVE WA-VERNR            TO BIG_TABLE-VERNR.
    MOVE VALUE               TO BIG_TABLE-VALUE.
    APPEND BIG_TABLE.
    CLEAR BIG_TABLE.
  ENDLOOP.
ENDFORM.

*------------------------  DISPLAY_TABLE  ------------------------------
*   - This subroutine displayes data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

  PERFORM PRINT_HEADINGS.
*  sort big_table by prart group kstar.
  SORT BIG_TABLE BY VERNR PRART KSTAR.
  LOOP AT BIG_TABLE.
    AT NEW VERNR.
      SELECT SINGLE * FROM TCJ04
          WHERE VERNR = BIG_TABLE-VERNR.
      PERFORM PRINT_VERT.
      WRITE:  BIG_TABLE-VERNR UNDER TEXT-002, TCJ04-VERNA.
    ENDAT.

    AT NEW PRART.
      SELECT SINGLE * FROM TCJ1T
         WHERE LANGU = SY-LANGU
         AND PRART = BIG_TABLE-PRART.
      PERFORM PRINT_VERT.
      WRITE:  BIG_TABLE-PRART UNDER TEXT-002, TCJ1T-PRATX.
    ENDAT.

    AT END OF KSTAR.
      SUM.
      SELECT SINGLE * FROM CSKU
            WHERE SPRAS = 'E'      AND
            KTOPL = 'COAT'   AND
            KSTAR = BIG_TABLE-KSTAR.
*        write: /5 big_table-prart, 11 big_table-group,
      PERFORM PRINT_VERT.
*        write:  big_table-group under text-004,
      WRITE:  CSKU-MCTXT      UNDER TEXT-005,
              BIG_TABLE-KSTAR UNDER TEXT-006,
              BIG_TABLE-VALUE UNDER TEXT-008.
    ENDAT.
    AT END OF PRART.
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 21 TEXT-013, BIG_TABLE-VALUE UNDER TEXT-008.
      WRITE: /1(132) SY-ULINE.
    ENDAT.

  ENDLOOP.
ENDFORM.
*-------------------  DISPLAY_TABLE2 --------------------------------
*   Description:
*   - This subroutine displayes data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE2.

  PERFORM PRINT_HEADINGS.
  SORT BIG_TABLE2 BY PRART KSTAR.
  LOOP AT BIG_TABLE2.

    AT NEW PRART.
      SELECT SINGLE * FROM TCJ1T
         WHERE LANGU = SY-LANGU
         AND PRART = BIG_TABLE2-PRART.
      PERFORM PRINT_VERT.
      WRITE:  BIG_TABLE2-PRART UNDER TEXT-002, TCJ1T-PRATX.
    ENDAT.

    AT END OF KSTAR.
      SUM.
      SELECT SINGLE * FROM CSKU
            WHERE SPRAS = 'E'      AND
            KTOPL = 'COAT'   AND
            KSTAR = BIG_TABLE2-KSTAR.
*        write: /5 big_table-prart, 11 big_table-group,
      PERFORM PRINT_VERT.
*        write:  big_table-group under text-004,
      WRITE:  CSKU-MCTXT      UNDER TEXT-005,
              BIG_TABLE2-KSTAR UNDER TEXT-006,
              BIG_TABLE2-VALUE UNDER TEXT-008.
    ENDAT.
    AT END OF PRART.
      SUM.
      PERFORM PRINT_VERT.
      WRITE: 21 TEXT-013, BIG_TABLE2-VALUE UNDER TEXT-008.
      WRITE: /1(132) SY-ULINE.
    ENDAT.

  ENDLOOP.
ENDFORM.

*-------------------  DISPLAY_TABLE3 -----------------------------------
*   - This subroutine displayes data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE3.

  PERFORM PRINT_HEADINGS.
  SORT BIG_TABLE3 BY KSTAR.
  LOOP AT BIG_TABLE3.

    AT END OF KSTAR.
      SUM.
      SELECT SINGLE * FROM CSKU
            WHERE SPRAS = 'E'      AND
            KTOPL = 'COAT'   AND
            KSTAR = BIG_TABLE3-KSTAR.
*        write: /5 big_table-prart, 11 big_table-group,
      PERFORM PRINT_VERT.
*        write:  big_table-group under text-004,
      WRITE:  CSKU-MCTXT      UNDER TEXT-005,
              BIG_TABLE3-KSTAR UNDER TEXT-006,
              BIG_TABLE3-VALUE UNDER TEXT-008.
    ENDAT.

  ENDLOOP.
  WRITE: /1(132) SY-ULINE.
ENDFORM.

*--------------------------  PRINT_HEADINGS  ---------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM PRINT_HEADINGS.
  FORMAT COLOR 2 ON.
*  write: /1 uline.
  PERFORM PRINT_VERT.
  WRITE:  2 TEXT-002, 61 TEXT-005, 87 TEXT-006, 100 TEXT-008.
  PERFORM PRINT_VERT.
  WRITE: TEXT-003 UNDER TEXT-002,
         TEXT-007 UNDER TEXT-006,
         TEXT-009 UNDER TEXT-008.
*  write:  2 text-002, 51 text-004, 61 text-005, 87 text-006,
*       100 text-008.
*   perform print_vert.
*   write:  3 text-003, 86 text-007, text-009 under text-008.
  WRITE: /1(132) SY-ULINE.
  FORMAT COLOR 2 OFF.
ENDFORM.

*----------------  GET_COST_RANGE_NAME  --------------------------------
*   The function mode is used to get cost element ranges for a selected
*   cost element group.  SETNAME --> name of cost element group
*-----------------------------------------------------------------------
FORM GET_COST_RANGE_NAME USING SETNAME.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0H'
            SETNR         = SETNAME
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = NAME_TAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.
ENDFORM.

*---------------------  GET_ALL_COST_EL  -------------------------------
*   This subroutine retrives all cost elements for a selected cost
*   element group.  All cost elements together with their description
*   are stored in ALLCOST_TAB table.
*---------------------------------------------------------------------*
FORM GET_ALL_COST_EL.
  LOOP AT NAME_TAB.
    SELECT * FROM CSKA
      WHERE KTOPL = 'COAT'
      AND KSTAR GE NAME_TAB-FROM
      AND KSTAR LE NAME_TAB-TO.
      SELECT SINGLE * FROM CSKU
        WHERE KTOPL = CSKA-KTOPL
        AND SPRAS = SY-LANGU
        AND KSTAR = CSKA-KSTAR.
      MOVE CSKA-KSTAR TO ALLCOST_TAB-COSTEL.
      MOVE NAME_TAB-TITLE TO ALLCOST_TAB-GRNAME.
      IF CSKU-LTEXT NE SPACE.
        MOVE CSKU-LTEXT TO ALLCOST_TAB-DESCRIPT.
      ELSE.
        MOVE CSKU-KTEXT TO ALLCOST_TAB-DESCRIPT.
      ENDIF.
      APPEND ALLCOST_TAB.
      CLEAR ALLCOST_TAB.

    ENDSELECT.
  ENDLOOP.

ENDFORM.
*-----------------  GET_ADDITIONAL_COST_EL  ----------------------------
* Pick up any additional cost elements and add them to the ALLCOST_TAB
*-----------------------------------------------------------------------
FORM GET_ADDITIONAL_COST_EL.
  SELECT * FROM CSKA
    WHERE KTOPL = 'COAT'
      AND KSTAR IN S_KSTAR.
    SELECT SINGLE * FROM CSKU
       WHERE KTOPL = CSKA-KTOPL
         AND SPRAS = SY-LANGU
         AND KSTAR = CSKA-KSTAR.
    IF  SY-SUBRC = '0'.
      MOVE CSKA-KSTAR TO ALLCOST_TAB-COSTEL.
      MOVE NAME_TAB-TITLE TO ALLCOST_TAB-GRNAME.
      IF CSKU-LTEXT NE SPACE.
        MOVE CSKU-LTEXT TO ALLCOST_TAB-DESCRIPT.
      ELSE.
        MOVE CSKU-KTEXT TO ALLCOST_TAB-DESCRIPT.
      ENDIF.
      APPEND ALLCOST_TAB.
      CLEAR ALLCOST_TAB.
    ENDIF.
  ENDSELECT.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_VERT
*-----------------------------------------------------------------------
*   Description:
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE: /1 SY-VLINE, 50 SY-VLINE, 60 SY-VLINE, 85 SY-VLINE,
         98 SY-VLINE, 125 SY-VLINE.
ENDFORM.
