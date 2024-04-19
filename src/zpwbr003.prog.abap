REPORT ZPWBR003 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 80
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
* 97/10/06 md7140 #225 Changed Trans$(WTG001) to CO $(WKG001)
*                      ADD statement rather than CALC-VALUE routine
* 97/03/31 MD7140 Added COSS table for internal postings
* 97/02/19 MD7140 Added numeric check - Project templates have
*                 characters 99-99-xxx-999 and should not be included
************************************************************************

TABLES:   COSP, COSS, CSKU, PRPS, T247, T001.

DATA:
    BEGIN OF WA OCCURS 10000,
        OBJNR         LIKE PRPS-OBJNR,           "Object Number
        PRART         LIKE PRPS-PRART,           "Project Type
        POSID         LIKE PRPS-POSID,           "WBS Element
    END OF WA.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
        PRART         LIKE PRPS-PRART,           "Project Type
        GROUP(9),                                "Sort Category
        KSTAR         LIKE COSP-KSTAR,           "Cost Element
        OBJNR         LIKE PRPS-OBJNR,           "Object Number
        POSID         LIKE PRPS-POSID,           "WBS Element
        VALUE         LIKE COSP-WKG001,          "Actual Amount
    END OF BIG_TABLE.

DATA:   VALUE         LIKE COSP-WKG001,
        GROUP(9),
        LEN           TYPE I,
        SYMBOL(4)     TYPE C,
        START         TYPE I,
        DESC          LIKE CSKU-MCTXT.           "Cost Element Desc


*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS: P_CCODE LIKE T001T-BUKRS DEFAULT 'UGL',
            P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
            P_MONTH(2)              DEFAULT SY-DATUM+4(2).
SELECTION-SCREEN END OF BLOCK BOX.

* End of selection screen
*-----------------------------------------------------------------------

START-OF-SELECTION.
    SELECT OBJNR PRART POSID                    "Select required values
        INTO WA
        FROM PRPS WHERE PBUKR = P_CCODE        "Company Code
                   AND PKOKR = '10'            "Controlling area
        ORDER BY PRART.                        "in Project Type order
        CLEAR: SYMBOL, GROUP.
*       if  wa-posid+4(3) co '1234567890'.
        IF  WA-POSID+5(2) CO '1234567890'.                "98/03/23
            LEN = STRLEN( WA-POSID ).
            CHECK LEN GE 4.
            START = LEN - 4.
            SYMBOL = WA-POSID+START(4).        "last 4 digits of WSB
            IF  SYMBOL > '9000'.
                GROUP = '9000-9999'.           "sort key
            ELSE.
               GROUP = '0001-8999'.
            ENDIF.

            SELECT * FROM COSP                 "EXTERNAL POSTINGs
                WHERE OBJNR = WA-OBJNR  AND    "Matching objects
                      GJAHR = P_FYEAR   AND    "Fiscal Year selected
                      WRTTP = '04'.            "Record with actuals
*               perform calc_value.                 "md7140 97/10/06
                CLEAR VALUE.
                ADD COSP-WKG001 FROM 1 TO P_MONTH GIVING VALUE.
                PERFORM BUILD_TABLE.
            ENDSELECT.

            SELECT * FROM COSS                 "INTERNAL POSTINGS
                WHERE OBJNR = WA-OBJNR  AND    "Matching objects
                      GJAHR = P_FYEAR   AND    "Fiscal Year selected
                      WRTTP = '04'.            "Record with actuals
*                perform calc_value_coss.        "md7140 97/10/06
                CLEAR VALUE.
                ADD COSS-WKG001 FROM 1 TO P_MONTH GIVING VALUE.
                PERFORM BUILD_TABLE_COSS.
            ENDSELECT.
        ENDIF.

    ENDSELECT.

    IF SY-SUBRC EQ 0.
       PERFORM DISPLAY_TABLE.
    ENDIF.


*-----------------------------------------------------------------------

TOP-OF-PAGE.
   FORMAT INTENSIFIED OFF.
   WRITE: / TEXT-RPT, SY-REPID, 54 TEXT-DTE, SY-DATUM,
            TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
   WRITE: /20 TEXT-001 COLOR COL_GROUP.             "Title
   WRITE: /32 TEXT-012.                             "For
   SELECT SINGLE * FROM T001                        "Company Name
        WHERE BUKRS = P_CCODE.
   WRITE: /26 T001-BUTXT.

   SELECT SINGLE * FROM T247                        "Month Name
       WHERE SPRAS = 'E' AND
             MNR = P_MONTH.
   WRITE: /30 T247-LTX.



   ULINE.
   FORMAT INTENSIFIED ON.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************



*-----------------------------------------------------------------------
*     FORM CALC_VALUE
*-----------------------------------------------------------------------
*   Description:
*   - Because YEAR-TO-DATE value has to be calculated from the beginig
*     of the year up to the current month, this procedure is used to do
*     so.
*-----------------------------------------------------------------------
*form calc_value.
*    clear value.
*    case p_month.
*        when '01'.
*            value = cosp-wkg001.
*        when '02'.
*            value = cosp-wkg001 + cosp-wkg002.
*        when '03'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003.
*        when '04'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004.
*        when '05'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005.
*        when '06'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005 + cosp-wkg006.
*        when '07'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005 + cosp-wkg006 +
*                    cosp-wkg007.
*        when '08'.
*           value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005 + cosp-wkg006 +
*                    cosp-wkg007 + cosp-wkg008.
*       when '09'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005 + cosp-wkg006 +
*                    cosp-wkg007 + cosp-wkg008 + cosp-wkg009.
*        when '10'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005 + cosp-wkg006 +
*                    cosp-wkg007 + cosp-wkg008 + cosp-wkg009 +
*                    cosp-wkg010.
*        when '11'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005 + cosp-wkg006 +
*                    cosp-wkg007 + cosp-wkg008 + cosp-wkg009 +
*                    cosp-wkg010 + cosp-wkg011.
*        when '12'.
*            value = cosp-wkg001 + cosp-wkg002 + cosp-wkg003 +
*                    cosp-wkg004 + cosp-wkg005 + cosp-wkg006 +
*                    cosp-wkg007 + cosp-wkg008 + cosp-wkg009 +
*                    cosp-wkg010 + cosp-wkg011 + cosp-wkg012.
*     endcase.
*endform.

*-----------------------------------------------------------------------
*     FORM CALC_VALUE_COSS
*-----------------------------------------------------------------------
*   Description:
*   - Because YEAR-TO-DATE value has to be calculated from the beginning
*     of the year up to the current month, this procedure is used to do
*     so.
*-----------------------------------------------------------------------
*form calc_value_coss.
*    clear value.
*    case p_month.
*        when '01'.
*            value = coss-wkg001.
*        when '02'.
*            value = coss-wkg001 + coss-wkg002.
*        when '03'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003.
*        when '04'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004.
*        when '05'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005.
*        when '06'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005 + coss-wkg006.
*        when '07'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005 + coss-wkg006 +
*                    coss-wkg007.
*        when '08'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005 + coss-wkg006 +
*                    coss-wkg007 + coss-wkg008.
*        when '09'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005 + coss-wkg006 +
*                    coss-wkg007 + coss-wkg008 + coss-wkg009.
*        when '10'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005 + coss-wkg006 +
*                    coss-wkg007 + coss-wkg008 + coss-wkg009 +
*                    coss-wkg010.
*        when '11'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005 + coss-wkg006 +
*                    coss-wkg007 + coss-wkg008 + coss-wkg009 +
*                    coss-wkg010 + coss-wkg011.
*        when '12'.
*            value = coss-wkg001 + coss-wkg002 + coss-wkg003 +
*                    coss-wkg004 + coss-wkg005 + coss-wkg006 +
*                    coss-wkg007 + coss-wkg008 + coss-wkg009 +
*                    coss-wkg010 + coss-wkg011 + coss-wkg012.
*     endcase.
*endform.

*-----------------------------------------------------------------------
*     FORM BUILD_TABLE
*-----------------------------------------------------------------------
*   Description:
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE.

    MOVE WA-OBJNR            TO BIG_TABLE-OBJNR.
    MOVE WA-PRART            TO BIG_TABLE-PRART.
    MOVE WA-POSID            TO BIG_TABLE-POSID.
    MOVE COSP-KSTAR          TO BIG_TABLE-KSTAR.
    MOVE GROUP               TO BIG_TABLE-GROUP.
    MOVE VALUE               TO BIG_TABLE-VALUE.
    APPEND BIG_TABLE.
    CLEAR BIG_TABLE.

ENDFORM.
*-----------------------------------------------------------------------
*     FORM BUILD_TABLE_COSS
*-----------------------------------------------------------------------
*   Description:
*   All data (subtracted from tables and calculated) required to
*   produce report is stored in TABLE for further processing.
*-----------------------------------------------------------------------
FORM BUILD_TABLE_COSS.

    MOVE WA-OBJNR            TO BIG_TABLE-OBJNR.
    MOVE WA-PRART            TO BIG_TABLE-PRART.
    MOVE WA-POSID            TO BIG_TABLE-POSID.
    MOVE COSS-KSTAR          TO BIG_TABLE-KSTAR.
    MOVE GROUP               TO BIG_TABLE-GROUP.
    MOVE VALUE               TO BIG_TABLE-VALUE.
    APPEND BIG_TABLE.
    CLEAR BIG_TABLE.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine displayes data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

   PERFORM PRINT_HEADINGS.
   SORT BIG_TABLE BY PRART GROUP KSTAR.
   LOOP AT BIG_TABLE.
      AT END OF KSTAR.
         SUM.
         SELECT SINGLE * FROM CSKU
               WHERE SPRAS = 'E'      AND
               KTOPL = 'COAT'   AND
               KSTAR = BIG_TABLE-KSTAR.
         WRITE: /5 BIG_TABLE-PRART, 11 BIG_TABLE-GROUP,
                21 CSKU-MCTXT,
                46 BIG_TABLE-KSTAR, 54 BIG_TABLE-VALUE.
         WRITE: 1 SY-VLINE, 10 SY-VLINE, 20 SY-VLINE, 45 SY-VLINE,
               53 SY-VLINE,
               80 SY-VLINE.
      ENDAT.
      AT END OF PRART.
         SUM.
         WRITE: /21 TEXT-013, 54 BIG_TABLE-VALUE.
         WRITE: 1 SY-VLINE, 10 SY-VLINE, 20 SY-VLINE, 45 SY-VLINE,
               53 SY-VLINE,
               80 SY-VLINE.
       WRITE: /1(80) SY-ULINE.
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
   WRITE: /1(80) SY-ULINE.
   PERFORM PRINT_VERT.
   WRITE:  2 TEXT-002, 11 TEXT-004, 21 TEXT-005, 47 TEXT-006,
         60 TEXT-008.
   PERFORM PRINT_VERT.
   WRITE:  3 TEXT-003, 46 TEXT-007, TEXT-009 UNDER TEXT-008.
   WRITE: /1(80) SY-ULINE.
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
  WRITE: /1 SY-VLINE, 10 SY-VLINE, 20 SY-VLINE, 45 SY-VLINE, 53
          SY-VLINE, 80 SY-VLINE.
ENDFORM.
