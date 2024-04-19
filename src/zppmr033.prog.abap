REPORT ZPWBR003A NO STANDARD PAGE HEADING.
* LINE-COUNT 65 LINE-SIZE 80
*                MESSAGE-ID ZP.
************************************************************************
*  Author:
*  Date:
*  Description:
*     - The purpose of this program is to produce a summary by Project
*       Type broken down by WBS Element & Cost Element
************************************************************************
*  CHANGES/CORRECTIONS
*
*
************************************************************************

TABLES:   COSP, COSS, CSKU, PRPS, T247, T001.

DATA:
    BEGIN OF WA OCCURS 10000,
        PRART         LIKE PRPS-PRART,           "Project Type
        KSTAR         LIKE COSP-KSTAR,           "Cost Element
        POSID         LIKE PRPS-POSID,           "WBS Element
        WKG001        LIKE COSP-WKG001,
        WKG002        LIKE COSP-WKG001,
        WKG003        LIKE COSP-WKG001,
        WKG004        LIKE COSP-WKG001,
        WKG005        LIKE COSP-WKG001,
        WKG006        LIKE COSP-WKG001,
        WKG007        LIKE COSP-WKG001,
        WKG008        LIKE COSP-WKG001,
        WKG009        LIKE COSP-WKG001,
        WKG010        LIKE COSP-WKG001,
        WKG011        LIKE COSP-WKG001,
        WKG012        LIKE COSP-WKG001,
    END OF WA.

DATA:
    BEGIN OF ITAB OCCURS 0,
        PRART          LIKE PRPS-PRART,           "Project Type
        GROUP(9),                                 "Sort Category
        KSTAR          LIKE COSP-KSTAR,           "Cost Element
        VALUE          LIKE COSP-WKG001,          "Actual Amount
    END OF ITAB.

DATA:
    BEGIN OF EXCLTAB OCCURS 0,
        PRART          LIKE PRPS-PRART,           "Project Type
        GROUP(9),                                 "Sort Category
        KSTAR          LIKE COSP-KSTAR,           "Cost Element
        MCTXT          LIKE CSKU-MCTXT,           "Cost Elem. Text
        VALUE          LIKE COSP-WKG001,          "Actual Amount
    END OF EXCLTAB.

DATA:   VALUE           LIKE COSP-WKG001,
        LEN             TYPE I,
        SYMBOL(4)       TYPE C,
        START           TYPE I,
        DESC            LIKE CSKU-MCTXT,           "Cost Element Desc
        WORK_HEAD1(27)   TYPE C,
        WORK_HEAD2(40)  TYPE C,
        WORK_TITLE      LIKE SY-TITLE,
        WORK_OPTION(11) TYPE C VALUE 'START_EXCEL'.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5)   TYPE C,
        DDIC_FIELD(5)   TYPE C,
        KEY             TYPE C,
      END OF PROT_HEADER.

DATA  ERRORTAB    LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
DATA: RETCODE LIKE SY-SUBRC.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS: P_CCODE LIKE T001T-BUKRS DEFAULT 'UGL',
            P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
            P_MONTH(2)              DEFAULT SY-DATUM+4(2).
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-003.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_EXCL RADIOBUTTON GROUP RBCR.            "EXCEL SHEET
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN END OF BLOCK BOX.

* End of selection screen
*-----------------------------------------------------------------------

START-OF-SELECTION.

*Primary Cost
  SELECT PRPS~PRART COSP~KSTAR PRPS~POSID
         COSP~WKG001 COSP~WKG002 COSP~WKG003 COSP~WKG004
         COSP~WKG005 COSP~WKG006 COSP~WKG007 COSP~WKG008
         COSP~WKG009 COSP~WKG010 COSP~WKG011 COSP~WKG012
    INTO WA
    FROM COSP INNER JOIN PRPS
              ON COSP~OBJNR = PRPS~OBJNR
   WHERE COSP~OBJNR LIKE 'PR%'                 "Project System
     AND COSP~GJAHR = P_FYEAR                  "Fiscal Year selected
     AND COSP~WRTTP = '04'                     "Record with actuals
     AND PRPS~PBUKR = P_CCODE.                 "Company Code

   IF SY-SUBRC = 0.
      CLEAR VALUE.
      ADD  WA-WKG001  FROM 1 TO P_MONTH GIVING VALUE.
      MOVE WA-KSTAR   TO   ITAB-KSTAR.
      MOVE WA-PRART   TO   ITAB-PRART.
      MOVE VALUE      TO   ITAB-VALUE.
      CLEAR: SYMBOL.
      IF  WA-POSID+5(2) CO '1234567890'.
          LEN = STRLEN( WA-POSID ).
          CHECK LEN GE 4.
          START = LEN - 4.
          SYMBOL = WA-POSID+START(4).        "last 4 digits of WSB
          IF  SYMBOL > '9000'.
              ITAB-GROUP = '9000-9999'.           "sort key
          ELSE.
              ITAB-GROUP = '0001-8999'.
          ENDIF.
       COLLECT ITAB.
       CLEAR: ITAB, WA.
     ENDIF.
   ENDIF.
 ENDSELECT.

*Secondary Cost
  SELECT PRPS~PRART COSS~KSTAR PRPS~POSID
         COSS~WKG001 COSS~WKG002 COSS~WKG003 COSS~WKG004
         COSS~WKG005 COSS~WKG006 COSS~WKG007 COSS~WKG008
         COSS~WKG009 COSS~WKG010 COSS~WKG011 COSS~WKG012
    INTO WA
    FROM COSS INNER JOIN PRPS
              ON COSS~OBJNR = PRPS~OBJNR
   WHERE COSS~OBJNR LIKE 'PR%'                 "Project System
     AND COSS~GJAHR = P_FYEAR                  "Fiscal Year selected
     AND COSS~WRTTP = '04'                     "Record with actuals
     AND PRPS~PBUKR = P_CCODE.                   "Company Code

   IF SY-SUBRC = 0.
      CLEAR VALUE.
      ADD  WA-WKG001  FROM 1 TO P_MONTH GIVING VALUE.
      MOVE WA-KSTAR   TO   ITAB-KSTAR.
      MOVE WA-PRART   TO   ITAB-PRART.
      MOVE VALUE      TO   ITAB-VALUE.
      CLEAR: SYMBOL.
      IF  WA-POSID+5(2) CO '1234567890'.
          LEN = STRLEN( WA-POSID ).
          CHECK LEN GE 4.
          START = LEN - 4.
          SYMBOL = WA-POSID+START(4).        "last 4 digits of WSB
          IF  SYMBOL > '9000'.
              ITAB-GROUP = '9000-9999'.
          ELSE.
              ITAB-GROUP = '0001-8999'.
          ENDIF.
          COLLECT ITAB.
          CLEAR: ITAB, WA.
     ENDIF.
   ENDIF.
 ENDSELECT.

 IF NOT ITAB[] IS INITIAL.
    PERFORM DISPLAY_TABLE.
    PERFORM REPORT_PRINTING.
 ENDIF.

*-----------------------------------------------------------------------
*     FORM DISPLAY_TABLE
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.

   SORT ITAB BY PRART GROUP KSTAR.
   LOOP AT ITAB.
      AT END OF KSTAR.
         SUM.
         SELECT SINGLE * FROM CSKU
               WHERE SPRAS = 'E'      AND
               KTOPL = 'COAT'   AND
               KSTAR = ITAB-KSTAR.

         MOVE  ITAB-PRART  TO  EXCLTAB-PRART.
         MOVE  ITAB-GROUP  TO  EXCLTAB-GROUP.
         MOVE  CSKU-MCTXT  TO  EXCLTAB-MCTXT.
         MOVE  ITAB-KSTAR  TO  EXCLTAB-KSTAR.
         MOVE  ITAB-VALUE  TO  EXCLTAB-VALUE.
         APPEND EXCLTAB.
         CLEAR  EXCLTAB.
      ENDAT.

      AT END OF GROUP.
         SUM.
         MOVE  TEXT-014    TO  EXCLTAB-MCTXT.
         MOVE  ITAB-VALUE  TO  EXCLTAB-VALUE.
         APPEND EXCLTAB.
         CLEAR  EXCLTAB.
      ENDAT.

      AT END OF PRART.
         SUM.
         MOVE  TEXT-013    TO  EXCLTAB-MCTXT.
         MOVE  ITAB-VALUE  TO  EXCLTAB-VALUE.
         APPEND EXCLTAB.
         CLEAR  EXCLTAB.
      ENDAT.

      AT LAST.
         SUM.
         MOVE  TEXT-015    TO  EXCLTAB-MCTXT.
         MOVE  ITAB-VALUE  TO  EXCLTAB-VALUE.
         APPEND EXCLTAB.
      ENDAT.

   ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
*--------------------------REPORT_PRINTING.    -------------------------
* Routine used to create the report or excel sheet                     *
*-----------------------------------------------------------------------
FORM REPORT_PRINTING.
   PERFORM PROT_HEADER.
   CONCATENATE P_CCODE '-' TEXT-018 P_FYEAR '/' P_MONTH INTO WORK_HEAD1
               SEPARATED BY SPACE.
   CONCATENATE SY-REPID  '-'  TEXT-TTL INTO WORK_TITLE
               SEPARATED BY SPACE.
   MOVE TEXT-016  TO WORK_HEAD2+0(5).
   WRITE SY-DATUM TO WORK_HEAD2+6(10).
*   MOVE  SPACE    TO WORK_HEAD2+17(3).
   MOVE TEXT-CLT  TO WORK_HEAD2+21(7).
   MOVE SY-MANDT  TO WORK_HEAD2+29(4).
   MOVE SY-SYSID  TO WORK_HEAD2+34(5).
*   CONCATENATE TEXT-016 SY-DATUM INTO WORK_HEAD2 SEPARATED BY SPACE.

   IF P_EXCL <> 'X'.
      CLEAR WORK_OPTION.
      IF SY-BATCH = 'X'.
         WORK_OPTION = 'LINESELMOD:1'.
      ENDIF.
   ENDIF.

   CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
          EXPORTING
            BASIC_LIST_TITLE        = WORK_TITLE
            HEAD_LINE1              = WORK_HEAD1
            HEAD_LINE2              = WORK_HEAD2
            FILE_NAME               = SY-CPROG
            ADDITIONAL_OPTIONS      = WORK_OPTION
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
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PROT_HEADER                                              *
*---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-CL1 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL2 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL3 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL4 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL5 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER

