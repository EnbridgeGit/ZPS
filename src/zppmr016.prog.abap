REPORT ZPPMR016 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        April 1998
*  Description:
*     - The purpose of this program is to produce a List of Projects
*       Not Settled for Fiscal Year
*       All costs are added up, including those with BEKNZ = A
*       (BEKNZ=A ==> Dollars that were settled) and only those
*       WBS elements not equal to zero are reported.
************************************************************************
* 98/11/27 md7140 #456 Original Request
************************************************************************
*   Date      By          Issue Description:
* -------- -------------  ----- ----------------------------------------
* 04/11/01 Mohammad Khan  I1076 Add period to varants and make program
*                               Changes accordingly.
************************************************************************
TABLES: PROJ,                             "Project Table
        PRPS,                             "WBS Element
        COSP,                             "External Postings
        COSS,                             "Internal Postings
        T001.                             "Company Code Table


DATA:
  BEGIN OF BIG_TABLE OCCURS 10000,
        BUKRS         LIKE PROJ-VBUKR,           "Company Code
        VERNR         LIKE PROJ-VERNR,           "Division
        PSPID         LIKE PROJ-PSPID,           "Project (7 digits)
        OBJNR         LIKE PRPS-OBJNR,           " Object number - Added by akmadasu "
        TXT04         type CHAR25, "J_TXT04,             " Status - Added by akmadasu "
        POSID         LIKE PRPS-POSID,           "WBS Element
        NETAMT        LIKE COSP-WKG001,
  END OF BIG_TABLE.
**-START OF CHANGES BY AKMADASU
TYPES: BEGIN OF ty_cobrb,
       OBJNR type J_OBJNR,
       LFDNR type BR_LFDNR,
       PERBZ TYPE PERBZ_LD,
       GBISJ type GBISJ,
       GBISP type GBISP,
       LETJA type LETJA,
       LETPE type LETPE,
       HKONT type SAKNR,
       ANLN1 type ANLN1,
       end of ty_cobrb.
data: gt_cobrb type TABLE OF ty_cobrb,
      gs_cobrb type ty_cobrb.
**-- END OF CHANGES BY AKMADASU
DATA:   NETAMT        LIKE COSP-WKG001.
DATA:   VALUE         LIKE COSP-WKG001.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:     P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL'.
SELECT-OPTIONS: SPSPID  FOR PROJ-PSPID,                    "Projects
                SDIV    FOR PRPS-VERNR.                    "Division
PARAMETERS:     P_VERS  LIKE COSP-VERSN DEFAULT '0',       "Version
                P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM(4),
                P_MTH   LIKE SY-DATUM+4(2) OBLIGATORY.     "I1076 Month
SELECTION-SCREEN END OF BLOCK BOX.
*------------------------  AT SELECTION-SCREEN  ------------------------

AT SELECTION-SCREEN ON P_CCODE.
  SELECT SINGLE * FROM T001                        "Get Company Name
    WHERE BUKRS = P_CCODE.

*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.

  SELECT * FROM PROJ                                "PROJ TABLE SELECT
    WHERE PSPID IN SPSPID                           "Projects
      AND VERNR IN SDIV                             "Division
      AND VBUKR = P_CCODE.                          "Company Code

    CLEAR NETAMT.
    IF PROJ-PSPID+5(2) CO '1234567890'.             "Eliminates templates
      SELECT * FROM PRPS
        WHERE PSPHI = PROJ-PSPNR
*         and pkokr = '10'                         "Controlling area
          AND LOEVM <> 'X'                         "Not flagged deleted
*         AND ( BELKZ = 'X').              "4.6 b  changes
          AND ( BELKZ = 'X' ).

        PERFORM GET_FISCAL_DATA.

        IF NETAMT <> 0.
          PERFORM BUILD_TABLE.
          CLEAR NETAMT.
        ENDIF.

      ENDSELECT.                                    "PRPS end
    ENDIF.                                           "end of templates
  ENDSELECT.                                         "PROJ end
**--sTART OF CHANGES BY AKMADASU
  PERFORM GET_STATUS.
**-- END OF CHANGES BY AKMADASU
  SORT BIG_TABLE BY BUKRS  VERNR  POSID.
  PERFORM DISPLAY_TABLE.

*  WRITE: /1 SY-VLINE, 80 SY-VLINE, 25 TEXT-005.  "End of Report" COMMENTED BY AKMADASU
  WRITE: /1 SY-VLINE, 110 SY-VLINE, 25 TEXT-005. " ADDED BY AKMADASU

*----------------------------------------------------------------------

*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: /1 TEXT-RPT, SY-REPID,                          "Report Id
         46 T001-BUTXT,                                  "Company Name
        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,  "Client
         37 TEXT-TTL,                                    "Report Title
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.           "Page Number
  WRITE: / TEXT-VRS UNDER TEXT-RPT, P_VERS,              "Version
*        42 TEXT-004, P_FYEAR.                        "I1076 Fiscal Year
       42 TEXT-004, P_FYEAR NO-GAP, '/' NO-GAP, P_MTH. "I1076 Fiscal Year

  FORMAT INTENSIFIED ON.
  ULINE.
  PERFORM PRINT_VERT.
*--start of changes by akmadasu
* WRITE: 2 TEXT-001, 25 TEXT-002, 51 TEXT-003.
  WRITE: 2 TEXT-001, 25 TEXT-002, 54 TEXT-100, 83 TEXT-003.
**-- end f changes by akmadasu
  ULINE.
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
  LOOP AT BIG_TABLE.
    AT NEW VERNR.                  "New page for each division
      NEW-PAGE.
    ENDAT.

    AT NEW PSPID.
      PERFORM PRINT_VERT.
      WRITE: (15) BIG_TABLE-PSPID UNDER TEXT-001.
    ENDAT.

    PERFORM PRINT_VERT.
**--START OF CHANGES BY AKMADASU
    clear:gs_cobrb.
    READ TABLE gt_cobrb into gs_cobrb with key OBJNR = BIG_TABLE-OBJNR.
    IF sy-subrc is INITIAL.
      IF ( gs_cobrb-GBISJ is INITIAL and gs_cobrb-GBISP is INITIAL ) or
         ( gs_cobrb-LETJA is INITIAL and gs_cobrb-LETPE is INITIAL ).
         IF gs_cobrb-hkont is NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_PERBZ_OUTPUT'
            EXPORTING
              INPUT  = gs_cobRB-PERBZ
            IMPORTING
              OUTPUT = gs_cobRB-PERBZ.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT         = gs_cobrb-hkont
           IMPORTING
             OUTPUT        = gs_cobrb-hkont
                    .
          CONCATENATE 'GL - ' gs_cobrb-hkont '-' gs_cobRB-PERBZ INTO BIG_TABLE-TXT04.
        else.
          CALL FUNCTION 'CONVERSION_EXIT_PERBZ_OUTPUT'
            EXPORTING
              INPUT  = gs_cobRB-PERBZ
            IMPORTING
              OUTPUT = gs_cobRB-PERBZ.
          CONCATENATE 'FXA - ' gs_cobrb-ANLN1 '-' gs_cobRB-PERBZ INTO BIG_TABLE-TXT04.
        ENDIF.
      ENDIF.
    ENDIF.
    WRITE BIG_TABLE-TXT04 UNDER TEXT-100.
**-- END OF CHANGES BY AKMADASU
    WRITE: BIG_TABLE-POSID  UNDER TEXT-002,
           BIG_TABLE-NETAMT UNDER TEXT-003.

    AT END OF PSPID.
      ULINE.
    ENDAT.


  ENDLOOP.
ENDFORM.

*----------------------- PRINT_HEADINGS -------------------------------
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------

*--------------------------  PRINT_VERT  -------------------------------
*   - This procedure is used to print vertical lines to separate one
*     column from another.
*-----------------------------------------------------------------------
FORM PRINT_VERT.
*  WRITE:  /1 SY-VLINE,  20 SY-VLINE,  50 SY-VLINE, 80 SY-VLINE. " commencted by akmadasu
  WRITE:  /1 SY-VLINE,  20 SY-VLINE,  50 SY-VLINE, 80 SY-VLINE, 110 sy-vline." added by akmadasu

ENDFORM.


FORM GET_FISCAL_DATA.
* COST TOTALS - External Postings
  SELECT * FROM COSP
     WHERE OBJNR = PRPS-OBJNR                    "Matching objects
       AND GJAHR = P_FYEAR                       "Fiscal Year selected
       AND VERSN = P_VERS                        "Version
       AND WRTTP = '04'.                          "Actuals only

    CLEAR VALUE.
*    ADD COSP-WKG001 FROM 1 TO 12 GIVING VALUE.     "I1076
    ADD COSP-WKG001 FROM 1 TO P_MTH GIVING VALUE.   "I1076
    ADD VALUE TO NETAMT.

  ENDSELECT.

* COST TOTALS - Internal Postings
  SELECT * FROM COSS
    WHERE OBJNR = PRPS-OBJNR                     "Matching objects
      AND GJAHR = P_FYEAR                        "Fiscal Year selected
      AND VERSN = P_VERS                         "Version
      AND WRTTP IN ('04').                       "Record with actuals

    CLEAR VALUE.
*   ADD COSS-WKG001 FROM 1 TO 12 GIVING VALUE.      "I1076
    ADD COSS-WKG001 FROM 1 TO P_MTH GIVING VALUE.   "I1076
    ADD VALUE TO NETAMT.

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
  CLEAR BIG_TABLE.
  MOVE PROJ-VBUKR      TO BIG_TABLE-BUKRS.
  MOVE PRPS-VERNR      TO BIG_TABLE-VERNR.
  MOVE PROJ-PSPID      TO BIG_TABLE-PSPID.
  MOVE PRPS-OBJNR      TO BIG_TABLE-OBJNR. " added by akmadasu
  MOVE PRPS-POSID      TO BIG_TABLE-POSID.
  MOVE NETAMT          TO BIG_TABLE-NETAMT.
  APPEND BIG_TABLE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_STATUS .
  SELECT OBJNR LFDNR PERBZ  GBISJ  GBISP
    LETJA LETPE
    HKONT ANLN1 FROM COBRB INTO TABLE gt_COBRB
                        FOR ALL ENTRIES IN BIG_TABLE[]
                        WHERE OBJNR = BIG_TABLE-OBJNR.
  IF SY-SUBRC IS INITIAL.
    sort gt_cobrb by objnr lfdnr DESCENDING.
  ENDIF.
ENDFORM.                    " GET_STATUS
