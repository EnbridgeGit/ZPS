REPORT ZPBUR001 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        Auguest 1998
*  Requestor:   Kathleen Woodcock/Pat Hill
*  Description:
*     - The purpose of this program is to produce a Capital Expenditure
*       Report for Westcoast (Expenditures by Major Project)
*       summarized by Project Type.
************************************************************************
* 98/01/13 md7140 #289 Initial request
************************************************************************
TABLES:
        PROJ,                          "Project Definition
        PRPS,                          "WBS Element Master Data
        COSP,                          "External Postings
        COSS,                          "Internal Postings
        JEST,                          "Status of Project/WBS Element
        T247,                          "Month Name
        TCN7T,                         "Priority Descriptions
        TCJ1T,                         "Project Type Descriptions
        T001.                          "Company Code Table

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_WESTCOAST LIKE CABN-ATINN,       "Westcoast Budget Ind
       WESTCOAST(6)      TYPE C.                "Westcoast value

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.
*-----------------------------------------------------------------------


DATA:  VERS           LIKE COSP-VERSN,
       ACTPRV         LIKE COSP-WKG001,"Previous Years Actual
       ACTCUR         LIKE COSP-WKG001,"Current Years Actual
       ACT_TOT        LIKE COSP-WKG001,"Sum of Previous & Current Actual

       COSPCUR        LIKE COSP-WKG001,"Value for Current Year
       COSPPRV        LIKE COSP-WKG001,"Value for all other Years
       COSPAMT        LIKE COSP-WKG001,
       PLANANN        LIKE COSP-WKG001,"Annual Plan
       PLANYTD        LIKE COSP-WKG001,"YTD Plan
       VARIANCE       LIKE COSP-WKG001,"Variance (YTD plan - YTD act)
       PRINT_FLAG(1)  TYPE C,
       KTEXT          LIKE TCN7T-KTEXT,
       BEGMTH(2)      TYPE C,
       ENDMTH(2)      TYPE C,
       TTL2(40)       TYPE C,
       VERNA          LIKE PROJ-VERNA,

       PSPRI          LIKE PRPS-PSPRI,
       POST1          LIKE PRPS-POST1, "Description
       BZDAT          LIKE COBRA-BZDAT,"Asset valuation date
       PRART          LIKE PRPS-PRART,
*       prjgroup(40)   type c,
       FIRST(1)       TYPE C,
       WA_GJAHR       LIKE COSP-GJAHR,
       WA_MTH(2)      TYPE C,
       WA_LIM         LIKE COSP-WKG001,
       C491003        LIKE COSP-WKG001, "Ttl  491003 in all proj types
       WBS5850        LIKE COSP-WKG001, "Ttl WSB5850 in all proj types
       PRATX          LIKE TCJ1T-PRATX.

DATA:  TITLE          LIKE PROJ-POST1.

DATA:                                  "Report Data
  BEGIN OF BIG_TABLE OCCURS 10000,
      PRART         LIKE PRPS-PRART,   "Project Type
      ATWRT(6)      TYPE C,            "Westcoast Budget Control Number
      pspri         like prps-pspri,   "Priority Type
      VERNR         LIKE PROJ-VERNR,   "Division Name
      PSPID         LIKE PROJ-PSPID,   "Project Number
      VERNA         LIKE PROJ-VERNA,   "Division Name
      POST1         LIKE PRPS-POST1,   "Description
      ACTPRV        LIKE COSP-WKG001,  "Previous Years Actual
      ACTCUR        LIKE COSP-WKG001,  "Current Year Actual
      PLANANN       LIKE COSP-WKG001,  "Annual Plan
      PLANYTD       LIKE COSP-WKG001,  "YTD Plan
  END OF BIG_TABLE.


*----------------------  SELECTION SCREEN  -----------------------------

SELECTION-SCREEN  BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETER:    PBUKRS LIKE PRPS-PBUKR DEFAULT 'UGL',        "Company Code
              PVERS  LIKE COSP-VERSN DEFAULT '000',        "Version
              PGJAHR LIKE COSP-GJAHR DEFAULT SY-DATUM(4).  "Fiscal Year
*             pper(2) type c         default sy-datum+4(2)."Period End
SELECT-OPTIONS: SPER FOR SY-DATUM+4(2) OBLIGATORY
                    DEFAULT '01',
                SDESC FOR PROJ-POST1 NO INTERVALS,
                SPRART FOR PRPS-PRART, "Project Type
                SPROJ  FOR PROJ-PSPID.
PARAMETER:    PAMT  LIKE COSP-WKG001 DEFAULT 0.

SELECTION-SCREEN   END OF BLOCK BOX.

SELECTION-SCREEN  BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-100.
PARAMETER:    CHECK1 AS CHECKBOX,                          "Summary
              CHECK2 AS CHECKBOX,                          "Detail
              CHECK3 AS CHECKBOX DEFAULT 'X'.              "blankets
SELECTION-SCREEN   END OF BLOCK BOX1.

*---------------------  TOP-OF-PAGE  -----------------------------------
TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-RPT, SY-REPID, 40 TEXT-TTL,               "Title
           105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO,
           TEXT-004 UNDER TEXT-TTL, PGJAHR.                "Fiscal Year
  WRITE: / TEXT-VRN UNDER TEXT-RPT, PVERS,                 "Version
           TTL2     UNDER TEXT-TTL.                        "Month Name
  WRITE: / TEXT-002 UNDER TEXT-RPT, PBUKRS.
  WRITE: / TEXT-022 UNDER TEXT-RPT, PAMT UNDER PBUKRS.

  ULINE.
  FORMAT INTENSIFIED ON.
  WRITE: /2 TEXT-019,                                      "PROJECT TYPE
         61 TEXT-005,  73 TEXT-006,  85 TEXT-020,
         97 TEXT-007, 109 TEXT-013, 121 TEXT-008.
  PERFORM PRINT_VERT.
  WRITE: / TEXT-023 UNDER TEXT-019,
           TEXT-011 UNDER TEXT-005, TEXT-017 UNDER TEXT-006,
           TEXT-021 UNDER TEXT-020, TEXT-027 UNDER TEXT-007,
           TEXT-010 UNDER TEXT-013, TEXT-028 UNDER TEXT-008.
  PERFORM PRINT_VERT.
  WRITE: / TEXT-024 UNDER TEXT-019,
           TEXT-014 UNDER TEXT-005,   "Prev Actual vers 0
           TEXT-014 UNDER TEXT-006,   "Curr Actual vers 0
           TEXT-014 UNDER TEXT-020,   "Total Actuals (Prev + Curr)
           TEXT-014 UNDER TEXT-007, 103 PVERS,    "Ytd plan as specified
           TEXT-014 UNDER TEXT-013, 115 PVERS,    "Variance
           TEXT-014 UNDER TEXT-008, 127 PVERS.    "Ann plan as specified
  PERFORM PRINT_VERT.
  ULINE.
*----------------------- AT SELECTION-SCREEN ---------------------------
AT SELECTION-SCREEN ON SPER.
  MOVE SPER+3(2)     TO BEGMTH.
  PERFORM GET_MONTH USING BEGMTH.
  IF SPER+5(2) CA '123456789'.
     MOVE TEXT-015 TO TTL2.
     MOVE T247-LTX TO TTL2+(10).
     MOVE SPER+5(2) TO ENDMTH.
     PERFORM GET_MONTH USING ENDMTH.
     MOVE T247-LTX TO TTL2+16(10).
  ELSE.
     MOVE T247-LTX TO TTL2+13(10).
     MOVE SPER+3(2) TO ENDMTH.
  ENDIF.

AT SELECTION-SCREEN ON PBUKRS.
 SELECT SINGLE * FROM T001            "Company Code Description
   WHERE BUKRS = PBUKRS.

AT SELECTION-SCREEN.
  IF ( CHECK1 IS INITIAL ) AND
     ( CHECK2 IS INITIAL ).
     MESSAGE E100 WITH 'Please select Summary or Detail Report'.
  ENDIF.
*----------------------  START-OF-SELECTION  ---------------------------

START-OF-SELECTION.
 MOVE 'WCREPT'         TO CHARIC.     "Characteristics Required
 PERFORM GET_ATINN.
 MOVE G_ATINN          TO G_ATINN_WESTCOAST.

 SELECT * FROM PROJ
    WHERE PSPID IN SPROJ
      AND VBUKR = PBUKRS.
    CLEAR: ACTCUR, ACTPRV, PLANANN, PLANYTD.
*   clear: actcur, plcur, actprv, westcoast, post1, bzdat,
*          plcompl.
  IF PROJ-PSPID+5(2) CO '1234567890 '. "Template test
     MOVE PROJ-POST1 TO POST1.    "Project Description

     SELECT SINGLE * FROM JEST    "Check if project is DELETED
       WHERE OBJNR = PROJ-OBJNR
         AND STAT = 'I0076'
         AND INACT NE 'X'.

     IF SY-SUBRC = '4'.           "Allows all non-deleted projects
       SELECT * FROM PRPS
         WHERE PSPHI = PROJ-PSPNR
            AND PRART IN SPRART
            ORDER BY STUFE.
       IF PRPS-STUFE = '1'.          "Level 1 requirements
          OBJECT = PRPS-OBJNR.       "Characteristic WESTCOAST
          PERFORM FIND_CHARACTERISTIC.
          PRART = PRPS-PRART.        "Assumption #1
          PSPRI = PRPS-PSPRI.        "Assumption #1
       ENDIF.                       "Endo of STUFE = '1'

* Calculate ACTUALS/PLANS
       IF ( PRPS-BELKZ = 'X' OR          "Lowest Level Acct
            PRPS-PLAKZ = 'X' ).          "Lowest Level Planning
         CLEAR: COSPCUR, COSPPRV.
         PERFORM GET_COSP USING '000' '04' BEGMTH ENDMTH."Actuals-COSP
         ACTCUR = ACTCUR + COSPCUR.
         ACTPRV = ACTPRV + COSPPRV.

         CLEAR: COSPCUR, COSPPRV.
         PERFORM GET_COSS USING '000' '04' BEGMTH ENDMTH. "Actuals-COSS
         ACTCUR = ACTCUR + COSPCUR.
         ACTPRV = ACTPRV + COSPPRV.

         CLEAR: COSPCUR, COSPPRV.
         PERFORM GET_COSP USING PVERS '01' BEGMTH ENDMTH.  "Plans
         PLANANN = PLANANN + COSPPRV.
         PLANYTD = PLANYTD + COSPCUR.
       ENDIF.



        ENDSELECT.                     "End of PRPS
        PERFORM BUILD_BIG_TABLE.
      ENDIF.
    ENDIF.                             "End of Templates
  ENDSELECT.                           "End of PROJ



* eliminate blankets if check3 = ' '.
  LOOP AT BIG_TABLE.
  IF CHECK3 = 'X'.
  ELSE.
     IF BIG_TABLE-PSPID+4(3) < '200'.
        DELETE BIG_TABLE.
     ENDIF.
  ENDIF.
  ENDLOOP.

  LOOP AT BIG_TABLE.
* clearing fields not required for header breaks on report
    IF BIG_TABLE-ATWRT <> SPACE.
       CLEAR: BIG_TABLE-PSPRI.
       MODIFY BIG_TABLE.
    ENDIF.

* current actuals and annual plan.
  IF ( BIG_TABLE-ACTCUR < PAMT AND
      BIG_TABLE-PLANANN < PAMT ).
      DELETE BIG_TABLE.
  ENDIF.

 ENDLOOP.

*  perform big_table_dump.
  SORT BIG_TABLE BY PRART ATWRT DESCENDING PSPRI VERNR PSPID.

  IF CHECK1 = 'X'.
     PERFORM DISPLAY_SUMMARY_REPORT.
  ELSEIF CHECK2 = 'X'.
     PERFORM DISPLAY_DETAIL_REPORT.
  ENDIF.


*-----------------------------------------------------------------------

************************************************************************
*                     Subroutines in main program                      *
************************************************************************

*------------------------  PRINT_VERT  ---------------------------------
*  Print vertical lines to separate one column from another
*-----------------------------------------------------------------------
FORM PRINT_VERT.
  WRITE:   1 SY-VLINE,  60 SY-VLINE,  72 SY-VLINE,  84 SY-VLINE,
          96 SY-VLINE, 108 SY-VLINE, 120 SY-VLINE, 132 SY-VLINE.
ENDFORM.

*------------------------  DISPLAY_SUMMARY_REPORT  --------------------
*   This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_SUMMARY_REPORT.

 LOOP AT BIG_TABLE.
  MOVE BIG_TABLE-VERNA TO VERNA.

  AT NEW PRART.                      "PROJECT TYPE TITLE
    NEW-PAGE.
    SELECT SINGLE * FROM TCJ1T
      WHERE LANGU = SY-LANGU
        AND PRART = BIG_TABLE-PRART.
    FORMAT COLOR COL_GROUP.
    WRITE: /2 BIG_TABLE-PRART NO-GAP, '-' NO-GAP, TCJ1T-PRATX.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
  ENDAT.

  AT NEW ATWRT.                      "WESTCOAST TITLE
    CLEAR: TITLE, PRINT_FLAG.
    IF BIG_TABLE-ATWRT <> SPACE.
       MOVE 'X' TO PRINT_FLAG.
    ENDIF.

    LOOP AT SDESC.
      IF BIG_TABLE-ATWRT = SDESC+3(6).
         MOVE SDESC+12(30) TO TITLE.
      ENDIF.
    ENDLOOP.
    FORMAT COLOR COL_GROUP.
    WRITE: /4 TITLE.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
  ENDAT.

  AT NEW PSPRI.                                           "PRIORITY TYPE
    IF PRINT_FLAG <> 'X'.
    CLEAR KTEXT.
    SELECT SINGLE * FROM TCN7T
      WHERE LANGU = SY-LANGU
       AND NPRIO = BIG_TABLE-PSPRI.
    IF SY-SUBRC = '0'.
       MOVE TCN7T-KTEXT TO KTEXT.
    ELSE.
       MOVE TEXT-009    TO KTEXT.
    ENDIF.
    FORMAT COLOR COL_POSITIVE.
*    write: /5 big_table-pspri no-gap, '-' no-gap, ktext.
     WRITE: /5 KTEXT.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
    ENDIF.
  ENDAT.

* at new vernr.                                           "Division Name
*    write: /7 verna.
*   perform print_vert.
* endat.

* compute variance = big_table-planytd - big_table-actcur.
* write: /7 big_table-pspid, 20 big_table-post1,
*      (11) big_table-actcur  decimals 0 under text-006,
*      (11) big_table-actprv  decimals 0 under text-011,
*      (11) big_table-planann decimals 0 under text-008,
*      (11) big_table-planytd decimals 0 under text-007,
*      (11) variance          decimals 0 under text-010.
* perform print_vert.

  AT END OF VERNR.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /7 VERNA,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    PERFORM PRINT_VERT.
  ENDAT.

  AT END OF PSPRI.
    IF PRINT_FLAG <> 'X'.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.
    FORMAT COLOR COL_POSITIVE.
    WRITE: /5 TEXT-018, KTEXT,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.

    ENDIF.
  ENDAT.

  AT END OF ATWRT.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /4(128) SY-ULINE.
    PERFORM PRINT_VERT.
    FORMAT COLOR COL_GROUP.
    WRITE: /4 TEXT-018, TITLE,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
    WRITE: /4(128) SY-ULINE.
    PERFORM PRINT_VERT.
  ENDAT.

  AT END OF PRART.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.
    FORMAT COLOR COL_GROUP.
    WRITE: /2 TEXT-018, TCJ1T-PRATX,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
  ENDAT.

  AT LAST.
    ULINE.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /2 TEXT-012,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    PERFORM PRINT_VERT.
    ULINE.
  ENDAT.

 ENDLOOP.

ENDFORM.
*------------------------  DISPLAY_DETAIL_REPORT ----------------------
*   This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_DETAIL_REPORT..

 LOOP AT BIG_TABLE.
  MOVE BIG_TABLE-VERNA TO VERNA.

  AT NEW PRART.                      "PROJECT TYPE TITLE
    NEW-PAGE.
    SELECT SINGLE * FROM TCJ1T
      WHERE LANGU = SY-LANGU
        AND PRART = BIG_TABLE-PRART.
    FORMAT COLOR COL_GROUP.
    WRITE: /2 BIG_TABLE-PRART NO-GAP, '-' NO-GAP, TCJ1T-PRATX.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
  ENDAT.

  AT NEW ATWRT.                      "WESTCOAST TITLE
    CLEAR: TITLE, PRINT_FLAG.
    IF BIG_TABLE-ATWRT <> SPACE.
       MOVE 'X' TO PRINT_FLAG.
    ENDIF.

    LOOP AT SDESC.
      IF BIG_TABLE-ATWRT = SDESC+3(6).
         MOVE SDESC+12(30) TO TITLE.
      ENDIF.
    ENDLOOP.
    FORMAT COLOR COL_GROUP.
    WRITE: /4 TITLE.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
  ENDAT.

  AT NEW PSPRI.                                           "PRIORITY TYPE
    IF PRINT_FLAG <> 'X'.
    CLEAR KTEXT.
    SELECT SINGLE * FROM TCN7T
      WHERE LANGU = SY-LANGU
        AND NPRIO = BIG_TABLE-PSPRI.
    IF SY-SUBRC = '0'.
       MOVE TCN7T-KTEXT TO KTEXT.
    ELSE.
       MOVE TEXT-009    TO KTEXT.
    ENDIF.
    FORMAT COLOR COL_POSITIVE.
*   write: /5 big_table-pspri no-gap, '-' no-gap, ktext.
    WRITE: /5 KTEXT.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
    ENDIF.
  ENDAT.

  AT NEW VERNR.                                           "Division Name
     WRITE: /7 VERNA.
    PERFORM PRINT_VERT.
  ENDAT.

  COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
  COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
  WRITE: /7 BIG_TABLE-PSPID, 20 BIG_TABLE-POST1,
       (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
       (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
       (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
       (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
       (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
       (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
  PERFORM PRINT_VERT.

  AT END OF VERNR.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.
    WRITE: /7 TEXT-018, VERNA,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    PERFORM PRINT_VERT.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.

  ENDAT.

  AT END OF PSPRI.
    IF PRINT_FLAG <> 'X'.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.
    FORMAT COLOR COL_POSITIVE.
    WRITE: /5 TEXT-018, KTEXT,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.

    ENDIF.
  ENDAT.

  AT END OF ATWRT.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /4(128) SY-ULINE.
    PERFORM PRINT_VERT.
    FORMAT COLOR COL_GROUP.
    WRITE: /4 TEXT-018, TITLE,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
    WRITE: /4(128) SY-ULINE.
    PERFORM PRINT_VERT.
  ENDAT.

  AT END OF PRART.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /61(71) SY-ULINE.
    PERFORM PRINT_VERT.
    FORMAT COLOR COL_GROUP.
    WRITE: /2 TEXT-018, TCJ1T-PRATX,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM PRINT_VERT.
  ENDAT.

  AT LAST.
    ULINE.
    SUM.
    COMPUTE VARIANCE = BIG_TABLE-PLANYTD - BIG_TABLE-ACTCUR.
    COMPUTE ACT_TOT  = BIG_TABLE-ACTCUR  + BIG_TABLE-ACTPRV.
    WRITE: /2 TEXT-012,
            (11) BIG_TABLE-ACTCUR  DECIMALS 0 UNDER TEXT-006,
            (11) BIG_TABLE-ACTPRV  DECIMALS 0 UNDER TEXT-005,
            (11) BIG_TABLE-PLANANN DECIMALS 0 UNDER TEXT-008,
            (11) BIG_TABLE-PLANYTD DECIMALS 0 UNDER TEXT-007,
            (11) VARIANCE          DECIMALS 0 UNDER TEXT-010,
            (11) ACT_TOT           DECIMALS 0 UNDER TEXT-020.
    PERFORM PRINT_VERT.
    ULINE.
  ENDAT.

 ENDLOOP.

ENDFORM.

*--------------------  GET_COSP  ---------------------------------------
* Variables: 1. Version Number
*            2. Value Type: 01 - Plan; 04 - Actual
*            3. Beginning month for summation of values
*            4. Ending month for summation of values
*-----------------------------------------------------------------------
FORM GET_COSP USING VERS WRTTP MTHSTRT MTHEND.
 SELECT * FROM COSP
   WHERE OBJNR = PRPS-OBJNR
     AND VERSN = VERS
     AND WRTTP = WRTTP
     AND BEKNZ IN ('S', 'H', 'L').
     IF COSP-WRTTP = '04'.                                      "ACTUALS
        IF COSP-GJAHR = PGJAHR.
           ADD COSP-WKG001 FROM MTHSTRT TO MTHEND GIVING COSPAMT.
           ADD COSPAMT TO COSPCUR.
        ELSEIF COSP-GJAHR < PGJAHR.
           ADD COSP-WKG001 FROM '01' TO '12' GIVING COSPAMT.
           ADD COSPAMT TO COSPPRV.
         ENDIF.
     ELSEIF COSP-WRTTP = '01'.                                  "PLANS
        IF COSP-GJAHR = PGJAHR.
           ADD COSP-WKG001 FROM MTHSTRT TO MTHEND GIVING COSPAMT.
           ADD COSPAMT TO COSPCUR.
           ADD COSP-WKG001 FROM '01' TO '12' GIVING COSPAMT.
           ADD COSPAMT TO COSPPRV.
         ENDIF.
     ENDIF.

 ENDSELECT.
ENDFORM.

*-------------------------  GET_COSS  ----------------------------------

FORM GET_COSS USING VERS WRTTP MTHSTRT MTHEND.
 SELECT * FROM COSS
   WHERE OBJNR = PRPS-OBJNR
     AND VERSN = VERS
     AND WRTTP = WRTTP.
*    and beknz in ('S', 'H', 'L').    " 'A' are required
     IF COSS-WRTTP = '04'.                                      "ACTUALS
        IF COSS-GJAHR = PGJAHR.
           ADD COSS-WKG001 FROM MTHSTRT TO MTHEND GIVING COSPAMT.
           ADD COSPAMT TO COSPCUR.
       ELSEIF COSS-GJAHR < PGJAHR.
           ADD COSS-WKG001 FROM '01' TO '12' GIVING COSPAMT.
          ADD COSPAMT TO COSPPRV.
       ENDIF.
     ENDIF.

 ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BIG_TABLE_DUMP
*-----------------------------------------------------------------------
*   Description:
*   To view contents loop thru BIG_TABLE
*-----------------------------------------------------------------------
FORM BIG_TABLE_DUMP.
  LOOP AT BIG_TABLE.
  WRITE: /1 BIG_TABLE-PRART, 5 BIG_TABLE-ATWRT,
         20 BIG_TABLE-PSPRI, 25 BIG_TABLE-PSPID,
         50 BIG_TABLE-POST1.
  ENDLOOP.
ENDFORM.
************************************************************************
*--------------------- BUILD_BIG_TABLE ---------------------------------
* move all info to big_table for sorting and reporting
*-----------------------------------------------------------------------
FORM BUILD_BIG_TABLE.
  IF ( ACTCUR = 0 AND PLANANN = 0 AND PLANYTD = 0 ).
  ELSE.
    MOVE PRART          TO BIG_TABLE-PRART.     "Assumption #1
    MOVE WESTCOAST      TO BIG_TABLE-ATWRT.
    MOVE PSPRI          TO BIG_TABLE-PSPRI.     "Assumption #1
    MOVE PROJ-PSPID     TO BIG_TABLE-PSPID.
    MOVE PROJ-VERNA     TO BIG_TABLE-VERNA.     "division name
    MOVE PROJ-VERNR     TO BIG_TABLE-VERNR.     "division number
    MOVE POST1          TO BIG_TABLE-POST1.
    MOVE ACTCUR         TO BIG_TABLE-ACTCUR.
    MOVE ACTPRV         TO BIG_TABLE-ACTPRV.
    MOVE PLANANN        TO BIG_TABLE-PLANANN.
    MOVE PLANYTD        TO BIG_TABLE-PLANYTD.
    APPEND BIG_TABLE.
  ENDIF.
ENDFORM.

*----------------------  GET_MONTH  ------------------------------------
*  Month name
*-----------------------------------------------------------------------
FORM GET_MONTH USING MONTH.
 SELECT SINGLE * FROM T247                   "Get Month Name(Begin)
   WHERE SPRAS = 'E'
     AND MNR = MONTH.
ENDFORM.


*-----------------------  GET_ATINN  -----------------------------------
* Routine used to get the internal character number
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
*loop at char_tab.
*write: / char_tab-atinn, char_tab-atflv, char_tab-atwrt.
*endloop.
* Character  values in "ATWRT', numeric values in"ATFLV".
  CLEAR: WESTCOAST.
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
*  WESTCOAST budget project control number
   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_WESTCOAST BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATWRT TO WESTCOAST.
    ENDIF.
  ENDIF.
  CLEAR: OBJECT.
ENDFORM.
