REPORT ZPPMI010 NO STANDARD PAGE HEADING LINE-SIZE 120 LINE-COUNT 90.

*---------------------------------------------------------------------*
*       REPORT: ZPPMI010                                              *
*       AUTHOR: M. Khan                                               *
*       DATE:   June, 2001.                                           *
*       ISSUE LOG: 880                                                *
*---------------------------------------------------------------------*
*  This program produces the report of wbs which were settled in the  *
*  last year but there are some costs in the current year. If the user*
*  choose to create BDC session, then it will change the Asset Value  *
*  date to January 01 of the current year.                            *
*                                                                     *
*---------------------------------------------------------------------*

TABLES:
  COSP,      "CO Object: Cost Totals for External Postings
*  COSS,      "CO Object: Cost Totals for Internal Postings
  ZAFXA,
  PROJ,      "Project definition
  PRPS,      "WBS (Work Breakdown Structure) Element Master Data
  JEST,      "Object Status
  COBRA,     "Settlement Rule For Order Settlement
  COBRB.     "Distribution Rules Settlement Rule Order Settlement

DATA:  BEGIN OF ITAB OCCURS 0,
         VERNR  TYPE PROJ-VERNR,                            "Division
         POSKI  TYPE PRPS-POSKI,                            "Project ID
         POST1  TYPE PROJ-POST1,                   "Project Description
         STAT   TYPE JEST-STAT,                             "WBS status
         BZDAT  TYPE COBRA-BZDAT,                     "Asset value date
         WKG001 TYPE COSP-WKG001,
         WKG002 TYPE COSP-WKG002,
         WKG003 TYPE COSP-WKG003,
         WKG004 TYPE COSP-WKG004,
         WKG005 TYPE COSP-WKG005,
         WKG006 TYPE COSP-WKG006,
         WKG007 TYPE COSP-WKG007,
         WKG008 TYPE COSP-WKG008,
         WKG009 TYPE COSP-WKG009,
         WKG0010 TYPE COSP-WKG010,
         WKG0011 TYPE COSP-WKG011,
         WKG0012 TYPE COSP-WKG012,
       END OF ITAB.

DATA: BEGIN OF WHERE_CLAUSE  OCCURS 10,
      CONDITION(30)  TYPE C,
      END OF WHERE_CLAUSE.

DATA WRKAREA LIKE LINE OF ITAB.

DATA:
  CURRENT_YEAR   TYPE COSP-GJAHR,
  TOTAMOUNT      TYPE COSP-WKG001,
  NEW_DATE       TYPE SY-DATUM,
  TOT_LINES      TYPE I,
  KOUNT(4)       TYPE N VALUE 1,
  FIELD_NAME     LIKE BDCDATA-FNAM,
  CUR_PERIOD(7),
  WKG_FIELD(11),
  DIV2           TYPE STRING.


* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

FIELD-SYMBOLS: <AMOUNT>.

*------------------------ Selection Screen  ---------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.

PARAMETERS:
   P_CCODE LIKE PRPS-PBUKR DEFAULT 'UGL'.                "Company code
SELECT-OPTIONS:
   S_VERNR FOR PRPS-VERNR,                                  "Division
   S_PSPID  FOR PROJ-PSPID.                              "Project

PARAMETERS:
   P_STAT   LIKE JEST-STAT  DEFAULT 'I0045',             "WBS Status
   P_MONTH  LIKE COSP-PERBL OBLIGATORY.                  "Fiscal Month

SELECT-OPTIONS:
   SAMORT_4   FOR ZAFXA-Z_WBS.                           "Amortized WBS

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-006.
SELECT-OPTIONS: S_KSTAR    FOR COSP-KSTAR
                DEFAULT '0000491001' TO '0000491002'.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-016.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     P_BDC     AS   CHECKBOX.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.

*-----------------------  END of SELECTION SCREEN-----------------------
*-----------------------  INITIALIZATION     ---------------------------

INITIALIZATION.

CALL FUNCTION 'SET_PRINT_PARAMETERS'
          EXPORTING COVER_PAGE = 'X'.

  SAMORT_4-SIGN   = 'I'.
  SAMORT_4-OPTION = 'EQ'.
  SAMORT_4-LOW    = '7630'.
  APPEND SAMORT_4.
  SAMORT_4-LOW    = '7641'.
  APPEND SAMORT_4.
  SAMORT_4-LOW    = '7642'.
  APPEND SAMORT_4.
  SAMORT_4-LOW    = '7660'.
  APPEND SAMORT_4.
  SAMORT_4-LOW    = '7671'.
  APPEND SAMORT_4.
  SAMORT_4-LOW    = '7673'.
  APPEND SAMORT_4.
  SAMORT_4-LOW    = '7674'.
  APPEND SAMORT_4.
  SAMORT_4-LOW    = '7675'.
  APPEND SAMORT_4.
  SAMORT_4  = 'IBT90009999'.
  APPEND SAMORT_4.

*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.

  MOVE SY-DATUM+0(4) TO CURRENT_YEAR.
  MOVE SY-DATUM      TO NEW_DATE.
  MOVE '0101' TO       NEW_DATE+4(4).
  CONCATENATE P_MONTH+1(2) '/' CURRENT_YEAR INTO CUR_PERIOD.
  CONCATENATE 'ITAB-WKG'      P_MONTH INTO WKG_FIELD.
  CONCATENATE 'COSP~WKG' P_MONTH ' > 0' INTO WHERE_CLAUSE-CONDITION.
  APPEND WHERE_CLAUSE.

  SELECT PROJ~VERNR  PRPS~POSKI  PROJ~POST1  JEST~STAT  COBRA~BZDAT
         COSP~WKG001 COSP~WKG002 COSP~WKG003 COSP~WKG004
         COSP~WKG005 COSP~WKG006 COSP~WKG007 COSP~WKG008
         COSP~WKG009 COSP~WKG010 COSP~WKG011 COSP~WKG012
    INTO TABLE ITAB
    FROM ( ( ( ( COSP
           INNER JOIN JEST
           ON JEST~OBJNR  = COSP~OBJNR )
           INNER JOIN COBRA
           ON COBRA~OBJNR = COSP~OBJNR )
           INNER JOIN PRPS
           ON PRPS~OBJNR  = COSP~OBJNR )
           INNER JOIN PROJ
           ON PROJ~PSPNR  = PRPS~PSPHI )
  WHERE (WHERE_CLAUSE)
    AND  COSP~GJAHR  = CURRENT_YEAR
    AND  COSP~WRTTP  = '04'                "Actual amount
    AND  COSP~VERSN  = '000'               "Version
    AND  COSP~BEKNZ  IN ('S', 'H', 'L')    "Debit/Credit Indicator
    AND  COSP~KSTAR  NOT IN S_KSTAR        "Exclude Cost Elements
    AND  JEST~STAT   =  P_STAT             "WBS status
    AND  JEST~INACT  <> 'X'                "Active
    AND  COBRA~BZDAT > '1994/01/01'        "AVD not blank
    AND  COBRA~BZDAT <  NEW_DATE           "Asset Valuation Date
    AND  PRPS~PBUKR  =  P_CCODE            "Company code
    AND  PRPS~VERNR  IN S_VERNR            "Division
    AND  PROJ~PSPID  IN S_PSPID.           "Project number


  IF ITAB[] IS INITIAL.
     STOP.
  ENDIF.

  LOOP AT ITAB.
       IF ITAB-POSKI+10(4) IN SAMORT_4.
          DELETE ITAB INDEX  SY-TABIX.
       ELSE.
       IF ITAB-POSKI+6(3) < '200'.
          DELETE ITAB INDEX  SY-TABIX.
       ENDIF.
       ENDIF.
  ENDLOOP.
  DESCRIBE TABLE ITAB LINES TOT_LINES.
  IF TOT_LINES > 0.
     SORT ITAB BY VERNR POSKI.
     IF P_BDC = 'X'.
      PERFORM OPEN_BDC.
     ENDIF.
  ELSE.
   STOP.
  ENDIF.

  ASSIGN (WKG_FIELD) TO <AMOUNT>.
  LOOP AT ITAB.
     TOTAMOUNT = TOTAMOUNT + <AMOUNT>.
     MOVE ITAB TO WRKAREA.
     AT END OF POSKI.
        MOVE WRKAREA-VERNR TO DIV2.
        SHIFT DIV2 BY 6 PLACES.
        WRITE: /3 DIV2, 11 WRKAREA-POSKI, 28 WRKAREA-POST1,
        72 WRKAREA-STAT, 79 WRKAREA-BZDAT, 90 TOTAMOUNT.
        CLEAR TOTAMOUNT.
       IF P_BDC = 'X'.
          PERFORM FORMAT_BDC_DATA.
       ENDIF.
     ENDAT.
  ENDLOOP.
  IF P_BDC = 'X'.
     PERFORM CLOSE_BDC.
  ENDIF.

*----------------------------------------------------------------------
*--------------   FORMAT BDC DATA           ---------------------------
*----------------------------------------------------------------------


FORM FORMAT_BDC_DATA.
  REFRESH BDCDATA.
*                             Change Project: Initial Screen
  PERFORM BDC_SCREEN USING 'SAPLCJWB'    '100'.
  PERFORM BDC_FIELD  USING '*PRPS-POSID' WRKAREA-POSKI.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/5'.        "STRUCTURE button

  PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.
  CONCATENATE 'RCJ_MARKL-MARK(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME  'X'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/7'.        "SETTLEMENT RULE

  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.        "PARAMETERS (AVD)

  PERFORM BDC_SCREEN USING 'SAPLKOBS' '110'.
  PERFORM BDC_FIELD  USING  'COBRA-BZDAT' NEW_DATE.  "AVD DATE
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.               "BACK

  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.


  PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

  PERFORM INSERT_BDC.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with screen
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.



*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  FNAM - name of the field on the screen
*           FVAL - value to be entered for that field on the
*                  screen
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM Open_BDC                                                 *
*---------------------------------------------------------------------*
FORM Open_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = sy-mandt
            GROUP               = 'ZPS_AVD_CHNG'
            KEEP                = 'X'
            USER                = sy-uname
       EXCEPTIONS
            CLIENT_INVALID      = 1
            DESTINATION_INVALID = 2
            GROUP_INVALID       = 3
            GROUP_IS_LOCKED     = 4
            HOLDDATE_INVALID    = 5
            INTERNAL_ERROR      = 6
            QUEUE_ERROR         = 7
            RUNNING             = 8
            SYSTEM_LOCK_ERROR   = 9
            USER_INVALID        = 10
            OTHERS              = 11.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Insert_BDC                                               *
*---------------------------------------------------------------------*
FORM Insert_BDC.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'CJ02'
       TABLES
            DYNPROTAB      = BDCData
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4
            OTHERS         = 5.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Close_BDC                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM Close_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
ENDFORM.
*-------------------------  TOP-OF-PAGE  -------------------------------
TOP-OF-PAGE.
 WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP INTENSIFIED ON INVERSE ON,
             25 TEXT-TL1,
             90 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
           TEXT-TL2 UNDER TEXT-TL1, NEW_DATE,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  ULINE.
     WRITE: /1 TEXT-002, 10 TEXT-003, 28 TEXT-004, 72 TEXT-005,
            79 TEXT-009, 97 TEXT-010, CUR_PERIOD.
  ULINE.

*&---------------------------------------------------------------------*

