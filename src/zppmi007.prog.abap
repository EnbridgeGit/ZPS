REPORT ZPPMI007 NO STANDARD PAGE HEADING LINE-SIZE 120 LINE-COUNT 90.

*---------------------------------------------------------------------*
*       REPORT ZPPMI007                                               *
*       AUTHOR M. Khan                                                *
*       DATE   August, 2000.                                          *
*---------------------------------------------------------------------*
*   This program creates a BDC session   for  SAP    project copy     *
*   function in PS (CJ02).  The program will delete the settlemnt     *
*   rules if the status is released (REL) - I0002 and settlement      *
*   rule exists starting with '4'. This record must exist in ZAFXA    *
*   table as well. It's a clear out of program ZPPMI006.              *
*---------------------------------------------------------------------*
*CHANGES:
*
* BY:    ISSUE:  DATE:        DESCRIPTION
* M.Khan TR314 2009/05/15 Change to select assets starting with 4 and 5
*                        instead of starting with 4 only.
************************************************************************
* MOKHAN   953   2002/04/08   Cursor set up changes.
*
************************************************************************
* mk9717   871                Program changes to handle multiple
*                             settlement rules.
*---------------------------------------------------------------------*

TABLES:
  COBRB,     "Distribution Rules Settlement Rule Order Settlement
  ZAFXA,     "XREF TO LINK WBS WITH ASSET CLASS IN PS
  PRPS,      "WBS (Work Breakdown Structure) Element Master Data
  JEST,                                "Status of WBS elements
  COBRA.

DATA:
  ZPOSID         LIKE PRPS-POSID,      "Var used to select prps records
  ENTRYBURE      LIKE DKOBR-ENTRYBURE,
  ASSET          LIKE DKOBR-EMPGE,
  KOUNT(4)       TYPE N VALUE 1,       " 4.6 b  changes
  FIELD_NAME     LIKE BDCDATA-FNAM,    " 4.6 b  changes
  ASSETCOUNT(2)  TYPE N VALUE 0,
  TOTALCOUNT(2)  TYPE N VALUE 0,       "953
  WINDEX(2)      TYPE N VALUE 0,
  OFFSET(2)      TYPE N VALUE 0,
  LSHIFT(2)      TYPE N VALUE 0,
  TOTLINES(5)    TYPE N VALUE 0,
  WRK_ASSET      LIKE DKOBR-EMPGE,     "953
  WRK_LFDNR(75),                       "953

  BEGIN OF TAB   OCCURS 50,
    POSID_TEXT(24),                    "WBS structure element(WBS)
    ASSET        LIKE DKOBR-EMPGE,     "Asset Plus Sub Asset N
    KOUNTER(2)   TYPE N,               "# of settlement rules with 4
    LFDNR(75),
  END OF TAB.

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

*------------------------ Selection Screen  ---------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) TEXT-016.
PARAMETERS: P_TEST  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
PARAMETERS:     P_PBUKR LIKE PRPS-PBUKR DEFAULT 'UGL'.
SELECT-OPTIONS: SPOSID_4 FOR ZAFXA-Z_WBS,                   "WBS
                SVERNR   FOR PRPS-VERNR.                    "Division
SELECTION-SCREEN END OF BLOCK BOX3.

*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.
  PERFORM GET-SETTLEMENT-RECORDS.
  IF P_TEST = 'X'.
    PERFORM OPEN_BDC.
    LOOP AT TAB.
      REFRESH BDCDATA.
*                             Change Project: Initial Screen
      PERFORM BDC_SCREEN USING 'SAPLCJWB'    '100'.
      PERFORM BDC_FIELD  USING '*PROJ-PSPID' TAB-POSID_TEXT(7).
      PERFORM BDC_FIELD  USING '*PRPS-POSID' TAB-POSID_TEXT.

    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/5'.        "STRUCTURE BUTTON
      PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.

      CONCATENATE 'RCJ_MARKL-MARK(' KOUNT ')' INTO FIELD_NAME.
      PERFORM BDC_FIELD  USING  FIELD_NAME  'X'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/7'.        "SETTLEMENT RULE

      CLEAR: OFFSET, LSHIFT.
      DO TAB-KOUNTER TIMES.
        PERFORM BDC_SCREEN USING 'SAPLKOBS' '130'.
        WINDEX = TAB-LFDNR+OFFSET(3) - LSHIFT.
        ADD  3   TO  OFFSET.
        ADD  1   TO  LSHIFT.
        CONCATENATE 'COBRB-KONTY(' WINDEX ')' INTO FIELD_NAME.
        PERFORM BDC_FIELD  USING 'BDC_CURSOR' FIELD_NAME.   "SET CURSOR
       PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'DELL'.        "DELETE RULE
      ENDDO.

      PERFORM BDC_SCREEN USING 'SAPLKOBS' '130'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.           "BACK
      PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.


     PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.       "SETTLEMENT RULE

      PERFORM INSERT_BDC.
    ENDLOOP.
    PERFORM CLOSE_BDC.
  ENDIF.

*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*-----------------------------------------------------------------------
FORM GET-SETTLEMENT-RECORDS.

  SELECT * FROM ZAFXA
     WHERE Z_WBS IN SPOSID_4.
    CONCATENATE '_______' ZAFXA-Z_WBS INTO ZPOSID.
    SELECT * FROM PRPS                                      "Select WBS
        WHERE POSID LIKE ZPOSID
          AND PBUKR = P_PBUKR
          AND VERNR IN SVERNR
          AND BELKZ = 'X'              "Account Assignment
          AND LOEVM <> 'X'.            "Deletion flag
      IF SY-SUBRC = '0'.
        SELECT SINGLE * FROM JEST      "Select REL  WBS elements
           WHERE OBJNR = PRPS-OBJNR
             AND  STAT = 'I0002'
             AND INACT <> 'X'.

        IF SY-SUBRC = 0.
*          SELECT SINGLE * FROM COBRB                     "871
************************************************************************
*                                                                      *
*   ISSUE LOG 953 CHANGES - START                                      *
*                                                                      *
************************************************************************

*          CLEAR: ASSETCOUNT, OFFSET.
   CLEAR: ASSETCOUNT, OFFSET, TOTALCOUNT, WRK_LFDNR.

   SELECT * FROM COBRB
    WHERE OBJNR = PRPS-OBJNR
    ORDER BY EXTNR.

    IF SY-SUBRC = 0.
       ADD 1 TO ASSETCOUNT.
*       IF COBRB-ANLN1+0(1) = '4' AND COBRB-ERSJA = 0.  "TR413
       IF ( COBRB-ANLN1+0(1) BETWEEN '4' AND '5' ) AND COBRB-ERSJA = 0.
           MOVE  ASSETCOUNT  TO WRK_LFDNR+OFFSET(3).
           CONCATENATE  COBRB-ANLN1 COBRB-ANLN2 INTO WRK_ASSET.
           ADD 3 TO OFFSET.
           ADD 1 TO TOTALCOUNT.
       ENDIF.
       ENDIF.
   ENDSELECT.

*          SELECT * FROM COBRB
*            WHERE OBJNR = PRPS-OBJNR
*              AND ANLN1 LIKE '4%'.      "settlement beginning with 4
*            IF SY-SUBRC = 0.
*              MOVE COBRB-LFDNR TO TAB-LFDNR+OFFSET(3).
*              ADD 3 TO OFFSET.
*              ADD 1 TO ASSETCOUNT.
*            ENDIF.
*          ENDSELECT.
*          IF ASSETCOUNT > 0.                       "953
************************************************************************
*                                                                      *
*   ISSUE LOG 953 CHANGES - END                                        *
*                                                                      *
************************************************************************
          IF TOTALCOUNT > 0.
             SELECT SINGLE * FROM JEST      "Remove which are deleted
              WHERE OBJNR = PRPS-OBJNR
                AND  STAT = 'I0076'.
              IF SY-SUBRC <> 0.
                 PERFORM ADD_TO_TAB.
              ELSE.                                "953
              CLEAR TAB-LFDNR.                     "953
              ENDIF.
          ENDIF.                       "settlement beginning with 4
        ENDIF.
      ENDIF.
    ENDSELECT.                         "end of PRPS select
  ENDSELECT.                           "end of ZAFXA select
  DESCRIBE TABLE TAB LINES TOTLINES.
  SKIP 2.
  WRITE: /20 'TOTAL RECORDS : ', TOTLINES.
ENDFORM.

*------------------------- ADD_TO_TAB   --------------------------------
FORM ADD_TO_TAB.
  TAB-KOUNTER    = TOTALCOUNT.
  TAB-POSID_TEXT = PRPS-POSID.
*  CONCATENATE  COBRB-ANLN1 COBRB-ANLN2 INTO TAB-ASSET.      "953
  TAB-ASSET = WRK_ASSET.
  TAB-LFDNR = WRK_LFDNR.
  WRITE: /20 TAB-POSID_TEXT, TAB-ASSET.
  APPEND TAB.
  CLEAR TAB.

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
            GROUP               = 'ZAM_ASSET_DL'
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
            40 TEXT-TTL,
            90 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.

  ULINE.
  WRITE:/20 TEXT-004.
