REPORT ZPPMI006 NO STANDARD PAGE HEADING LINE-SIZE 120 LINE-COUNT 90.

*---------------------------------------------------------------------*
*       REPORT ZPPMI006                                               *
*       AUTHOR M. Khan                                                *
*       DATE   July, 2000.                                            *
*---------------------------------------------------------------------*
*  This program is used to perform a mass population of asset settlement
*  rules for those WBS which have a status of Released or Technically
*  Closed (REL, TECO).
*  If the asset is amoritized (Division = 50), then a valid from and to
*  period and year is populated in the settlement rules as well.
*  These settlement rules must exist in table ZAFXA.
*  G/L accounts are not in scope of this program.
*NOTE: When adding current year as sub number to asset, this will be
*      over ridden by the year of any existing asset value date.
*      For example, In 2000, we would add 2000 sub number, but 1999
*      appears if asset valuation date is in year 1999.
*---------------------------------------------------------------------*

TABLES:
  COBRB,     "Distribution Rules Settlement Rule Order Settlement
  ZAFXA,     "XREF TO LINK WBS WITH ASSET CLASS IN PS
  PRPS,      "WBS (Work Breakdown Structure) Element Master Data
  JEST,      "Status of WBS elements
  COBRA.     "Settlement Rule For Order Settlement

DATA:
  ZPOSID         LIKE PRPS-POSID,      "Var used to select prps records
  ENTRYBURE      LIKE DKOBR-ENTRYBURE,
  ASSET          LIKE DKOBR-EMPGE,
  KOUNT(4)       TYPE N VALUE 1,       " 4.6 b  changes*
  FIELD_NAME     LIKE BDCDATA-FNAM,    " 4.6 b  changes
  INDEX(2)       TYPE N,               "For BDC session
  PERBZ(15)      VALUE 'COBRB-PERBZ(--)',
  KONTY(15)      VALUE 'COBRB-KONTY(--)',
  EMPGE(15)      VALUE 'DKOBR-EMPGE(--)',
  PROZS(15)      VALUE 'COBRB-PROZS(--)',
  SPERIOD        LIKE  COBRB-GABPE VALUE 1,            "Issue 504
  EPERIOD        LIKE  COBRB-GBISP VALUE 12,           "Issue 504

  BEGIN OF TAB   OCCURS 50,
    POSID_TEXT(24),                    "WBS structure element(WBS)
*   ASSET(24),                         " 4.6 B  CHANGES
    ASSET        LIKE DKOBR-EMPGE,     "Asset Plus Sub Asset N
    POSID_4      LIKE PRPS-POSID,      "WBS structure element(WBS)
    Z_UNIQ_ID    LIKE ZAFXA-Z_UNIQ_ID, "unique identifier
    BZDAT        LIKE COBRA-BZDAT,     "Settlement date
    ENTRYBURE    LIKE DKOBR-ENTRYBURE, "No.of settlement records
  END OF TAB,

  BEGIN OF WBS    OCCURS 0,
    DIV(2)       TYPE C,
    POSID        LIKE PRPS-POSID,
  END OF WBS.

DATA: BEGIN OF ASSETALL,
       ASSET(24),
       DASH(1)            VALUE '-',
       BZDAT(4),
      END OF ASSETALL.

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

*------------------------ Selection Screen  ---------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(73) TEXT-001.
SELECTION-SCREEN END OF LINE.
*
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(69) TEXT-003.
SELECTION-SCREEN END OF LINE.
*
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(66) TEXT-004.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT  1(31) TEXT-016.
      PARAMETERS: P_TEST  AS CHECKBOX DEFAULT ' '.
    SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
PARAMETERS:     P_PBUKR LIKE PRPS-PBUKR DEFAULT 'UGL'.           "omning
SELECT-OPTIONS: SPOSID_4 FOR ZAFXA-Z_WBS,                   "WBS
                SSTAT    FOR JEST-STAT,                     "Status
                SVERNR   FOR PRPS-VERNR,                    "Division
                SAMORT_4 FOR ZAFXA-Z_WBS.          "WBS for Amortization
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(14) TEXT-005.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.


*-----------------------  INITIALIZATION     ---------------------------

INITIALIZATION.
 SSTAT-SIGN   = 'I'.
 SSTAT-OPTION = 'EQ'.
 SSTAT-LOW    = 'I0002'.
 APPEND SSTAT.
 SSTAT-LOW    = 'I0045'.
 APPEND SSTAT.
*
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
 SAMORT_4-LOW    = '9830'.
 APPEND SAMORT_4.
 SAMORT_4-LOW    = '9841'.
 APPEND SAMORT_4.
 SAMORT_4-LOW    = '9842'.
 APPEND SAMORT_4.
 SAMORT_4-LOW    = '9850'.
 APPEND SAMORT_4.
 SAMORT_4-LOW    = '9880'.
 APPEND SAMORT_4.
*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.
  PERFORM GET-SETTLEMENT-RECORDS.
  PERFORM WRITE_REPORT.
  NEW-PAGE.
  PERFORM WRITE_ERROR_REPORT.

IF P_TEST = 'X'.
  PERFORM OPEN_BDC.
  LOOP AT TAB.
    REFRESH BDCDATA.
*                             Change Project: Initial Screen
    PERFORM BDC_SCREEN USING 'SAPLCJWB'    '100'.
    PERFORM BDC_FIELD  USING '*PROJ-PSPID' TAB-POSID_TEXT(7).
    PERFORM BDC_FIELD  USING '*PRPS-POSID' TAB-POSID_TEXT.

    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/5'.        "STRUCTURE button
*                             Change Project: WBS Elements - Description
*   PERFORM BDC_SCREEN USING 'SAPLCJWB' '300'.         " 4.6 b  changes
    PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.

    CONCATENATE 'RCJ_MARKL-MARK(' KOUNT ')' INTO FIELD_NAME. "4.6 change
    PERFORM BDC_FIELD  USING  FIELD_NAME  'X'.               "4.6 change
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/7'.        "SETTLEMENT RULE

*   PERFORM BDC_SCREEN USING 'SAPLKOBS' '0100'.        " 4.6 b  changes
    PERFORM BDC_SCREEN USING 'SAPLKOBS' '0130'.
*   PERFORM BDC_FIELD  USING 'COBL-ANLN1' TAB-ASSET.    " 4.6 b  changes
*   PERFORM BDC_FIELD  USING 'COBL-ANLN2' TAB-BZDAT(4). " 4.6 b  changes
**  PERFORM BDC_FIELD  USING 'DKOBR-EMPGE' TAB-ASSET.   " 4.6 b  changes
**  CONCATENATE 'DKOBR-EMPGE(' KOUNT ')' INTO FIELD_NAME. "4.6 change
    CONCATENATE 'DKOBR-EMPGE(' TAB-ENTRYBURE ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING  FIELD_NAME  TAB-ASSET.         "4.6 change

**  PERFORM BDC_FIELD  USING 'COBRB-PROZS' '100'.
**  CONCATENATE 'COBRB-PROZS(' KOUNT ')' INTO FIELD_NAME. "4.6 change
    CONCATENATE 'COBRB-PROZS(' TAB-ENTRYBURE ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING  FIELD_NAME  '100'.             "4.6 change

*   PERFORM BDC_FIELD  USING 'COBRB-PERBZ' 'FUL'.
**  CONCATENATE 'COBRB-PERBZ(' KOUNT ')' INTO FIELD_NAME. "4.6 change
    CONCATENATE 'COBRB-PERBZ(' TAB-ENTRYBURE ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING  FIELD_NAME  'FUL'.             "4.6 change
*                                       Start of Issue no. 504 changes
 IF TAB-ASSET+5(2) = '50'.
    CONCATENATE 'COBRB-GABPE(' TAB-ENTRYBURE ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING  FIELD_NAME  SPERIOD.
    CONCATENATE 'COBRB-GABJA(' TAB-ENTRYBURE ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING  FIELD_NAME  TAB-BZDAT(4).
    CONCATENATE 'COBRB-GBISP(' TAB-ENTRYBURE ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING  FIELD_NAME  EPERIOD.
    CONCATENATE 'COBRB-GBISJ(' TAB-ENTRYBURE ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING  FIELD_NAME  TAB-BZDAT(4).
 ENDIF.
*                                         End of Issue no. 504 changes

    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.       "BACK
*   PERFORM BDC_SCREEN USING 'SAPLCJWB' '300'.        " 4.6 b changes
    PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.


    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.      "SETTLEMENT RULE

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
    SELECT * FROM PRPS                 "Select WBS
        WHERE POSID LIKE ZPOSID
          AND PBUKR = P_PBUKR
          AND VERNR IN SVERNR
          AND BELKZ = 'X'.             "Account Assignment
    IF SY-SUBRC = '0'.
      SELECT SINGLE * FROM JEST       "Check if WBS is deleted or closed
         WHERE OBJNR = PRPS-OBJNR
           AND ( STAT = 'I0046' OR STAT = 'I0076' ).
      IF SY-SUBRC NE '0'.              "WBS is not deleted so continue
        SELECT SINGLE * FROM JEST      "Select TECO & REL  WBS elements
           WHERE OBJNR = PRPS-OBJNR
             AND  STAT IN SSTAT
             AND  INACT <> 'X'.
        IF SY-SUBRC = '0'.             "WBS elements that are TECO
          SELECT * FROM COBRB
              WHERE OBJNR = PRPS-OBJNR.
          ENDSELECT.
          ENTRYBURE = SY-DBCNT + 1.    "Next Settlement Record
          SELECT SINGLE * FROM COBRB
            WHERE OBJNR = PRPS-OBJNR
              AND ( ANLN1 LIKE '4%'  OR
                    KONTY = 'SK' ).         "Discard G/L accounts
          IF SY-SUBRC > 0.
            SELECT SINGLE * FROM COBRA
              WHERE OBJNR = PRPS-OBJNR.
            IF SY-SUBRC = 0.
              IF COBRA-BZDAT(4) = '0000' AND JEST-STAT <> 'I0002'.
                WBS-POSID = PRPS-POSID.
                WBS-DIV   = PRPS-POSID(2).
                APPEND WBS.
              ELSE.
                PERFORM ADD_TO_TAB.
              ENDIF.
            ELSE.
              WBS-POSID = PRPS-POSID.
              WBS-DIV   = PRPS-POSID(2).
              IF JEST-STAT <> 'I0002'.
                 APPEND WBS.
              ELSE.
                 PERFORM ADD_TO_TAB.
              ENDIF.
            ENDIF.
          ENDIF.                       "No settlement beginning with 4
        ENDIF.                         "WBS elements that are TECO
      ENDIF.                           "Eliminate WBS that are DELETED
    ENDIF.                             "Selected division
    ENDSELECT.                         "end of PRPS select
  ENDSELECT.                           "end of ZAFXA select

ENDFORM.

*------------------------- ADD_TO_TAB   --------------------------------
FORM ADD_TO_TAB.
     IF JEST-STAT = 'I0002'.
        TAB-BZDAT = SY-DATUM.
     ELSE.
        TAB-BZDAT = COBRA-BZDAT.
     ENDIF.
  CONCATENATE ZAFXA-ANLKL+3(5) PRPS-POSID(2) ZAFXA-Z_UNIQ_ID
              '-' TAB-BZDAT+0(4)                        "4.6 b  changes
                 INTO TAB-ASSET.
     IF ZAFXA-Z_WBS IN SAMORT_4.
        TAB-ASSET+5(2) = '50'.
     ENDIF.
     TAB-ENTRYBURE = ENTRYBURE.
     TAB-POSID_TEXT = PRPS-POSID.
     APPEND TAB.
     CLEAR TAB.

ENDFORM.
*------------------------- WRITE_REPORT --------------------------------
FORM WRITE_ERROR_REPORT.
  SORT WBS BY DIV POSID.
  LOOP AT WBS.
    AT NEW DIV.                        "Each division to be mailed out
      NEW-PAGE.
      WRITE: /1 TEXT-TL4.
      WRITE: /1 TEXT-DSH.
    ENDAT.

    WRITE: /1 WBS-POSID.

  ENDLOOP.
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
            GROUP               = 'ZAM_ASSET_CP'
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
          30 TEXT-TL1,
          90 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
           TEXT-TL2 UNDER TEXT-TL1,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  ULINE.
*&---------------------------------------------------------------------*
*&      Form  WRITE_REPORT
*&---------------------------------------------------------------------*
FORM WRITE_REPORT.
 WRITE: /20 TEXT-TL3.                              "Write Sub heading
 WRITE: /20 TEXT-DSH.
 WRITE: /20 TEXT-009.

 SORT TAB BY POSID_TEXT+7(4)  POSID_TEXT+0(7).
 LOOP AT TAB.
   WRITE: /20 TAB-POSID_TEXT, TAB-ASSET.
 ENDLOOP.
ENDFORM.                    " WRITE_RECORD
