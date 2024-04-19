REPORT zppmi007_copy NO STANDARD PAGE HEADING LINE-SIZE 120 LINE-COUNT 90.

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
  cobrb,     "Distribution Rules Settlement Rule Order Settlement
  zafxa,     "XREF TO LINK WBS WITH ASSET CLASS IN PS
  prps,      "WBS (Work Breakdown Structure) Element Master Data
  jest,                                "Status of WBS elements
  cobra.

DATA:
  zposid         LIKE prps-posid,      "Var used to select prps records
  entrybure      LIKE dkobr-entrybure,
  asset          LIKE dkobr-empge,
  kount(4)       TYPE n VALUE 1,       " 4.6 b  changes
  field_name     LIKE bdcdata-fnam,    " 4.6 b  changes
  assetcount(2)  TYPE n VALUE 0,
  totalcount(2)  TYPE n VALUE 0,       "953
  windex(2)      TYPE n VALUE 0,
  offset(2)      TYPE n VALUE 0,
  lshift(2)      TYPE n VALUE 0,
  totlines(5)    TYPE n VALUE 0,
  wrk_asset      LIKE dkobr-empge,     "953
  wrk_lfdnr(75),                       "953

  BEGIN OF tab   OCCURS 50,
    posid_text(24),                    "WBS structure element(WBS)
    asset        LIKE dkobr-empge,     "Asset Plus Sub Asset N
    kounter(2)   TYPE n,               "# of settlement rules with 4
    lfdnr(75),
  END OF tab.

DATA: gt_intern    TYPE kcde_intern WITH HEADER LINE,
      gwa_intern   TYPE kcde_intern_struc.

DATA: gt_input     TYPE TABLE OF tab,
      gwa_input    TYPE tab.

* batch input data
DATA: BEGIN OF bdcdata OCCURS 500.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

*------------------------ Selection Screen  ---------------------------

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
PARAMETERS: p_file TYPE localfile OBLIGATORY.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-016.
PARAMETERS: p_test  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME.
PARAMETERS:     p_pbukr LIKE prps-pbukr DEFAULT 'UGL'.
SELECT-OPTIONS: sposid_4 FOR zafxa-z_wbs,                   "WBS
                svernr   FOR prps-vernr.                    "Division
SELECTION-SCREEN END OF BLOCK box3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*F4 help for the file in selection screen
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.

*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.
*Read data from File
  PERFORM readfile USING p_file.


  PERFORM get-settlement-records.
  IF p_test = 'X'.
    PERFORM open_bdc.
    LOOP AT tab.
      REFRESH bdcdata.
*                             Change Project: Initial Screen
      PERFORM bdc_screen USING 'SAPLCJWB'    '100'.
      PERFORM bdc_field  USING '*PROJ-PSPID' tab-posid_text(7).
      PERFORM bdc_field  USING '*PRPS-POSID' tab-posid_text.

      PERFORM bdc_field  USING 'BDC_OKCODE' '/5'.        "STRUCTURE BUTTON
      PERFORM bdc_screen USING 'SAPLCJWB' '901'.

      CONCATENATE 'RCJ_MARKL-MARK(' kount ')' INTO field_name.
      PERFORM bdc_field  USING  field_name  'X'.
      PERFORM bdc_field  USING 'BDC_OKCODE' '/7'.        "SETTLEMENT RULE

      CLEAR: offset, lshift.
      DO tab-kounter TIMES.
        PERFORM bdc_screen USING 'SAPLKOBS' '130'.
        windex = tab-lfdnr+offset(3) - lshift.
        ADD  3   TO  offset.
        ADD  1   TO  lshift.
        CONCATENATE 'COBRB-KONTY(' windex ')' INTO field_name.
        PERFORM bdc_field  USING 'BDC_CURSOR' field_name.   "SET CURSOR
        PERFORM bdc_field  USING 'BDC_OKCODE' 'DELL'.        "DELETE RULE
      ENDDO.

      PERFORM bdc_screen USING 'SAPLKOBS' '130'.
      PERFORM bdc_field  USING 'BDC_OKCODE' '/3'.           "BACK
      PERFORM bdc_screen USING 'SAPLCJWB' '901'.


      PERFORM bdc_field  USING 'BDC_OKCODE' '/11'.       "SETTLEMENT RULE

      PERFORM insert_bdc.
    ENDLOOP.
    PERFORM close_bdc.
  ENDIF.

*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*-----------------------------------------------------------------------
FORM get-settlement-records.

  SELECT * FROM zafxa
     WHERE z_wbs IN sposid_4.
    CONCATENATE '_______' zafxa-z_wbs INTO zposid.
    SELECT * FROM prps                                      "Select WBS
        WHERE posid LIKE zposid
          AND pbukr = p_pbukr
          AND vernr IN svernr
          AND belkz = 'X'              "Account Assignment
          AND loevm <> 'X'.            "Deletion flag
      IF sy-subrc = '0'.
        SELECT SINGLE * FROM jest      "Select REL  WBS elements
           WHERE objnr = prps-objnr
             AND  stat = 'I0002'
             AND inact <> 'X'.

        IF sy-subrc = 0.
*          SELECT SINGLE * FROM COBRB                     "871
************************************************************************
*                                                                      *
*   ISSUE LOG 953 CHANGES - START                                      *
*                                                                      *
************************************************************************

*          CLEAR: ASSETCOUNT, OFFSET.
          CLEAR: assetcount, offset, totalcount, wrk_lfdnr.

          SELECT * FROM cobrb
           WHERE objnr = prps-objnr
           ORDER BY extnr.

            IF sy-subrc = 0.
              ADD 1 TO assetcount.
*       IF COBRB-ANLN1+0(1) = '4' AND COBRB-ERSJA = 0.  "TR413
              IF ( cobrb-anln1+0(1) BETWEEN '4' AND '5' ) AND cobrb-ersja = 0.
                MOVE  assetcount  TO wrk_lfdnr+offset(3).
                CONCATENATE  cobrb-anln1 cobrb-anln2 INTO wrk_asset.
                ADD 3 TO offset.
                ADD 1 TO totalcount.
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
          IF totalcount > 0.
            SELECT SINGLE * FROM jest      "Remove which are deleted
             WHERE objnr = prps-objnr
               AND  stat = 'I0076'.
            IF sy-subrc <> 0.
              PERFORM add_to_tab.
            ELSE.                                "953
              CLEAR tab-lfdnr.                     "953
            ENDIF.
          ENDIF.                       "settlement beginning with 4
        ENDIF.
      ENDIF.
    ENDSELECT.                         "end of PRPS select
  ENDSELECT.                           "end of ZAFXA select
  DESCRIBE TABLE tab LINES totlines.
  SKIP 2.
  WRITE: /20 'TOTAL RECORDS : ', totlines.
ENDFORM.                    "GET-SETTLEMENT-RECORDS

*------------------------- ADD_TO_TAB   --------------------------------
FORM add_to_tab.
  tab-kounter    = totalcount.
  tab-posid_text = prps-posid.
*  CONCATENATE  COBRB-ANLN1 COBRB-ANLN2 INTO TAB-ASSET.      "953
  tab-asset = wrk_asset.
  tab-lfdnr = wrk_lfdnr.
  WRITE: /20 tab-posid_text, tab-asset.
  APPEND tab.
  CLEAR tab.

ENDFORM.                    "ADD_TO_TAB
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
FORM bdc_screen USING program dynpro.

  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    "BDC_SCREEN



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
FORM bdc_field USING fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD

*---------------------------------------------------------------------*
*       FORM Open_BDC                                                 *
*---------------------------------------------------------------------*
FORM open_bdc.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = 'ZAM_ASSET_DL'
      keep                = 'X'
      user                = sy-uname
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.
ENDFORM.                    "Open_BDC

*---------------------------------------------------------------------*
*       FORM Insert_BDC                                               *
*---------------------------------------------------------------------*
FORM insert_bdc.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = 'CJ02'
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4
      OTHERS         = 5.
ENDFORM.                    "Insert_BDC

*---------------------------------------------------------------------*
*       FORM Close_BDC                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM close_bdc.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.
ENDFORM.                    "Close_BDC
*-------------------------  TOP-OF-PAGE  -------------------------------
TOP-OF-PAGE.
  WRITE: /1 text-rpt, sy-repid COLOR COL_GROUP INTENSIFIED ON INVERSE ON,
             40 text-ttl,
             90 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: / text-clt UNDER text-rpt, sy-mandt UNDER sy-repid, sy-sysid,
           text-pge UNDER text-dte, sy-pagno UNDER sy-datum.

  ULINE.
  WRITE:/20 text-004.

*&---------------------------------------------------------------------*
*&      Form  READFILE
*&---------------------------------------------------------------------*
FORM readfile USING p_file TYPE localfile.

  DATA: lv_filename TYPE rlgrap-filename.
  DATA: lv_index TYPE i.

  lv_filename = p_file.

*Read the CSV file data to an internal table
  CALL FUNCTION 'KCD_CSV_FILE_TO_INTERN_CONVERT'
    EXPORTING
      i_filename      = lv_filename
      i_separator     = ','
    TABLES
      e_intern        = gt_intern
    EXCEPTIONS
      upload_csv      = 1
      upload_filetype = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT gt_intern.
    SPLIT gt_intern-value AT ',' INTO tab-posid_text tab-asset tab-kounter tab-lfdnr.
    APPEND tab.
    CLEAR tab.
  ENDLOOP.
ENDFORM.                    "readfile
