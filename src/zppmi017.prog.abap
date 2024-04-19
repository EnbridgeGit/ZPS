REPORT ZPPMI017 NO STANDARD PAGE HEADING LINE-SIZE 120 LINE-COUNT 90
                            MESSAGE-ID ZS.

*---------------------------------------------------------------------*
*       REPORT: ZPPMI017                                              *
*       AUTHOR: M. Khan                                               *
*       DATE:   June, 2008.                                           *
*       ISSUE LOG: TR494                                              *
*---------------------------------------------------------------------*
*  The purpose of this program is to add the settlement rules and     *
*  assset valuation date for existing WBS using transaction CJ02.     *
*  The EXCEL sheet is used as input file.                             *
*  If the settlement rule percentage exceeds 100% the report will be  *
*  printed but BDC session will not be created.                       *
*---------------------------------------------------------------------*

TABLES:
  PROJ,      "Project definition
  PRPS,      "WBS (Work Breakdown Structure) Element Master Data
  COBRB.     "Distribution Rules Settlement Rule Order Settlement

DATA:  BEGIN OF XLTAB OCCURS 0,
         POSID  TYPE PRPS-POSID,                           "WBS Element
         CTGRY(3) TYPE C,                  "Account assignment category
         EMPGE  TYPE DKOBR-EMPGE,          "General settlement receiver
         PROZS  TYPE COBRB-PROZS,           "Settlement percentage rate
         GABPE  TYPE COBRB-GABPE,                    "Valid-from period
         GABJA  TYPE COBRB-GABJA,                      "Valid-from year
         GBISP  TYPE COBRB-GBISP,                      "Valid-to period
         GBISJ  TYPE COBRB-GBISJ,                        "Valid-to year
         BZDAT  TYPE COBRA-BZDAT,                     "Asset value date
       END OF XLTAB.

DATA:
FIELD_NAME     LIKE BDCDATA-FNAM,
KOUNT(4)       TYPE N VALUE 0,
W_BZDAT        LIKE COBRA-BZDAT,
W_POSID        LIKE PRPS-POSID,
W_PROZS(6)     TYPE C.

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

TYPES: BEGIN OF KCDE_INTERN_STRUC.
          INCLUDE STRUCTURE  KCDE_CELLS.
TYPES: END OF KCDE_INTERN_STRUC.

DATA: EXCELTAB TYPE KCDE_INTERN_STRUC OCCURS 0 with header line.
*------------------------ Selection Screen  ---------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.

 PARAMETERS: P_FILEIN LIKE RLGRAP-FILENAME OBLIGATORY DEFAULT
                      'c:\saptemp\aid-to-construct.xls'.

SELECTION-SCREEN END OF BLOCK BOX1.
*-----------------------  END of SELECTION SCREEN-----------------------
*-----------------------  INITIALIZATION     ---------------------------

*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.

    PERFORM UPLOAD_EXCE_TO_INTERNAL_TAB.
    IF XLTAB[] IS INITIAL.
       STOP.
    ELSE.
       PERFORM CHECK_PERCENTAGE.
       PERFORM CREATE_BDC_SESSION.
    ENDIF.

*----------------------------------------------------------------------*
*  Upload EXCEL data                                                   *
*----------------------------------------------------------------------*
FORM UPLOAD_EXCE_TO_INTERNAL_TAB.
CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
  EXPORTING
    FILENAME                      = P_FILEIN
    I_BEGIN_COL                   = 1
    I_BEGIN_ROW                   = 3
    I_END_COL                     = 13
    I_END_ROW                     = 999
  TABLES
    INTERN                        = EXCELTAB
 EXCEPTIONS
   INCONSISTENT_PARAMETERS       = 1
   UPLOAD_OLE                    = 2
   OTHERS                        = 3
          .
 IF SY-SUBRC <> 0.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
         EXPORTING
         HEADLINE    = '!! ERROR !!'
         TEXT1       = 'Unsuccessful EXCEL Upload '
         TEXT2       = 'Please check the file path/name and try again'
         TEXT3       = ' '
         TEXT4       = 'Press OK Button to Continue'
         BUTTON_1    = 'OK'.
     STOP.
 ENDIF.

 LOOP AT EXCELTAB.
      CASE EXCELTAB-COL.
          WHEN 1 OR 2 OR 3 OR 4.
                CONCATENATE XLTAB-POSID EXCELTAB-VALUE INTO XLTAB-POSID.
                CONDENSE XLTAB-POSID NO-GAPS.
          WHEN 5.  MOVE EXCELTAB-VALUE TO XLTAB-CTGRY.
          WHEN 6.  MOVE EXCELTAB-VALUE TO XLTAB-EMPGE.
          WHEN 7.
            CONCATENATE XLTAB-EMPGE '-' EXCELTAB-VALUE INTO XLTAB-EMPGE.
                CONDENSE XLTAB-EMPGE NO-GAPS.
          WHEN 8.  MOVE EXCELTAB-VALUE TO XLTAB-PROZS.
          WHEN 9.  MOVE EXCELTAB-VALUE TO XLTAB-GABPE.
          WHEN 10.  MOVE EXCELTAB-VALUE TO XLTAB-GABJA.
          WHEN 11.  MOVE EXCELTAB-VALUE TO XLTAB-GBISP.
          WHEN 12.  MOVE EXCELTAB-VALUE TO XLTAB-GBISJ.
          WHEN 13.
              REPLACE ALL OCCURRENCES OF '-' IN EXCELTAB-VALUE WITH ''.
              REPLACE ALL OCCURRENCES OF '/' IN EXCELTAB-VALUE WITH ''.
              REPLACE ALL OCCURRENCES OF '.' IN EXCELTAB-VALUE WITH ''.
              MOVE EXCELTAB-VALUE TO XLTAB-BZDAT.
          WHEN OTHERS.
      ENDCASE.
      AT END OF ROW.
         APPEND XLTAB.
         CLEAR  XLTAB.
      ENDAT.
 ENDLOOP.
ENDFORM.
************************************************************************
FORM CHECK_PERCENTAGE.

SORT XLTAB BY POSID.
LOOP AT XLTAB.
  AT END OF POSID.
     SUM.
*     Check Percentage
  IF XLTAB-PROZS <> 100.
    WRITE: /1 'PROJECT =', XLTAB-POSID, 'HAS PERCENTAGE =', XLTAB-PROZS.
    STOP.
   ENDIF.
   ENDAT.
ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------
*--------------------- CREATE_BDC_SESSION   ---------------------------
*----------------------------------------------------------------------

FORM CREATE_BDC_SESSION.
  REFRESH BDCDATA.
  PERFORM OPEN_BDC.

  LOOP AT XLTAB.
  MOVE XLTAB-BZDAT TO W_BZDAT.
  MOVE XLTAB-PROZS TO W_PROZS.
  MOVE XLTAB-POSID TO W_POSID.

*              Project: Initial Screen
AT NEW POSID.
  PERFORM BDC_SCREEN USING 'SAPLCJWB'    '100'.
  PERFORM BDC_FIELD  USING '*PRPS-POSID' W_POSID.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/5'.        "STRUCTURE button
  PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.
  PERFORM BDC_FIELD  USING  'RCJ_MARKL-MARK(1)'  'X'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/7'.        "SETTLEMENT RULE
  PERFORM GET_HOW_MANY_EXISTING_SRULES.
ENDAT.

  KOUNT = KOUNT + 1.
  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0130'.
  CONCATENATE 'COBRB-KONTY(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME      XLTAB-CTGRY.
  CLEAR FIELD_NAME.

  CONCATENATE 'DKOBR-EMPGE(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME      XLTAB-EMPGE.
  CLEAR FIELD_NAME.

  CONCATENATE 'COBRB-PROZS(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME      W_PROZS.
  CLEAR FIELD_NAME.

  CONCATENATE 'COBRB-GABPE(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME      XLTAB-GABPE.
  CLEAR FIELD_NAME.

  CONCATENATE 'COBRB-GABJA(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME      XLTAB-GABJA.
  CLEAR FIELD_NAME.

  CONCATENATE 'COBRB-GBISP(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME      XLTAB-GBISP.
  CLEAR FIELD_NAME.

  CONCATENATE 'COBRB-GBISJ(' KOUNT ')' INTO FIELD_NAME.
  PERFORM BDC_FIELD  USING  FIELD_NAME      XLTAB-GBISJ.
  CLEAR FIELD_NAME.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.        "PARAMETERS (AVD)
  PERFORM BDC_SCREEN USING 'SAPLKOBS' '110'.
  IF W_BZDAT <> '00000000'.
  PERFORM BDC_FIELD  USING  'COBRA-BZDAT' W_BZDAT.   "AVD DATE
  ENDIF.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.        "BACK

AT END OF POSID.
  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.
  PERFORM BDC_SCREEN USING 'SAPLCJWB' '901'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
  PERFORM INSERT_BDC.
  REFRESH BDCDATA.
  CLEAR:   BDCDATA, KOUNT.
ENDAT.

ENDLOOP.

  PERFORM CLOSE_BDC.
  MESSAGE I019 WITH 'BDC SESSION CREATED - ZPS_SRUL_ADD'.
  ENDFORM.

*----------------------------------------------------------------------
*-------------------GET_HOW_MANY_EXISTING_SRULES-----------------------
*----------------------------------------------------------------------
FORM GET_HOW_MANY_EXISTING_SRULES.

 SELECT SINGLE OBJNR INTO PRPS-OBJNR
   FROM PRPS
  WHERE POSID = XLTAB-POSID.

 IF SY-SUBRC = 0.
    SELECT COUNT(*) INTO KOUNT
      FROM COBRB
     WHERE OBJNR = PRPS-OBJNR.
 ENDIF.

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
            GROUP               = 'ZPS_SRUL_ADD'
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
