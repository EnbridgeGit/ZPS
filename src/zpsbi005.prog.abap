REPORT ZPSBI005 MESSAGE-ID ZS LINE-SIZE 80 LINE-COUNT 59.
************************************************************************
*       Owner: Centra/Union                                            *
*  Programmer: Mary Lou DeMeester                                      *
*        Date: May 7, 1997                                             *
*  Request ID: Requested by M. Caughy/H. Allison                       *
*                                                                      *
* This BDC program will select specified WBS elements and change their *
* Investment Profile to the new profile as specified on the variant.   *
************************************************************************
TABLES: PROJ,                         "Project definition
        PRPS.                         "WBS element table

DATA:  BDCCNT TYPE I,                 "BDC counter
       REC_CNT TYPE I,                "Record counter
       TMP_CNT TYPE I,                "Temporary counter
       TMP_IMPRF LIKE PRPS-IMPRF,     "Temporary Replace Investment Prof
       TMPSTRG LIKE PRPS-POSID.       "Temporary string variable

DATA:  BEGIN OF WBS OCCURS 100000,
         PSPID  LIKE PROJ-PSPID,      "Project ID
         POSID  LIKE PRPS-POSID,      "WBS element
         COUNT  LIKE REC_CNT,         "Record counter
       END OF WBS.

DATA: BEGIN OF BDCDATA OCCURS 100000.  "BDC Table
        INCLUDE STRUCTURE BDCDATA.    "Structure used for BDC Table
DATA: END OF BDCDATA.

SELECTION-SCREEN BEGIN OF BLOCK INTRO WITH FRAME TITLE TEXT-001.
PARAMETERS:
    P_WBSLOW(4)        TYPE C          OBLIGATORY,
    P_WBSHI(4)         TYPE C,
    P_IM_OLD           LIKE PRPS-IMPRF,
    P_IM_NEW           LIKE PRPS-IMPRF.

SELECTION-SCREEN END OF BLOCK INTRO.

************************************************************************
* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.
************************************************************************
START-OF-SELECTION.
   PERFORM INITIALIZE.
   PERFORM GET_WBS.
   PERFORM OPEN_SESSION.
   PERFORM PROC_WBS.
   PERFORM CLOSE_SESSION.
END-OF-SELECTION.

***********************  INITIALIZE  ***********************************
* This routine initializes variables and internal tables.              *
************************************************************************
FORM INITIALIZE.
 REFRESH: WBS.
 CLEAR:   WBS.
 WRITE: P_IM_NEW TO TMP_IMPRF.
ENDFORM.

***********************  GET_WBS  **************************************
* This routine will get all data for the selected projects.            *
************************************************************************
FORM GET_WBS.
  SELECT * FROM PRPS
     WHERE IMPRF = P_IM_OLD.
       WRITE: PRPS-POSID TO TMPSTRG.
       REC_CNT = 1.
       IF TMPSTRG+10(4) >= P_WBSLOW AND
          TMPSTRG+10(4) <= P_WBSHI.
          SELECT SINGLE * FROM PROJ WHERE PSPNR = PRPS-PSPHI.
          CLEAR: WBS.
          MOVE:  PROJ-PSPID  TO WBS-PSPID,
                 PRPS-POSID  TO WBS-POSID,
                 REC_CNT     TO WBS-COUNT.
          APPEND WBS.
     ENDIF.
  ENDSELECT.
ENDFORM.

***********************  PROC_WBS  *************************************
* This routine will process the internal table.                        *
************************************************************************
FORM PROC_WBS.
SORT WBS BY PSPID POSID.
LOOP AT WBS.
  AT NEW PSPID.
     SUM.
     MOVE: WBS-COUNT TO TMP_CNT.
     WRITE: /1 WBS-PSPID.
  ENDAT.
  PERFORM POST_WBS.
  WRITE: /30 WBS-POSID.
ENDLOOP.
ENDFORM.

* This routine processess the BDC session for the orders. *
FORM POST_WBS.
     REFRESH BDCDATA.
     CLEAR BDCDATA.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0100'.
     PERFORM BDC_FIELD  USING '*PRPS-POSID'    WBS-POSID.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/5'.

     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0300'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    'MRKA'.

     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0300'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    'W_PF'.

     PERFORM BDC_SCREEN USING 'SAPLCJWB'   '0406'.
     PERFORM BDC_FIELD  USING 'PRPS-IMPRF'  TMP_IMPRF.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/19'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/3'.

     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0300'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/11'.

*    PERFORM BDC_SCREEN USING 'SAPLSPO1'      '100'.
*    PERFORM BDC_SCREEN USING 'BDC_CURSOR'   'SPOP-OPTION1'.
*end of screen
     PERFORM INSERT_SESSION.
     IF BDCCNT > '2500'.
        PERFORM CLOSE_SESSION.
        MOVE '0' TO BDCCNT.
        PERFORM OPEN_SESSION.
        BDCCNT = BDCCNT + 1.
     ENDIF.
     BDCCNT = BDCCNT + 1.
ENDFORM.

* This routine closes a batch input session. *
FORM CLOSE_SESSION.
 CALL FUNCTION 'BDC_CLOSE_GROUP'.
 IF SY-SUBRC NE 0.
    WRITE: / 'BDC CLOSE GROUP ERROR. RC=', SY-SUBRC.
    EXIT.
 ENDIF.
ENDFORM.

* This routine inserts the transaction for the BDC session. *
FORM INSERT_SESSION.
 CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        TCODE          = 'CJ02'
        TABLES
        DYNPROTAB      = BDCDATA
      EXCEPTIONS
        INTERNAL_ERROR = 1
        NOT_OPEN       = 2
        QUEUE_ERROR    = 3
        TCODE_INVALID  = 4.
 IF SY-SUBRC NE 0.
    WRITE: / 'Error inserting data into session.'.
 ENDIF.
ENDFORM.

* This routine adds an entry to the BDCDATA table for transactions. *
FORM BDC_SCREEN USING PROGRAM DYNPRO.
 CLEAR BDCDATA.
 BDCDATA-PROGRAM  = PROGRAM.
 BDCDATA-DYNPRO   = DYNPRO.
 BDCDATA-DYNBEGIN = 'X'.
 APPEND BDCDATA.
ENDFORM.

* This routine adds an entry to the BDCDATA table using the fields. *
FORM BDC_FIELD USING FNAM FVAL.
 CLEAR BDCDATA.
 BDCDATA-FNAM = FNAM.
 BDCDATA-FVAL = FVAL.
 APPEND BDCDATA.
ENDFORM.

**********************   OPEN_SESSION   ********************************
* This routine will call the BDC function to open up a new BDC session *
************************************************************************
FORM OPEN_SESSION.
 CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT            = SY-MANDT
        GROUP             = 'ZPS_REPINVST'
*       HOLDDATE          =
        KEEP              = 'X'
        USER              = SY-UNAME
      EXCEPTIONS
        GROUP_INVALID     = 1
        GROUP_IS_LOCKED   = 2
        HOLDDATE_INVALID  = 3
        INTERNAL_ERROR    = 4
        QUEUE_ERRORID     = 5
        RUNNING           = 6
        SYSTEM_LOCK_ERROR = 7
        USER_INVALIDD     = 8.
 IF SY-SUBRC <> 0.
    MESSAGE E004.
 ENDIF.
ENDFORM.
