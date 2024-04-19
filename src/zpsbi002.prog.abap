REPORT ZPSBI002 MESSAGE-ID ZS LINE-SIZE 80 LINE-COUNT 59.
***********************************************************************
*       Owner: Centra/Union                                           *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)        *
*        Date: December 30th, 1996                                    *
*  Request ID: Emergency BDC for PS (Requested by ROB Van Bremen)     *
*                                                                     *
* This BDC program will delete the costing sheet for selected projects*
***********************************************************************
TABLES: PROJ,                         "Project definition
        PRPS.                         "WBS element table

DATA:  BDCCNT TYPE I,                 "BDC counter
       REC_CNT TYPE I,                "Record counter
       TMP_CNT TYPE I,                "Temporary counter
       TMP_KALSM LIKE PRPS-KALSM.     "Temporary Costing Sheet

DATA:  BEGIN OF WBS OCCURS 40000,
         PSPID  LIKE PROJ-PSPID,      "Project ID
         POSID  LIKE PRPS-POSID,      "WBS element
         COUNT  LIKE REC_CNT,         "Record counter
       END OF WBS.

DATA: BEGIN OF BDCDATA OCCURS 10000.  "BDC Table
        INCLUDE STRUCTURE BDCDATA.    "Structure used for BDC Table
DATA: END OF BDCDATA.

SELECTION-SCREEN BEGIN OF BLOCK INTRO WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN SKIP.
SELECT-OPTIONS S_PROJ FOR PROJ-PSPID MODIF ID ABC.
PARAMETERS P_KALSM LIKE PRPS-KALSM MODIF ID ABC.
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
 PERFORM GET_PROJ.
 PERFORM OPEN_SESSION.
 PERFORM PROC_PROJ.
 PERFORM CLOSE_SESSION.
END-OF-SELECTION.
************************************************************************

* This routine intializes variables and internal tables.*
FORM INITIALIZE.
 REFRESH: WBS.
 CLEAR:   WBS.
 WRITE: P_KALSM TO TMP_KALSM.
ENDFORM.

* This routine will get all data for the selected projects. *
FORM GET_PROJ.
SELECT * FROM PROJ WHERE PSPID IN S_PROJ.
 SELECT * FROM PRPS WHERE PSPHI = PROJ-PSPNR.
   REC_CNT = 1.
   CLEAR: WBS.
   MOVE:  PROJ-PSPID  TO WBS-PSPID,
          PRPS-POSID  TO WBS-POSID,
          REC_CNT     TO WBS-COUNT.
   APPEND WBS.
 ENDSELECT.
ENDSELECT.
ENDFORM.

* This routine will process the internal table. *
FORM PROC_PROJ.
SORT WBS BY PSPID POSID.
LOOP AT WBS.
  AT NEW PSPID.
     SUM.
     MOVE: WBS-COUNT TO TMP_CNT.
     PERFORM POST_PROJECT.
     WRITE: /1 WBS-PSPID.
  ENDAT.
  WRITE: /30 WBS-POSID.
ENDLOOP.
ENDFORM.

* This routine processess the BDC session for the orders. *
FORM POST_PROJECT.
     REFRESH BDCDATA.
     CLEAR BDCDATA.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0100'.
     PERFORM BDC_FIELD  USING '*PROJ-PSPID'    WBS-PSPID.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/5'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0300'.

     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    'MRKA'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0300'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    'W_PF'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0406'.

     DO TMP_CNT TIMES.
        PERFORM BDC_SCREEN USING 'SAPLCJWB'   '0406'.
        PERFORM BDC_FIELD  USING 'PRPS-KALSM'  TMP_KALSM.
        PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/19'.
     ENDDO.

     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/3'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0300'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/3'.
     PERFORM BDC_SCREEN USING 'SAPLSPO1'      '100'.
     PERFORM BDC_SCREEN USING 'BDC_CURSOR'   'SPOP-OPTION1'.
*end of screen
     PERFORM INSERT_SESSION.
     IF BDCCNT > '1000'.
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

* This routine will call the BDC function to open up a new BDC session *
FORM OPEN_SESSION.
 CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT            = SY-MANDT
        GROUP             = 'ZPS_C_SHEET'
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
