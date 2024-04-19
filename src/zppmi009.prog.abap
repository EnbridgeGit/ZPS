REPORT ZPPMI009 MESSAGE-ID ZS LINE-SIZE 80 LINE-COUNT 59.
***********************************************************************
*                                                                     *
*  Programmer: Mohammad Khan                                          *
*                                                                     *
*        Date: March, 2001.                                           *
*                                                                     *
*  Description: This BDC program can change the following status for  *
*               all the selected projects.                            *
*                                                                     *
*                                  1- Close                           *
*                                  2- Cancel close                    *
*                                  3- Close technically               *
*                                  4- Cancel close technically        *
*                                  5- Release                         *
*                                                                     *
***********************************************************************
* CHANGES:                                                            *
* 2001/12/19  mokhan   issue: 940
*             Description: Don't put the AVD when there is no
*             settlement rule and the action required is tech. close. *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
***********************************************************************
TABLES: PROJ,                         "Project definition
        PRPS,                         "WBS element table
        COBRA,                        "Order setlement rules
        COBRB.                        "Order setlement rules

DATA:  BDCCNT      TYPE I,            "BDC counter to separate batch
       GRPNAME(12) TYPE C,            "BDC session group name
       FLAG(1)     TYPE C,            "Checks if box was clicked
       FUNCID(4)   TYPE C,            "Function ID
       REC_CNT     TYPE I,            "Record counter
       TEXT(30)    TYPE C,            "Holds text variables
       XBZDAT      LIKE COBRA-BZDAT,  "inservice date
       STLIND(1)   TYPE C.            "is order settled ind

DATA:  BEGIN OF WBS OCCURS 10000,
         PSPID  LIKE PROJ-PSPID,      "Project ID
         POSID  LIKE PRPS-POSID,      "WBS element
         BELKZ  LIKE PRPS-BELKZ,      "Account assignment ind
         OBJNR  LIKE PRPS-OBJNR,      "object number
         COUNT  LIKE REC_CNT,         "Record-counter
       END OF WBS.

DATA: BEGIN OF BDCDATA OCCURS 10000.  "BDC Table
        INCLUDE STRUCTURE BDCDATA.    "Structure used for BDC Table
DATA: END OF BDCDATA.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS S_PROJ FOR PROJ-PSPID MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK BOX1.


SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(30) TEXT-012 MODIF ID ABC.
PARAMETERS:    P_CLOSE RADIOBUTTON GROUP SGRP.                    "Close
SELECTION-SCREEN: COMMENT 35(30) TEXT-009 MODIF ID ABC.
PARAMETERS:    P_TACT  RADIOBUTTON GROUP SGRP.        "Close technically
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(30) TEXT-013 MODIF ID ABC.
PARAMETERS:    P_CUND   RADIOBUTTON GROUP SGRP.            "Cancel Close
SELECTION-SCREEN: COMMENT 35(30) TEXT-010 MODIF ID ABC.
PARAMETERS:    P_TUND     RADIOBUTTON GROUP SGRP.     "Cancel Close tech
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(30) TEXT-015 MODIF ID ABC.
PARAMETERS:    P_UNLK   RADIOBUTTON GROUP SGRP.            "Unlock
SELECTION-SCREEN: COMMENT 35(30) TEXT-016 MODIF ID ABC.
PARAMETERS:    P_DFLG     RADIOBUTTON GROUP SGRP.          "Delete flag
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(30) TEXT-011 MODIF ID ABC.
PARAMETERS:    P_REL    RADIOBUTTON GROUP SGRP.            "Release
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(30) TEXT-014 MODIF ID ABC.
PARAMETERS:    P_BZDAT LIKE COBRA-BZDAT.     " DEFAULT SY-DATUM.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.

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
 PERFORM OPTIONS.
 IF FLAG = 'N'.
    STOP.
 ENDIF.
END-OF-SELECTION.
************************************************************************

* This routine intializes variables and internal tables.*
FORM INITIALIZE.
 REFRESH: WBS.
 CLEAR:   WBS.
ENDFORM.

************************************************************************
* This routine will get all appropriate data for the selected projects.*
FORM GET_PROJ.
 SELECT * FROM PROJ WHERE PSPID IN S_PROJ.
  SELECT * FROM PRPS WHERE PSPHI = PROJ-PSPNR.
    REC_CNT = 1.
    CLEAR: WBS.
    MOVE:  PROJ-PSPID  TO WBS-PSPID,
           PRPS-POSID  TO WBS-POSID,
           PRPS-BELKZ  TO WBS-BELKZ,
           PRPS-OBJNR  TO WBS-OBJNR,
           REC_CNT     TO WBS-COUNT.
    APPEND WBS.
 ENDSELECT.
 ENDSELECT.
 IF SY-SUBRC NE 0.
    STOP.
 ENDIF.
ENDFORM.

************************************************************************
* This routine handles the radiobutton options. *
FORM OPTIONS.

***********   Technically close   **************
 IF P_TACT = 'X'.
    IF P_BZDAT = '00000000'.
      MESSAGE E019 WITH 'Must have an inservice date to TECO projects'.
    ENDIF.
    WRITE: 'STTE'         TO FUNCID,
           'ZPS_TECH_ACT' TO GRPNAME,
           TEXT-009       TO TEXT.
    PERFORM OPEN_SESSION USING GRPNAME.
    PERFORM PROC_PROJ_LEVEL USING TEXT.
    PERFORM CLOSE_SESSION.
 ELSE.
    MOVE 'N' TO FLAG.
 ENDIF.

***********   Cancel Technically close   **************
 IF P_TUND = 'X'.
    WRITE: 'STTR'         TO FUNCID,
           'ZPS_TECH_UND' TO GRPNAME,
           TEXT-010       TO TEXT.
    PERFORM OPEN_SESSION USING GRPNAME.
    PERFORM PROC_WBS_LEVEL USING TEXT.
    PERFORM CLOSE_SESSION.
 ELSE.
    MOVE 'N' TO FLAG.
 ENDIF.

***********   Release   **************
 IF P_REL = 'X'.
    WRITE: 'STFR'         TO FUNCID,
           'ZPS_RELEASE'  TO GRPNAME,
           TEXT-011       TO TEXT.
    PERFORM OPEN_SESSION USING GRPNAME.
    PERFORM PROC_PROJ_LEVEL USING TEXT.
    PERFORM CLOSE_SESSION.
 ELSE.
    MOVE 'N' TO FLAG.
 ENDIF.

***********   Close   **************
 IF P_CLOSE = 'X'.
    WRITE: 'STAB'         TO FUNCID,
           'ZPS_CLOSE'    TO GRPNAME,
           TEXT-012       TO TEXT.
    PERFORM OPEN_SESSION USING GRPNAME.
    PERFORM PROC_PROJ_LEVEL USING TEXT.
    PERFORM CLOSE_SESSION.
 ELSE.
    MOVE 'N' TO FLAG.
 ENDIF.

***********   Cancel Close   ************
 IF P_CUND = 'X'.
    WRITE: 'STAB'         TO FUNCID,
           'ZPS_CLOSE'    TO GRPNAME,
           TEXT-013       TO TEXT.
    PERFORM OPEN_SESSION USING GRPNAME.
    PERFORM PROC_WBS_LEVEL USING TEXT.
    PERFORM CLOSE_SESSION.
 ELSE.
    MOVE 'N' TO FLAG.
 ENDIF.
***********   Unlock   ************
 IF P_UNLK = 'X'.
    WRITE: 'STEM'         TO FUNCID,
           'ZPS_UNLOCK'   TO GRPNAME,
           TEXT-015       TO TEXT.
    PERFORM OPEN_SESSION USING GRPNAME.
    PERFORM PROC_PROJ_LEVEL USING TEXT.
    PERFORM CLOSE_SESSION.
 ELSE.
    MOVE 'N' TO FLAG.
 ENDIF.
***********   Deletion flag  ************
 IF P_DFLG = 'X'.
    WRITE: 'STDL'         TO FUNCID,
           'ZPS_DEL_FLAG' TO GRPNAME,
           TEXT-016       TO TEXT.
    PERFORM OPEN_SESSION USING GRPNAME.
    PERFORM PROC_PROJ_LEVEL USING TEXT.
    PERFORM CLOSE_SESSION.
 ELSE.
    MOVE 'N' TO FLAG.
 ENDIF.
ENDFORM.

* This routine will call the BDC function to open up a new BDC session *
FORM OPEN_SESSION USING GRPNAME.
 CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT            = SY-MANDT
        GROUP             = GRPNAME
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


* This routine closes a batch input session. *
FORM CLOSE_SESSION.
 CALL FUNCTION 'BDC_CLOSE_GROUP'.
 IF SY-SUBRC NE 0.
    WRITE: / 'BDC CLOSE GROUP ERROR. RC=', SY-SUBRC.
    EXIT.
 ENDIF.
ENDFORM.
*-----------------  PROC_WBS_LEVEL       -------------------------------
* This routine will process the internal table WBS (selected data).
*-----------------------------------------------------------------------
FORM PROC_WBS_LEVEL USING TEXT.
 CLEAR: BDCCNT.
 SORT WBS BY PSPID POSID.
 IF P_TACT = 'X'.
   WRITE: P_BZDAT TO XBZDAT.
 ELSE.
   XBZDAT = SPACE.
 ENDIF.
 LOOP AT WBS.
   AT NEW PSPID.
      PERFORM POST_WBS_LEVEL USING FUNCID.
      WRITE: /1 WBS-PSPID, 50 TEXT.
   ENDAT.
   IF P_TACT = 'X'
   OR P_TUND = 'X'.
     PERFORM SET_INSERV_DATE.
   ENDIF.
   WRITE: /30 WBS-POSID.
 ENDLOOP.
ENDFORM.
*------------------------  PROC_PROJ_RELEASE  --------------------------
* This routine will process the internal table WBS (selected data).
*-----------------------------------------------------------------------
**FORM PROC_PROJ_RELEASE USING TEXT.
FORM PROC_PROJ_LEVEL USING TEXT.
 CLEAR: BDCCNT.
 SORT WBS BY PSPID POSID.
 IF P_TACT = 'X'.
   WRITE: P_BZDAT TO XBZDAT.
 ELSE.
   XBZDAT = SPACE.
 ENDIF.
 LOOP AT WBS.
   AT NEW PSPID.
      PERFORM POST_PROJECT_LEVEL USING FUNCID.
      WRITE: /1 WBS-PSPID, 50 TEXT.
   ENDAT.
   IF P_TACT = 'X'
   OR P_TUND = 'X'.
     PERFORM SET_INSERV_DATE.
   ENDIF.
   WRITE: /30 WBS-POSID.
 ENDLOOP.
ENDFORM.
*---------------------  POST_WBS_LEVEL----------------------------------
* This routine processess the BDC session for the orders.
*-----------------------------------------------------------------------
FORM POST_WBS_LEVEL USING FUNCID.
     REFRESH BDCDATA.
     CLEAR BDCDATA.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0100'.
     PERFORM BDC_FIELD  USING '*PROJ-PSPID'        WBS-PSPID.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/5'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        'MRKA'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        FUNCID.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/11'.

     PERFORM INSERT_SESSION.
     IF BDCCNT > '1000'.
        PERFORM CLOSE_SESSION.
        MOVE '0' TO BDCCNT.
        PERFORM OPEN_SESSION USING GRPNAME.
        BDCCNT = BDCCNT + 1.
     ENDIF.
     BDCCNT = BDCCNT + 1.
ENDFORM.
*---------------------  POST_PROJECT_LEVEL ---------------------------
* This routine processess the BDC session for the orders (released)
*-----------------------------------------------------------------------
**FORM POST_PROJECT_LEVEL USING FUNCID.
FORM POST_PROJECT_LEVEL USING FUNCID.
     REFRESH BDCDATA.
     CLEAR BDCDATA.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0100'.
     PERFORM BDC_FIELD  USING '*PROJ-PSPID'        WBS-PSPID.
*     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        'MD01'.   "PROJ DEF'N
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        'MDTB'.   "PROJ DEF'N

*     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0200'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '998'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        FUNCID.

*     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0200'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '998'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/11'.    "SAVE

     PERFORM INSERT_SESSION.
     IF BDCCNT > '1000'.
        PERFORM CLOSE_SESSION.
        MOVE '0' TO BDCCNT.
        PERFORM OPEN_SESSION USING GRPNAME.
        BDCCNT = BDCCNT + 1.
     ENDIF.
     BDCCNT = BDCCNT + 1.
ENDFORM.

*---------------------  SET_INSERV_DATE -------------------------------
* This routine first checks what the inservice date is and then if
* required creates the transaction to correct it.  If the transaction
* is to set the status to TECO and the date is blank, then set it.  If
* the transaction is to UNDO the TECO status then the date must be
* blank.  Only lowest level WBS elements have an inservice date.
*-----------------------------------------------------------------------
FORM SET_INSERV_DATE.

     CHECK WBS-BELKZ = 'X'.                                 "low level ?

     SELECT SINGLE * FROM COBRA
     WHERE  OBJNR EQ WBS-OBJNR.

     CHECK SY-SUBRC = 0.                                    "rec found ?

     IF P_TACT = 'X'.
       CHECK COBRA-BZDAT = '00000000'.
     ELSE.
       CHECK COBRA-BZDAT <> '00000000'.
     ENDIF.

     STLIND = 'N'.
     SELECT * FROM COBRB
     WHERE  OBJNR EQ WBS-OBJNR.
       STLIND = 'Y'.                                        "ord settled
     ENDSELECT.

     REFRESH BDCDATA.
     CLEAR BDCDATA.

     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0100'.
     PERFORM BDC_FIELD  USING '*PRPS-POSID'       WBS-POSID.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/5'.     "structure

     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        'MRKA'.   "select all

     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/7'.     "settlement

     IF STLIND = 'Y'.
       PERFORM BDC_SCREEN USING 'SAPLKOBS'        '0130'.
       PERFORM BDC_FIELD  USING 'BDC_OKCODE'      '/8'.     "parameters
*     ELSE.                                                 "Issue 940
*       PERFORM BDC_SCREEN USING 'SAPLKOBS'        '0100'.  "Issue 940
*       PERFORM BDC_FIELD  USING 'BDC_OKCODE'      '/8'.    "Issue 940
*     ENDIF.                                                "Issue 940

      PERFORM BDC_SCREEN USING 'SAPLKOBS'          '0110'.
      PERFORM BDC_FIELD  USING 'COBRA-BZDAT'       XBZDAT.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/3'.     "return
     ENDIF.                                                  "Issue 940

     PERFORM BDC_SCREEN USING 'SAPLKOBS'          '0130'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/3'.

     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/11'.    "SAVE

     PERFORM INSERT_SESSION.

     IF BDCCNT > '1000'.
        PERFORM CLOSE_SESSION.
        MOVE '0' TO BDCCNT.
        PERFORM OPEN_SESSION USING GRPNAME.
        BDCCNT = BDCCNT + 1.
     ENDIF.
     BDCCNT = BDCCNT + 1.

ENDFORM.
*-------------------------------------------------------------------

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

