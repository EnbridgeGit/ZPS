REPORT ZPPMR035 NO STANDARD PAGE HEADING.
* DB-Table
TABLES: PROJ, PRPS, COSP, JEST.
* Working Data
DATA:  BDCCNT      TYPE I,                "BDC counter to separate batch
       WPSPNR LIKE PRPS-PSPNR,            "Project
       LAST_PROJ LIKE PRPS-PSPNR.         "Last Project
*       FUNCID(4)   TYPE C.                "Function ID

DATA: BEGIN OF BDCDATA OCCURS 0.          "BDC Table
        INCLUDE STRUCTURE BDCDATA.        "Structure used for BDC Table
DATA: END OF BDCDATA.

* Type-pool of ALV
TYPE-POOLS: SLIS.
INCLUDE VEX01TOP.

************************************************************************
*                                                                      *
*                   Report Selections (VARIANTS)                       *
*                                                                      *
************************************************************************
*
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
*SELECT-OPTIONS SPOSID FOR  PRPS-POSID.
SELECT-OPTIONS SPSPHI FOR  PRPS-PSPHI.
SELECT-OPTIONS SVERNR FOR  PRPS-VERNR.
SELECT-OPTIONS SSTAT  FOR  JEST-STAT DEFAULT 'I0045' TO 'I0046'.
PARAMETERS     PVERSN LIKE COSP-VERSN.
PARAMETERS     PGJAHR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-001.
PARAMETERS: REPONLY RADIOBUTTON GROUP RBG1,     "Display Report Only
            STATCH1 RADIOBUTTON GROUP RBG1, "Change status for TECO/CLSD
            STATCH2 RADIOBUTTON GROUP RBG1. "Reverse status to TECO/CLSD
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 2(55) TEXT-002.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN INCLUDE BLOCKS EXTRACT.
SELECTION-SCREEN END OF BLOCK BOX.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
* Data to be displayed
DATA:
   BEGIN OF ITABLE OCCURS 0,
         VERNR  LIKE PRPS-VERNR,      "Division
         POSID  LIKE PRPS-POSID,      "Project ID
         PSPHI  LIKE PRPS-PSPHI,      "Project #
         POST1  LIKE PRPS-POST1,      "Project Description
         STAT   LIKE JEST-STAT,       "Project status
   END OF ITABLE.

DATA:  BEGIN OF FINTAB OCCURS 0,
         VERNR  LIKE PRPS-VERNR,      "Division
         POSID  LIKE PRPS-POSID,      "Project ID
         POST1  LIKE PRPS-POST1,      "Project Description
         STAT   LIKE JEST-STAT,       "Project status
         REMKS(14)  TYPE C,           "Remarks
       END OF FINTAB.

DATA: GS_EXTRACT1 LIKE DISEXTRACT.
DATA: GS_EXTRACT2 LIKE DISEXTRACT.
*---------------------------------------------------------------------*
*
INITIALIZATION.

  CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_INIT'
       CHANGING
            CS_EXTRACT1 = GS_EXTRACT1
            CS_EXTRACT2 = GS_EXTRACT2.

  PERFORM FIELDCAT_INIT  USING GT_FIELDCAT[].
*
AT SELECTION-SCREEN.

  PERFORM CHECK_VARIANTS.
  CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_SELSCREEN'
       EXPORTING
            I_P_SAVE  = P_SAVE
            I_P_LOAD  = P_LOAD
       CHANGING
            C_P_EX1   = P_EX1
            C_P_EX2   = P_EX2
            C_P_EXT1  = P_EXT1
            C_P_EXT2  = P_EXT2
            CS_EXTRACT1 = GS_EXTRACT1
            CS_EXTRACT2 = GS_EXTRACT2.


************************************************************************
*                                                                      *
*    Process on value request                                          *
*                                                                      *
************************************************************************
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_EX1.

  CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_F4_P_EX1'
*      EXPORTING
*           I_PARNAME_P_EXT1 = 'P_EXT1'
       CHANGING
            C_P_EX1          = P_EX1
            C_P_EXT1         = P_EXT1
            CS_EXTRACT1      = GS_EXTRACT1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_EX2.

  CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_F4_P_EX2'
*      EXPORTING
*           I_PARNAME_P_EXT2 = 'P_EXT2'
       CHANGING
            C_P_EX2          = P_EX2
            C_P_EXT2         = P_EXT2
            CS_EXTRACT2      = GS_EXTRACT2.

START-OF-SELECTION.
*PERFORM CHECK_VARIANTS.
************************************************************************
*                                                                      *
*            DATA SELECTION                                            *
*                                                                      *
************************************************************************
*
IF P_LOAD IS INITIAL.

SELECT PRPS~VERNR PRPS~POSID PRPS~PSPHI PRPS~POST1 JEST~STAT
  INTO TABLE ITABLE
FROM ( ( PRPS
       INNER JOIN COSP
       ON  COSP~OBJNR = PRPS~OBJNR )
       INNER JOIN JEST
       ON  JEST~OBJNR = PRPS~OBJNR )
*WHERE  PRPS~POSID IN SPOSID
WHERE  PRPS~PSPHI IN SPSPHI
  AND  PRPS~VERNR IN SVERNR
  AND  COSP~GJAHR =  PGJAHR
  AND  COSP~VERSN =  PVERSN
  AND  COSP~WRTTP =  '01'
  AND  JEST~STAT  IN SSTAT
  AND  JEST~INACT = SPACE.

   IF ITABLE[] IS INITIAL.
      WRITE: /1 'NO DATA SELECTED'.
      STOP.
   ENDIF.

  SORT ITABLE BY VERNR POSID STAT.

  DELETE ADJACENT DUPLICATES FROM ITABLE
         COMPARING VERNR POSID STAT.
*Determine all the projoect elements have same status or different.
LOOP AT ITABLE.

SELECT OBJNR INTO PRPS-OBJNR
  FROM PRPS
 WHERE PRPS~PSPHI = ITABLE-PSPHI.

 SELECT SINGLE STAT INTO JEST-STAT
   FROM JEST
  WHERE OBJNR = PRPS-OBJNR
    AND STAT  = ITABLE-STAT
    AND INACT = SPACE.
    IF SY-SUBRC <> 0.
       MOVE TEXT-005 TO FINTAB-REMKS.      "Do element level posting
    ENDIF.
ENDSELECT.
  MOVE-CORRESPONDING ITABLE TO FINTAB.
  APPEND FINTAB.
  CLEAR  FINTAB.
ENDLOOP.
*PERFORM OPEN_SESSION.

ENDIF.
************************************************************************
*         SAVE EXTRACT                                                 *
*         This paragraph is executed if the option to save             *
*         extract is chosen thru variants.                             *
*                                                                      *
************************************************************************
  IF P_SAVE = 'X'.
    CALL FUNCTION 'REUSE_ALV_EXTRACT_SAVE'
         EXPORTING
              IS_EXTRACT     = GS_EXTRACT1
              I_GET_SELINFOS = 'X'
         TABLES
              IT_EXP01       = FINTAB.
*              IT_EXP01       = GT_SFLIGHT.
***  EXIT.    V.V.I - Comment when report is required with save extract.
  ENDIF.


************************************************************************
* This paragraph is executed if the load from the previous             *
* extract is chosen thru variants. In this case new data is            *
* not selected from the SAP Tables.                                    *
************************************************************************
  IF P_LOAD = 'X'.
    CALL FUNCTION 'REUSE_ALV_EXTRACT_LOAD'
         EXPORTING
              IS_EXTRACT = GS_EXTRACT2
         TABLES
              ET_EXP01   = FINTAB.

  ENDIF.

  IF REPONLY <> 'X'.
     PERFORM CHANGE_PROJECT_STATUS.
  ENDIF.

************************************************************************
*                                                                      *
*                 Call ABAP/4 List Viewer                              *
*                                                                      *
************************************************************************
*
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = GS_EXTRACT1-REPORT
            IT_FIELDCAT              = GT_FIELDCAT[]
            I_SAVE           = 'A'
       TABLES
            T_OUTTAB         = FINTAB.


************************************************************************
*                                                                      *
*            Change the project status                                 *
*                                                                      *
************************************************************************
*
FORM CHANGE_PROJECT_STATUS.

     PERFORM OPEN_SESSION.
     IF STATCH1 = 'X'.
        PERFORM CANCEL_TECO_AND_CLSD.
     ELSEIF STATCH2 = 'X'.
        PERFORM DO_TECO_AND_CLSD.
     ENDIF.
    PERFORM CLOSE_SESSION.

ENDFORM.

************************************************************************
*                                                                      *
*          Change the status to Cancel TECO/CLSD.                      *
*                                                                      *
************************************************************************
*
FORM CANCEL_TECO_AND_CLSD.
 CLEAR LAST_PROJ.
 LOOP AT FINTAB.
   IF FINTAB-POSID+0(7) <> LAST_PROJ.
      MOVE FINTAB-POSID+0(7) TO LAST_PROJ.
      IF FINTAB-REMKS = SPACE.
         PERFORM POST_WBS_LEVEL.
      ENDIF.
   ENDIF.
 ENDLOOP.

ENDFORM.

************************************************************************
*                                                                      *
*              Change the status to TECO/CLSD                          *
*                                                                      *
************************************************************************
*
FORM DO_TECO_AND_CLSD.

 CLEAR LAST_PROJ.
 LOOP AT FINTAB.
   IF FINTAB-POSID+0(7) <> LAST_PROJ.
      MOVE FINTAB-POSID+0(7) TO LAST_PROJ.
      IF  FINTAB-REMKS = SPACE.
          PERFORM POST_PROJECT_LEVEL.
      ENDIF.
   ENDIF.
 ENDLOOP.

ENDFORM.

************************************************************************
*                  POST_WBS_LEVEL                                      *
* This routine processess the BDC session for the orders.              *
*                                                                      *
************************************************************************
*
FORM POST_WBS_LEVEL.
     WRITE FINTAB-POSID+0(7) TO WPSPNR.
     REFRESH BDCDATA.
     CLEAR BDCDATA.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0100'.
     PERFORM BDC_FIELD  USING '*PROJ-PSPID'        WPSPNR.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/5'.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        'MRKA'.
     IF FINTAB-STAT = 'I0045'.
        PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0901'.
        PERFORM BDC_FIELD  USING 'BDC_OKCODE'     'STTR'.
     ELSE.
        PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0901'.
        PERFORM BDC_FIELD  USING 'BDC_OKCODE'     'STAR'.
        PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0901'.
        PERFORM BDC_FIELD  USING 'BDC_OKCODE'     'STTR'.
     ENDIF.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0901'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/11'.

     PERFORM INSERT_SESSION.
     IF BDCCNT > '1000'.
        PERFORM CLOSE_SESSION.
        MOVE '0' TO BDCCNT.
        PERFORM OPEN_SESSION.
        BDCCNT = BDCCNT + 1.
     ENDIF.
     BDCCNT = BDCCNT + 1.
ENDFORM.
************************************************************************
*             POST_PROJECT_LEVEL                                       *
*This routine processess the BDC session for the orders                *
*                                                                      *
************************************************************************
FORM POST_PROJECT_LEVEL.
     WRITE FINTAB-POSID+0(7) TO WPSPNR.
     REFRESH BDCDATA.
     CLEAR BDCDATA.
     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '0100'.
     PERFORM BDC_FIELD  USING '*PROJ-PSPID'        WPSPNR.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        'MDTB'.   "PROJ DEF'N

     IF FINTAB-STAT = 'I0045'.
        PERFORM BDC_SCREEN USING 'SAPLCJWB'       '998'.
        PERFORM BDC_FIELD  USING 'BDC_OKCODE'     'STTE'.   "Close Tech.
     ELSE.
        PERFORM BDC_SCREEN USING 'SAPLCJWB'       '998'.
        PERFORM BDC_FIELD  USING 'BDC_OKCODE'     'STAB'.   "Close
     ENDIF.

     PERFORM BDC_SCREEN USING 'SAPLCJWB'          '998'.
     PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/11'.    "SAVE

     PERFORM INSERT_SESSION.
     IF BDCCNT > '1000'.
        PERFORM CLOSE_SESSION.
        MOVE '0' TO BDCCNT.
        PERFORM OPEN_SESSION.
        BDCCNT = BDCCNT + 1.
     ENDIF.
     BDCCNT = BDCCNT + 1.
ENDFORM.

************************************************************************
************************************************************************
*                                                                      *
* This routine will call the BDC function to open up a new BDC session *
*                                                                      *
************************************************************************
*
FORM OPEN_SESSION.
 CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT            = SY-MANDT
        GROUP             = 'ZPS_CHG_STAT'
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
    WRITE: /5 'ERROR IF OPENING SESSION'.
 ENDIF.
ENDFORM.
************************************************************************
*                                                                      *
*      This routine closes a batch input session.                      *
*                                                                      *
************************************************************************
*
FORM CLOSE_SESSION.
 CALL FUNCTION 'BDC_CLOSE_GROUP'.
 IF SY-SUBRC NE 0.
    WRITE: / 'BDC CLOSE GROUP ERROR. RC=', SY-SUBRC.
    EXIT.
 ENDIF.
ENDFORM.
*                                                                      *
*   This routine inserts the transaction for the BDC session           *
*                                                                      *
************************************************************************
*
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

************************************************************************
*                                                                      *
* This routine adds an entry to the BDCDATA table for transactions     *
*                                                                      *
************************************************************************
* . *
FORM BDC_SCREEN USING PROGRAM DYNPRO.
 CLEAR BDCDATA.
 BDCDATA-PROGRAM  = PROGRAM.
 BDCDATA-DYNPRO   = DYNPRO.
 BDCDATA-DYNBEGIN = 'X'.
 APPEND BDCDATA.
ENDFORM.

************************************************************************
*                                                                      *
* This routine adds an entry to the BDCDATA table using the fields     *
*                                                                      *
************************************************************************
*
FORM BDC_FIELD USING FNAM FVAL.
 CLEAR BDCDATA.
 BDCDATA-FNAM = FNAM.
 BDCDATA-FVAL = FVAL.
 APPEND BDCDATA.
ENDFORM.

************************************************************************
*                                                                      *
* This routine validates the varians data entered by the user          *
*                                                                      *
************************************************************************
*
FORM CHECK_VARIANTS.
     IF ( STATCH1 = 'X' AND P_SAVE <> 'X' )  OR
        ( STATCH2 = 'X' AND P_LOAD <> 'X' )  OR
        ( REPONLY = 'X' AND P_SAVE = 'X'  ).
        CALL FUNCTION 'POPUP_FOR_INTERACTION'
             EXPORTING
             HEADLINE    = '!! ERROR !!'
             TEXT1       = 'Invalid Radio Button Combo Selected'
             BUTTON_1    = 'OK'.
     STOP.
     ENDIF.
ENDFORM.

************************************************************************
*                                                                      *
*           Set up the column headings                                 *
*                                                                      *
************************************************************************
*
FORM FIELDCAT_INIT USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR LS_FIELDCAT.

  LS_FIELDCAT-FIELDNAME     = 'VERNR'.
  LS_FIELDCAT-COL_POS       = 1.
  LS_FIELDCAT-REPTEXT_DDIC  = 'DIVISION'.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

  LS_FIELDCAT-FIELDNAME     = 'POSID'.
  LS_FIELDCAT-COL_POS       = 2.
  LS_FIELDCAT-REPTEXT_DDIC  = '   PROJECT   .'.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

  LS_FIELDCAT-FIELDNAME     = 'POST1'.
  LS_FIELDCAT-COL_POS       = 3.
  LS_FIELDCAT-REPTEXT_DDIC  = '       DESCRIPTION       .'.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

  LS_FIELDCAT-FIELDNAME     = 'STAT'.
  LS_FIELDCAT-COL_POS       = 4.
  LS_FIELDCAT-REPTEXT_DDIC  = 'STATUS'.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

  LS_FIELDCAT-FIELDNAME     = 'REMKS'.
  LS_FIELDCAT-COL_POS       = 5.
  LS_FIELDCAT-REPTEXT_DDIC  = '   REMARKS   .'.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

ENDFORM.

