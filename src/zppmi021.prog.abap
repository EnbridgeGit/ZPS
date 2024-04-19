REPORT ZPPMI021 NO STANDARD PAGE HEADING LINE-SIZE 100
LINE-COUNT 60 MESSAGE-ID ZS.

************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        MARCH 2005.                                            *
*  Description:                                                        *
*     - The purpose of this program is to creatr a file for Project    *
*       system consolidation.                                          *
************************************************************************

TABLES: PROJ,         "Project
        PRPS,         "WBS
        COSP,         "External Costs
        COSS,         "Internal Costs
        ZJHDR,        "Journal Header for Consolidation
        ZPCMS,        "Project System Consolidation
        T001B.        "For field reference only

DATA:
    BEGIN OF ITABLE OCCURS 0,
       VBUKR LIKE PROJ-VBUKR,
       KSTAR LIKE COSP-KSTAR,
       PSPID LIKE PROJ-PSPID,
       WKG001 LIKE COSP-WKG001,
    END OF ITABLE.

DATA:
    BEGIN OF OUTREC OCCURS 0,
       REC01(60),
    END OF OUTREC.

DATA:
    BEGIN OF OUTAB OCCURS 0,
       BUNIT         LIKE ZJHDR-BUNIT,          "Business unit
       PSPID(9)      TYPE C,                    "Project Number
       GJAHR(4)      TYPE N,                    "Fiscal Year
       PEROD(4)      TYPE N,                    "Period
       CATGR(2)      TYPE C,                    "Amount Category
       DTYPE(6)      TYPE C,                    "Detail Type
       AMOUNT        LIKE COSP-WKG001,          "Money
   END OF OUTAB.

DATA: BEGIN OF SEL_FIELDS  OCCURS 3,
      AFIELD(30)  TYPE C,
      END OF SEL_FIELDS.

DATA: WRK_BUNIT  LIKE ZJHDR-BUNIT,
      WRK_CATGR  LIKE ZPCMS-CATGR,
      WRK_DTYPE  LIKE ZPCMS-DTYPE,
      WRK_AMOUNT(15) TYPE C.


CONSTANTS:  H_DMTR  TYPE X VALUE '09'.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 20(55) TEXT-101.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
  S_VBUKR FOR  PROJ-VBUKR,                                    "Company
  S_PSPID FOR  PROJ-PSPID  OBLIGATORY.                        "Proj #
PARAMETERS:
  P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY,   "Year
  P_PEROD LIKE T001B-FRPE1 OBLIGATORY.                        "Period

PARAMETERS:    FNAME1   TYPE RLGRAP-FILENAME DEFAULT
                            'C:\saptemp\sap_proj_actuals_East.txt'.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  D_BUNIT LIKE ZJHDR-BUNIT DEFAULT '10999'  OBLIGATORY,  "Business Unit
  D_CATGR LIKE ZPCMS-CATGR DEFAULT 'X'      OBLIGATORY,  "Amt. Category
  D_DTYPE LIKE ZPCMS-DTYPE DEFAULT 'XXXXXX' OBLIGATORY.  "Detail Type
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN END OF BLOCK BOX.

*-------------------AT SELECTION SCREEN -------------------------------*
AT SELECTION-SCREEN.
  IF SY-BATCH = 'X'.
     MESSAGE E019 WITH TEXT-101.
  ENDIF.

  IF P_PEROD < 1 OR P_PEROD > 12.
     MESSAGE E019 WITH TEXT-102.
  ENDIF.

*----------------------- MAIN SECTION ---------------------------------*

START-OF-SELECTION.

*Get COSP data
  MOVE  'PROJ~VBUKR' TO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.
  MOVE  'COSP~KSTAR' TO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.
  MOVE  'PROJ~PSPID' TO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.
  CONCATENATE 'COSP~WKG' P_PEROD INTO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.

 SELECT (SEL_FIELDS)
   INTO TABLE ITABLE
   FROM  ( ( PROJ INNER JOIN PRPS
              ON PROJ~PSPNR = PRPS~PSPHI )
              INNER JOIN COSP
              ON PRPS~OBJNR = COSP~OBJNR )

 WHERE  PROJ~PSPID  IN  S_PSPID           "Project #
   AND  PROJ~VBUKR  IN  S_VBUKR           "Company Code
   AND  PROJ~VKOKR   =  '10'              "Controlling Area
   AND  PROJ~LOEVM  <>  'X'               "Deletion Indicator PROJ
   AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator PRPS
   AND  COSP~GJAHR   =  P_FYEAR           "Fiscal Year
   AND  COSP~WRTTP   =  '04'              "Record with actuals $
   AND  COSP~KSTAR  NOT IN ('491001','491002')  "Cost Element
   AND  COSP~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator

*Get COSS data
  CLEAR SEL_FIELDS.
  REFRESH SEL_FIELDS.
  MOVE  'PROJ~VBUKR' TO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.
  MOVE  'COSS~KSTAR' TO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.
  MOVE  'PROJ~PSPID' TO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.
  CONCATENATE 'COSS~WKG' P_PEROD INTO SEL_FIELDS-AFIELD.
  APPEND SEL_FIELDS.

 SELECT (SEL_FIELDS)
 APPENDING  TABLE ITABLE
   FROM  ( ( PROJ INNER JOIN PRPS
              ON PROJ~PSPNR = PRPS~PSPHI )
              INNER JOIN COSS
              ON PRPS~OBJNR = COSS~OBJNR )

 WHERE  PROJ~PSPID  IN  S_PSPID           "Project #
   AND  PROJ~VBUKR  IN  S_VBUKR           "Company Code
   AND  PROJ~VKOKR   =  '10'              "Controlling Area
   AND  PROJ~LOEVM  <>  'X'               "Deletion Indicator PROJ
   AND  PRPS~LOEVM  <>  'X'               "Deletion Indicator PRPS
   AND  COSS~GJAHR   =  P_FYEAR           "Fiscal Year
   AND  COSS~WRTTP   =  '04'              "Record with actuals $
   AND  COSS~KSTAR  NOT IN ('491001','491002')  "Cost Element
   AND  COSS~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator

IF ITABLE[] IS INITIAL.
   WRITE: /1 'NO DATA SELECTED'.
   STOP.
ENDIF.

SORT ITABLE BY VBUKR KSTAR PSPID.
 LOOP AT ITABLE.

*Get Business Unit
   AT NEW VBUKR.
      CLEAR WRK_BUNIT.
      SELECT SINGLE BUNIT INTO WRK_BUNIT FROM ZJHDR
       WHERE BUKRS = ITABLE-VBUKR.
            IF SY-SUBRC <> 0.
               MOVE D_BUNIT TO WRK_BUNIT.     "Use default value
            ENDIF.
   ENDAT.

*Get Amount Category and Detail Type
   AT NEW KSTAR.
   CLEAR: WRK_CATGR, WRK_DTYPE.
   SELECT SINGLE CATGR DTYPE INTO (WRK_CATGR, WRK_DTYPE)
    FROM  ZPCMS
    WHERE KSTAR = ITABLE-KSTAR.
    IF SY-SUBRC <> 0.
       MOVE D_CATGR TO WRK_CATGR.     "Use default value
       MOVE D_DTYPE TO WRK_DTYPE.     "Use default value
    ENDIF.
   ENDAT.

   MOVE ITABLE-PSPID+0(7)  TO  OUTAB-PSPID.
   MOVE ITABLE-WKG001      TO  OUTAB-AMOUNT.
   MOVE P_FYEAR            TO  OUTAB-GJAHR.
   MOVE P_PEROD            TO  OUTAB-PEROD.
   MOVE WRK_BUNIT          TO  OUTAB-BUNIT.
   MOVE WRK_CATGR          TO  OUTAB-CATGR.
   MOVE WRK_DTYPE          TO  OUTAB-DTYPE.
   COLLECT OUTAB.
   CLEAR   OUTAB.
 ENDLOOP.
*
 LOOP AT OUTAB.
   IF OUTAB-AMOUNT <> 0.
      MOVE OUTAB-AMOUNT TO WRK_AMOUNT.
      PERFORM PUT_SIGN_IN_FRONT CHANGING WRK_AMOUNT.
      CONCATENATE OUTAB-BUNIT OUTAB-PSPID+0(7) OUTAB-GJAHR OUTAB-PEROD
                  OUTAB-CATGR OUTAB-DTYPE WRK_AMOUNT
                  INTO OUTREC-REC01
                  SEPARATED BY H_DMTR.
      APPEND OUTREC.
      CLEAR  OUTREC.
   ENDIF.
 ENDLOOP.
 PERFORM DOWNLOAD_FILE.

************************************************************************
FORM PUT_SIGN_IN_FRONT CHANGING VALUE.
  DATA: TEXT1(1) TYPE C.

  SEARCH VALUE FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    SPLIT VALUE AT '-' INTO VALUE TEXT1.
    CONDENSE VALUE.
    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE VALUE.
  ENDIF.
ENDFORM.
************************************************************************
FORM DOWNLOAD_FILE.

    CALL FUNCTION 'WS_DOWNLOAD'
         EXPORTING
              FILENAME            = FNAME1
              FILETYPE            = 'ASC'
         TABLES
              DATA_TAB            = OUTREC
         EXCEPTIONS
              FILE_OPEN_ERROR     = 1
              FILE_WRITE_ERROR    = 2
              INVALID_FILESIZE    = 3
              INVALID_TABLE_WIDTH = 4
              INVALID_TYPE        = 5
              NO_BATCH            = 6
              UNKNOWN_ERROR       = 7
              OTHERS              = 8.

ENDFORM.
