REPORT ZPPMI008 NO STANDARD PAGE HEADING LINE-SIZE 120 LINE-COUNT 90.

*---------------------------------------------------------------------*
*       REPORT ZPPMI008                                               *
*       AUTHOR M. Khan                                                *
*       DATE   October, 2000.                                         *
*---------------------------------------------------------------------*
*   This program is used to replace the existing SAP sum up planning  *
*   amount  in  PS for transaction (CJ40).                            *
*   It will collect the from and to projects and version info. from   *
*   the variants. This range is supposed to be the same as used in    *
*   transaction CJ9B which should be run before this program.         *
*---------------------------------------------------------------------*
*CHANGES:                                                             *
*                                                                     *
*Date     TR#       BY       D E S C R I P T I O N                    *
*------- ------  ----------  -----------------------------------------*
*27/1/09  TR580  M. Khan     Change screen 310 to 320 for Upgrade 6.0 *
*                                                                     *
*---------------------------------------------------------------------*

TABLES:
  PROJ.     "Project Definition

DATA: BEGIN OF PROJTAB OCCURS 100,
      PSPID  LIKE PROJ-PSPID,                       "Project number
      POST1  LIKE PROJ-POST1.                       "Description
DATA: END OF PROJTAB.

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

*------------------------ Selection Screen  ---------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT  1(31) TEXT-016.
      PARAMETERS: P_TEST  AS CHECKBOX DEFAULT ' '.
    SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS: SPSPID   FOR PROJ-PSPID.
PARAMETERS:     PVERSION LIKE COSP-VERSN DEFAULT '000'.
SELECTION-SCREEN END OF BLOCK BOX2.

*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.
  PERFORM GET-PROJECTS-RECORDS.
* NEW-PAGE.
  PERFORM WRITE_REPORT.
IF P_TEST = 'X'.
  PERFORM OPEN_BDC.
  LOOP AT PROJTAB.
    REFRESH BDCDATA.
*                             Change Project: Initial Screen
    PERFORM BDC_SCREEN USING 'SAPMKBUD'    '200'.
    PERFORM BDC_FIELD  USING 'PROJ-PSPID'  PROJTAB-PSPID.
    PERFORM BDC_FIELD  USING 'BPDY-VERSN'  PVERSION.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/5'.    "Cost planning button

*                             Next Screen
*    PERFORM BDC_SCREEN USING 'SAPLKBPP' '310'.             "TR580
    PERFORM BDC_SCREEN USING 'SAPLKBPP' '320'.              "TR580
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'SYNC'. "Total Up

    PERFORM BDC_SCREEN USING 'SAPLKBPP' '705'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/5'.  "Press Enter Key

*    PERFORM BDC_SCREEN USING 'SAPLKBPP' '310'.             "TR580
    PERFORM BDC_SCREEN USING 'SAPLKBPP' '320'.              "TR580
    PERFORM BDC_SCREEN USING 'SAPLKBPP' '310'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.  "Save

    PERFORM INSERT_BDC.
  ENDLOOP.
  PERFORM CLOSE_BDC.
ENDIF.

*-----------------------------------------------------------------------
*  Description:
*  - This routine selects the projects depending opon selection cirteria
*    and load them into internal table.
*-----------------------------------------------------------------------
FORM GET-PROJECTS-RECORDS.

  SELECT PSPID POST1
    FROM PROJ
    INTO CORRESPONDING FIELDS OF PROJTAB
    WHERE PSPID IN SPSPID.
    APPEND PROJTAB.

  ENDSELECT.

ENDFORM.

*------------------------- WRITE_REPORT --------------------------------
FORM WRITE_REPORT.

  SORT PROJTAB BY PSPID.
  LOOP AT PROJTAB.
    WRITE: /1 PROJTAB-PSPID, 15 PROJTAB-POST1.
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
            GROUP               = 'ZPS_PLAN_SUM'
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
            TCODE          = 'CJ40'
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

WRITE: /20 'PROJECTS TO BE PROCESSED'.
WRITE: /20 '------------------------'.
    WRITE: /1 text-006, 15 text-007.
  ULINE.
