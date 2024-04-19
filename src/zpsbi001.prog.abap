REPORT ZPSBI001 MESSAGE-ID ZS LINE-SIZE 80 LINE-COUNT 59.
***********************************************************************
*       Owner: Centra/Union                                           *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)        *
*        Date: December 12th, 1996                                    *
*  Request ID: DRPP0124                                               *
*                                                                     *
* This BDC program produces a mass copy of planning from one version  *
* to another.                                                         *
***********************************************************************
* 97/11/04 #274 md7140 - add year select-options & eliminate template
*                        processing
***********************************************************************
TABLES: PROJ,                         "Project definition
        TKA09.                        "Basic settings for versions

DATA: TMPDATE LIKE SY-DATUM,          "Temporary date
      AMOUNT  LIKE BSEG-WRBTR,        "Amount variable
      BDCCNT1      TYPE I,            "BDC counter 1
      BDCCNT2      TYPE I.            "BDC counter 2
DATA: BEGIN OF BDCDATA OCCURS 10000.  "BDC Table
        INCLUDE STRUCTURE BDCDATA.    "Structure used for BDC Table
DATA: END OF BDCDATA.

DATA: BEGIN OF PROJECT OCCURS 10000,
        PSPID LIKE PROJ-PSPID,         "Project ID
      END OF PROJECT,

      BEGIN OF VERSION OCCURS 500,
        VERSN LIKE TKA09-VERSN,         "Project version
      END OF VERSION.
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BEGIN WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS S_PROJ FOR PROJ-PSPID MODIF ID ABC.

SELECTION-SCREEN BEGIN OF BLOCK INPUTS WITH FRAME TITLE TEXT-001.
PARAMETERS: P_REF   LIKE TKA09-VERSN OBLIGATORY DEFAULT '000'
                    MODIF ID ABC,
            P_TAR   LIKE TKA09-VERSN OBLIGATORY DEFAULT '001'
                    MODIF ID ABC,
            P_YEAR  LIKE BPJA-GJAHR OBLIGATORY DEFAULT SY-DATUM(4)
                    MODIF ID ABC,
            P_YRTO  LIKE BPJA-GJAHR MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK INPUTS.

SELECTION-SCREEN END OF BLOCK BEGIN.
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
 PERFORM GET_PROJECT.
 PERFORM GET_VERSION.
 PERFORM OPEN_SESSION1.
 LOOP AT PROJECT.
   PERFORM POST_PROJECT.
 ENDLOOP.
 PERFORM CLOSE_SESSION1.

*perform open_session2.
*loop at project.
*  perform post_update.
*endloop.
*perform close_session2.

END-OF-SELECTION.
************************************************************************

* This routine intializes variables and internal tables.*
FORM INITIALIZE.
 REFRESH: PROJECT, VERSION.
 CLEAR:   PROJECT, VERSION.
 WRITE    SY-DATUM TO TMPDATE.
ENDFORM.

* This routine will get all the appropriate data for projects. *
FORM GET_PROJECT.
  SELECT * FROM PROJ WHERE PSPID IN S_PROJ
    ORDER BY PSPID.
    IF PROJ-PSPID+4(3) CO '0123456789'.           "Template elimination
       CLEAR PROJECT.
       MOVE  PROJ-PSPID TO PROJECT-PSPID.
       APPEND PROJECT.
    ENDIF.                                        "End of template
  ENDSELECT.
ENDFORM.

* This routine will get all the appropriate data for versions. *
FORM GET_VERSION.
  SELECT * FROM TKA09 WHERE KOKRS = '10' AND VERSN >'000'
    ORDER BY VERSN.
    CLEAR VERSION.
    MOVE TKA09-VERSN TO VERSION-VERSN.
    APPEND VERSION.
  ENDSELECT.
ENDFORM.

* This routine will process a BDC session for projects (copy). *
FORM POST_PROJECT.
     REFRESH BDCDATA.
     CLEAR BDCDATA.
     PERFORM BDC_SCREEN USING 'SAPMKBUD'      '0910'.
     PERFORM BDC_FIELD  USING 'PROJ-PSPID'    PROJECT-PSPID.
     PERFORM BDC_FIELD  USING 'BPDY-V_VERS_V' P_REF.
     PERFORM BDC_FIELD  USING 'BPDY-V_VERS_Z' P_TAR.

     PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/7'.       "Timeframe key

     PERFORM BDC_SCREEN USING 'SAPMKBUD'         '0911'.
     PERFORM BDC_FIELD  USING 'BPDY-V_YEAR_X'    'X'.
     PERFORM BDC_FIELD  USING 'BPDY-V_YEAR_XS'   P_YEAR.
     PERFORM BDC_FIELD  USING 'BPDY-V_YEAR_XE'   P_YRTO.
     PERFORM BDC_FIELD  USING 'BPDY-V_GESAMT'    'X'.

     PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/2'.
     PERFORM BDC_SCREEN USING 'SAPMKBUD'      '0910'.

     PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/5'.

     PERFORM BDC_SCREEN USING 'SAPLSPO1'      '100'.
     PERFORM BDC_FIELD  USING 'BDC_CURSOR'   'SPOP-OPTION1'.
*end of screen
     PERFORM INSERT_SESSION1.
     IF BDCCNT1 > '1000'.
        PERFORM CLOSE_SESSION1.
        MOVE '0' TO BDCCNT1.
        PERFORM OPEN_SESSION1.
        BDCCNT1 = BDCCNT1 + 1.
     ENDIF.
     BDCCNT1 = BDCCNT1 + 1.
     WRITE: /1 PROJECT-PSPID, 33 P_REF, 48 P_TAR.
ENDFORM.

* This routine will process a BDC session for projects (copy). *
*form post_update.
*     refresh bdcdata.
*     clear bdcdata.
*     perform bdc_screen using 'SAPMKBUD'      '0200'.
*     perform bdc_field  using 'PROJ-PSPID'    project-pspid.
*     perform bdc_field  using 'BPDY-VERSN'    p_tar.
*     perform bdc_screen using 'SAPLKBPP'      '0300'.
*     perform bdc_field  using 'BDC_OKCODE'    'SYNC'.
*    perform bdc_screen using 'SAPLKBPP'      '0705'.
*     perform bdc_field  using 'BDC_OKCODE'    '/5'.
*     perform bdc_screen using 'SAPLKBPP'      '0300'.
*     perform bdc_field  using 'BDC_OKCODE'   '/11'.

*    PERFORM BDC_SCREEN USING 'SAPLSPO1'      '100'.
*    PERFORM BDC_FIELD  USING 'BDC_CURSOR'   'SPOP-OPTION1'.
*    PERFORM BDC_SCREEN USING 'SAPMKBUD'      '0910'.
*    PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/3'.
*end of screen
*     perform insert_session2.
*     if bdccnt2 > '1000'.
*        perform close_session2.
*        move '0' to bdccnt2.
*        perform open_session2.
*       bdccnt2 = bdccnt2 + 1.
*    endif.
*     bdccnt2 = bdccnt2 + 1.
*endform.

* This routine closes a batch input session. *
FORM CLOSE_SESSION1.
 CALL FUNCTION 'BDC_CLOSE_GROUP'.
 IF SY-SUBRC NE 0.
    WRITE: / 'BDC CLOSE GROUP ERROR. RC=', SY-SUBRC.
    EXIT.
 ENDIF.
ENDFORM.


* This routine closes a batch input session. *
FORM CLOSE_SESSION2.
 CALL FUNCTION 'BDC_CLOSE_GROUP'.
 IF SY-SUBRC NE 0.
    WRITE: / 'BDC CLOSE GROUP ERROR. RC=', SY-SUBRC.
    EXIT.
 ENDIF.
ENDFORM.

* This routine inserts the transaction for the BDC session1. *
FORM INSERT_SESSION1.
 CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        TCODE          = 'CJ9B'
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

* This routine inserts the transaction for the BDC session1. *
FORM INSERT_SESSION2.
 CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        TCODE          = 'CJ40'
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
FORM OPEN_SESSION1.
 CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT            = SY-MANDT
        GROUP             = 'ZPS_MAS_CPY'
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

* This routine will call the BDC function to open up a new BDC session *
*form open_session2.
* call function 'BDC_OPEN_GROUP'
*      exporting
*        client            = sy-mandt
*        group             = 'ZPS_UPDATE'
*       HOLDDATE          =
*        keep              = 'X'
*        user              = sy-uname
*      exceptions
*        group_invalid     = 1
*        group_is_locked   = 2
*        holddate_invalid  = 3
*       internal_error    = 4
*        queue_errorid     = 5
*        running           = 6
*        system_lock_error = 7
*        user_invalidd     = 8.
* if sy-subrc <> 0.
*    message e004.
* endif.
*endform.
