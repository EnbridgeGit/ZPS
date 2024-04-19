REPORT ZPPMI003 NO STANDARD PAGE HEADING LINE-SIZE 74
                LINE-COUNT 65 MESSAGE-ID ZM.
************************************************************************
*
*   PROGRAM:    ZPPMI003
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       May 1998.
*
*   The purpose of this program is to do a mass creation of blanket
*   projects.  The projects are created by the parameters on the
*   selection screen.  Project dd-y2-rrr is created based on project
*   dd-y1-rrr where dd is the division, y1 is the copy from year, y2
*   is the create year and rrr is the project range.  All WBS elements
*   are also created from the source project.  The update is performed
*   via a BDC session.
*
*CHANGES:
*   2000/06/07   Issue No. 691        By: M. Khan
*                Fill up the start and finish dates of projct.
*                Delete the Settlement rules & Asset Value Date
*   2000/07/25                        By: M. Khan
*                4.6 b changes:  1- Screen number changes.
*                                2- PF keys/Buttons assignment changes.
* 2018/09/18   AKMADASU        AAdd GL account to settlement rule upon
*                              project creation
************************************************************************

TABLES:   PROJ,                           " Project definition
          PRPS, COBRB.                                  "Issue no. 691
RANGES:   XPSPID        FOR PROJ-PSPID.                 " project range

DATA:     SOURCE_PR(9)  TYPE C,                         " blanket proj
          TARGET_PR(9)  TYPE C,                         " project creatd
          COUNT(6)      TYPE N VALUE 0,                 " record count
          KOUNT(4)      TYPE N,                         "Issue no. 691
          DEL_RECS(4)   TYPE N,                         "Issue no. 691
          LOW_POSKI     LIKE PRPS-POSKI,                "Issue no. 691
          HIGH_POSKI    LIKE PRPS-POSKI,                "Issue no. 691
          SEPARATOR     VALUE '-',                      "Issue no. 691
          FIRST_PRPS    VALUE 'Y',                      "Issue no. 691
          FIRST_COBRB   VALUE 'Y',                      "Issue no. 691
          FIELD_NAME    LIKE BDCDATA-FNAM,              "Issue no. 691
          START_DATE    LIKE PROJ-PLFAZ,                "Issue no. 691
          FINISH_DATE   LIKE PROJ-PLSEZ,                "Issue no. 691
          SEARCH_PROJ(5),                               "Issue no. 867
          REPLAC_PROJ(5).                               "Issue no. 867

CONSTANTS: START_MMDD(4)   TYPE C VALUE '0102',         "Issue no. 691
           END_MMDD(4)     TYPE C VALUE '1231'.         "Issue no. 691

DATA:     BEGIN OF IPROJ OCCURS 500,
            PSPID_S     LIKE PROJ-PSPID,                " source project
            PSPHI_S(7)  TYPE N,                         " issue no 691
            POST1       LIKE PROJ-POST1,                " description
            PSPID_T     LIKE PROJ-PSPID,                " target project
          END OF IPROJ.

DATA:     BEGIN OF BDCDATA OCCURS 100.
            INCLUDE STRUCTURE BDCDATA.
DATA:     END OF BDCDATA.
**--START OF CHANGES BY AKMADASU
TYPES: BEGIN OF ty_prps,
  PSPNR	  TYPE PS_POSNR,    "WBS Element
  PSPHI	  TYPE PS_PSPHI,    "Current number of the appropriate project
  BELKZ	  TYPE PS_BELKZ,    "Indicator: Account assignment element
  END OF ty_prps.
DATA: gv_wbs_count TYPE i.    "To hold the WBS count for each project
**00 END OF CHANGES BY AKMDASU
************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
 SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 26(24) TEXT-003.
 SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    SELECT-OPTIONS: S_PSPID   FOR PROJ-PSPID OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK BOX2.
  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-011.
      PARAMETERS: P_YR_TO(4)  TYPE C OBLIGATORY
                                     MODIF ID ABC.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK BOX3.
  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT  3(27) TEXT-016.
      PARAMETERS: P_TEST      AS CHECKBOX DEFAULT ' '.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK BOX4.
SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: IPROJ, BDCDATA.
CLEAR:   IPROJ, BDCDATA.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* default year on selection screen to last year and this year
 AT SELECTION-SCREEN OUTPUT.
   IF P_YR_TO IS INITIAL.
   P_YR_TO = SY-DATUM(4) + 2.
   LOOP AT SCREEN.
     CHECK SCREEN-GROUP1 = 'ABC'.
     MODIFY SCREEN.
   ENDLOOP.
   ENDIF.
* extract required data
START-OF-SELECTION.

* verify that the target year is valid
  IF P_YR_TO < '0001'
  OR P_YR_TO > '9999'.
    MESSAGE E100 WITH TEXT-015.
  ENDIF.

* calculate start and finish dates of projects.          "Issue no. 691
  CONCATENATE P_YR_TO START_MMDD INTO START_DATE.
  CONCATENATE P_YR_TO   END_MMDD INTO FINISH_DATE.

* now get the projects to be copied and created
  SELECT * FROM PROJ
  WHERE PSPID IN S_PSPID.

    COUNT              = COUNT + 1.
    IPROJ-PSPID_S      = PROJ-PSPID.
    MOVE PROJ-PSPID+0(7) TO IPROJ-PSPHI_S.                "Issue no. 691
    IPROJ-POST1        = PROJ-POST1.
    IPROJ-PSPID_T      = PROJ-PSPID.
    IPROJ-PSPID_T+2(2) = P_YR_TO+2(2).
    APPEND IPROJ.
    CLEAR IPROJ.

  ENDSELECT.

* sort the project table by the source project code
  SORT IPROJ BY PSPID_S.

* open the BDC session if required
  IF P_TEST = 'X'.
    PERFORM OPEN_BATCH_SESSION.
  ENDIF.

* Process the table, outputing the report and create BDC if required
  LOOP AT IPROJ.

    PERFORM WRITE_DETAIL.

    IF P_TEST = 'X'.
      PERFORM CREATE_NEW_PROJECT.
    ENDIF.

  endloop.

  PERFORM WRITE_TOTALS.

* close the BDC session if required
  IF P_TEST = 'X'.
    PERFORM CLOSE_BATCH_SESSION.
  ENDIF.

*-----------------------------------------------------------------------
*   FORM CREATE_NEW_PROJECT
*-----------------------------------------------------------------------
*  This section uses transaction CJ01 to create new blanket projects
*  based on last years blanket projects.
*-----------------------------------------------------------------------
FORM CREATE_NEW_PROJECT.
 CONCATENATE IPROJ-PSPID_S+0(2) '-' IPROJ-PSPID_S+2(2) INTO SEARCH_PROJ.
 CONCATENATE IPROJ-PSPID_T+0(2) '-' IPROJ-PSPID_T+2(2) INTO REPLAC_PROJ.
  PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0100'.
  PERFORM BDC_FIELD  USING '*PROJ-PSPID'    IPROJ-PSPID_T. " new project
  PERFORM BDC_FIELD  USING 'RCWKP-VORLAGE'  IPROJ-PSPID_S. " old project
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     'MDTB'.        " proj defin.

* PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0200'.
  PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0998'.       "4.6 b changes

  PERFORM BDC_FIELD  USING 'PROJ-PLFAZ'     START_DATE.   "Issue no. 691
  PERFORM BDC_FIELD  USING 'PROJ-PLSEZ'     FINISH_DATE.  "Issue no. 691
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/5'.          " <WBS struc>

* PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0300'.
  PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0900'.       "4.6 b changes
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/6'.          " <Replace>

  PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0560'.
  PERFORM BDC_FIELD  USING 'RCWKP-SUCHEN'   SEARCH_PROJ.    "Issue 867
  PERFORM BDC_FIELD  USING 'RCWKP-ERSETZEN' REPLAC_PROJ.    "Issue 867
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/6'.          " <Replace>

* PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0300'.
  PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0900'.       "4.6 b changes
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/7'.          " <Check>

  PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0900'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.          " <Include>
*                                              "Issue no.691 changes
* PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0200'.
  PERFORM BDC_SCREEN USING 'SAPLCJWB'       '0998'.       "4.6 b changes
* PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/5'.          " <WBS struc>
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     'LETB'.       "4.6 b changes
  PERFORM SETTLEMENT_RULE.
**--START OF CHANGES BY AKMADASU
    PERFORM add_gl_account.
**--END OF CHANGES BY AKMADASU
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/11'.         " <Save>

  PERFORM INSERT_SESSION.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*                                                       Issue no. 691
*  This section fills the bdc table for settlement rules delete
*-----------------------------------------------------------------------
FORM SETTLEMENT_RULE.
 KOUNT = 0.
 CLEAR: LOW_POSKI, HIGH_POSKI.
 CONCATENATE IPROJ-PSPHI_S+0(2) IPROJ-PSPHI_S+2(2) IPROJ-PSPHI_S+4(3)
                      INTO LOW_POSKI SEPARATED BY SEPARATOR.
 CONCATENATE LOW_POSKI '9999' INTO HIGH_POSKI SEPARATED BY SEPARATOR.
 FIRST_PRPS = 'Y'.
 SELECT * FROM PRPS
    WHERE POSKI BETWEEN LOW_POSKI AND HIGH_POSKI.
    IF SY-SUBRC = 0.
       KOUNT = KOUNT + 1.
       IF KOUNT = 18.
          PERFORM BDC_SCREEN USING  'SAPLCJWB'      '0901'.
          PERFORM BDC_FIELD  USING  'BDC_OKCODE'    'P+'.
          KOUNT = 1.
       ENDIF.
       IF PRPS-BELKZ = 'X'.
          IF FIRST_PRPS = 'Y'.
             PERFORM BDC_SCREEN USING 'SAPLCJWB'        '0901'.
             FIRST_PRPS = 'N'.
          ENDIF.
          CLEAR: FIELD_NAME, DEL_RECS.
          FIRST_COBRB = 'Y'.
          SELECT * FROM COBRB
            WHERE  OBJNR = PRPS-OBJNR.
            IF SY-SUBRC = '0' AND COBRB-ANLN1+0(1) > '2'.  "Ignore 1 & 2
               IF FIRST_COBRB = 'Y'.
                CONCATENATE 'RCJ_MARKL-MARK(' KOUNT ')' INTO FIELD_NAME.
                PERFORM BDC_FIELD  USING  FIELD_NAME          'X'.
                PERFORM BDC_FIELD  USING 'BDC_OKCODE'        '/7'.
                FIRST_COBRB = 'N'.
               ENDIF.
               PERFORM BDC_SCREEN USING 'SAPLKOBS'      '0130'.
               PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'COBRB-KONTY(1)'.
               PERFORM BDC_FIELD  USING 'BDC_OKCODE'    'DELE'.
               DEL_RECS = DEL_RECS + 1.
            ENDIF.
          ENDSELECT.
            IF DEL_RECS > 0.
               PERFORM BDC_SCREEN USING 'SAPLKOBS'      '0130'.
               PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/8'.
               PERFORM BDC_SCREEN USING 'SAPLKOBS'      '0110'.
               PERFORM BDC_FIELD  USING 'COBRA-BZDAT'   ''.
               PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/3'.
               PERFORM BDC_SCREEN USING 'SAPLKOBS'      '0130'.
               PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/3'.
               PERFORM BDC_SCREEN USING 'SAPLCJWB'      '0901'.
               PERFORM BDC_FIELD  USING  FIELD_NAME     ' '.
            ENDIF.
       ENDIF.
    ENDIF.
 ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*  this section outputs the report headings
*-----------------------------------------------------------------------
FORM WRITE_HEADER.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 030 TEXT-001
            , 064 TEXT-002,  SY-PAGNO
            , 074 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 025 TEXT-003
            ,     SY-REPID UNDER TEXT-002
            , 074 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-004
            , 018 TEXT-005
            , 063 TEXT-006.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

*-----------------------------------------------------------------------
*  this section writes detail lines to the report
*-----------------------------------------------------------------------
FORM WRITE_DETAIL.
     CONCATENATE IPROJ-PSPID_S(2)   '-'
                 IPROJ-PSPID_S+2(2) '-'
                 IPROJ-PSPID_S+4(3) INTO SOURCE_PR.
     CONCATENATE IPROJ-PSPID_T(2)   '-'
                 IPROJ-PSPID_T+2(2) '-'
                 IPROJ-PSPID_T+4(3) INTO TARGET_PR.
     WRITE: /    SOURCE_PR          UNDER TEXT-004
            ,    IPROJ-POST1        UNDER TEXT-005
            ,    TARGET_PR          UNDER TEXT-006.
     PERFORM SHOWVLINE.
ENDFORM.

*-----------------------------------------------------------------------
*  this section writes a total line to the report showing recs processed
*-----------------------------------------------------------------------
FORM WRITE_TOTALS.
     ULINE.
     WRITE:  /001 SY-VLINE
            , 003 COUNT, TEXT-007
            , 074 SY-VLINE.
     ULINE.
ENDFORM.

*-----------------------------------------------------------------------
*  this section outputs vertical lines to the report
*-----------------------------------------------------------------------
FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 015 SY-VLINE
            , 060 SY-VLINE
            , 074 SY-VLINE.
ENDFORM.

*-------------------------  BDC_SCREEN  --------------------------------
* This routine adds an entry to the table BDCDATA with screen
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  PROGRAM - Program name of the screen
*      DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*-------------------------  BDC_FIELD  ---------------------------------
* This routine adds an entry to the table BDCDATA with field
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  fnam - name of the field on the screen
*      fval - value to be entered for that field on the screen.
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

*------------------------  OPEN_BATCH_SESSION --------------------------
*   - This opens up the batch session for input.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
  CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
          CLIENT              = SY-MANDT
          GROUP               = 'ZPS_BLANKETS'
          KEEP                = 'X'
          USER                = SY-UNAME
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
  IF SY-SUBRC NE 0.
    MESSAGE E001 WITH 'ZPS_BLANKETS'.
  ENDIF.
ENDFORM.

*-------------------------  INSERT_SESSION  ----------------------------
*-----------------------------------------------------------------------
FORM INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
      EXPORTING
          TCODE               = 'CJ01'
      TABLES
          DYNPROTAB           = BDCDATA
      EXCEPTIONS
          INTERNAL_ERROR      = 1
          NOT_OPEN            = 2
          QUEUE_ERROR         = 3
          TCODE_INVALID       = 4
          OTHERS              = 5.
  IF SY-SUBRC NE 0.
    MESSAGE I100 WITH 'Error inserting data to BDC for '
                       SOURCE_PR ' / ' TARGET_PR.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    CLOSE_BATCH_SESSION
*-----------------------------------------------------------------------
*  - This closes the batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_BATCH_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
      EXCEPTIONS
          NOT_OPEN            = 1
          QUEUE_ERROR         = 2
          OTHERS              = 3.
  IF SY-SUBRC NE 0.
    MESSAGE I003 WITH 'ZPS_BLANKETS'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       THIS IS THE END, MY FREIND                                    *
*---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ADD_GL_ACCOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_GL_ACCOUNT .
DATA: lt_prps TYPE TABLE OF ty_prps,
        lwa_prps LIKE LINE OF lt_prps,
        lv_ofset(2) TYPE n,
        lv_count TYPE i,
        lv_wbse_out TYPE char24,
        lr_item9 TYPE RANGE OF i,
        ls_item9 LIKE LINE OF lr_item9..
* Get the no.of wbses for the project.
  CLEAR gv_wbs_count.
  CALL FUNCTION 'CONVERSION_EXIT_KONPD_INPUT'
    EXPORTING
      input     = iproj-psphi_s
    IMPORTING
      output    = iproj-psphi_s
*     PROJWA    =
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.

  ENDIF.

  SELECT
    PSPNR	"WBS Element
    PSPHI	"Current number of the appropriate project
    BELKZ	"Indicator: Account assignment element
    FROM prps
    INTO TABLE lt_prps
    WHERE psphi = iproj-psphi_s.
  IF sy-subrc = 0.
       SORT lt_prps by pspnr.
       CLEAR lv_count.
*      For the WBSE, who's having Account asignmnet indicator
       LOOP AT lt_prps INTO lwa_prps WHERE belkz = 'X'.
*        Default table control size is 13, after 13 page down should happen
         IF sy-tabix gt 13.
           lv_ofset = sy-tabix - 13.
           perform bdc_field       using 'BDC_OKCODE'
                               '=P+'.
           ELSE.
             lv_ofset = sy-tabix.
         ENDIF.
*        Convert WBSE to output format
         CLEAR lv_wbse_out.
         CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
           EXPORTING
             input         = lwa_prps-pspnr
          IMPORTING
            OUTPUT        = lv_wbse_out
                   .
*        Increment the no.of WBSEs having Account assingment indicator
         lv_count = lv_count + 1.
         IF lv_wbse_out+10(1) =  '9'.
*          Collect all the places of WBSEs which is having '9'
           ls_item9-low = lv_count.
           ls_item9-sign = 'I'.
           ls_item9-option = 'EQ'.
           APPEND ls_item9 TO lr_item9.
           CLEAR ls_item9.
         ENDIF.
*        Settlment Rule button
         IF lv_count = 1.
            perform bdc_field       using 'BDC_OKCODE'
                              '=ABRV'.
         ENDIF.
         CLEAR field_name.
*        Select the rows having Account asignment indicator
         CONCATENATE 'PRPS-STUFE(' lv_ofset ')' INTO field_name.
         perform bdc_field       using 'BDC_CURSOR' field_name.
         CLEAR field_name.
         CONCATENATE 'RCJ_MARKL-MARK(' lv_ofset ')' INTO field_name.
         perform bdc_field       using field_name  'X'.
       ENDLOOP.
*      Repeat GL updation for all selected WBSE
       DO  lv_count TIMES.
*      Settlement screen to enter GL Account
          PERFORM bdc_screen USING 'SAPLKOBS'       '0130'.
          PERFORM bdc_field  USING 'BDC_CURSOR' 'DKOBR-EMPGE(01)'.
          PERFORM bdc_field  USING 'BDC_OKCODE'     '=NOBJ'.
          PERFORM bdc_field  USING 'COBRB-KONTY(01)' 'G/L'.
          IF lr_item9 IS NOT INITIAL AND sy-index in lr_item9.
            PERFORM bdc_field  USING 'DKOBR-EMPGE(01)' '115998'.
            ELSE.
              PERFORM bdc_field  USING 'DKOBR-EMPGE(01)' '115996'.
          ENDIF.
       PERFORM BDC_FIELD  USING 'BDC_SUBSCR' 'SAPLKOBS                                0205BLOCK1'.
    ENDDO.
    PERFORM bdc_screen      USING 'SAPLCJWB' '0901'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                           'PROJ-POST1'.


  ENDIF.
ENDFORM.                    " ADD_GL_ACCOUNT
