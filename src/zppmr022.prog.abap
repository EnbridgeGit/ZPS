*-----------------------------------------------------------------------
*  Copied from SAP rkplub01 --- standard template for customizing
*  by customers for their own plan file layouts.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*  This is used to load plan data for cost centres by g/l account
*  in an on-line mode by reading a flat file from your local PC.
*  Code to load from unix file exists, but is commented out.
*
*-----------------------------------------------------------------------
REPORT ZPPMR022.

*-----------------------------------------------------------------------
*  Local tables for the program
*-----------------------------------------------------------------------
TABLES:
  CSKS, CSKA.


PARAMETERS:
*    P_BUKRS LIKE CSKS-BUKRS DEFAULT 'MNPP',
     KOKRS LIKE CSKS-KOKRS,
     VERSN LIKE COSP-VERSN,
*    RVERS LIKE COSP-VERSN,
     GJAHR LIKE COSP-GJAHR,
*    RJAHR LIKE COSP-GJAHR,
     PERAB LIKE RKU01JA-PERAB,
     PERBI LIKE RKU01JA-PERBI,
     FILE_IN LIKE RLGRAP-FILENAME
             DEFAULT 'c:\saptemp\ccplanload.prn' ,
     P_DELTA(1) TYPE C,
*    p_chart(1) type c obligatory,
     P_VRGNG LIKE COSP-VRGNG,
     P_COMMIT(1) TYPE C,
     P_UPDATE(1) TYPE C.

* Internal table for plan records
DATA: BEGIN OF IRKU01JA OCCURS 20.
        INCLUDE STRUCTURE RKU01JA.
DATA: END OF IRKU01JA.

DATA: BEGIN OF ZRKU01_CUR.
        INCLUDE STRUCTURE RKU01_CUR.
DATA: END OF ZRKU01_CUR.

DATA: TYPE LIKE RLGRAP-FILETYPE VALUE 'ASC',
      MSG(100) TYPE C.

DATA:
  CNTR_OUT LIKE RLGRAP-FILENAME VALUE 'c:\windows\temp\ctrerr.txt',
  ACCT_OUT LIKE RLGRAP-FILENAME VALUE 'c:\windows\temp\acterr.txt'.

*-----------------------------------------------------------------------
*  Here is the layout and fields for the INREC file
*-----------------------------------------------------------------------
DATA: BEGIN OF INREC OCCURS 100,
           CNTR(7) TYPE C,
           ACCT(6) TYPE C,
           JAN(16) TYPE C,
           FEB(16) TYPE C,
           MAR(16) TYPE C,
           APR(16) TYPE C,
           MAY(16) TYPE C,
           JUN(16) TYPE C,
           JUL(16) TYPE C,
           AUG(16) TYPE C,
           SEP(16) TYPE C,
           OCT(16) TYPE C,
           NOV(16) TYPE C,
           DEC(16) TYPE C,
           KOSTL LIKE CSKS-KOSTL,
           KSTAR LIKE CSKA-KSTAR,
*          trans like cska-kstar,
     END OF INREC.

*-----------------------------------------------------------------------
*  Used to capture cost centre master errors
*-----------------------------------------------------------------------
DATA: BEGIN OF CTRERR OCCURS 100,
           CNTR(7) TYPE C,
           ACCT(6) TYPE C,
           JAN(16)  TYPE C,
           FEB(16)  TYPE C,
           MAR(16)  TYPE C,
           APR(16)  TYPE C,
           MAY(16)  TYPE C,
           JUN(16)  TYPE C,
           JUL(16)  TYPE C,
           AUG(16)  TYPE C,
           SEP(16)  TYPE C,
           OCT(16)  TYPE C,
           NOV(16)  TYPE C,
           DEC(16)  TYPE C,
           KOSTL    LIKE CSKS-KOSTL,
           KSTAR    LIKE CSKA-KSTAR,
 END OF CTRERR.

*-----------------------------------------------------------------------
*  Used to capture errors for g/l accounts
*-----------------------------------------------------------------------
DATA: BEGIN OF ACTERR OCCURS 100,
           CNTR(7) TYPE C,
           ACCT(6) TYPE C,
           JAN(16) TYPE C,
           FEB(16)  TYPE C,
           MAR(16)  TYPE C,
           APR(16)  TYPE C,
           MAY(16)  TYPE C,
           JUN(16)  TYPE C,
           JUL(16)  TYPE C,
           AUG(16)  TYPE C,
           SEP(16)  TYPE C,
           OCT(16)  TYPE C,
           NOV(16)  TYPE C,
           DEC(16)  TYPE C,
           KOSTL    LIKE CSKS-KOSTL,
           KSTAR    LIKE CSKA-KSTAR,
 END OF ACTERR.

*-----------------------------------------------------------------------
*  default values used for every record defined below
*-----------------------------------------------------------------------
DATA:  PYEAR LIKE IRKU01JA-GJAHR VALUE '2001',
       CURR  LIKE IRKU01JA-TWAER VALUE 'CAD',
       CAREA LIKE CSKS-KOKRS VALUE '10',
       IDATE LIKE CSKS-DATBI VALUE '99991231',
       RECCNT TYPE P VALUE 0,
       COMCNT TYPE P VALUE 0,
       ERRCTR TYPE P VALUE 0,
       ERRACT TYPE P VALUE 0.

*-----------------------------------------------------------------------
*  Open the file on the server to get the data
*-----------------------------------------------------------------------

*PEN DATASET FILE_IN FOR INPUT IN TEXT MODE MESSAGE MSG.
*F SY-SUBRC NE 0.
* WRITE: 'File cannot be opened. Reason:', MSG.
* EXIT.
*NDIF.

*-----------------------------------------------------------------------
*  Read the unix file into the internal table
*-----------------------------------------------------------------------
*O.
* READ DATASET FILE_IN INTO INREC.
*IF SY-SUBRC NE 0.
*  EXIT.
*ENDIF.
*APPEND INREC.
*ADD 1 TO RECCNT.
*ADD 1 TO COMCNT.
*DDO.

*-----------------------------------------------------------------------
*  Read a file from the presentation server (i.e. local PC File)
*-----------------------------------------------------------------------
CALL FUNCTION 'WS_UPLOAD'
     EXPORTING
          FILENAME        = FILE_IN
     TABLES
          DATA_TAB        = INREC
     EXCEPTIONS
          FILE_OPEN_ERROR = 1
          OTHERS          = 2.

CASE SY-SUBRC.
  WHEN 1.
    WRITE 'Error opening file'.
    EXIT.
  WHEN 2.
    WRITE 'Error during data transfer'.
    EXIT.
ENDCASE.

LOOP AT INREC.
  ADD 1 TO RECCNT.
  ADD 1 TO COMCNT.
ENDLOOP.


*-----------------------------------------------------------------------
*  Move the 7 digit cost centre to 10 digit cost centre and
*  move the 6 digit g/l account to 10 digit g/l account
*-----------------------------------------------------------------------

LOOP AT INREC.
  MOVE '000' TO INREC-KOSTL+0(4).
  MODIFY INREC.
  MOVE INREC-CNTR TO INREC-KOSTL+3(7).
  MODIFY INREC.
  MOVE '0000' TO INREC-KSTAR+0(4).
  MODIFY INREC.
  MOVE INREC-ACCT TO INREC-KSTAR+4(6).
  MODIFY INREC.
ENDLOOP.

*-----------------------------------------------------------------------
*  The data just read into INREC should be checked for master data
*  errors in both cost centres and accounts.  Will be done with 2 forms
*  so error types can be distinguished. Two further files for
*  processing will be created.
*-----------------------------------------------------------------------
LOOP AT INREC.
  PERFORM CHECK_CENTRE.
ENDLOOP.

LOOP AT INREC.
* if p_chart = 'Y'.
    PERFORM CHECK_SAP_COST_ELEMENT.
* else.
*   perform convert_psoft_acct_to_sap_acct.
* endif.
ENDLOOP.

*LOSE DATASET FILE_IN.

*-----------------------------------------------------------------------
*  Move the valid records to the plan record structure IRKU01JA which
*  will be passed to the function K_COSTS_PLAN_INTERFACE_PERIOD
*-----------------------------------------------------------------------

LOOP AT INREC.
  IRKU01JA-GJAHR  = PYEAR.
  IRKU01JA-TWAER  = CURR.
  IRKU01JA-KOKRS  = CAREA.
  IRKU01JA-KOSTL  = INREC-KOSTL.
  IRKU01JA-KSTAR  = INREC-KSTAR.
  IRKU01JA-WTG001 = INREC-JAN.
  IRKU01JA-WTG002 = INREC-FEB.
  IRKU01JA-WTG003 = INREC-MAR.
  IRKU01JA-WTG004 = INREC-APR.
  IRKU01JA-WTG005 = INREC-MAY.
  IRKU01JA-WTG006 = INREC-JUN.
  IRKU01JA-WTG007 = INREC-JUL.
  IRKU01JA-WTG008 = INREC-AUG.
  IRKU01JA-WTG009 = INREC-SEP.
  IRKU01JA-WTG010 = INREC-OCT.
  IRKU01JA-WTG011 = INREC-NOV.
  IRKU01JA-WTG012 = INREC-DEC.
  APPEND IRKU01JA.
ENDLOOP.

ZRKU01_CUR-WTG_MAN = 'X'.

*3. call plan interface for primary costs
CALL FUNCTION 'K_COSTS_PLAN_INTERFACE_PERIOD'
     EXPORTING
          BLTXT            = 'Plan Data Load'
          DELTA            = P_DELTA
          GJAHR            = GJAHR
          KOKRS            = KOKRS
          MESSAGES_SHOW    = 'X'
          PERAB            = PERAB
          PERBI            = PERBI
*          RPLAN            = 'CO-PLAN1'
          VERSN            = VERSN
          VRGNG            = P_VRGNG
          IRKU01_CUR       = ZRKU01_CUR   "LS0004A
          COMMIT           = P_COMMIT
          UPDATE_VALUES    = P_UPDATE
     TABLES
          IRKU01JA         = IRKU01JA
     EXCEPTIONS
          MESSAGES_OCCURED = 01.

*-----------------------------------------------------------------------
*  Write statistics to the screen separated by error type
*-----------------------------------------------------------------------
IF P_COMMIT = ' '.
WRITE:   / '*** Test run of plan file load, no records loaded ***'(007).
ENDIF.
SKIP 2.
WRITE:    / 'Records read from file:'(001), FILE_IN , RECCNT.
SKIP 2.

WRITE:    / 'Records processed completely from source file:'(002)
                                                             , COMCNT.
WRITE:    / 'Centre'(003),
            12 'Account'(004).
*           23 'Conv. From P/Soft Acct' .
LOOP AT INREC.
* write:    / inrec-kostl, inrec-kstar, inrec-trans.
  WRITE:    / INREC-KOSTL, INREC-KSTAR.
ENDLOOP.
SKIP 2.

WRITE:    / 'Cost Centre Records which are invalid:'(005) , ERRCTR.
WRITE:    / 'Centre'(003),
            12 'Account'(004).
LOOP AT CTRERR.
  WRITE:    / CTRERR-KOSTL, CTRERR-KSTAR.
ENDLOOP.

*-----------------------------------------------------------------------
*  Set the screen to have download buttons if there are records with
*  errors.
*-----------------------------------------------------------------------
IF SY-SUBRC = 0.                                      "970428
  SET PF-STATUS 'ZDLD'.                               "970428
ENDIF.                                                "970428

SKIP 2.

WRITE:    / 'G/L Accounts which are invalid:'(006) , ERRACT.
WRITE:    / 'Account'(004),
            12 'Centre'(003).
LOOP AT ACTERR.
  WRITE:    / ACTERR-KSTAR, ACTERR-KOSTL.
ENDLOOP.

*-----------------------------------------------------------------------
*  Set the screen to have download buttons if there are records with
*  errors.
*-----------------------------------------------------------------------
IF SY-SUBRC = 0.
  SET PF-STATUS 'ZDLD'.
ENDIF.
*-----------------------------------------------------------------------
*  Trap user-command so we can download cost centre and account error
*  files to local pc file for editing purposes
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'CLOD'.
      PERFORM DOWNLOAD_COST_CENTRE_ERRORS.
    WHEN 'ALOD'.
      PERFORM DOWNLOAD_ACCOUNT_ERRORS.
  ENDCASE.

*-----------------------------------------------------------------------
*  Check to see if centre on record in INREC is valid
*-----------------------------------------------------------------------
FORM CHECK_CENTRE.
  SELECT SINGLE * FROM CSKS
    WHERE KOKRS = CAREA
      AND KOSTL = INREC-KOSTL
*     AND BUKRS = P_BUKRS
      AND DATBI = IDATE.
  IF SY-SUBRC NE 0.
    ADD 1 TO ERRCTR.
    MOVE-CORRESPONDING INREC TO CTRERR.
    APPEND CTRERR.
    DELETE INREC.
    ADD -1 TO COMCNT.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*  It was indicated on the parameters for running the program that the
*  plan file was created using the SAP Chart of Accounts.  Are all of
*  the accounts used valid SAP accounts?
*-----------------------------------------------------------------------
FORM CHECK_SAP_COST_ELEMENT.
  SELECT SINGLE * FROM CSKA
    WHERE KTOPL = 'COAT'
      AND KSTAR = INREC-KSTAR.
  IF SY-SUBRC = 0.
    EXIT.
  ELSE.
    ADD 1 TO ERRACT.
    MOVE-CORRESPONDING INREC TO ACTERR.
    APPEND ACTERR.
    DELETE INREC.
    ADD -1 TO COMCNT.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*  It was indicated on the parameters for running the program that the
*  plan file was created using the P/Soft Chart of Accounts.  Now need
*  to convert the P/Soft Accounts to SAP Accounts.
*-----------------------------------------------------------------------
*orm convert_psoft_acct_to_sap_acct.
* clear find_sacct.
* move inrec-kstar+4(6) to find_sacct.
* read table i_acct with key find_sacct binary search.
* if sy-subrc = 0.
*   move inrec-kstar to inrec-trans.
*   modify inrec.
*   move i_acct-saknr to inrec-kstar.
*   modify inrec.
* else.
*   add 1 to erract.
*   move-corresponding inrec to acterr.
*   append acterr.
*   delete inrec.
*   add -1 to comcnt.
* endif.
*ndform.

*-----------------------------------------------------------------------
*  Form download_cost_centre_errors - used to create cost centre edit
*  cost centre errors.
*-----------------------------------------------------------------------
FORM DOWNLOAD_COST_CENTRE_ERRORS.
  CALL FUNCTION 'DOWNLOAD'                                  "
      EXPORTING                                             "
       FILENAME            = CNTR_OUT                       "
       FILETYPE            = TYPE                           "
       FILETYPE_NO_CHANGE  = 'X'                            "
       MODE                = 'S'                            "
      TABLES                                                "
       DATA_TAB            =  CTRERR                        "
      EXCEPTIONS                                            "
       INVALID_FILESIZE    = 01                             "
       INVALID_TABLE_WIDTH = 02                             "
       INVALID_TYPE        = 03                             "
       NO_BATCH            = 04                             "
       UNKNOWN_ERROR       = 05.                            "
ENDFORM.

*-----------------------------------------------------------------------
*  Form download_account_errors - used to create account error format
*-----------------------------------------------------------------------
FORM DOWNLOAD_ACCOUNT_ERRORS.
  CALL FUNCTION 'DOWNLOAD'                                  "
      EXPORTING                                             "
       FILENAME            = ACCT_OUT                       "
       FILETYPE            = TYPE                           "
       FILETYPE_NO_CHANGE  = 'X'                            "
       MODE                = 'S'                            "
      TABLES                                                "
       DATA_TAB            =  ACTERR                        "
      EXCEPTIONS                                            "
       INVALID_FILESIZE    = 01                             "
       INVALID_TABLE_WIDTH = 02                             "
       INVALID_TYPE        = 03                             "
       NO_BATCH            = 04                             "
       UNKNOWN_ERROR       = 05.                            "
ENDFORM.
