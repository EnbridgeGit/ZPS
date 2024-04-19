REPORT ZPPMR034 .
*
************************************************************************
*  Author:      Mohammad T. Khan
*  Date:        April, 2003
*  Description:
*     - The purpose of this program is to extract the
*       Plan data for a specified version. The extract
*       file will be used by program ZPPMR023 to create
*       a new plan version or overwrite an existing one.
************************************************************************
* 2012/08/07 M Khan   - TR995 Change C: drive to H: drive with         *
*                             directory, file selection using F4 and   *
*                             add variant field for file name.         *
************************************************************************
TABLES COSP.
TABLES PRPS.

DATA: BEGIN OF OUTREC OCCURS 0,
           WBS(11) TYPE C,
           ACCT(6) TYPE C,
           JAN(17) TYPE C,
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
*           KOSTL LIKE CSKS-KOSTL,
*           KSTAR LIKE CSKA-KSTAR,
     END OF OUTREC.

DATA: WFNAME LIKE RLGRAP-FILENAME VALUE SPACE.
.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS SPSPHI FOR  PRPS-PSPHI.
SELECT-OPTIONS SVERNR FOR  PRPS-VERNR.
*SELECT-OPTIONS SKSTAR FOR  COSP-KSTAR.
PARAMETERS     PVERSN LIKE COSP-VERSN.
PARAMETERS     PGJAHR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4).
PARAMETERS     P_FILE LIKE RLGRAP-FILENAME DEFAULT 'H:\'
                                             OBLIGATORY.     "TR995


SELECTION-SCREEN END OF BLOCK BOX.

*Start of TR995 changes
INITIALIZATION.
CONCATENATE 'H:\SAPTEMP\PLAN' PGJAHR SY-DATUM+4(4) '.prn' INTO P_FILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
*End of TR995 changes

START-OF-SELECTION.
SELECT PRPS~POSID  COSP~WKG001 COSP~WKG002 COSP~WKG003 COSP~WKG004
       COSP~WKG005 COSP~WKG006 COSP~WKG007 COSP~WKG008 COSP~WKG009
       COSP~WKG010 COSP~WKG011 COSP~WKG012 COSP~KSTAR
INTO (PRPS-POSID, COSP-WKG001, COSP-WKG002, COSP-WKG003,
     COSP-WKG004, COSP-WKG005, COSP-WKG006, COSP-WKG007,
     COSP-WKG008, COSP-WKG009, COSP-WKG010, COSP-WKG011,
     COSP-WKG012, COSP-KSTAR)
FROM ( PRPS
       INNER JOIN COSP
       ON COSP~OBJNR = PRPS~OBJNR
       AND COSP~PARGB = PRPS~PGSBR
       AND COSP~TWAER = PRPS~PWPOS )
       WHERE PRPS~PSPHI IN SPSPHI
         AND PRPS~VERNR IN SVERNR
         AND COSP~GJAHR =  PGJAHR
*         AND COSP~KSTAR IN SKSTAR
         AND COSP~VERSN =  PVERSN
         AND COSP~WRTTP =  '01'.


IF SY-SUBRC = 0.

IF COSP-WKG001 <> 0  OR  COSP-WKG002 <> 0  OR COSP-WKG003 <> 0  OR
   COSP-WKG004 <> 0  OR  COSP-WKG005 <> 0  OR COSP-WKG006 <> 0  OR
   COSP-WKG007 <> 0  OR  COSP-WKG008 <> 0  OR COSP-WKG009 <> 0  OR
   COSP-WKG010 <> 0  OR  COSP-WKG011 <> 0  OR COSP-WKG012 <> 0.

   MOVE: PRPS-POSID      TO  OUTREC-WBS,
         COSP-KSTAR+4(6) TO  OUTREC-ACCT.

   MOVE: COSP-WKG001   TO  OUTREC-JAN,
         COSP-WKG002   TO  OUTREC-FEB,
         COSP-WKG003   TO  OUTREC-MAR,
         COSP-WKG004   TO  OUTREC-APR,
         COSP-WKG005   TO  OUTREC-MAY,
         COSP-WKG006   TO  OUTREC-JUN,
         COSP-WKG007   TO  OUTREC-JUL,
         COSP-WKG008   TO  OUTREC-AUG,
         COSP-WKG009   TO  OUTREC-SEP,
         COSP-WKG010   TO  OUTREC-OCT,
         COSP-WKG011   TO  OUTREC-NOV,
         COSP-WKG012   TO  OUTREC-DEC.
         IF COSP-WKG001 < 0.
            MOVE OUTREC-JAN+2(15) TO OUTREC-JAN+1(16).
         ENDIF.
         IF COSP-WKG002 < 0.
            MOVE OUTREC-FEB+1(15) TO OUTREC-FEB+0(16).
         ENDIF.
         IF COSP-WKG003 < 0.
            MOVE OUTREC-MAR+1(15) TO OUTREC-MAR+0(16).
         ENDIF.
         IF COSP-WKG004 < 0.
            MOVE OUTREC-APR+1(15) TO OUTREC-APR+0(16).
         ENDIF.
         IF COSP-WKG005 < 0.
            MOVE OUTREC-MAY+1(15) TO OUTREC-MAY+0(16).
         ENDIF.
         IF COSP-WKG006 < 0.
            MOVE OUTREC-JUN+1(15) TO OUTREC-JUN+0(16).
         ENDIF.
         IF COSP-WKG007 < 0.
            MOVE OUTREC-JUL+1(15) TO OUTREC-JUL+0(16).
         ENDIF.
         IF COSP-WKG008 < 0.
            MOVE OUTREC-AUG+1(15) TO OUTREC-AUG+0(16).
         ENDIF.
         IF COSP-WKG009 < 0.
            MOVE OUTREC-SEP+1(15) TO OUTREC-SEP+0(16).
         ENDIF.
         IF COSP-WKG010 < 0.
            MOVE OUTREC-OCT+1(15) TO OUTREC-OCT+0(16).
         ENDIF.
         IF COSP-WKG011 < 0.
            MOVE OUTREC-NOV+1(15) TO OUTREC-NOV+0(16).
         ENDIF.
         IF COSP-WKG012 < 0.
            MOVE OUTREC-DEC+1(15) TO OUTREC-DEC+0(16).
         ENDIF.
   APPEND OUTREC.
   CLEAR OUTREC.
 ENDIF.
 ENDIF.
ENDSELECT.


* ---------------------------------------------------------------------*
* Downloading data in Microsoft Excel Format with automatic            *
* prompt popup dialog.                                                 *
* ---------------------------------------------------------------------*
*CONCATENATE 'C:\SAPTEMP\PLAN' PGJAHR SY-DATUM+4(4) '.prn' INTO WFNAME.
*                                                              "TR995
*  CALL FUNCTION 'DOWNLOAD'
  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
*      FILENAME     = 'C:\SAPTEMP\PLANEXL1.XLS'                "TR995
*      FILETYPE     = 'WK1'   "ASC, WK1, DBF, DAT, bin
      FILENAME     =  P_FILE                                   "TR995
      FILETYPE     = 'ASC'   "ASC, WK1, DBF, DAT, bin
      MODE         = ' '     "Mode ' ' = Rewrite Mode 'A' = Appending
    TABLES
      DATA_TAB     = OUTREC.
