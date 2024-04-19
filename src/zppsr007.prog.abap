REPORT ZPPSR007 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170.

************************************************************************
*  Author:      Cathy McCoy
*  Date:        July 2001
*  Description:
*      The purpose of this report is to learn about report writing
*      in Project Systems
*      This Program will not be transported and # can be reused.

************************************************************************

TABLES: PROJ,           "Project
        PRPS,           "WBS
        T001.           "Company Code

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX.
SELECT-OPTIONS: S_DIV FOR PRPS-POSID.
SELECTION-SCREEN END OF BLOCK BOX.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-001.
PARAMETER: P_CHKWBS AS CHECKBOX,
           P_CHKPRJ AS CHECKBOX,
           P_WBSPRJ as CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX2.


IF P_CHKWBS = 'X'.
    PERFORM PRPS_SELECT.
ENDIF.

IF P_CHKPRJ = 'X'.
    PERFORM PROJ_SELECT.
ENDIF.

IF P_WBSPRJ = 'X'.
    PERFORM: PRPS_SELECT,
             PROJ_SELECT.
ENDIF.

FORM PRPS_SELECT.
SELECT * FROM PRPS.
  IF PRPS-POSID(2) = S_DIV+3(2).
      IF PRPS-POSID(2) <> PRPS-VERNR.
      WRITE: /, PRPS-POSID, PRPS-VERNR.
      ENDIF.
  ENDIF.

ENDSELECT.
ENDFORM.

FORM PROJ_SELECT.
SELECT * FROM PROJ.
  IF PROJ-PSPID(2) = S_DIV+3(2).
      IF PROJ-PSPID(2) <> PROJ-VERNR.
      WRITE: /, PROJ-PSPID, PROJ-VERNR.
      ENDIF.
  ENDIF.

ENDSELECT.
ENDFORM.







