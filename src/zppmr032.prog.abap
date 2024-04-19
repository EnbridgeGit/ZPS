REPORT ZPPMR032 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 100.
*                MESSAGE-ID ZP.
************************************************************************
*  Author:      Mohammad T. Khan
*  Date:        January, 2003
*  Description:
*     - The purpose of this program is to produce PFA summary report
*       by Division Group, Project Type, Project Priority and Project.
*       Optionaly, it can print the detail lines by project.
************************************************************************
* 2010/07/07 btboundy - TR843 Include plan amount from COSS table.
************************************************************************

TABLES: COSP,              "CO Object: Cost Totals for External Postings
        COSS,              "CO Object: Cost Totals for Internal Postings
        PROJ,              "Project definition
        PRPS,              "WBS Element Master Data
        T001,              "Company Codes
        TCJ1T,             "Project Types
        TCN7T,             "Text table for priorities (TCN07)
        ZPSDIVGR.          "Division Groups for PS

DATA:
   BEGIN OF ITAB  OCCURS 0,
      VERNR       LIKE PRPS-VERNR,          "Division
      PRART       LIKE PRPS-PRART,          "Project Type
      PSPRI       LIKE PRPS-PSPRI,          "Priority Type
      POSID       LIKE PRPS-POSID,          "WBS
      VERSN       LIKE COSS-VERSN,          "Version
      WRTTP       LIKE COSS-WRTTP,          "Plan/Actual Indicator
      WKG001      LIKE COSS-WKG001,                         "amt per 01
      WKG002      LIKE COSS-WKG002,                         "amt per 02
      WKG003      LIKE COSS-WKG003,                         "amt per 03
      WKG004      LIKE COSS-WKG004,                         "amt per 04
      WKG005      LIKE COSS-WKG005,                         "amt per 05
      WKG006      LIKE COSS-WKG006,                         "amt per 06
      WKG007      LIKE COSS-WKG007,                         "amt per 07
      WKG008      LIKE COSS-WKG008,                         "amt per 08
      WKG009      LIKE COSS-WKG009,                         "amt per 09
      WKG010      LIKE COSS-WKG010,                         "amt per 10
      WKG011      LIKE COSS-WKG011,                         "amt per 11
      WKG012      LIKE COSS-WKG012,                         "amt per 12
   END OF ITAB.

DATA:
   BEGIN OF REPTAB  OCCURS 0,
      DGROUP        LIKE ZPSDIVGR,            "Division Group
      GRPDSCR       LIKE ZPSDIVGR-TXT30,      "Div group descripton
      PRART         LIKE PRPS-PRART,          "Project Type
      TYPDSCR       LIKE TCJ1T-PRATX,         "Proj Type Description
      PSPRI         LIKE PRPS-PSPRI,          "Proj. Priority
      PRIDSCR       LIKE TCN7T-KTEXT,         "Priority Description
      POSID         LIKE PRPS-POSID,          "Project Number
      PRJDSCR       LIKE PROJ-POST1,          "Project Description
      YTDPLAN(8)    TYPE P DECIMALS 2,        "YTD Plan Amount
      YTDACTUAL(8)  TYPE P DECIMALS 2,        "YTD Actual Amount
      ANUALPLAN(8)  TYPE P DECIMALS 2,        "Annual Plan Amount
   END OF REPTAB.

DATA:
   BEGIN OF EXCLTAB  OCCURS 0,
      GRPDSCR(40)   TYPE C,                   "Div group descripton
      TYPDSCR(50)   TYPE C,                   "Proj. Type descripton
      PRIDSCR(50)   TYPE C,                   "Proj. Priority descripton
      PRJDSCR(50)   TYPE C,                   "Proj. descripton
      YTDPLAN(8)    TYPE P DECIMALS 2,        "YTD Plan Amount
      YTDACTUAL(8)  TYPE P DECIMALS 2,        "YTD Actual Amount
      ANUALPLAN(8)  TYPE P DECIMALS 2,        "Annual Plan Amount
   END OF EXCLTAB.

DATA:
      W_GRP_DSCR    LIKE ZPSDIVGR-TXT30,
      W_TYP_DSCR    LIKE TCJ1T-PRATX,
      W_PRI_DSCR    LIKE TCN7T-KTEXT,
      W_PRJ_DSCR    LIKE PROJ-POST1,
      W_GRP_CODE    LIKE ZPSDIVGR-DGROUP,
      W_MMYYFROM(7) TYPE C,
      W_MMYYTO(7)   TYPE C,
      W_MONTHS(5)   TYPE C,
      W_TEXT(24)    TYPE C,
      W_TXT-CL5(14) TYPE C,
      W_TXT-CL6(14) TYPE C,
      W_TXT-CL7(14) TYPE C,
      W_OPTION(11)  TYPE C VALUE 'START_EXCEL'.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5) TYPE C,
        DDIC_FIELD(5) TYPE C,
        KEY TYPE C,
      END OF PROT_HEADER.

DATA  ERRORTAB    LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
DATA: RETCODE LIKE SY-SUBRC.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS: P_CCODE LIKE T001T-BUKRS  DEFAULT 'UGL',
            P_VERS  LIKE COSP-VERSN DEFAULT '0',  "Plan version
            P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
            P_TGROUP LIKE ZPSDIVGR-TGROUP DEFAULT 'D1',
            P_BEGMTH(2),
            P_ENDMTH(2).
SELECT-OPTIONS:  S_PSPHI FOR PRPS-PSPHI,                 "Project
*SELECT-OPTIONS:  S_POSKI FOR PRPS-POSKI default '01-03-000-0000' to
*                               '99-03-999-9999',
"Project
                 S_VERNR FOR PROJ-VERNR,                 "Division
                 SPRART FOR PRPS-PRART,                  "Project Type
                 SPSPRI FOR PRPS-PSPRI.                  "Priority Type
SELECTION-SCREEN SKIP 1.
PARAMETER: P_CHECK AS CHECKBOX.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-067.
PARAMETERS:     P_EXCL RADIOBUTTON GROUP RBCR,    "EXCEL SHEET
                P_RPRT RADIOBUTTON GROUP RBCR.    "PRINT REPORT
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.
*-----------------------------------------------------------------------

START-OF-SELECTION.

  IF P_ENDMTH = SPACE.
    P_ENDMTH = P_BEGMTH.
  ENDIF.
  CONCATENATE P_BEGMTH '-' P_ENDMTH INTO W_MONTHS.
  CONCATENATE TEXT-CL5 W_MONTHS INTO W_TXT-CL5 SEPARATED BY SPACE.
  CONCATENATE TEXT-CL6 W_MONTHS INTO W_TXT-CL6 SEPARATED BY SPACE.
  CONCATENATE TEXT-CL7 P_FYEAR  INTO W_TXT-CL7 SEPARATED BY SPACE.

  SELECT SINGLE * FROM T001                    "Get Company Name
    WHERE BUKRS = P_CCODE.

  PERFORM GET_COSP_DATA.
  PERFORM BUILD_REPORT_TABLE.
  PERFORM GET_COSS_DATA.
  PERFORM BUILD_REPORT_TABLE.
  SORT REPTAB BY DGROUP PRART PSPRI POSID.
  IF NOT REPTAB[] IS INITIAL.
    IF P_EXCL <> 'X'.
      CLEAR W_OPTION.
      CONCATENATE P_BEGMTH '/' P_FYEAR INTO W_MMYYFROM.
      CONCATENATE P_ENDMTH '/' P_FYEAR INTO W_MMYYTO.
      CONCATENATE 'FROM' W_MMYYFROM 'TO' W_MMYYTO INTO W_TEXT
                   SEPARATED BY SPACE.
    ENDIF.
    PERFORM CREATE_EXCEL_SHEET.
  ENDIF.

************************************************************************
*                                                                      *
*                    GET_COSP_DATA.                                    *
*                                                                      *
************************************************************************
FORM GET_COSP_DATA.

  SELECT PRPS~VERNR PRPS~PRART PRPS~PSPRI PRPS~POSID COSP~VERSN COSP~WRTTP
   COSP~WKG001 COSP~WKG002 COSP~WKG003 COSP~WKG004 COSP~WKG005 COSP~WKG006
   COSP~WKG007 COSP~WKG008 COSP~WKG009 COSP~WKG010 COSP~WKG011 COSP~WKG012
   INTO TABLE ITAB
   FROM ( PRPS INNER JOIN COSP
          ON PRPS~OBJNR = COSP~OBJNR )

   WHERE   PRPS~PBUKR   =  P_CCODE             "Company Code
       AND PRPS~PKOKR   =  '10'                "Controlling area
*     AND PRPS~POSKI  IN  S_POSKI
       AND PRPS~PSPHI  IN  S_PSPHI
       AND PRPS~LOEVM  <>  'X'
       AND PRPS~PRART  IN  SPRART
       AND PRPS~PSPRI  IN  SPSPRI
       AND COSP~GJAHR   =  P_FYEAR
       AND ( COSP~VERSN =  P_VERS            "Version
        OR COSP~VERSN   = '000' )
       AND COSP~WRTTP  IN ('01','04')        "Record with actuals & plans
       AND COSP~BEKNZ  IN ('S','H','L').     "Debit/Credit Indicator
ENDFORM.                    "GET_COSP_DATA

************************************************************************
*                                                                      *
*                    GET_COSS_DATA.                                    *
*                                                                      *
************************************************************************
FORM GET_COSS_DATA.
  CLEAR   ITAB.
  REFRESH ITAB.
  SELECT PRPS~VERNR PRPS~PRART PRPS~PSPRI PRPS~POSID COSS~VERSN COSS~WRTTP
   COSS~WKG001 COSS~WKG002 COSS~WKG003 COSS~WKG004 COSS~WKG005 COSS~WKG006
   COSS~WKG007 COSS~WKG008 COSS~WKG009 COSS~WKG010 COSS~WKG011 COSS~WKG012
   INTO TABLE ITAB
   FROM ( PRPS INNER JOIN COSS
          ON PRPS~OBJNR = COSS~OBJNR )
   WHERE   PRPS~PBUKR  =  P_CCODE             "Company Code
       AND PRPS~PKOKR  = '10'                "Controlling area
*     AND PRPS~POSKI  IN  S_POSKI
       AND PRPS~PSPHI IN  S_PSPHI
       AND PRPS~LOEVM <> 'X'
       AND PRPS~PRART IN  SPRART
       AND PRPS~PSPRI IN  SPSPRI
       AND COSS~GJAHR  =  P_FYEAR
*       AND COSS~VERSN  = '000'              "                                    TR843
       AND ( COSS~VERSN =  P_VERS            "Version
        OR COSS~VERSN   = '000' )            "                                    TR843
*     AND COSS~WRTTP  = '04'.                "Record with actuals
       AND COSS~WRTTP  IN ('01','04').       "Record with actuals and plan        TR843
ENDFORM.                    "GET_COSS_DATA

************************************************************************
*                                                                      *
*            BUILD_REPORT_TABLE                                        *
*                                                                      *
************************************************************************
FORM BUILD_REPORT_TABLE.
  LOOP AT ITAB.
    AT NEW VERNR.
      PERFORM GET_GROUP_DESCRIPTION.
    ENDAT.
    AT NEW PRART.
      PERFORM GET_TYPE_DESCRIPTION.
    ENDAT.
    AT NEW PSPRI.
      PERFORM GET_PRIORITY_DESCRIPTION.
    ENDAT.
    AT NEW POSID.
      PERFORM GET_PROJECT_DESCRIPTION.
    ENDAT.
    IF ITAB-POSID+5(2)  CO '1234567890'.    "Eliminates templates
      MOVE: ITAB-PRART TO REPTAB-PRART,
            ITAB-PSPRI TO REPTAB-PSPRI,
            W_GRP_CODE TO REPTAB-DGROUP,
            W_GRP_DSCR TO REPTAB-GRPDSCR,
            W_TYP_DSCR TO REPTAB-TYPDSCR,
            W_PRI_DSCR TO REPTAB-PRIDSCR,
            W_PRJ_DSCR TO REPTAB-PRJDSCR.
      CONCATENATE ITAB-POSID+0(2) ITAB-POSID+2(2) ITAB-POSID+4(3)
                  INTO REPTAB-POSID SEPARATED BY '-'.

      CASE ITAB-WRTTP.
        WHEN '01'.
          IF ITAB-VERSN = P_VERS.
            ADD ITAB-WKG001 FROM P_BEGMTH TO P_ENDMTH GIVING REPTAB-YTDPLAN.
            ADD ITAB-WKG001 FROM 1 TO 12 GIVING REPTAB-ANUALPLAN.
          ENDIF.
        WHEN '04'.
          IF ITAB-VERSN = '000'.
            ADD ITAB-WKG001 FROM P_BEGMTH TO P_ENDMTH GIVING REPTAB-YTDACTUAL.
          ENDIF.
      ENDCASE.
      COLLECT REPTAB.
      CLEAR   REPTAB.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "BUILD_REPORT_TABLE

************************************************************************
*                                                                      *
*            GET_GROUP_DESCRIPTION                                     *
*                                                                      *
************************************************************************
FORM GET_GROUP_DESCRIPTION.

  SELECT SINGLE * FROM ZPSDIVGR
   WHERE TGROUP = P_TGROUP
     AND VERNR  = ITAB-VERNR.
  IF SY-SUBRC = 0.
    MOVE ZPSDIVGR-DGROUP  TO W_GRP_CODE.
    MOVE ZPSDIVGR-TXT30   TO W_GRP_DSCR.
  ELSE.
    MOVE '***'            TO W_GRP_CODE.
    MOVE '**NOT FOUND***' TO W_GRP_DSCR.
  ENDIF.

ENDFORM.                    "GET_GROUP_DESCRIPTION
************************************************************************
*                                                                      *
*            GET_TYPE_DESCRIPTION                                      *
*                                                                      *
************************************************************************
FORM GET_TYPE_DESCRIPTION.
  SELECT SINGLE * FROM TCJ1T
   WHERE LANGU = 'EN'
     AND PRART = ITAB-PRART.
  IF SY-SUBRC = 0.
    MOVE TCJ1T-PRATX      TO W_TYP_DSCR.
  ELSE.
    MOVE '**NOT FOUND***' TO W_TYP_DSCR.
  ENDIF.

ENDFORM.                    "GET_TYPE_DESCRIPTION
************************************************************************
*                                                                      *
*            GET_PRIORITY_DESCRIPTION                                  *
*                                                                      *
************************************************************************
FORM GET_PRIORITY_DESCRIPTION.
  SELECT SINGLE * FROM TCN7T
   WHERE LANGU = 'EN'
     AND NPRIO = ITAB-PSPRI.
  IF SY-SUBRC = 0.
    MOVE TCN7T-KTEXT      TO W_PRI_DSCR.
  ELSE.
    MOVE '**NOT FOUND***' TO W_PRI_DSCR.
  ENDIF.

ENDFORM.                    "GET_PRIORITY_DESCRIPTION
************************************************************************
*                                                                      *
*            GET_PROJECT_DESCRIPTION                                   *
*                                                                      *
************************************************************************
FORM GET_PROJECT_DESCRIPTION.
  DATA: WRK_PSPID LIKE PROJ-PSPID.
  CONCATENATE ITAB-POSID+0(7) '0000' INTO WRK_PSPID.
  SELECT SINGLE * FROM PROJ
   WHERE PSPID = WRK_PSPID.
  IF SY-SUBRC = 0.
    MOVE PROJ-POST1       TO W_PRJ_DSCR.
  ELSE.
    MOVE '**NOT FOUND***' TO W_PRJ_DSCR.
  ENDIF.

ENDFORM.                    "GET_PROJECT_DESCRIPTION
************************************************************************
*                                                                      *
*            CREATE_EXCEL_SHEET.                                       *
*                                                                      *
************************************************************************
FORM CREATE_EXCEL_SHEET.

  LOOP AT REPTAB.
    CLEAR EXCLTAB.
    MOVE REPTAB-GRPDSCR TO W_GRP_DSCR.
    MOVE REPTAB-TYPDSCR TO W_TYP_DSCR.
    MOVE REPTAB-PRIDSCR TO W_PRI_DSCR.
    MOVE REPTAB-GRPDSCR TO EXCLTAB-GRPDSCR.
    MOVE REPTAB-TYPDSCR TO EXCLTAB-TYPDSCR.
    MOVE REPTAB-PRIDSCR TO EXCLTAB-PRIDSCR.
    IF P_CHECK = 'X'.
      CONCATENATE REPTAB-POSID+0(9) REPTAB-PRJDSCR INTO
                  EXCLTAB-PRJDSCR SEPARATED BY SPACE.
      PERFORM MOVE_DOLLARS.
      APPEND EXCLTAB.
    ENDIF.

    AT END OF PSPRI.
      SUM.
      CLEAR: EXCLTAB-PRJDSCR.
      CONCATENATE '***TOTAL :' W_PRI_DSCR INTO EXCLTAB-PRIDSCR.
      PERFORM MOVE_DOLLARS.
      APPEND EXCLTAB.
    ENDAT.

    AT END OF PRART.
      SUM.
      CLEAR: EXCLTAB-PRJDSCR, EXCLTAB-PRIDSCR.
      CONCATENATE '***TOTAL :'  W_TYP_DSCR INTO EXCLTAB-TYPDSCR.
      PERFORM MOVE_DOLLARS.
      APPEND EXCLTAB.
    ENDAT.

    AT END OF DGROUP.
      SUM.
      CLEAR: EXCLTAB-PRJDSCR, EXCLTAB-PRIDSCR, EXCLTAB-TYPDSCR.
      CONCATENATE '***TOTAL :' W_GRP_DSCR INTO EXCLTAB-GRPDSCR.
      PERFORM MOVE_DOLLARS.
      APPEND EXCLTAB.
    ENDAT.

    AT LAST.
      SUM.
      CLEAR: EXCLTAB-PRJDSCR, EXCLTAB-PRIDSCR,
             EXCLTAB-TYPDSCR, EXCLTAB-GRPDSCR.
      MOVE '***GRAND TOTAL :' TO EXCLTAB-GRPDSCR.
      PERFORM MOVE_DOLLARS.
      APPEND EXCLTAB.
    ENDAT.
  ENDLOOP.

  PERFORM PROT_HEADER.

  CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
    EXPORTING
      BASIC_LIST_TITLE    = 'PFS Summary Report For Union Gas'
      HEAD_LINE1          = W_TEXT
      FILE_NAME           = SY-CPROG
      ADDITIONAL_OPTIONS  = W_OPTION
    IMPORTING
      RETURN_CODE         = RETCODE
    TABLES
      DATA_TAB            = EXCLTAB
      FIELDNAME_TAB       = PROT_HEADER
      ERROR_TAB           = ERRORTAB
    EXCEPTIONS
      DOWNLOAD_PROBLEM    = 1
      NO_DATA_TAB_ENTRIES = 2
      TABLE_MISMATCH      = 3
      PRINT_PROBLEMS      = 4
      OTHERS              = 5.
  IF SY-SUBRC NE 0.
    WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFULL - REASON = ', SY-SUBRC.
  ENDIF.
ENDFORM.                    "CREATE_EXCEL_SHEET
*---------------------------------------------------------------------*
*       FORM PROT_HEADER                                              *
*---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-CL1 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL2 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL3 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL4 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE W_TXT-CL5 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE W_TXT-CL6 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE W_TXT-CL7 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER

************************************************************************
*                 MOVE_DOLLARS.                                        *
************************************************************************
FORM MOVE_DOLLARS.
  MOVE:   REPTAB-YTDPLAN    TO EXCLTAB-YTDPLAN,
          REPTAB-YTDACTUAL  TO EXCLTAB-YTDACTUAL,
          REPTAB-ANUALPLAN  TO EXCLTAB-ANUALPLAN.
ENDFORM.                    "MOVE_DOLLARS
*************************END OF PROGRAM ********************************
