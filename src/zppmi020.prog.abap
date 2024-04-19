REPORT zppmi020 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170
                MESSAGE-ID zp.
************************************************************************
*  Author:      Cathy McCoy
*  Date:        July 2001
*  Description:
*      The purpose of this report is to produce exceptions
*      where Person Resonsible does not match Division.
*

************************************************************************

TABLES: proj,                                               "Project
        prps,           "WBS
        t001,           "Company Code
        tj02t,                                              "Status
        jest.            "object Status


DATA:
   BEGIN OF int_wa  OCCURS 0,

      vernr            LIKE prps-vernr,                     "Divison #
      posid            LIKE prps-posid,        "Project #(WBS)
      pspid            LIKE proj-pspid,                     "Project #
      vbukr            LIKE proj-vbukr,        "Company Code
      pbukr            LIKE prps-pbukr,
      erdat            LIKE proj-erdat,                     "created on
      objnr            LIKE proj-objnr,
      rtxt04           LIKE tj02t-txt04,

   END OF int_wa.

DATA:  status(40)  TYPE c.     "statuses from the function call


DATA:
  BEGIN OF int_jest OCCURS 0,

  objnr LIKE jest-objnr,
  stat LIKE jest-stat,
  txt04  LIKE tj02t-txt04,

  END OF int_jest.


*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-000.

PARAMETERS: s_buk LIKE t001-bukrs OBLIGATORY          "Company Code
            DEFAULT 'UGL'.
SELECT-OPTIONS:  s_div FOR prps-vernr.                      "Division
SELECT-OPTIONS: s_txt04  FOR tj02t-txt04                    "status
                NO INTERVALS  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK box.


*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.

  PERFORM: prps_select,
           print_wbs,
           proj_select,
           print_proj.

  SKIP 3.
  WRITE: /60 text-002.



*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
  FORMAT INTENSIFIED.
  WRITE: /1 text-rpt, sy-repid,                             "Report Id
         76 t001-butxt,                                  "Company Name
        140 text-dte, sy-datum, text-amp, sy-uzeit.         "Date/Time
  WRITE: / text-clt UNDER text-rpt, sy-mandt, sy-sysid,     "Client
        63 text-003,                                     "Report Title
           text-pge UNDER text-dte, sy-pagno.            "Page Number
  WRITE: /45 'PS: EXCEPTIONS - PERSON RESPONSIBLE DOES NOT MATCH',
              'DIVISION'.
  WRITE: /63 'FOR COMPANY CODE '.
  FORMAT COLOR 2 ON.
  WRITE: s_buk.
  FORMAT COLOR OFF.
  SKIP.
  WRITE: /1(170) sy-uline.
  SKIP.

  PERFORM print_headings_all.

  FORMAT INTENSIFIED OFF.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_STATUSES
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

FORM get_statuses.

 CALL FUNCTION 'AIP9_STATUS_READ'                  "Retrieve Status Info
          EXPORTING
               i_objnr = prps-objnr
               i_spras = sy-langu
          IMPORTING
               e_sysst = status
          EXCEPTIONS
               OTHERS = 1.

ENDFORM.                    " GET STATUSES


******************************* PRPS_SELECT ****************************
*select data where Division # does not match WBS Element #
************************************************************************
* get the projects/wbs elements with a status defined in the table above



FORM prps_select.
  SELECT * FROM prps
  WHERE vernr IN s_div
  AND pbukr EQ s_buk
  AND loevm <> 'X'.
    IF prps-posid(2) CO '0123456789'.
      PERFORM get_statuses.
      IF status CS 'DLIN'.
      ELSE.
        LOOP AT s_txt04.
          IF status CS s_txt04+3(4).
            PERFORM get_wbs.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDSELECT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_WBS
*&---------------------------------------------------------------------*
FORM get_wbs.

  IF prps-posid(2) <> prps-vernr.
    CLEAR int_wa.
    MOVE: prps-posid TO int_wa-posid,
          prps-vernr TO int_wa-vernr,
          prps-erdat TO int_wa-erdat.
    IF     status CS 'CLSD'.
      MOVE 'CLSD' TO int_wa-rtxt04.
    ELSEIF status CS 'TECO'.
      MOVE 'TECO' TO int_wa-rtxt04.
    ELSEIF status CS 'REL'.
      MOVE 'REL'  TO int_wa-rtxt04.
    ELSEIF status CS 'CRTD'.
      MOVE 'CRTD' TO int_wa-rtxt04.
    ENDIF.
    APPEND int_wa.
  ENDIF.

ENDFORM.                                                    " GET_WBS


***************************** PROJ_SELECT *****************************
*select data where Division # does not match Project #
************************************************************************
FORM proj_select.
  SELECT * FROM proj
    WHERE vernr IN s_div
    AND vbukr EQ s_buk
      AND loevm <> 'X'.
    SORT int_wa BY pspid.
    IF proj-pspid(2) CO '0123456789'.
      PERFORM get_statuses.
      IF status CS 'DLIN'.
      ELSE.
        LOOP AT s_txt04.
          IF status CS s_txt04+3(4).
            PERFORM get_proj.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_PROJ
*&---------------------------------------------------------------------*

FORM get_proj.

  IF proj-pspid(2) <> proj-vernr.
    CLEAR int_wa.
    MOVE: proj-pspid TO int_wa-pspid,
          proj-vernr TO int_wa-vernr,
          proj-erdat TO int_wa-erdat.
    IF     status CS 'CLSD'.
      MOVE 'CLSD' TO int_wa-rtxt04.
    ELSEIF status CS 'TECO'.
      MOVE 'TECO' TO int_wa-rtxt04.
    ELSEIF status CS 'REL'.
      MOVE 'REL'  TO int_wa-rtxt04.
    ELSEIF status CS 'CRTD'.
      MOVE 'CRTD' TO int_wa-rtxt04.
    ENDIF.
    APPEND int_wa.
  ENDIF.

ENDFORM.                                                    " GET_PROJ


***********************PRINT HEADINGS**********************************
*           this subroutine prints colum headings.

*---------------------------------------------------------------------*
*           PRINT_HEADINGS_ALL                                       *
*---------------------------------------------------------------------*
FORM print_headings_all.
  FORMAT COLOR 2 ON.
  WRITE: 10 text-crt, 27 text-div, 44 text-sts, 59 text-prj,
         76 text-wbs.
  FORMAT COLOR OFF.
  SKIP.
ENDFORM.

*---------------------------------------------------------------------*
*           PRINT_VERT                                       *
*---------------------------------------------------------------------*
FORM print_vert.
  WRITE: 25 sy-vline, 42 sy-vline, 57 sy-vline.
ENDFORM.

*****************************  PRINT DATA  *****************************
* write exceptions to report screen

*---------------------------------------------------------------------*
*       PRINT_WBS                                             *
*---------------------------------------------------------------------*

FORM print_wbs.
  SORT int_wa BY posid.
  LOOP AT int_wa.
    IF int_wa-posid <> space.
      WRITE:   / int_wa-vernr  UNDER text-div.
      WRITE: int_wa-posid UNDER text-wbs,
             int_wa-erdat UNDER text-crt,
             int_wa-rtxt04 UNDER text-sts.
    ENDIF.

    PERFORM: print_vert.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRINT_PROJ
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM print_proj.

  LOOP AT int_wa.

    IF int_wa-pspid <> space.
      WRITE:   / int_wa-vernr  UNDER text-div.
      WRITE: int_wa-pspid UNDER text-prj,
             int_wa-erdat UNDER text-crt,
             int_wa-rtxt04 UNDER text-sts.
    ENDIF.
    PERFORM: print_vert.
  ENDLOOP.

ENDFORM.                    " PRINT_PROJ





