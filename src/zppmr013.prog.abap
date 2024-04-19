REPORT ZPPMR013 NO STANDARD PAGE HEADING LINE-SIZE 88
                LINE-COUNT 65 MESSAGE-ID PP.
************************************************************************
*
*   PROGRAM:    ZPPMR013
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       May 1998.
*
*   The purpose of this program is to list all unique WBS elements.
*   This does not include the project number, just the 4 digit WBS
*   idnetifier portion of the number.  The class/asset the which the
*   element settles is also listed.
*
************************************************************************

TABLES:   PRPS,                         " WBS element master data
          COBRB.                        " Settlement rules by object nbr

DATA:     COUNT(6)      TYPE N.                         " line counter

DATA:     BEGIN OF WBSTAB OCCURS 50000,
            WBS(4)      TYPE C,                         " WBS element
            POST1       LIKE PRPS-POST1,                " WBS descripton
            BELKZ       LIKE PRPS-BELKZ,                " detail level
            ANLN1A(5)   TYPE C,                         " first 5 char
            ANLN1B(5)   TYPE C,                         " last 5 char
            PERCENT(3)  TYPE N,                         " allocation pct
          END OF WBSTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(27) TEXT-003.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK BOX1.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    PARAMETERS: P_PBUKR    LIKE PRPS-PBUKR DEFAULT 'UGL'.
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
    SELECT-OPTIONS: S_WBS  FOR PRPS-POSID+7(4).
  SELECTION-SCREEN END OF BLOCK BOX3.

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    SELECT-OPTIONS: S_CLS  FOR COBRB-ANLN1(5).
  SELECTION-SCREEN END OF BLOCK BOX4.

  SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME.
    SELECT-OPTIONS: S_PCT  FOR COBRB-PROZS.
  SELECTION-SCREEN END OF BLOCK BOX5.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: WBSTAB.
CLEAR:   WBSTAB.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.

  SELECT * FROM PRPS
  WHERE  PBUKR EQ P_PBUKR.

    CHECK PRPS-POSID+7(4) IN S_WBS.                      " WBS range

    WBSTAB-WBS      = PRPS-POSID+7(4).
    WBSTAB-POST1    = PRPS-POST1.
    WBSTAB-BELKZ    = PRPS-BELKZ.

    CLEAR COBRB.
    IF PRPS-BELKZ = 'X'.
      SELECT * FROM COBRB
      WHERE OBJNR EQ PRPS-OBJNR
      AND   KONTY EQ 'AN'                                " fixed asset
      AND   PERBZ EQ 'GES'.                              " full setlmnt
        WBSTAB-ANLN1A  = COBRB-ANLN1(5).
        WBSTAB-ANLN1B  = COBRB-ANLN1+7(5).
        WBSTAB-PERCENT = COBRB-PROZS(3).
      ENDSELECT.
    ENDIF.

    IF  WBSTAB-ANLN1A  IN S_CLS
    AND WBSTAB-PERCENT IN S_PCT.
      APPEND WBSTAB.
      CLEAR WBSTAB.
    ENDIF.

  ENDSELECT.

* sort the wbs table by the 4 digit wbs element
  SORT WBSTAB BY WBS ANLN1A DESCENDING.

* drop all duplicate wbs elemnts, leaving the unique ones
  DELETE ADJACENT DUPLICATES FROM WBSTAB COMPARING WBS.

* now output the report
  LOOP AT WBSTAB.

    COUNT = COUNT + 1.

    WRITE:   /01  SY-VLINE
         ,   003  COUNT
         ,   010  SY-VLINE
         ,   012  WBSTAB-WBS
         ,   017  SY-VLINE
         ,   019  WBSTAB-POST1
         ,   060  SY-VLINE
         ,   063  WBSTAB-BELKZ
         ,   066  SY-VLINE
         ,   068  WBSTAB-ANLN1A
         ,   074  SY-VLINE
         ,   076  WBSTAB-ANLN1B
         ,   082  SY-VLINE.
    IF WBSTAB-PERCENT <> 0.
      WRITE: 084  WBSTAB-PERCENT
           , 088  SY-VLINE.
    ELSE.
      WRITE: 088  SY-VLINE.
    ENDIF.

  ENDLOOP.

  ULINE.

END-OF-SELECTION.

************************************************************************
*  Listed below are subroutines used by the program.
************************************************************************

FORM WRITE_HEADER.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 036 TEXT-001,  P_PBUKR
            , 077 TEXT-002,  SY-PAGNO
            , 088 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     TEXT-012 UNDER SY-DATUM
            ,     SY-MANDT, SY-SYSID
            , 035 TEXT-003
            ,     SY-REPID UNDER TEXT-002
            , 088 SY-VLINE.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 TEXT-004
            , 010 SY-VLINE
            , 012 TEXT-005
            , 017 SY-VLINE
            , 019 TEXT-006
            , 060 SY-VLINE
            , 062 TEXT-007
            , 066 SY-VLINE
            , 068 TEXT-008
            , 074 SY-VLINE
            , 076 TEXT-009
            , 082 SY-VLINE
            , 084 TEXT-010
            , 088 SY-VLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

************************************************************************
*  This is the end, my freind
************************************************************************
