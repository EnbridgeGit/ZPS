* 2009/01/13 TR580 mdemeest - upgrade 4.7 changes identified with UGL

*----------------------------------------------------------------------*
*   INCLUDE LKAZBF12                                                   *
*----------------------------------------------------------------------*

INCLUDE DINCONST.

TYPE-POOLS: INTE,
            PSVZ.

*----------------------------------------------------------------------*
*&      Form  MAIN_INTEREST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MAIN_INTEREST.

* call the copied function module                         "UGL

*  call function 'INCO_INTEREST_CALL'                      UGL

  call function 'Z_INCO_INTEREST_CALL'                    "UGL
       exporting
            i_rkauf = rkauf
            i_gd    = gd
            i_stat  = stat
      IMPORTING
           E_GD    = gd
           E_STAT  = stat
       tables
            i_objnr    = it_objnr
            i_prot     = gt_prot
            i_sel_text = sel_text.

ENDFORM.                               " MAIN_INTEREST

*&---------------------------------------------------------------------*
*&      Form  INTEREST_TODATE
*&---------------------------------------------------------------------*
*       enters to-date in RKAUF
*       By this the user is able to enter a specific to-date which
*       overwrites the period and year entered in the first dynpro.
*----------------------------------------------------------------------*
FORM INTEREST_TODATE.

  DATA: LD_RETURN(1),
        LT_FIELDS LIKE SVAL OCCURS 0 WITH HEADER LINE.

  CLEAR: LT_FIELDS,  LT_FIELDS[].
  LT_FIELDS-TABNAME   = 'RKAUF'.
  LT_FIELDS-FIELDNAME = 'TODATE'.
  IF NOT RKAUF-TODATE IS INITIAL.
    LT_FIELDS-VALUE     = RKAUF-TODATE.
  ENDIF.
  APPEND LT_FIELDS.


  CALL FUNCTION 'POPUP_GET_VALUES'
       EXPORTING
            NO_VALUE_CHECK  = OFF
            POPUP_TITLE     = TEXT-001
            START_COLUMN    = '25'
            START_ROW       = '7'
       IMPORTING
            RETURNCODE      = LD_RETURN
       TABLES
            FIELDS          = LT_FIELDS
       EXCEPTIONS
            ERROR_IN_FIELDS = 1
            OTHERS          = 2.

  IF LD_RETURN IS INITIAL.
    READ TABLE LT_FIELDS WITH KEY TABNAME   = 'RKAUF'
                                  FIELDNAME = 'TODATE'.
    IF SY-SUBRC IS INITIAL.
      RKAUF-TODATE = LT_FIELDS-VALUE.
    ENDIF.
  ENDIF.
ENDFORM.                               " INTEREST_TODATE

*&---------------------------------------------------------------------*
*&      Form  INTEREST_GENERAL_CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INTEREST_GENERAL_CHECKS.

  DATA: LD_RETURN.
  CALL FUNCTION 'POPUP_TO_DECIDE'
       EXPORTING
            DEFAULTOPTION  = '2'
            TEXTLINE1      = TEXT-003
            TEXTLINE2      = TEXT-004
            TEXTLINE3      = TEXT-005
            TEXT_OPTION1   = TEXT-006
            TEXT_OPTION2   = TEXT-007
            TITEL          = TEXT-002
            START_COLUMN   = 25
            START_ROW      = 6
            CANCEL_DISPLAY = OFF
       IMPORTING
            ANSWER         = LD_RETURN.


  IF LD_RETURN = '1'.
    RKAUF-ACHECK = ON.
  ELSE.
    CLEAR RKAUF-ACHECK.
  ENDIF.

ENDFORM.                               " INTEREST_GENERAL_CHECKS
