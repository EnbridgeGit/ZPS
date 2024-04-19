*----------------------------------------------------------------------*
***INCLUDE LKAZBF13 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MAIN_OR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAIN_OR.

  CALL FUNCTION 'ORBF_ORDERS_RECEIVED'
       EXPORTING
            I_STAT    = STAT
            I_GD      = GD
            I_RKAUF   = RKAUF
       TABLES
            IT_OBJNR  = IT_OBJNR
            I_SELTEXT = SEL_TEXT.

ENDFORM.                    " MAIN_OR

*&---------------------------------------------------------------------*
*&      Form  OR_GET_ALV_VARIANT
*&---------------------------------------------------------------------*
*       Gets the ALV display variant and writes it to RKAUF.
*----------------------------------------------------------------------*
FORM OR_GET_ALV_VARIANT.

  DATA: LT_FIELDS       LIKE SVAL   OCCURS 0 WITH HEADER LINE,
        LS_VARIANT      LIKE DISVARIANT,
        RETURNCODE(1)   TYPE C,
        POPUP_TITLE(30) TYPE C.

* initialize
  LS_VARIANT-REPORT    = GD-REPORT_ALV.
  LS_VARIANT-USERNAME  = SY-UNAME.
  POPUP_TITLE          = TEXT-PO1.
  LT_FIELDS-TABNAME    = 'DISVARIANT'.
  LT_FIELDS-FIELDNAME  = 'VARIANT'.
  LT_FIELDS-VALUE      = RKAUF-VARIANT.
  LT_FIELDS-FIELD_OBL  = 'X'.             "Muß-Feld
  APPEND LT_FIELDS.

* get default variant
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            I_SAVE        = 'A'
       CHANGING
            CS_VARIANT    = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND     = 2.

* display variant in popup
  CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
       EXPORTING
*           F1_FORMNAME               = ' '
*           F1_PROGRAMNAME            = ' '
            F4_FORMNAME               = 'OR_F4_ALV_VARIANT'
            F4_PROGRAMNAME            = 'SAPLKAZB'
            FORMNAME                  = 'OR_CHECK_ALV_VARIANT'
            POPUP_TITLE               = POPUP_TITLE
            PROGRAMNAME               = 'SAPLKAZB'
       IMPORTING
            RETURNCODE                = RETURNCODE
       TABLES
            FIELDS                    = LT_FIELDS.

  IF NOT RETURNCODE = 'A'.
    READ TABLE LT_FIELDS WITH KEY TABNAME    = 'DISVARIANT'
                               FIELDNAME  = 'VARIANT'.
    IF SY-SUBRC IS INITIAL.
      RKAUF-VARIANT = LT_FIELDS-VALUE.
    ENDIF.
  ELSE.
    RKAUF-VARIANT = LS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    " OR_GET_ALV_VARIANT
*&---------------------------------------------------------------------*
*&      Form  OR_F4_ALV_VARIANT
*&---------------------------------------------------------------------*
*       Gets the ALV display variant and writes it to RKAUF.
*----------------------------------------------------------------------*
FORM OR_F4_ALV_VARIANT USING TABNAME FIELDNAME DISPLAY
                       CHANGING RETURNCODE VALUE.

  DATA: LS_VARIANT      LIKE DISVARIANT,
        LD_EXIT(1)      TYPE C.

* initialize
  LS_VARIANT-REPORT   = GD-REPORT_ALV.
  LS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = LS_VARIANT
            I_TABNAME_HEADER    = GD-TABNAME
*           I_TABNAME_ITEM      =
            I_SAVE              = 'A'
       IMPORTING
            E_EXIT              = LD_EXIT
            ES_VARIANT          = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF LD_EXIT IS INITIAL.
      CLEAR RETURNCODE.
      VALUE   = LS_VARIANT-VARIANT.
    ELSE.
      RETURNCODE = 'A'.
      CLEAR VALUE.
    ENDIF.
  ENDIF.

ENDFORM.                   " OR_F4_ALV_VARIANT
*&---------------------------------------------------------------------*
*&      Form  OR_CHECK_ALV_VARIANT
*&---------------------------------------------------------------------*
*       Gets the ALV display variant and writes it to RKAUF.
*----------------------------------------------------------------------*
FORM OR_CHECK_ALV_VARIANT TABLES   FIELDS STRUCTURE SVAL
                          CHANGING ERROR  STRUCTURE SVALE.

  DATA: LS_VARIANT LIKE DISVARIANT.

* initialize
  LS_VARIANT-REPORT   = GD-REPORT_ALV.
  LS_VARIANT-USERNAME = SY-UNAME.
  READ TABLE FIELDS WITH KEY TABNAME    = 'DISVARIANT'
                             FIELDNAME  = 'VARIANT'.
  IF    SY-SUBRC IS INITIAL
   AND  NOT ( FIELDS-VALUE IS INITIAL ).
    LS_VARIANT-VARIANT = FIELDS-VALUE.
  ELSE.
    ERROR-MSGID       = '0K'.
    ERROR-MSGTY       = 'E'.
    ERROR-MSGNO       = '204'.
    ERROR-MSGV1       = ' '.
    ERROR-MSGV2       = ' '.
    ERROR-MSGV3       = ' '.
    ERROR-MSGV4       = ' '.
    ERROR-ERRORTAB    = 'DISVARIANT'.
    ERROR-ERRORFIELD  = 'VARIANT'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
       EXPORTING
            I_SAVE        = 'A'
       CHANGING
            CS_VARIANT    = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND     = 1.
  IF NOT SY-SUBRC IS INITIAL.
    ERROR-MSGID       = '0K'.
    ERROR-MSGTY       = 'E'.
    ERROR-MSGNO       = '204'.
    ERROR-MSGV1       = ' '.
    ERROR-MSGV2       = ' '.
    ERROR-MSGV3       = ' '.
    ERROR-MSGV4       = ' '.
    ERROR-ERRORTAB    = 'DISVARIANT'.
    ERROR-ERRORFIELD  = 'VARIANT'.
  ENDIF.

ENDFORM.                   " OR_CHECK_ALV_VARIANT
