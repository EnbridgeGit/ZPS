*----------------------------------------------------------------------*
*   INCLUDE LKAZBF17                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_DATES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATES.
      DATA: L_PERIO TYPE PERBL.
      DATA: L_GJAHR TYPE GJAHR.
* ALRK181562
*-----if period to which poc/ev will be calculated is < actual period
*-----for online processing
      IF SY-BATCH IS INITIAL.
*------get actual period
        CALL FUNCTION 'K_DATE_TO_PERIOD_CONVERT'
             EXPORTING
                  I_DATE  = SY-DATUM
                  I_KOKRS = RKAUF-KOKRS
             IMPORTING
                  E_GJAHR = L_GJAHR
                  E_PERIO = L_PERIO.
*------compare year which was entered and actual year
        IF L_GJAHR > RKAUF-GJAHR.
          MESSAGE I060(7A).
        ENDIF.
*------compare period which was entered and actual period
        IF L_PERIO > RKAUF-TO.
          MESSAGE I060(7A).
        ENDIF.
      ELSE.
*-----for batch-processing call message-handler
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  ARBGB                   = '7A'
*         EXCEPTION_IF_NOT_ACTIVE = 'X'
                  MSGTY                   = 'I'
*         MSGV1                   = ' '
*         MSGV2                   = ' '
*         MSGV3                   = ' '
*         MSGV4                   = ' '
                  TXTNR                   = '060'
*         ZEILE                   = ' '
*    IMPORTING
*         ACT_SEVERITY            =
*         MAX_SEVERITY            =
*    EXCEPTIONS
*         MESSAGE_TYPE_NOT_VALID  = 1
*         NOT_ACTIVE              = 2
*         OTHERS                  = 3
                  .
        IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.


ENDFORM.                    " CHECK_DATES
