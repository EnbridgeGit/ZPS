*----------------------------------------------------------------------*
***INCLUDE LKAZBF15 .
*----------------------------------------------------------------------*
* Korrekturübersicht:
* PL0K007174: Note 499477 28.02.2002 FCL

*&---------------------------------------------------------------------*
*&      Form  MAIN_SRULE
*&---------------------------------------------------------------------*
FORM main_srule.

* if we do not enter KSRG_GENERATE_RULE, this works. but there will
* be no spool - what is inconsistent to nearly all other transactions
* in period-end closing
* IF NOT it_objnr IS INITIAL.          "<<<note499477
    CALL FUNCTION 'KSRG_GENERATE_RULE'
      EXPORTING
        i_stat    = stat
        i_gd      = gd
        i_rkauf   = rkauf
      TABLES
        it_objnr  = it_objnr
        i_seltext = sel_text.
* ELSE.                                "<<<note499477
*   MESSAGE i459(ka).                  "<<<note499477
* ENDIF.                               "<<<note499477

ENDFORM.                    " MAIN_SRULE
