*----------------------------------------------------------------------*
***INCLUDE LKAZBF16 .
*----------------------------------------------------------------------*
* changing history
* ----------------
* SCT ALRK136242 141098 include new
* P6BK109809: Note 668208 07.10.2003 FCL
* ----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_CA_CS
*----------------------------------------------------------------------*
form assignments_ca_cs
     using    r_kokrs       like tka01-kokrs
              r_kokrs_txt   like tka01-bezei
              r_sign_ap     like rkauf-sign_ap.

  data:
  ld_kosza  like  tksa0-ist_plan.

  if ( r_sign_ap = con_act ).
    ld_kosza  =  '1'.
  elseif ( r_sign_ap = con_plan ).
    ld_kosza  =  '2'.
  else.
    ld_kosza  =  '1'.
  endif.

  call function 'K_ICC_ASSIGNMENTS_PER_CA'
       exporting
            kokrs                 = r_kokrs
            kokrs_txt             = r_kokrs_txt
            actual_plan_indicator = ld_kosza
            for_information_only  = 'X'
       exceptions
            not_found             = 01.

  if syst-subrc <> 0.
    message i379 with r_kokrs.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  MAIN_ACCRUAL
*&---------------------------------------------------------------------*
form main_accrual.

  call function 'K_ACCRUAL_CALL'
       exporting
            i_rkauf    = rkauf
            i_gd       = gd
            i_stat     = stat
            i_tka07    = tka07
       importing
            e_stat     = stat
            e_gd       = gd
       tables
            it_objnr   = it_objnr
            it_periods = i_periods
            et_coeja   = xcoeja
            et_coep    = xcoep.

****NOTE 668208 Start
** close protocol
*  perform monitor_close_record.        "schedule manager
****NOTE 668208 End
* show result
  perform list_results tables xcoep
                              xcoeja
                              xcoaib
                      using   rkauf
                              gd
                              tka01
                              tka07
                              tkt09
                              stat.                               "RR

****NOTE 668208 Start
* close protocol
  perform monitor_close_record.        "schedule manager
****NOTE 668208 End
endform.
