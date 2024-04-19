*  P9CK097794 Fund Accounting(FM) 30.11.2000
*-------------------------------------------------------------------
***INCLUDE LKAZBF06 .
*-------------------------------------------------------------------
* changing history
* ----------------
* 4.6A
* SCT ALRK136242 151098 accrual calculation connected
* P9CK098015: Hinweis 362049 28.11.2000 FCL
* ----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*       FORM  CHECK_KOKRS.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form  check_kokrs.

  data: l_anzbp like t009-anzbp,
        l_anzsp like t009-anzsp,
        l_gjahr like rkauf-gjahr.

  if rkauf-wrttp ne wrttp_obli.
    l_gjahr = rkauf-gjahr.
  else.
    clear l_gjahr.
  endif.
  call function 'K_KOKRS_READ'
       exporting
            gjahr   = l_gjahr
            kokrs   = rkauf-kokrs
       importing
            e_tka00 = tka00
            e_tka01 = tka01
       exceptions
            others  = 4.
  if sy-subrc <> 0.
*   PERFORM MESSAGE_STORE.
    message id     sy-msgid
            type   sy-msgty
            number sy-msgno
            with   sy-msgv1
                   sy-msgv2
                   sy-msgv3
                   sy-msgv4.
    exit.
  endif.

  call function 'K_PERIODS_GET_FOR_KOKRS'
       exporting
            gjahr              = rkauf-gjahr
            kokrs              = rkauf-kokrs
       importing
            anzbp              = l_anzbp
            anzsp              = l_anzsp
       exceptions
            inconsistent_input = 1
            insufficient_input = 2
            kokrs_not_found    = 3
            periv_not_found    = 4
            others             = 5.
* period settings
  rkauf-anzbp   = l_anzbp.
  rkauf-anzsp   = l_anzsp.
* without special periods
  if l_anzsp = 0.
    rkauf-periods = l_anzbp + l_anzsp.
  else.
* with special periods
    call function 'K_PERIODS_GET_FOR_KOKRS'
         exporting
              kokrs              = rkauf-kokrs
         importing
              anzbp              = l_anzbp
              anzsp              = l_anzsp
         exceptions
              inconsistent_input = 1
              insufficient_input = 2
              kokrs_not_found    = 3
              periv_not_found    = 4
              others             = 5.
    rkauf-periods = l_anzbp + l_anzsp.
  endif.
* currency settings
  rkauf-kwaer   = tka01-waers.
  gd-tka00_rabwg = tka00-rabwg.                         "FM
  rkauf-allew   = tka00-allew.
  rkauf-waers   = tka01-waers.

endform.                               " CHECK_KOKRS

*&---------------------------------------------------------------------*
*       FORM  CHECK_VERSN.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form  check_versn.

  data: ld_actvt    like tact-actvt,
        ld_gjahr    like rkauf-gjahr.

*----------------------------------------------------------------------*
* set activity
*----------------------------------------------------------------------*
  if rkauf-sign_ap = con_plan.
    ld_actvt = '72'.
  endif.

*----------------------------------------------------------------------*
* read versn
*----------------------------------------------------------------------*
  ld_gjahr = rkauf-gjahr.

  while ld_gjahr le rkauf-gjahr_bis and
        not rkauf-gjahr is initial.

*   no general check for 5 (progress)
    if gd-processor <> 5 and gd-processor <> 6.

      call function 'K_VERSN_READ'
           exporting
                i_gjahr          = ld_gjahr
                i_kokrs          = rkauf-kokrs
                i_versn          = rkauf-versn
                i_actvt          = ld_actvt
                bypassing_buffer = 'X'
           importing
                e_tka09          = tka09
                e_tka07          = tka07
                e_tkt09          = tkt09.

      if tka09-planning <> true and rkauf-sign_ap = con_plan.
        message e388(ka) with rkauf-versn.
      endif.
      if tka09-actual <> true and rkauf-sign_ap = con_act.
        message e388(ka) with rkauf-versn.
      endif.

      if gd-processor ne 1.                                 "P9CK098015
        if tka07-vspkz = true and rkauf-sign_ap = con_plan. "ALRK136242
*         version locked against changes of plan data:
          message e455(ka) with rkauf-versn rkauf-gjahr.    "ALRK136242
        endif.                                              "P9CK098015
      endif.
    endif.

    case gd-processor.
      when 3.
        if tka07-acupd eq '00'.
          message e760(ki) with rkauf-versn rkauf-gjahr.
        endif.
      when 4.
        perform pri_prp_versn_check using rkauf.
      when 5.
        perform check_versn_eva.
      when 6.
        perform prc_versn_check using tka09 tka07 tkt09 rkauf.
      when 9.
*       accrual calculation: set rkauf-wsdat for plan:
        perform acc_versn_check                             "ALRK136242
                 using     tka07 tka09 tkt09
                 changing  rkauf.
    endcase.

    ld_gjahr = ld_gjahr + 1.

    if rkauf-sign_ap eq con_plan.
      rkauf-kurst   = tka07-kurst.
    else.
      rkauf-kurst   = kurst_act.
    endif.

  endwhile.
*----------------------------------------------------------------------*
* versn and vrgng check
*----------------------------------------------------------------------*
*   no general check for 5 (progress) & 6(ctc)
  if gd-processor <> 5 and gd-processor <> 6.

    call function 'K_VERSN_FOR_VRGNG_CHECK'
         exporting
              vrgng = rkauf-vrgng
              kokrs = rkauf-kokrs
              versn = rkauf-versn.
  endif.
endform.                               " CHECK_VERSN
*ENHANCEMENT-POINT LKAZBF06_01 SPOTS ES_LKAZBF06 STATIC INCLUDE BOUND.

*&---------------------------------------------------------------------*
*       FORM  CHECK_PERIOD.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form  check_period.

  data: ld_from_sav  like rkauf-from,
        ld_to_sav    like rkauf-to,
        ld_gjahr_sav like rkauf-gjahr,
        ld_lnp       like rkauf-from,
        ld_ldper     like sy-datum,
        ld_tka01     like tka01.

*----------------------------------------------------------------------*
* set unlimited period for obligo
*----------------------------------------------------------------------*
  if rkauf-wrttp eq wrttp_obli.
    clear rkauf-gjahr.
    rkauf-gjahr_bis = 9999.
    rkauf-from = 001.
    rkauf-to        = 999.
    exit.
  endif.

*** only for planned interest calculation:
*** if a date is specified, period and year is necessary
  if rkauf-vrgng = vrgng_kzrp.
    if ( ( rkauf-gjahr is initial ) and ( not rkauf-from is initial ) )
     or ( ( not rkauf-gjahr is initial ) and ( rkauf-from is initial ) )
     or ( ( rkauf-gjahr_bis is initial ) and ( not rkauf-to is initial )
   ) or ( ( not rkauf-gjahr_bis is initial ) and ( rkauf-to is initial )
   ).
      message e135(if).
    endif.
  endif.

* only for cost to complete (projects) ---> set periods to actual period
* because only rkauf-todate is relevant for CTC
  if rkauf-vrgng = vrgng_kctc.
    call function 'KAOP_SET_TODATE'
         changing
              c_rkauf = rkauf.
  endif.

*----------------------------------------------------------------------*
* first period
*----------------------------------------------------------------------*
  if   ( rkauf-from is initial
   or  rkauf-from eq '000' )
   and rkauf-vrgng <> vrgng_kzrp
   and rkauf-vrgng <> vrgng_kctc
   and rkauf-vrgng <> vrgng_pev1.
    message e114(ka).
  elseif not rkauf-from is initial
         and gd-single_period eq true.
    rkauf-to = rkauf-from.
  endif.

*----------------------------------------------------------------------*
* single period --> fill period-to with period-from
*----------------------------------------------------------------------*
*** not for planned interest calculation
  if rkauf-vrgng <> vrgng_kzrp.
***
    if rkauf-to   = space or
       rkauf-to   = '000'.
      if rkauf-gjahr_bis ne rkauf-gjahr.
        message e114(ka).
      else.
        rkauf-to = rkauf-from.
      endif.
    endif.
  endif.
*----------------------------------------------------------------------*
* single year
*----------------------------------------------------------------------*
*** not for planned interest calculation
  if rkauf-vrgng <> vrgng_kzrp.
***
    if    rkauf-gjahr_bis eq space
      or rkauf-gjahr_bis eq '0000'.
      rkauf-gjahr_bis = rkauf-gjahr.
    endif.
  endif.
*----------------------------------------------------------------------*
*  wrong period input
*----------------------------------------------------------------------*
  if rkauf-gjahr_bis eq rkauf-gjahr.
    if rkauf-from > rkauf-to.
      message e152(ka) with rkauf-from rkauf-to.
    endif.
  endif.
*----------------------------------------------------------------------*
* check if periods are ok
*----------------------------------------------------------------------*

 if ( rkauf-vrgng = vrgng_kzrp and    "this kind of period check only if
          not rkauf-gjahr is initial and"end and start date is specified
      not rkauf-gjahr_bis is initial )"or vrgng <> planned interest cal.
                 or
                 rkauf-vrgng <> vrgng_kzrp.

    ld_gjahr_sav = rkauf-gjahr.
    while ld_gjahr_sav le rkauf-gjahr_bis.

      if ld_gjahr_sav = rkauf-gjahr.
        ld_from_sav = rkauf-from.
      else.
        ld_from_sav = 1.
      endif.
      if ld_gjahr_sav = rkauf-gjahr_bis.
        ld_to_sav = rkauf-to.
      else.
        ld_to_sav = 12.
      endif.

      refresh i_periods.

      while ld_from_sav <= ld_to_sav.
        call function 'G_POSTING_DATE_OF_PERIOD_GET'
             exporting
                  period             = ld_from_sav
                  variant            = tka01-lmona
                  year               = ld_gjahr_sav
             importing
                  from_date          = i_periods-datab
                  to_date            = i_periods-datbi
                  last_normal_period = ld_lnp
             exceptions
                  others             = 4.
        if sy-subrc > 0.
*       PERFORM MESSAGE_STORE.
          message id     sy-msgid
                  type   sy-msgty
                  number sy-msgno
                  with   sy-msgv1
                         sy-msgv2
                         sy-msgv3
                         sy-msgv4.
          continue.
        endif.
        move ld_from_sav to i_periods-buper.
        append i_periods.

*      IF NOT ( LD_LNP IS INITIAL ).
*        CASE RKAUF-SIGN_AP.
*----------------------------------------------------------------------*
*       plan
*----------------------------------------------------------------------*
*          WHEN CON_PLAN.
*            MESSAGE E390(KA) WITH      "vgl. E239(KP)
*                              LD_FROM_SAV TKA01-LMONA
*                              TKA01-KOKRS LD_LNP.
*----------------------------------------------------------------------*
*       actual
*----------------------------------------------------------------------*
*          WHEN CON_ACT.
*            IF LD_LNP >= RKAUF-FROM.
*              MESSAGE E391(KA) WITH    "vgl. E261(KP)
*                                 TKA01-LMONA TKA01-KOKRS
*                                 RKAUF-FROM  RKAUF-TO.
*            ENDIF.
*        ENDCASE.
*      ENDIF.
        add 1 to ld_from_sav.
      endwhile.
      add 1 to ld_gjahr_sav.
    endwhile.
  else.
    if not rkauf-gjahr is initial.     "start date specified

      call function 'G_POSTING_DATE_OF_PERIOD_GET'
           exporting
                period             = rkauf-from
                variant            = tka01-lmona
                year               = rkauf-gjahr
           importing
                from_date          = i_periods-datab
                to_date            = i_periods-datbi
                last_normal_period = ld_lnp
           exceptions
                others             = 4.
      if sy-subrc > 0.
*       PERFORM MESSAGE_STORE.
        message id     sy-msgid
                type   sy-msgty
                number sy-msgno
                with   sy-msgv1
                       sy-msgv2
                       sy-msgv3
                       sy-msgv4.
      endif.
    endif.
    if not rkauf-gjahr_bis is initial. "end date specified
      call function 'G_POSTING_DATE_OF_PERIOD_GET'
           exporting
                period             = rkauf-to
                variant            = tka01-lmona
                year               = rkauf-gjahr_bis
           importing
                from_date          = i_periods-datab
                to_date            = i_periods-datbi
                last_normal_period = ld_lnp
           exceptions
                others             = 4.
      if sy-subrc > 0.
*       PERFORM MESSAGE_STORE.
        message id     sy-msgid
                type   sy-msgty
                number sy-msgno
                with   sy-msgv1
                       sy-msgv2
                       sy-msgv3
                       sy-msgv4.
      endif.
    endif.

  endif.
*----------------------------------------------------------------------*
* interest calculation on actaul values: check to-date
*----------------------------------------------------------------------*
  if rkauf-vrgng = vrgng_kzri.
    if not rkauf-todate is initial.    "date specified
      if rkauf-todate > sy-datum.
        if rkauf-batch is initial      "message 'w' would abort batch
         and gd-wl_active is initial.
          message w130(if).
        elseif ( not rkauf-batch is initial )
         and gd-wl_active is initial.
          message i130(if).
        else.
*         do nothing (message will abort job scheduling)
        endif.
      endif.
    else.                              "period specified
      call function 'K_PERIODS_GET'
           exporting
                par_igjahr    = rkauf-gjahr
                par_ipoper    = rkauf-to
                par_kokrs     = rkauf-kokrs
           importing
                par_ldper     = ld_ldper
           exceptions
                kokrs_invalid = 1
                poper_invalid = 2
                others        = 3.

      if ld_ldper > sy-datum.
        if rkauf-batch is initial      "message 'w' would abort batch
         and gd-wl_active is initial.
          message w130(if).
        elseif ( not rkauf-batch is initial )
         and gd-wl_active is initial.
          message i130(if).
        else.
*         do nothing (message will abort job scheduling)
        endif.
      endif.
    endif.
  endif.

*----------------------------------------------------------------------*
* progress analysis: check to-date
*----------------------------------------------------------------------*
  if rkauf-vrgng = vrgng_pev1 and rkauf-todate is initial.
    call function 'K_KOKRS_READ'
         exporting
              kokrs   = rkauf-kokrs
         importing
              e_tka01 = ld_tka01.
    call function 'LAST_DAY_IN_PERIOD_GET'
         exporting
              i_gjahr = rkauf-gjahr
              i_periv = ld_tka01-lmona
              i_poper = rkauf-from
         importing
              e_date  = rkauf-todate.
  endif.
*----------------------------------------------------------------------*
* surcharge (actual): check period and posting date
*----------------------------------------------------------------------*
  if ( gd-pfkey = 'SUBA' or gd-pfkey = 'SUBP' or gd-pfkey = 'SUBS' )
     and ( rkauf-wrttp = wrttp_act ).
    if not ( rkauf-budat is initial ).
      perform check_posting_date.
    endif.
  endif.

endform.                               " CHECK_PERIOD

*&---------------------------------------------------------------------*
*       FORM  AUTH.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form  auth.

  constants: begin of md,
          insert   like tact-actvt  value '01',
          change   like tact-actvt  value '02',
          show     like tact-actvt  value '03',
          delete   like tact-actvt  value '06',
          exec     like tact-actvt  value '16',
          simu     like tact-actvt  value '48',
          list     like tact-actvt  value '04',
        end of md.

*----------------------------------------------------------------------*
* test authority for execution
*----------------------------------------------------------------------*
  call function 'K_VRGNG_AUTHORITY_CHECK'
       exporting
            actvt               = md-exec
            kokrs               = rkauf-kokrs
            vrgng               = rkauf-vrgng
       exceptions
            system_error        = 1
            user_not_authorized = 2.
  if sy-subrc <> 0.
    if rkauf-test is initial.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
*----------------------------------------------------------------------*
*     test authority for simulation
*----------------------------------------------------------------------*
      call function 'K_VRGNG_AUTHORITY_CHECK'
           exporting
                actvt  = md-simu
                kokrs  = rkauf-kokrs
                vrgng  = rkauf-vrgng
           exceptions
                others = 4.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
      clear rkauf-book.
    endif.
  endif.

endform.                               " AUTH

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_BLOCKING
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_period_blocking.

  data: ld_from_sav  like rkauf-from,
        ld_to_sav    like rkauf-to,
        ld_gjahr_sav like rkauf-gjahr,
        ld_anzbp     like  t009-anzbp,
        ld_anzsp     like  t009-anzsp.

  ld_gjahr_sav = rkauf-gjahr.
  while ld_gjahr_sav le rkauf-gjahr_bis.
*----------------------------------------------------------------------*
* get maximum number of periods for actually processed year
*----------------------------------------------------------------------*
    call function 'K_PERIODS_GET_FOR_KOKRS'
         exporting
              gjahr   = ld_gjahr_sav
              i_tka01 = tka01
              kokrs   = rkauf-kokrs
         importing
              anzbp   = ld_anzbp
              anzsp   = ld_anzsp.
    if ld_anzsp = 0.                   "function module changed
* without special periods
      gd-periods = ld_anzbp.
    else.
* with special periods
      call function 'K_PERIODS_GET_FOR_KOKRS'
           exporting
                i_tka01 = tka01
                kokrs   = rkauf-kokrs
           importing
                anzbp   = ld_anzbp
                anzsp   = ld_anzsp.
      gd-periods = ld_anzbp + ld_anzsp.
    endif.
    if ld_gjahr_sav = rkauf-gjahr.
      ld_from_sav = rkauf-from.
    else.
      ld_from_sav = 1.
    endif.
    if ld_gjahr_sav = rkauf-gjahr_bis.
      ld_to_sav = rkauf-to.
    else.
      ld_to_sav = gd-periods.
    endif.
*   IF RKAUF-TEST NE FALSE.
*     PERFORM MESSAGE_LINE_ADD_1(SAPLKAZB).
*     PERFORM MESSAGE_DEFAULT(SAPLKAZB)
*                        USING 'KA' 'S' '159' TEXT-PER
*                                        ' '  ' '  ' '.
*   ENDIF.

    call function 'CO_PERIOD_BLOCKING_CHECK'
         exporting
              kokrs         = rkauf-kokrs
              versn         = rkauf-versn
              gjahr         = ld_gjahr_sav
              perab         = ld_from_sav
              perbi         = ld_to_sav
              vrgng         = rkauf-vrgng
         exceptions
              vrgng_blocked = 1.

    if sy-subrc ne 0.
      if rkauf-test eq false.
        message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        perform message_line_add_1(saplkazb) using gd-mesg_line.
        perform message_buffer using 'I'.
        perform message_default(saplkazb)
                           using gd-mesg_line
                                 'KA' 'S' '159' text-per
                                           ' '  ' '  ' '.
        perform message_buffer using 'O'.
        perform message_store(saplkazb) using gd-max_error.
      endif.
    endif.
    add 1 to ld_gjahr_sav.
  endwhile.

endform.                               " CHECK_PERIOD_BLOCKING

*&---------------------------------------------------------------------*
*&      Form  CHECK_GJAHR
*&---------------------------------------------------------------------*
*       check input of GJAHR
*----------------------------------------------------------------------*
form check_gjahr.

  if rkauf-gjahr is initial.
    if rkauf-vrgng <> vrgng_kzrp and
       rkauf-vrgng <> vrgng_kctc.
      message e114(ka).
    endif.
  elseif not rkauf-gjahr is initial.   "start date specified
    if rkauf-gjahr le 49.
      add 2000 to rkauf-gjahr.
    elseif rkauf-gjahr between 50 and 99.
      add 1900 to rkauf-gjahr.
    endif.
    if (    rkauf-gjahr_bis is initial
         or gd-single_gjahr eq true )
       and rkauf-vrgng <> vrgng_kzrp.
      rkauf-gjahr_bis = rkauf-gjahr.
      set parameter id 'GJR' field rkauf-gjahr.
    endif.
  endif.

  if not rkauf-gjahr_bis is initial.
    if rkauf-gjahr_bis le 49.
      add 2000 to rkauf-gjahr_bis.
    elseif rkauf-gjahr_bis between 50 and 99.
      add 1900 to rkauf-gjahr_bis.
    endif.
    set parameter id 'GJR' field rkauf-gjahr.
  endif.

  if not rkauf-gjahr_bis is initial.
    if rkauf-gjahr gt rkauf-gjahr_bis.
      message e152(ka) with rkauf-gjahr rkauf-gjahr_bis.
    endif.
  endif.
endform.                               " CHECK_GJAHR

*&---------------------------------------------------------------------*
*&      Form  CHECK_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  no dialog processing in batch
*----------------------------------------------------------------------*
form check_processing.

  if     rkauf-batch  eq true
     and rkauf-dialog eq true.
    rkauf-dialog = false.
  endif.

endform.                               " CHECK_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_BUFFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form message_buffer using p_io type c.

  statics: begin of l_mesg,
             msgid like sy-msgid,
             msgty like sy-msgty,
             msgno like sy-msgno,
             msgv1 like sy-msgv1,
             msgv2 like sy-msgv2,
             msgv3 like sy-msgv3,
             msgv4 like sy-msgv4,
        end of l_mesg.

  if p_io eq 'I'.
    move-corresponding sy to l_mesg.
  elseif p_io eq 'O'.
    move-corresponding l_mesg to sy.
  endif.

endform.                               " MESSAGE_BUFFER
*&---------------------------------------------------------------------*
*&      Form  PRC_VERSN_CHECK
*&---------------------------------------------------------------------*
*       Version allowed for cost forecast?
*----------------------------------------------------------------------*
*      -->P_TKA09  text
*      -->P_TKA07  text
*      -->P_TKT09  text
*----------------------------------------------------------------------*
form prc_versn_check using value(p_tka09) structure tka09
                           value(p_tka07) structure tka07
                           value(p_tkt09) structure tkt09
                           value(p_rkauf) structure rkauf.

  data: i_tkvs like tkvs.

  if p_rkauf-versn = '000'.
    message e015(kpn) with p_rkauf-versn.
*   Die Prognosekosten können nicht in der Version & abgelegt werden
  endif.

  call function 'K_TKVS_READ'
       exporting
            version   = p_rkauf-versn
*            langu     =
       importing
            e_tkvs    = i_tkvs
*         E_VTEXT   =
*    EXCEPTIONS
*         NOT_FOUND = 1
*         OTHERS    = 2
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

  if not i_tkvs-exuvs = '04'.
    message e015(kpn) with p_rkauf-versn.
*   Die Prognosekosten können nicht in der Version & abgelegt werden
  endif.

endform.                               " PRC_VERSN_CHECK
*&---------------------------------------------------------------------

*&      Form  PRI_PRP_VERSN_CHECK
*&---------------------------------------------------------------------

*       text
*----------------------------------------------------------------------

*      -->P_RKAUF  text
*----------------------------------------------------------------------

form pri_prp_versn_check using
                            p_rkauf structure rkauf.

  call function 'K_VERSN_FOR_VRGNG_CHECK'
     exporting
          vrgng             = 'KZRP'
          kokrs             = p_rkauf-kokrs
          versn             = p_rkauf-versn
*         DELTA_VERSN       =
     exceptions
          wrong_versn       = 1
          vrgng_not_allowed = 2
          others            = 3.

  if sy-subrc <> 0.
    message e138(if).
*** Die Planverzinsung in Deltaversionen ist nicht möglich
  endif.

endform.                               " PRI_PRP_VERSN_CHECK

*&---------------------------------------------------------------------*
*       FORM  CHECK_VERSN_EVA.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form  check_versn_eva.

  data: ld_evvs     like evvs.

*ENHANCEMENT-POINT CHECK_VERSN_EVA_02 SPOTS ES_LKAZBF06 STATIC INCLUDE BOUND.
*----------------------------------------------------------------------*
* does the ev-version exist
*----------------------------------------------------------------------*
  call function 'CNEV_01_SELECT_VERSION_EV'
       exporting
            i_kokrs            = rkauf-kokrs
            i_versn_ev         = rkauf-versn
       importing
            e_evvs             = ld_evvs
       exceptions
            versn_ev_not_found = 1
            others             = 2.
*ENHANCEMENT-SECTION     CHECK_VERSN_EVA_01 SPOTS ES_LKAZBF06 INCLUDE BOUND.
  if sy-subrc <> 0.
    message e301(7a) with rkauf-versn rkauf-kokrs.
  endif.
*END-ENHANCEMENT-SECTION.

endform.                               " CHECK_VERSN_EVA
*ENHANCEMENT-POINT LKAZBF06_02 SPOTS ES_LKAZBF06 STATIC INCLUDE BOUND.
*&---------------------------------------------------------------------*
*&      Form  ACC_VERSN_CHECK
*&---------------------------------------------------------------------*
form acc_versn_check                                        "ALRK136242
     using    r_tka07  structure tka07
              r_tka09  structure tka09
              r_tkt09  structure tkt09
     changing c_rkauf  structure rkauf.

  data:
  ld_txt  like  tj01t-txt.

  if ( rkauf-sign_ap = con_plan ) and ( rkauf-wsdat is initial ).
    rkauf-wsdat = r_tka07-pldat.
  endif.

  if not ( r_tka09-refvs is initial ).
*    this versn is a delta versn and cannot be used: get text of versn.
    call function 'BSV_GET_TEXT_VRGNG'
         exporting
              langu = sy-langu
              vrgng = rkauf-vrgng
         importing
              txt   = ld_txt.

    message e506(ki) with ld_txt rkauf-versn rkauf-vrgng.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  CHECK_POSTING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_posting_date.

  data: ld_message_type type c1 value 'E',
         ld_correct type c1.

  if sy-batch is initial.
    ld_message_type = 'I'.
  endif.
  call function 'K_POSTING_DATE_CHECK'
       exporting
            id_kokrs        = rkauf-kokrs
            id_gjahr        = rkauf-gjahr
            id_period_from  = rkauf-from
            id_period_to    = rkauf-from
            id_budat        = rkauf-budat
            id_message_type = ld_message_type
       importing
            ed_check        = ld_correct
       exceptions
            no_budat        = 0.

* error when not in popup -> goto popup (otherwise deadlock)
* but not on selection screen of report
  if ( ld_correct = off ) and ( sy-repid = 'SAPLKAZB' ).
    call function 'K_POSTING_DATE_GET'
         exporting
              id_kokrs       = rkauf-kokrs
              id_gjahr       = rkauf-gjahr
              id_period_from = rkauf-from
              id_period_to   = rkauf-from
              id_budat       = rkauf-budat
         importing
              ed_budat       = rkauf-budat.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  auth_tcode
*&---------------------------------------------------------------------*
* check authorization for navigating to transaction
*----------------------------------------------------------------------*
*   --> I_TCODE   transaction code
*   <-- stop navigation by error msg if authorization is insufficient
*----------------------------------------------------------------------*
FORM auth_tcode  USING    value(i_tcode).

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = i_tcode
    EXCEPTIONS
      ok     = 1
      not_ok = 2
      OTHERS = 3.

  IF sy-subrc <> 1.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " auth_tcode

