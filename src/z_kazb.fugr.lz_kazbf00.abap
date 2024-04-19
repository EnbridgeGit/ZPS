* ---------------------------------------------------------------------
* 2009/01/13 mdemeest TR580 - All changes identified with UGL
*                           - Upgrade 4.7
*----------------------------------------------------------------------*
*   INCLUDE LKAZBF00                                                   *
*----------------------------------------------------------------------*
* changing history
* ----------------
* SCT ALRK136242 151098 accrual calculation connected
* ----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  INIT_TCODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form init_tcode
          using
             p_tcode like sy-tcode.


*ENHANCEMENT-SECTION     INIT_TCODE_01 SPOTS ES_LKAZBF00 INCLUDE BOUND.
  case p_tcode.
************************************************************************
* SURCHARGE CALCULATION
************************************************************************
*----------------------------------------------------------------------*
*   PROJECT - single - actual
*----------------------------------------------------------------------*
    when 'CJ44'.
      gd-processor  = 1.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-pr.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBP'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '003'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*   PROJECT - group - actual
*----------------------------------------------------------------------*
    when 'CJ45'.
      gd-processor  = 1.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-pr.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBP'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
     if rkauf-report+8(4)   = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*   PROJECT - single - plan
*----------------------------------------------------------------------*
    when 'CJ46'.
      gd-processor  = 1.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-prs.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBP'.
      gd-titbar     = 'SUP'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kzpp.
      rkauf-wrttp   = wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.          "only one year
      clear gd-modify_parasc.
      gd-modify_procsc = '003'.
      gd-single_period = false.
      gd-single_gjahr  = true.                       "<<<note324479
*----------------------------------------------------------------------*
*   PROJECT - group - plan
*----------------------------------------------------------------------*
    when 'CJ47'.
      gd-processor  = 1.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-prs.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBP'.
      gd-titbar     = 'SUP'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kzpp.
      rkauf-wrttp   = wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.          "only one year
      clear gd-modify_parasc.
      gd-modify_procsc = '004'.
      gd-single_period = false.
      gd-single_gjahr  = true.                        "<<<note324479
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
     if rkauf-report+8(4)   = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*   PROJECT - single - obligo
*----------------------------------------------------------------------*
    when 'CJO8'.
      gd-processor  = 1.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-pr.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBP'.
      gd-titbar     = 'SUO'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_obli.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2300'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '003'.
      gd-single_period = false.
      gd-single_gjahr  = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
*----------------------------------------------------------------------*
*   PROJECT - group - obligo
*----------------------------------------------------------------------*
    when 'CJO9'.
      gd-processor  = 1.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-pr.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBP'.
      gd-titbar     = 'SUO'.
      rkauf-sign_ap = con_act.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_obli.
      rkauf-versn   = con_act_versn.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2300'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '004'.
      gd-single_period = false.
      gd-single_gjahr  = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.

*----------------------------------------------------------------------*
*   PROCESS COST COLLECTOR - single - actual
*----------------------------------------------------------------------*
    when 'CO42'
      or 'MFI2'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or2.
      gd-subart     = '05'.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '003'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*   PROCESS COST COLLECTOR - group - actual
*----------------------------------------------------------------------*
    when 'CO43'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or3.
      gd-subart     = '05'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
       if rkauf-report+8(4) = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*   INTERNAL ORDER - single - actual
*----------------------------------------------------------------------*
    when 'KGI2'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBA'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '001/003/004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*   INTERNAL ORDER - selection variant - actual
*----------------------------------------------------------------------*
    when 'KGI4'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBA'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
      if rkauf-report+8(4)  = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*   INTERNAL ORDER - single - plan
*----------------------------------------------------------------------*
    when 'KGP2'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUP'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kzpp.
      rkauf-wrttp   = wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '004'.
      gd-modify_procsc = '001/003/004'.
      gd-single_period = false.
      gd-single_gjahr  = true.
*----------------------------------------------------------------------*
*   INTERNAL ORDER - selection variant - plan
*----------------------------------------------------------------------*
    when 'KGP4'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBA'.
      gd-titbar     = 'SUP'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kzpp.
      rkauf-wrttp   = wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '004'.
      gd-modify_procsc = '004'.
      gd-single_period = false.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
      if rkauf-report+8(4)  = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*   INTERNAL ORDER - single - obligo
*----------------------------------------------------------------------*
    when 'KGO2'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBA'.
      gd-titbar     = 'SUO'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_obli.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2300'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '001/003/004'.
      gd-single_period = false.
      gd-single_gjahr  = false.
*----------------------------------------------------------------------*
*   INTERNAL ORDER - group - obligo
*----------------------------------------------------------------------*
    when 'KGO4'.
      gd-processor  = 1.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBA'.
      gd-titbar     = 'SUO'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_obli.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2300'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '004'.
      gd-single_period = false.
      gd-single_gjahr  = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
*----------------------------------------------------------------------*
*   COST OBJECT - single - actual
*----------------------------------------------------------------------*
    when 'KKPZ'.
      gd-processor  = 1.
      gd-obart      = objektart_hp.
      gd-selart     = con_selart-hp.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      rkauf-versn   = con_act_versn.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '003'.
      gd-single_period = true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*   COST OBJECT - group - actual
*----------------------------------------------------------------------*
    when 'KKPJ'.
      gd-processor  = 1.
      gd-obart      = objektart_hp.
      gd-selart     = con_selart-hp.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
     if rkauf-report+8(4)   = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
    endif.
*----------------------------------------------------------------------*
*   SALES ORDER (customer order)
*----------------------------------------------------------------------*
    when 'VA44'.
      gd-processor  = 1.
      gd-obart      = objektart_vb.
      gd-selart     = con_selart-vb.
      gd-subart     = '01'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
      if rkauf-report+8(4)  = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*   COST CENTERS - group - actual
*----------------------------------------------------------------------*
    when 'KSI4'.
      gd-processor  = 1.
      gd-obart      = objektart_ks.
      gd-selart     = con_selart-ksl.
      clear gd-oneobject.
      gd-allobjects = true.            "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
       GD-REPID_BTC+4(4)  = P_TCODE.                         "RR
*----------------------------------------------------------------------*
*   COST CENTERS - group - plan
*----------------------------------------------------------------------*
    when 'KSP4'.
      gd-processor  = 1.
      gd-obart      = objektart_ks.
      gd-selart     = con_selart-ksl.
      gd-subart     = '01'.
      clear gd-oneobject.
      gd-allobjects = true.            "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUP'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kzpp.
      rkauf-wrttp   = wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '004'.
      gd-modify_procsc = '004'.
      gd-single_period = false.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
       GD-REPID_BTC+4(4)  = P_TCODE.                           "RR
*----------------------------------------------------------------------*
*   COST CENTERS - group - obligo
*----------------------------------------------------------------------*
    when 'KSO9'.
      gd-processor  = 1.
      gd-obart      = objektart_ks.
      gd-selart     = con_selart-ksl.
      gd-subart     = '01'.
      clear gd-oneobject.
      gd-allobjects = true.            "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUO'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_obli.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2300'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '004'.
      gd-single_period = false.
      gd-single_gjahr  = false.
* batch parameters
      GD-REPID_BTC+4(4)  = P_TCODE.                             "RR
*----------------------------------------------------------------------*
*    business process - group - actual
*----------------------------------------------------------------------*
    when 'CPZI'.
      gd-processor  = 1.
      gd-obart      = objektart_bp.
      gd-selart     = con_selart-bp.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kzpi.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
       GD-REPID_BTC+4(4)  = P_TCODE.                             "RR
*----------------------------------------------------------------------*
*    business process - group - plan
*----------------------------------------------------------------------*
    when 'CPZP'.
      gd-processor  = 1.
      gd-obart      = objektart_bp.
      gd-selart     = con_selart-bp.
      gd-subart     = '01'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SUBS'.
      gd-titbar     = 'SUP'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kzpp.
      rkauf-wrttp   = wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '004'.
      gd-modify_procsc = '004'.
      gd-single_period = false.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
* batch parameters
       GD-REPID_BTC+4(4)  = P_TCODE.                              "RR
************************************************************************
* REVALUATION
************************************************************************
*----------------------------------------------------------------------*
*     internal order - single
*----------------------------------------------------------------------*
    when 'KON1'.
      gd-processor  =  3.
      gd-obart      =  objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      gd-oneobject  =  true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIA'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '001/002/003/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*     internal order - group
*----------------------------------------------------------------------*
    when 'KON2'.
      gd-processor  =  3.
      gd-obart      =  objektart_or.
      gd-selart     = con_selart-or1.
      gd-subart     = '01'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIA'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
       if rkauf-report+8(4) = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*     production order - single
*----------------------------------------------------------------------*
    when 'CON1'.
      gd-processor  = 3.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or2.
      gd-subart     = '05'.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIS'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/003'.
      gd-single_period = true.
      gd-single_gjahr  = true.
*----------------------------------------------------------------------*
*     production order - group
*----------------------------------------------------------------------*
    when 'CON2'.
      gd-processor  = 3.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or2.
      gd-subart     = '05'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIS'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
       if rkauf-report+8(4) = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*     product cost collector - single
*----------------------------------------------------------------------*
    when 'MFN1'.
      gd-processor  = 3.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or3.
      gd-subart     = '10'.            " 40, 4
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIS'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/003'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*     product cost collector - group
*----------------------------------------------------------------------*
    when 'MFN2'.
      gd-processor  = 3.
      gd-obart      = objektart_or.
      gd-selart     = con_selart-or3.
      gd-subart     = '10'.            " 40, 4
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIS'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
*----------------------------------------------------------------------*
*     project - single
*----------------------------------------------------------------------*
    when 'CJN1'.
      gd-processor  = 3.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-pr.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIP'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/003'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*     project - group
*----------------------------------------------------------------------*
    when 'CJN2'.
      gd-processor  =  3.
      gd-obart      =  objektart_pr.
      gd-selart     = con_selart-pr.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIP'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '001/002/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
       if rkauf-report+8(4) = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
*----------------------------------------------------------------------*
*     cost object - single
*----------------------------------------------------------------------*
    when 'KKN1'.
      gd-processor  = 3.
      gd-obart      = objektart_hp.
      gd-selart     = con_selart-hp.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIS'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/003'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
*----------------------------------------------------------------------*
*     cost object - group
*----------------------------------------------------------------------*
    when 'KKN2'.
      gd-processor  =  3.
      gd-obart      =  objektart_hp.
      gd-selart     = con_selart-hp.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIS'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
     if rkauf-report+8(4)   = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
    endif.
*----------------------------------------------------------------------*
*   SALES ORDER (customer order)
*----------------------------------------------------------------------*
    when 'VAN1'.
      gd-processor  = 3.
      gd-obart      = objektart_vb.
      gd-selart     = con_selart-vb.
*     GD-SUBART     = '01'.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'NBIS'.
      gd-titbar     = 'NBI'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_rkln.
      rkauf-wrttp   = wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
* worklists
       if rkauf-report+8(4) = 'COWL'.
          gd-repid_btc+8(4) = 'COWL'.
          gd-wl_active      =  true.            "worklists active
          gd-selart         =  con_selart-wl.
     endif.
************************************************************************
* INTEREST CALCUALATION
************************************************************************
*----------------------------------------------------------------------*
*     interest calculation single projects - actual
*----------------------------------------------------------------------*
*    when 'CJZ2'.                                 "UGL
     when 'ZINT'.                                 "UGL
      gd-processor  =  4.
      gd-obart      =  objektart_pr.
      gd-selart     =  con_selart-pri.
      gd-oneobject  =  true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPP'.
      gd-titbar     = 'III'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   =  con_act_versn.
      rkauf-vrgng   =  vrgng_kzri.
      rkauf-wrttp   =  wrttp_act.
*      gd-repid_sub1 = 'SAPLKAZB'.                "UGL
      gd-repid_sub1 = 'SAPLZ_KAZB'.               "UGL

      gd-dynnr_sub1 = '2400'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '002/003/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      =  false.
      gd-int_mode      =  con_act_int_projects_single.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
*----------------------------------------------------------------------*
*     interest calculation single projects - plan
*----------------------------------------------------------------------*
    when 'CJZ3'.
      gd-processor  =  4.
      gd-obart      =  objektart_pr.
      gd-selart     =  con_selart-prp.
      gd-oneobject  =  true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPP'.
      gd-titbar     = 'IIP'.
      rkauf-sign_ap =  con_plan.
      rkauf-vrgng   =  vrgng_kzrp.
      rkauf-wrttp   =  wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2200'.
      clear gd-modify_parasc.
      gd-modify_procsc = '002/003/004'.
      gd-single_period =  false.
      gd-single_gjahr  =  false.
      gd-int_mode      =  con_plan_int_projects_single.
      gd-protocol      =  false.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
*----------------------------------------------------------------------*
*     interest calculation multiple projects - actual
*----------------------------------------------------------------------*
    when 'CJZ1'.
      gd-processor  =  4.
      gd-obart      =  objektart_pr.
      gd-selart     =  con_selart-pri.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPP'.
      gd-titbar     = 'III'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   =  con_act_versn.
      rkauf-vrgng   =  vrgng_kzri.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2400'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '002/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
      if rkauf-report+8(4) = 'COWL'.
        gd-repid_btc+8(4)  = 'COWL'.
        gd-selart          = con_selart-wl.
        gd-wl_active       = true.             "worklists active
      endif.
      gd-int_mode = con_act_int_projects_multi.
*----------------------------------------------------------------------*
*     interest calculation multiple projects - plan
*----------------------------------------------------------------------*
    when 'CJZ5'.
      gd-processor  =  4.
      gd-obart      =  objektart_pr.
      gd-selart     =  con_selart-prp.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPP'.
      gd-titbar     = 'IIP'.
      rkauf-sign_ap =  con_plan.
      rkauf-vrgng   =  vrgng_kzrp.
      rkauf-wrttp   =  wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2200'.
      clear gd-modify_parasc.
      gd-modify_procsc = '002/004'.
      gd-single_period =  false.
      gd-single_gjahr  =  false.
      gd-int_mode  = con_plan_int_projects_multi.
      gd-protocol  = false.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
      if rkauf-report+8(4) = 'COWL'.
        gd-repid_btc+8(4)  = 'COWL'.
        gd-selart          = con_selart-wl.
        gd-wl_active       = true.             "worklists active
      endif.
*----------------------------------------------------------------------*
*     interest calculation single orders - actual
*----------------------------------------------------------------------*
    when 'CJZ6'.
      gd-processor  =  4.
      gd-obart      =  objektart_or.
      gd-selart     =  con_selart-or1.
      gd-oneobject  =  true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPO'.
      gd-titbar     = 'III'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   =  con_act_versn.
      rkauf-vrgng   =  vrgng_kzri.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2400'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '002/003/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = false.
      gd-int_mode      = con_act_int_orders_single.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
* batch parameters
*     gd-repid_btc+4(4)  = p_tcode.   "note 160241
*----------------------------------------------------------------------*
*     interest calculation single orders - plan
*----------------------------------------------------------------------*
    when 'CJZ7'.
      gd-processor  =  4.
      gd-obart      =  objektart_or.
      gd-selart     =  con_selart-or1.
      gd-oneobject  =  true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPO'.
      gd-titbar     = 'IIP'.
      rkauf-sign_ap =  con_plan.
      rkauf-vrgng   =  vrgng_kzrp.
      rkauf-wrttp   =  wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2200'.
      clear gd-modify_parasc.
      gd-modify_procsc = '001/002/003/004'.
      gd-single_period =  false.
      gd-single_gjahr  =  false.
      gd-int_mode      =  con_plan_int_orders_single.
      gd-protocol  = false.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
*----------------------------------------------------------------------*
*     interest calculation multiple orders - actual
*----------------------------------------------------------------------*
    when 'CJZ8'.
      gd-processor  =  4.
      gd-obart      =  objektart_or.
      gd-selart     =  con_selart-or1.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPO'.
      gd-titbar     = 'III'.
      rkauf-sign_ap =  con_act.
      rkauf-versn   =  con_act_versn.
      rkauf-vrgng   =  vrgng_kzri.
      rkauf-wrttp   =  wrttp_act.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2400'.
      gd-modify_parasc = '002'.
      gd-modify_procsc = '002/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
      if rkauf-report+8(4) = 'COWL'.
        gd-repid_btc+8(4)  = 'COWL'.
        gd-selart          = con_selart-wl.
        gd-wl_active       = true.             "worklists active
      endif.
      gd-int_mode      = con_act_int_orders_multi.
*----------------------------------------------------------------------*
*     interest calculation multiple orders - plan
*----------------------------------------------------------------------*
    when 'CJZ9'.
      gd-processor  =  4.
      gd-obart      =  objektart_or.
      gd-selart     =  con_selart-or1.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'IIPO'.
      gd-titbar     = 'IIP'.
      rkauf-sign_ap =  con_plan.
      rkauf-vrgng   =  vrgng_kzrp.
      rkauf-wrttp   =  wrttp_plan.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2200'.
      clear gd-modify_parasc.
      gd-modify_procsc = '002/003/004'.
      gd-single_period =  false.
      gd-single_gjahr  =  false.
      gd-int_mode    =    con_plan_int_orders_multi.
      gd-protocol  = false.
      gd-report_alv    = 'SAPLINLV'.
      gd-tabname       = 'GT_LIST_DATA'.
*----------------------------------------------------------------------*
*     project progress (individual)
*----------------------------------------------------------------------*
    when 'CNE1'.
      gd-processor  =  5.
      gd-obart      =  objektart_pr.
      gd-selart     =  con_selart-pre.
      gd-oneobject  =  true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'EV01'.
      gd-titbar     = 'EV1'.
      rkauf-sign_ap =  con_plan.
      rkauf-vrgng   =  vrgng_pev1.
      rkauf-wrttp   =  wrttp_pev1.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2450'.
      clear gd-modify_parasc.
      gd-modify_procsc = '001/002/003/004'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = false.
*----------------------------------------------------------------------*
*     project progress (collective)
*----------------------------------------------------------------------*
    when 'CNE2'.
      gd-processor  =  5.
      gd-obart      =  objektart_pr.
      gd-selart     =  con_selart-pre.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'EV01'.
      gd-titbar     = 'EV1'.
      rkauf-sign_ap =  con_plan.
      rkauf-vrgng   =  vrgng_pev1.
      rkauf-wrttp   =  wrttp_pev1.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2450'.
      gd-modify_procsc = '001/002/004/FKT'.
      gd-single_period =  true.
      gd-single_gjahr  =  true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-repid_btc+4(4) = p_tcode.
      if rkauf-report+8(4) = 'COWL'.
        gd-repid_btc+8(4)  = 'COWL'.
        gd-selart          = con_selart-wl.
        gd-wl_active       = true.             "worklists active
      endif.
*----------------------------------------------------------------------*
*     cost forecast on projects (individual)
*----------------------------------------------------------------------*
    when 'CJ9L'.
      gd-processor  = 6.
      gd-obart      = objektart_np.
      gd-selart     = con_selart-prc.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'CTC2'.
      gd-titbar     = 'CTC'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kctc.
      rkauf-wrttp   = wrttp_kctc.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2250'.
      clear gd-modify_parasc.
      gd-modify_procsc = '002/003/004'.
      gd-single_period = false.
      gd-single_gjahr  = false.
      gd-report_alv    = 'SAPLKALK'.
      gd-tabname       = 'T_LOG_HEADER'.
      gd-tabname_item  = 'T_LOG_ITEM'.
      gd-protocol      = false.
*----------------------------------------------------------------------*
*     cost forecast on projects (collective)
*----------------------------------------------------------------------*
    when 'CJ9M'.
      gd-processor  = 6.
      gd-obart      = objektart_np.
      gd-selart     = con_selart-prc.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'CTC1'.
      gd-titbar     = 'CTC'.
      rkauf-sign_ap = con_plan.
      rkauf-vrgng   = vrgng_kctc.
      rkauf-wrttp   = wrttp_kctc.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2250'.
      gd-modify_parasc = '001/003'.
      gd-modify_procsc = '002/FKT'.
      gd-single_period = false.
      gd-single_gjahr  = false.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
      gd-report_alv    = 'SAPLKALK'.
      gd-tabname       = 'T_LOG_HEADER'.
      gd-tabname_item  = 'T_LOG_ITEM'.
*     set start date for cost distribution and posting date for
*     period end partner
      call function 'KAOP_SET_TODATE'
           changing
                c_rkauf = rkauf.
* batch parameters
      gd-repid_btc+4(4) = p_tcode.
* worklist
      if rkauf-report+8(4) = 'COWL'.
        gd-repid_btc+8(4)  = 'COWL'.
        gd-wl_active       = true.                 "worklist active
        gd-selart          = con_selart-wl.
      endif.
*----------------------------------------------------------------------*
*     project oriented orders received (collective)
*----------------------------------------------------------------------*
    when 'CJA1'.
      gd-processor  = 7.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-prv.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'ORB1'.
      gd-titbar     = 'ORB'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kor1.
      rkauf-wrttp   = wrttp_orb.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003'.
      gd-modify_procsc = '002/004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = true.
      gd-report_alv    = 'SAPLORLV'.
      gd-tabname       = 'GT_LIST_DATA'.
      gd-repid_btc+4(4) = p_tcode.
      if rkauf-report+8(4) = 'COWL'.
        gd-repid_btc+8(4)  = 'COWL'.
        gd-selart          = con_selart-wl.
        gd-wl_active       = true.             "worklists active
      endif.
*----------------------------------------------------------------------*
*     project oriented orders received (single)
*----------------------------------------------------------------------*
    when 'CJA2'.
      gd-processor  = 7.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-prv.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'ORB1'.
      gd-titbar     = 'ORB'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_kor1.
      rkauf-wrttp   = wrttp_orb.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003'.
      gd-modify_procsc = '002/003'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = false.
      gd-report_alv    = 'SAPLORLV'.
      gd-tabname       = 'GT_LIST_DATA'.

*----------------------------------------------------------------------*
*   automatic generation of settlement rules (collective)
*----------------------------------------------------------------------*
    when 'CJB1'.
      gd-processor  = 8.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-prv.
      clear gd-oneobject.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SRG1'.
      gd-titbar     = 'AGS'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_srgn.
      rkauf-wrttp   = wrttp_orb.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003'.
      gd-modify_procsc = '002/004/FKT'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
      gd-report_alv    = 'SAPLKSLV'.
      gd-tabname       = 'GT_LIST_DATA'.
      gd-repid_btc+4(4) = p_tcode.
      if rkauf-report+8(4) = 'COWL'.
        gd-repid_btc+8(4)  = 'COWL'.
        gd-selart          = con_selart-wl.
        gd-wl_active       = true.             "worklists active
      endif.
*----------------------------------------------------------------------*
*  automatic generation of settlement rules (single)
*----------------------------------------------------------------------*
    when 'CJB2'.
      gd-processor  = 8.
      gd-obart      = objektart_pr.
      gd-selart     = con_selart-prv.
      gd-oneobject  = true.
      clear gd-allobjects.             "4.6
      gd-pfkey      = 'SRG1'.
      gd-titbar     = 'AGS'.
      rkauf-sign_ap = con_act.
      rkauf-versn   = con_act_versn.
      rkauf-vrgng   = vrgng_srgn.
      rkauf-wrttp   = wrttp_orb.
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.
      gd-modify_parasc = '002/003'.
      gd-modify_procsc = '002/003'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = false.
      gd-report_alv    = 'SAPLKSLV'.
      gd-tabname       = 'GT_LIST_DATA'.
************************************************************************
* ACCRUAL CALCULATION
************************************************************************
*----------------------------------------------------------------------*
*     accrual calculation actual (cost centers)
*----------------------------------------------------------------------*
    when 'KSA3'.  "accrual calc. actual                     "ALRK136242
      gd-processor     = 9.
      gd-obart         = objektart_ks.
      gd-selart        = con_selart-ksl.
      gd-oneobject     = false.
      gd-allobjects    = true.         "4.6
      gd-pfkey         = 'ACCRUAL_ACTUAL_CC'.
      gd-titbar        = 'ACCRUAL_ACTUAL_CC'.
      rkauf-sign_ap    = con_act.
      rkauf-versn      = con_act_versn.
      rkauf-vrgng      = vrgng_kazi.
      rkauf-wrttp      = wrttp_act.
      gd-repid_sub1    = 'SAPLKAZB'.
      gd-dynnr_sub1    = '2100'.
      gd-modify_parasc = '002/003/004'.
      gd-modify_procsc = '002/004'.
      gd-single_period = true.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-moni_save     = true.
      gd-delayed_update = false.
      gd-report_alv    = 'SAPMKSZB'.   "not used in KALV!
      gd-tabname       = 'GT_LIST_DATA'.    "not used in KALV!
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
*----------------------------------------------------------------------*
*     accrual calculation plan (cost centers)
*----------------------------------------------------------------------*
    when 'KSA8'.  "accrual calc. plan                       "ALRK136242
      gd-processor     = 9.
      gd-obart         = objektart_ks.
      gd-selart        = con_selart-ksl.
      gd-oneobject     = false.
      gd-allobjects    = true.         "4.6
      gd-pfkey         = 'ACCRUAL_PLAN_CC'.
      gd-titbar        = 'ACCRUAL_PLAN_CC'.
      rkauf-sign_ap    = con_plan.
*      RKAUF-VERSN      = CON_ACT_VERSN.   "entered on screen
      rkauf-vrgng      = vrgng_kazp.
      rkauf-wrttp      = wrttp_plan.
      gd-repid_sub1    = 'SAPLKAZB'.
      gd-dynnr_sub1    = '2100'.
      gd-modify_parasc = ''.
      gd-modify_procsc = '002/004'.
      gd-single_period = false.
      gd-single_gjahr  = true.
      gd-protocol      = true.
      gd-report_alv    = 'SAPMKSZB'.
      gd-tabname       = 'GT_LIST_DATA'.
* batch parameters
      gd-repid_btc+4(4)  = p_tcode.
*----------------------------------------------------------------------*
*   others
*----------------------------------------------------------------------*
    when others.
*     do not leave program, as action log supplies other TCODEs
      gd-repid_sub1 = 'SAPLKAZB'.
      gd-dynnr_sub1 = '2100'.

  endcase.
*END-ENHANCEMENT-SECTION.

endform.                               " INIT_TCODE
*&---------------------------------------------------------------------*
*&      Form  INIT_SAPLKAZB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RKAUF  text
*      -->P_GD  text
*----------------------------------------------------------------------*
form init_saplkazb using    p_rkauf  like rkauf
                            p_gd     like gd.

  rkauf = p_rkauf.
  gd = p_gd.

endform.                               " INIT_SAPLKAZB
