*-------------------------------------------------------------------
***INCLUDE lkazbF01.
*-------------------------------------------------------------------
* changing history
* ----------------
* 2009/01/13 mdemeest - TR580 - Upgrade - code identified with UGL
* 4.6A
* SCT ALRK136242 141098 accrual calculation connected
* AL0K056573: Note 436929 08.10.2001 FCL
* PL0K027549: Note 513792 19.04.2002 FCL
* P6BK092983: Note 640230 10.07.2003 FCL
* P6BK103855: Note 657796 04.09.2003 FCL
* P6BK113990: NOte 675070 26.10.2003 FCL
* ----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*       FORM  FCODE.
*&---------------------------------------------------------------------*
*       FCODE handling
*       start program execution in online and batch
*----------------------------------------------------------------------*
FORM  fcode.

  DATA:    ld_fcode      LIKE sy-ucomm.

  DATA:    lt_selpa      LIKE kaba00 OCCURS 1 WITH HEADER LINE.

*----------------------------------------------------------------------*
  ld_fcode = fcode.
  CLEAR fcode.

  CHECK NOT ld_fcode IS INITIAL.

*----------------------------------------------------------------------*
* FCODE handling for variants
*----------------------------------------------------------------------*
  CALL FUNCTION 'K_OBJECT_SELECTION_FCODE'
    EXPORTING
      fcode     = ld_fcode
    EXCEPTIONS
      not_found = 1.

  CHECK sy-subrc = 1.

*----------------------------------------------------------------------*
* FCODE handling for other functions
*----------------------------------------------------------------------*
* special FCODE handling
  PERFORM fcode_special USING ld_fcode.

*** check initial screen again
  CALL FUNCTION 'KAOP_CHECK_INITIAL_SCREEN'.
* interpret FCODE
  CASE ld_fcode.
*----------------------------------------------------------------------*
* execute
*----------------------------------------------------------------------*
    WHEN 'RUN '.
      CLEAR rkauf-stno.             "<<<note436929
*     activate message collector
*      PERFORM MESSAGES_INITIALIZE.
*     get selection criteria
      PERFORM get_selection_criteria TABLES lt_selpa.
*     fill selection information for list header
      IF  gd-processor = 4             "interest
       OR gd-processor = 7             "incoming orders
       OR gd-processor = 8.            "generate WBS settlement rules
        PERFORM fill_sel_text TABLES lt_selpa
                                     sel_text
                              USING  gd-obart.
      ENDIF.
*     simulation only ?
      PERFORM init_bookflg.

      IF rkauf-batch = true.
*       create batch request
        PERFORM batch TABLES lt_selpa.
      ELSE.
*       go
        PERFORM fcode_run USING sy-tcode.
      ENDIF.
*----------------------------------------------------------------------*
* reverse
*----------------------------------------------------------------------*
    WHEN 'STNO'.
      rkauf-stno  = on.
*     activate message collector
*      PERFORM MESSAGES_INITIALIZE.
*     get selection criteria
      PERFORM get_selection_criteria TABLES lt_selpa.
*     fill selection information for list header
      IF gd-processor = 4 OR gd-processor = 7 OR gd-processor = 8.
        PERFORM fill_sel_text TABLES lt_selpa
                                     sel_text
                              USING  gd-obart.
      ENDIF.
*     simulation only ?
      PERFORM init_bookflg.

      IF rkauf-batch = true.
*       create batch request
        PERFORM batch TABLES lt_selpa.
      ELSE.
*       go
        PERFORM fcode_run USING sy-tcode.
      ENDIF.
*----------------------------------------------------------------------*
* value date
*----------------------------------------------------------------------*
    WHEN 'WERT'.
      PERFORM curr_popup.
*----------------------------------------------------------------------*
* posting date
*----------------------------------------------------------------------*
    WHEN 'POSTDATE'.
      CALL FUNCTION 'K_POSTING_DATE_GET'
        EXPORTING
          id_kokrs       = rkauf-kokrs
          id_gjahr       = rkauf-gjahr
          id_period_from = rkauf-from
          id_period_to   = rkauf-to
          id_budat       = rkauf-budat
        IMPORTING
          ed_budat       = rkauf-budat
        EXCEPTIONS
          no_budat       = 1.
      CASE sy-subrc.
        WHEN 0.
*                  gd_budat verarbeiten
        WHEN 1.
*                  kein Buchungsdatum eingegeben.
      ENDCASE.

*----------------------------------------------------------------------*
* to-date for interest calculation
*----------------------------------------------------------------------*
    WHEN 'BSDT'.
      PERFORM interest_todate.
*----------------------------------------------------------------------*
* general checks for interest calculation
*----------------------------------------------------------------------*
    WHEN 'ERPR'.
      PERFORM interest_general_checks.
*----------------------------------------------------------------------*
* date for project progress
*----------------------------------------------------------------------*
    WHEN 'EVDT'.
      CALL FUNCTION 'CNEV_01_POPUP_GET_DATE'
        EXPORTING
          i_kokrs = rkauf-kokrs
        CHANGING
          c_perio = rkauf-to
          c_gjahr = rkauf-gjahr
          c_datum = rkauf-todate
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.
      IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      rkauf-from = rkauf-to.

*----------------------------------------------------------------------*
* date for cost forecast on projects
*----------------------------------------------------------------------*
    WHEN 'ABDT'.
      PERFORM prc_todate.

*----------------------------------------------------------------------*
* accrual calculation: show settings
*----------------------------------------------------------------------*
    WHEN 'TAM'.   "target=actual method                     "ALRK136242
      PERFORM auth_tcode USING 'KSAJ'.                      "Hw839597
      CALL TRANSACTION 'KSAJ'.
      LEAVE TO SCREEN '1000'.

    WHEN 'SCM'.   "surcharge_method                         "ALRK136242
      PERFORM auth_tcode USING 'KSAP'.                      "Hw839597
      CALL TRANSACTION 'KSAP'.
      LEAVE TO SCREEN '1000'.

    WHEN 'CSD'.   "show costing sheet (only in plan)        "ALRK136242
      PERFORM auth_tcode USING 'KSAI'.                      "Hw839597
      CALL TRANSACTION 'KSAI'.
      LEAVE TO SCREEN '1000'.

*----------------------------------------------------------------------*
* variant for ALV
*----------------------------------------------------------------------*
    WHEN 'AVAR'.
      IF rkauf-vrgng = vrgng_kctc.
* für hierarchisch sequentielle ALV Liste
        PERFORM ctc_get_alv_variant.
      ELSE.
        PERFORM or_get_alv_variant.
      ENDIF.
  ENDCASE.

ENDFORM.                               " FCODE

*&---------------------------------------------------------------------*
*       FORM  ECODE.
*&---------------------------------------------------------------------*
*       handle FCODE with type 'E'
*----------------------------------------------------------------------*
FORM  ecode.

  DATA: ld_ecode LIKE sy-ucomm,
        ld_subobject LIKE balhdri-subobject.

*  DATA: NO_OF_PROTOCOLS TYPE I.
*----------------------------------------------------------------------*
  ld_ecode = fcode.
  CLEAR fcode.

* handle selection variants
  CALL FUNCTION 'K_OBJECT_SELECTION_ECODE'
    EXPORTING
      ecode     = ld_ecode
    EXCEPTIONS
      not_found = 1.
  CHECK sy-subrc = 1.

  CASE ld_ecode.
*----------------------------------------------------------------------*
* back
*----------------------------------------------------------------------*
    WHEN 'EF03'
      OR 'F03'
*----------------------------------------------------------------------*
* end
*----------------------------------------------------------------------*
      OR 'EF15'
      OR 'F15'
*----------------------------------------------------------------------*
* abort
*----------------------------------------------------------------------*
      OR 'EF12'
      OR 'F12'.
      SET SCREEN 0.
      LEAVE SCREEN.
*----------------------------------------------------------------------*
*   show messages
*----------------------------------------------------------------------*
    WHEN 'PROT'.
      PERFORM messages_show.
      LEAVE TO SCREEN 1000.
*----------------------------------------------------------------------*
* set controlling area
*----------------------------------------------------------------------*
    WHEN 'OKKS'.
      CALL FUNCTION 'K_KOKRS_SET'
        EXPORTING
          popup   = '1'
        IMPORTING
          e_kokrs = rkauf-kokrs.

*----------------------------------------------------------------------*
* set user paramerters
*----------------------------------------------------------------------*
    WHEN 'PAR '.
      PERFORM  set_user_parameters.
*----------------------------------------------------------------------*
* change interest profile
*----------------------------------------------------------------------*
    WHEN 'ZSM1'.
      CALL TRANSACTION 'OPIA'.
*----------------------------------------------------------------------*
* details interest profile
*----------------------------------------------------------------------*
    WHEN 'ZSM2'.
      CALL TRANSACTION 'OPIB'.
*----------------------------------------------------------------------*
* interest relevant value categories
*----------------------------------------------------------------------*
    WHEN 'ZSM3'.
      CALL TRANSACTION 'OPIC'.
*----------------------------------------------------------------------*
* general conditions interest indicator
*----------------------------------------------------------------------*
    WHEN 'ZKZ1'.
      CALL TRANSACTION 'OPIH'.
*----------------------------------------------------------------------*
* define reference interest rates
*----------------------------------------------------------------------*
    WHEN 'ZKZ2'.
      CALL TRANSACTION 'OBAC'.
*----------------------------------------------------------------------*
* enter values for reference interest rates
*----------------------------------------------------------------------*
    WHEN 'ZKZ3'.
      CALL TRANSACTION 'OB83'.
*----------------------------------------------------------------------*
* change interest rates
*----------------------------------------------------------------------*
    WHEN 'ZKZ4'.
      CALL TRANSACTION 'OT03'.
*----------------------------------------------------------------------*
* accrual calculation: show assignments Contr. area <-> costing sheet
*----------------------------------------------------------------------*
    WHEN 'ASCA'.                                            "ALRK136242
      PERFORM assignments_ca_cs
              USING tka01-kokrs
                    tka01-bezei
                    rkauf-sign_ap.
*----------------------------------------------------------------------*
* calculation scheme
*----------------------------------------------------------------------*
    WHEN 'ECUS'.
      CALL TRANSACTION 'KZA1'.
*----------------------------------------------------------------------*
* define variant
* Wenn die Variante neu definiert wurde wird die Transaktion neu
* gestartet.
*----------------------------------------------------------------------*
*    WHEN 'ECSV'.
*      IF GD-OBART EQ OBJEKTART_OR.
*        CALL TRANSACTION 'OKOV'.
*      ELSEIF GD-OBART EQ OBJEKTART_PR.
*        CALL TRANSACTION 'CJ8V'.
*      ENDIF.
*      LEAVE TO TRANSACTION SY-TCODE.
*----------------------------------------------------------------------*
* call action log
*----------------------------------------------------------------------*
    WHEN 'EALB'.
      PERFORM appl_log_display USING ld_subobject.          "ALR
*----------------------------------------------------------------------*
* delete action log
*----------------------------------------------------------------------*
    WHEN 'EALL'.
      PERFORM action_log_delete.
*----------------------------------------------------------------------*
  ENDCASE.

  LEAVE TO SCREEN 1000.

ENDFORM.                               " ECODE


*&---------------------------------------------------------------------*
*       FORM  BATCH.
*&---------------------------------------------------------------------*
*       prepare start of batch processing
*----------------------------------------------------------------------*
FORM  batch TABLES lt_selpa STRUCTURE kaba00.

  DATA: ld_pri_params LIKE pri_params,
        ld_valid      TYPE boolean.

*----------------------------------------------------------------------*
* Temporäre Änderung der Selektionskriterien im BATCH nicht erlaubt!!!
* --> WARNING !!!
* GD-SUB0_FLG Flag wird nur durch Neustart (auch über ECODE 'ECSV')
* gelöscht.
* Im BATCH wird immer die gesicherte Variante verwendet !!!
*----------------------------------------------------------------------*
* convert selection criteria to form short field names for report
  PERFORM convert_seltab IN PROGRAM saplkass
        TABLES lt_selpa
        USING  on
               on.
* append objects from action log to selection info
  IF NOT it_objnr IS INITIAL.
    lt_selpa-option = 'EQ'.
    lt_selpa-field  = 'SELTAB'.
    LOOP AT it_objnr.
      lt_selpa-low    = 'OBJNR'.
      lt_selpa-low+10 = it_objnr.
      APPEND lt_selpa.
    ENDLOOP.
  ENDIF.

* no dialog in batch run
  CLEAR rkauf-dialog.
* append global parameters from selection screen to selection info
*  if gd-repid_btc = 'RKAZUB70'.                             "RR
*     lt_selpa-option = 'EQ'.                                 "RR
*     lt_selpa-field  = 'PRKAUF1'.                            "RR
*     lt_selpa-low    = rkauf+0(190).                         "RR
*     append lt_selpa.                                        "RR
*     lt_selpa-field  = 'PRKAUF2'.                            "RR
*     lt_selpa-low    = rkauf+190.                            "RR
*     append lt_selpa.                                        "RR
* append KOKRS to selection info
*     lt_selpa-field  = 'KOKRS'.
*     lt_selpa-sign = 'I'.
*     lt_selpa-option = 'EQ'.
*     lt_selpa-low    = rkauf-kokrs.
*     lt_selpa-high   = rkauf-kokrs.                            "ALR
*     append lt_selpa.
*  endif.                                                    "RR
* append TCODE to selection info
  lt_selpa-field  = 'PTCODE'.
  lt_selpa-sign   = 'I'.                                    "ALR
  lt_selpa-option = 'EQ'.                                   "ALR
  lt_selpa-low    = sy-tcode.
  lt_selpa-high   = sy-tcode.                               "ALR
  APPEND lt_selpa.
* append VERSN to selection info                           "note127098
  IF NOT gd-repid_btc = 'RKAZCNE2'.
    lt_selpa-field  = 'P_VERSN'.                            "note127098
  ELSE.
    lt_selpa-field  = 'VERSN_EV'.
  ENDIF.
  lt_selpa-sign = 'I'.                                      "note127098
  lt_selpa-option = 'EQ'.                                   "note127098
  lt_selpa-low    = rkauf-versn.                            "note127098
  lt_selpa-high   = rkauf-versn.                            "note127098
  APPEND lt_selpa.                                          "note127098
* append P_GJAHR to selection info                        "ALRK147952
  CLEAR lt_selpa.                                           "ALRK147952
  lt_selpa-field = 'P_GJAHR'.                               "ALRK147952
  lt_selpa-sign = 'I'.                                      "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  lt_selpa-low = rkauf-gjahr.                               "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append P_GJAHRB to selection info
  CLEAR lt_selpa.
  lt_selpa-field = 'P_GJAHRB'.
  lt_selpa-sign = 'I'.
  lt_selpa-option = 'EQ'.
  lt_selpa-low = rkauf-gjahr_bis.
  APPEND lt_selpa.
* append P_FROM to selection info                         "ALRK147952
  CLEAR lt_selpa.                                           "ALRK147952
  lt_selpa-field = 'P_FROM'.                                "ALRK147952
  lt_selpa-sign = 'I'.                                      "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  lt_selpa-low = rkauf-from.                                "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append P_TO to selection info                           "ALRK147952
  CLEAR lt_selpa.                                           "ALRK147952
  lt_selpa-field = 'P_TO'.                                  "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  lt_selpa-low = rkauf-to.                                  "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append P_TODATE to selection info
  CLEAR lt_selpa.
  lt_selpa-field = 'P_TODATE'.
  lt_selpa-option = 'EQ'.
  lt_selpa-low = rkauf-todate.
  APPEND lt_selpa.
* append P_BUDAT to selection info
  IF NOT ( rkauf-budat IS INITIAL ).
    CLEAR lt_selpa.
    lt_selpa-field = 'P_BUDAT'.
    lt_selpa-sign = 'I'.
    lt_selpa-option = 'EQ'.
    lt_selpa-low = rkauf-budat.
    APPEND lt_selpa.
  ENDIF.
* append P_WSDAT to selection info
  IF NOT ( rkauf-wsdat IS INITIAL ).
    CLEAR lt_selpa.
    lt_selpa-field = 'P_WSDAT'.
    lt_selpa-sign = 'I'.
    lt_selpa-option = 'EQ'.
    lt_selpa-low = rkauf-wsdat.
    APPEND lt_selpa.
  ENDIF.
* append P_TEST to selection info                         "ALRK147952
  CLEAR lt_selpa.                                           "ALRK147952
  lt_selpa-field = 'P_TEST'.                                "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  lt_selpa-low = rkauf-test.                                "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append P_LIST to selection info                         "ALRK147952
  CLEAR lt_selpa.                                           "ALRK147952
  lt_selpa-field = 'P_LIST'.                                "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  lt_selpa-low = rkauf-list.                                "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append P_ACTLOG to selection info                       "ALRK147952
  CLEAR lt_selpa.                                           "ALRK147952
  lt_selpa-field = 'P_ACTLOG'.                              "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  lt_selpa-low = rkauf-act_log.                             "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append AUSF to selection info                           "ALRK147952
  CLEAR lt_selpa.                                           "ALRK147952
  lt_selpa-field = 'AUSF'.                                  "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  IF rkauf-stno = ' '.                                      "ALRK147952
    lt_selpa-low = 'X'.                                     "ALRK147952
  ELSE.                                                     "ALRK147952
    lt_selpa-low = ' '.                                     "ALRK147952
  ENDIF.                                                    "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append STOR to selection info                           "ALRK147952
  lt_selpa-field = 'STOR'.                                  "ALRK147952
  lt_selpa-option = 'EQ'.                                   "ALRK147952
  lt_selpa-low =  rkauf-stno.                               "ALRK147952
  APPEND lt_selpa.                                          "ALRK147952
* append ALV variant to selection info                     "ALRK240552
  CLEAR lt_selpa.                                           "ALRK240552
  lt_selpa-field  = 'P_ALV'.                                "ALRK240552
  lt_selpa-option = 'EQ'.                                   "ALRK240552
  lt_selpa-low    = rkauf-variant.                          "ALRK240552
  APPEND lt_selpa.                                          "ALRK240552
* append indicator 'adv. checks' to selection info         "ALRK240552
  CLEAR lt_selpa.                                           "ALRK240552
  lt_selpa-field  = 'P_ACHECK'.                             "ALRK240552
  lt_selpa-option = 'EQ'.                                   "ALRK240552
  lt_selpa-low    = rkauf-acheck.                           "ALRK240552
  APPEND lt_selpa.                                          "ALRK240552

*----------------------------------------------------------------------*
* get print parameters
*----------------------------------------------------------------------*
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      line_count     = 65
      line_size      = 132
      layout         = 'X_65_132'
      no_dialog      = true
    IMPORTING
      out_parameters = ld_pri_params
      valid          = ld_valid.

  IF ld_valid IS INITIAL.
    CLEAR ld_pri_params.
  ENDIF.

*----------------------------------------------------------------------*
* create batch process
*----------------------------------------------------------------------*

  CALL FUNCTION 'K_BATCH_REQUEST'
       EXPORTING
            par_dialg = 'X'
            par_pname = 'RKAZUB7D'
            par_rname = gd-repid_btc
            par_pripa = ld_pri_params
            par_print = 'X'
            par_sdmsg = 'X'
*           PAR_STDAY = SY-DATUM
*           PAR_RFCGN = 'PRFC_GRP'
            par_rfcgn = 'RFCGROUP'
       TABLES
            tab_selpa = lt_selpa.

* restore original parameter choice for next selection screen
  rkauf-batch = true.

ENDFORM.                               " BATCH

*&---------------------------------------------------------------------*
*       FORM  BATCH_START.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM  batch_start TABLES seltab
                  USING  i_rkauf LIKE rkauf
                         i_tcode LIKE sy-tcode.

  DATA: lt_selpa      LIKE kaba00 OCCURS 1 WITH HEADER LINE,
        l_variant    LIKE rsvar-variant.

*----------------------------------------------------------------------*
* init structure GD
*----------------------------------------------------------------------*
  PERFORM init_gd.

  rkauf = i_rkauf.
  rkauf-batch = true.

*----------------------------------------------------------------------*
* get objects if an action log is being processed
*----------------------------------------------------------------------*
  PERFORM action_log_batch_start TABLES seltab.
*----------------------------------------------------------------------*
* activate message collector
*----------------------------------------------------------------------*
  CALL FUNCTION 'MESSAGES_INITIALIZE'  "<<<NOTE134290
      IMPORTING                        "<<<NOTE134290
            e_identification = gd-identification.        "<<<NOTE134290
  gd-mesg_line = 1.
  CALL FUNCTION 'MESSAGE_LINE_SET'
    EXPORTING
      zeile = gd-mesg_line.
  CLEAR vorher.
  CLEAR nachher.
*----------------------------------------------------------------------*
* set kokrs
*----------------------------------------------------------------------*
  CALL FUNCTION 'K_KOKRS_SET'
    EXPORTING
      i_kokrs       = rkauf-kokrs
      popup         = '0'
    EXCEPTIONS
      error_message = 1.
  IF sy-subrc = 1.
    PERFORM message_store USING gd-max_error.
  ENDIF.
*----------------------------------------------------------------------*
* initialization and basic checks
*----------------------------------------------------------------------*
  PERFORM: init_tcode USING i_tcode.

  IF rkauf-wrttp NE wrttp_obli.
    PERFORM: check_gjahr.
  ENDIF.

  PERFORM check_kokrs.

  IF rkauf-wrttp NE wrttp_obli.
    PERFORM check_versn.
  ENDIF.

  PERFORM check_period.

  IF rkauf-wrttp NE wrttp_obli.
    PERFORM check_period_blocking.
  ENDIF.
*----------------------------------------------------------------------*
* transform table of selection criteria
*----------------------------------------------------------------------*
  PERFORM fill_seltab_into_selpa TABLES seltab
                                        lt_selpa.
*----------------------------------------------------------------------*
* fill selection information for list header
*----------------------------------------------------------------------*
  IF gd-processor = 4 OR gd-processor = 7 OR gd-processor = 8.
    PERFORM fill_sel_text TABLES lt_selpa
                                 sel_text
                          USING  gd-obart.
  ENDIF.
*----------------------------------------------------------------------*
* read variant for orders
*----------------------------------------------------------------------*
  PERFORM get_variant TABLES seltab
                    USING l_variant.
*----------------------------------------------------------------------*
* fill select options for co objects
*----------------------------------------------------------------------*
  CALL FUNCTION 'K_OBJECT_SELECTION_CRIT_FILL'
    EXPORTING
      selart     = gd-selart
      oneobject  = gd-oneobject
      variant    = l_variant
    IMPORTING
      selart_txt = gd-obart_txt
    TABLES
      it_cosel2  = seltab.

  gd-obart_txt = gd-obart_txt(40).

*----------------------------------------------------------------------*
* parallel processing if server group was specified.
*----------------------------------------------------------------------*
  IF NOT rkauf-rfc_grp IS INITIAL.
    rkauf-para  = true.
*   RKAUF-DEBUG = TRUE.
  ENDIF.
*----------------------------------------------------------------------*
* run program
*----------------------------------------------------------------------*
  PERFORM fcode_run USING i_tcode.

*----------------------------------------------------------------------*
* list error messages on spool list
*----------------------------------------------------------------------*
*  PERFORM MESSAGE_LIST.

ENDFORM.                               "BATCH_START
*&---------------------------------------------------------------------*
*       FORM  CURR_POPUP.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM  curr_popup.

  DATA: l_wsdat LIKE rkauf-wsdat.

  IF rkauf-sign_ap EQ con_plan.
    l_wsdat = tka07-pldat.
  ELSE.
    l_wsdat = rkauf-wsdat.
  ENDIF.

  CALL FUNCTION 'K_BULK_PROCESSING_CURR_POPUP'
    EXPORTING
      trancur      = tka01-waers
      trancur_flag = '1'
      typecur      = rkauf-kurst
      typecur_flag = '1'
      valuta       = l_wsdat
      valuta_flag  = '2'
    IMPORTING
      trancur      = rkauf-waers
      typecur      = rkauf-kurst
      valuta       = rkauf-wsdat.

ENDFORM.                               " CURR_POPUP

*&---------------------------------------------------------------------*
*         FORM  FCODE_RUN.
*&---------------------------------------------------------------------*
*         starting main routine for online processing
*----------------------------------------------------------------------*
FORM  fcode_run USING p_tcode LIKE sy-tcode.

*----------------------------------------------------------------------*
* 0. open protocol
*----------------------------------------------------------------------*
  IF gd-moni_save = true.
    PERFORM monitor_init_record USING p_tcode.         "schedule manager
  ENDIF.
*----------------------------------------------------------------------*
* 1. activate action log
*----------------------------------------------------------------------*
  PERFORM action_log_activate.

*----------------------------------------------------------------------*
* 2. fill object table from selection
*----------------------------------------------------------------------*
  PERFORM object_selection.

*----------------------------------------------------------------------*
* 3. perform main programs
*    avaiable functionalities: - SURCHARGE CALCULATION
*                              - REVALUATION
*                              - INTEREST CALCULATION
*----------------------------------------------------------------------*

  CASE gd-processor.

*----------------------------------------------------------------------*
*     SURCHARGE CALCULATION
*----------------------------------------------------------------------*
    WHEN 1.
* check balance of projects in period (actual surcharge)
      PERFORM projects_check_balance_act_per.

      CALL FUNCTION 'K_SURCHARGE_CALL'
        EXPORTING
          i_rkauf     = rkauf
          i_gd        = gd
          i_stat      = stat
          i_tka01     = tka01
        IMPORTING
          e_stat      = stat
          e_gd        = gd
          e_rkauf     = rkauf
        TABLES
          i_objnr     = it_objnr
          i_objnr_log = it_objnr_log
          i_periods   = i_periods
          i_prot      = gt_prot
          e_coeja     = xcoeja
          e_coep      = xcoep.

*     store objects with errors in action log
      PERFORM action_log_append_objnr TABLES it_objnr_log.
*     store action log
      PERFORM action_log_store.
****NOTE 657796 Start
**     close protocol
*      IF GD-MONI_SAVE = TRUE.
*        PERFORM MONITOR_CLOSE_RECORD.  "schedule manager
*      ENDIF.
****NOTE 657796 End
*     show result if no dialog processing (--> own list for each object)
      IF NOT rkauf-dialog = true.      "run or trace
        PERFORM list_results TABLES xcoep
                                    xcoeja
                                    xcoaib
                            USING   rkauf
                                    gd
                                    tka01
                                    tka07
                                    tkt09
                                    stat.                   "RR
      ENDIF.
****NOTE 657796 Start
*     close protocol
      IF GD-MONI_SAVE = TRUE.
        PERFORM MONITOR_CLOSE_RECORD.  "schedule manager
      ENDIF.
****NOTE 657796 End


*----------------------------------------------------------------------*
*     REVALUATION
*----------------------------------------------------------------------*
    WHEN 3.
* delete production orders with update of output
      PERFORM prod_ord_la_revaluation_check.
      CALL FUNCTION 'K_REVALUATION_COSTS_CALL'
        EXPORTING
          i_rkauf     = rkauf
          i_gd        = gd
          i_stat      = stat
          i_ratmi     = tka07-ratmi  "<<<note212258
        IMPORTING
          e_stat      = stat
          e_gd        = gd
        TABLES
          i_objnr     = it_objnr
          i_periods   = i_periods
          i_objnr_log = it_objnr_log
          i_prot      = gt_prot
          e_coeja     = xcoeja
          e_coaib     = xcoaib.

*     store objects with errors in action log
      PERFORM action_log_append_objnr TABLES it_objnr_log.
*     store action log
      PERFORM action_log_store.
****NOTE 657796 Start
**     close protocol
*      IF GD-MONI_SAVE = TRUE.
*        PERFORM MONITOR_CLOSE_RECORD.  "schedule manager
*      ENDIF.
****NOTE 657796 End
*     show result
      PERFORM list_results TABLES xcoep
                                  xcoeja
                                  xcoaib
                          USING   rkauf
                                  gd
                                  tka01
                                  tka07
                                  tkt09
                                  stat.                     "RR
****NOTE 657796 Start
*     close protocol
      IF GD-MONI_SAVE = TRUE.
        PERFORM MONITOR_CLOSE_RECORD.  "schedule manager
      ENDIF.
****NOTE 657796 End


*----------------------------------------------------------------------*
*     INTEREST CALCULATION
*----------------------------------------------------------------------*
    WHEN 4.
      PERFORM main_interest.
*----------------------------------------------------------------------*
*     PROJECT PROGRESS
*----------------------------------------------------------------------*
    WHEN 5.
*---data definitions
      DATA: lt_objnr LIKE jsto_pre OCCURS 0.
      lt_objnr[] = it_objnr[].
*---check dates (deletion of actual values) - only real run
      IF rkauf-test IS INITIAL.
        PERFORM check_dates.
      ENDIF.
*---further processing
      CALL FUNCTION 'CNEV_01_MAIN'
        EXPORTING
          i_rkauf = rkauf
          t_objnr = lt_objnr
        CHANGING
          c_gd    = gd.

*----------------------------------------------------------------------*
*     COST FORECAST ON PROJECTS
*----------------------------------------------------------------------*
    WHEN 6.
      IF rkauf-todate IS INITIAL.
        PERFORM prc_set_todate.
      ENDIF.
      CALL FUNCTION 'KCTC_FORECAST_MAIN'
        EXPORTING
          i_rkauf   = rkauf
          i_stat    = stat
        TABLES
          e_t_objnr = it_objnr
        CHANGING
          c_gd      = gd.
*----------------------------------------------------------------------*
*     Project Related Orders Received / Orders Backlog
*----------------------------------------------------------------------*
    WHEN 7.
      PERFORM main_or.
*----------------------------------------------------------------------*
*     Automatic Generation of Settlement Rules
*----------------------------------------------------------------------*
    WHEN 8.
      PERFORM main_srule.
*----------------------------------------------------------------------*
*     Cost center accrual calculation
*----------------------------------------------------------------------*
    WHEN 9.
      PERFORM main_accrual.                                 "ALRK136242

  ENDCASE.
*----------------------------------------------------------------------*
* 4. workflow events
*----------------------------------------------------------------------*
*  IF NOT ( RKAUF-WF_OKEY IS INITIAL ).              "spc832739
*    PERFORM WORKFLOW_EVENTS USING RKAUF-WF_OKEY     "spc832739
*                                  RKAUF-WITEM.      "spc832739
*  ENDIF.                                            "spc832739

*----------------------------------------------------------------------*
* 5. reset
*----------------------------------------------------------------------*
  PERFORM reset_programm.
  IF NOT gd-act_log IS INITIAL.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                               " FCODE_RUN

*&---------------------------------------------------------------------*
*       FORM  RESET_PROGRAMM.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM  reset_programm.

* init parameters on first screen

  gd_init_surcharge = true.
  rkauf-test        = true.            "<<<message2411367 1998
  rkauf-book        = false.
  rkauf-dialog      = false.
  rkauf-stno        = false.
  CLEAR rkauf-todate.

  IF gd-single_period EQ true.
    CLEAR rkauf-to.
  ENDIF.

* reset internal tables and parameters

  REFRESH: sel_text,
           it_coiob,
           it_objnr,
           gt_prot,
           i_periods,
           xcoep,
           xcoeja,
           gt_mesg_rel.

  CLEAR:   sel_text,
           it_coiob,
           i_periods,
           gt_prot,
           xcoep,
           xcoeja,
           gt_mesg_rel.

  CLEAR:   gd-mesg_line_sav,
           gd-selected,
           gd-processed,
           stat.

  REFRESH: gt_selcrit.
  CLEAR:   gd-pflid.

ENDFORM.                               " RESET_PROGRAMM
*&---------------------------------------------------------------------*
*&      Form  OBJECT_SELECTION
*&---------------------------------------------------------------------*
*       get object list for processing
*----------------------------------------------------------------------*
FORM object_selection.

  DATA:        ld_text(30)       TYPE c.
  DATA:        ld_text_elem(132) TYPE c.
  DATA:        l_date_from       LIKE sy-datum.
  DATA:        l_date_to         LIKE sy-datum.
  DATA:        l_index           LIKE sy-tabix.
  DATA:        lb_objdel(1)      TYPE c VALUE ' '.      "delete objects
  DATA:        ld_posfrom        LIKE sy-tabix.         "delete objects
  DATA:        ld_posto          LIKE sy-tabix.         "delete objects

  DATA:  l_if_ex_kazb_obj_check                             "note640230
         TYPE REF TO if_ex_kazb_obj_check,                  "note640230
         lt_objnr TYPE jsto_pre_tt.                         "note640230

*----------------------------------------------------------------------*
* get object list
* ( if an old action log is processed the table has already been
*   filled by routine ACTION_LOG_PROCESS)
*----------------------------------------------------------------------*
  ld_text_elem = text-p01.
  PERFORM progress_indicator_text USING ld_text_elem ' '.
  IF gd-act_log EQ false.
    IF rkauf-wrttp   = wrttp_obli.
      l_date_from = '00000101'.
      l_date_to   = '99991231'.
    ENDIF.

    CALL FUNCTION 'K_OBJECT_SELECTION_RUN'
      EXPORTING
        i_kokrs     = rkauf-kokrs
        i_vrgng     = rkauf-vrgng
        i_versn     = rkauf-versn
        i_date_from = l_date_from
        i_date_to   = l_date_to
      TABLES
        it_objnr    = it_objnr
        it_periods  = i_periods.
  ENDIF.

  SORT it_objnr.

* copy internal table                                       "note640230
  lt_objnr[] = it_objnr[].                                  "note640230

* get instance for BADI objectselection                     "note640230
  CALL FUNCTION 'KAZB_GET_INSTANCE_BADI'                    "note640230
    IMPORTING                                               "note640230
      e_if_ex_kazb_obj_check = l_if_ex_kazb_obj_check       "note640230
            .
* call BADI                                                 "note640230
  CALL METHOD   l_if_ex_kazb_obj_check->check_autyp         "note640230
    CHANGING ct_objnr = lt_objnr.                           "note640230

* copy internal table back                                  "note640230
  it_objnr[] = lt_objnr[].                                  "note640230


* check if worklists are active
  CALL FUNCTION 'KPEP_WLA_CHECK_ACTIVE'"worklist by work flow
      IMPORTING
           is_active = gd-wl_active.

  IF lb_objdel = 'X'.                "flag to delete objects in debugger
    DELETE it_objnr FROM ld_posfrom TO ld_posto.
  ENDIF.
*----------------------------------------------------------------------*
* get number of selected objects
*----------------------------------------------------------------------*
  PERFORM clean_object_list.
  DESCRIBE TABLE it_objnr LINES gd-selected.

  IF gd-selected EQ 0.
*    if gd-moni_save = true.                       "<<<note180778
*       perform monitor_close_record.              "<<<note180778
*    endif.                                        "<<<note180778
    PERFORM message USING gd-max_error
                          'KA' 'I' '459'
                          ' ' ' ' ' ' ' '.
    IF sy-batch NE true.
      IF gd-moni_save = true.          "<<<note180778
        PERFORM monitor_close_record.  "<<<note180778
      ENDIF.                           "<<<note180778
      MESSAGE ID 'KI' TYPE 'S' NUMBER '753'.
      SET SCREEN dynpro.
      LEAVE SCREEN.
    ENDIF.
  ELSE.
    WRITE gd-selected TO ld_text LEFT-JUSTIFIED.
    ld_text_elem = text-p02.
    PERFORM progress_indicator_text USING ld_text ld_text_elem.
  ENDIF.

ENDFORM.                               " OBJECT_SELECTION

*&---------------------------------------------------------------------*
*&      Form  FILL_SElTAB_INTO_SELPA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_seltab_into_selpa TABLES lt_seltab STRUCTURE cosel2
                                   lt_selpa  STRUCTURE kaba00.

  LOOP AT lt_seltab.
    MOVE-CORRESPONDING lt_seltab TO lt_selpa.
    APPEND lt_selpa.
  ENDLOOP.

ENDFORM.                               " FILL_SElTAB_INTO_SELPA
*&---------------------------------------------------------------------*
*&      Form  SET_USER_PARAMETERS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_user_parameters.

  DATA: BEGIN OF ld_exception OCCURS 5.
          INCLUDE STRUCTURE usr05.
  DATA: END   OF ld_exception.
  DATA: BEGIN OF ld_addit_param OCCURS 5.
          INCLUDE STRUCTURE usr05.
  DATA: END   OF ld_addit_param.

* SD-Felder Verwendung und Applikation werden nie gesetzt
  REFRESH ld_exception.
  ld_exception-parid = 'KVW'.   APPEND ld_exception.
  ld_exception-parid = 'KAP'.   APPEND ld_exception.
* Kostenrechnungskreis wird immer angeboten
  REFRESH ld_addit_param.
  ld_addit_param-parid = 'CAC'.
  ld_addit_param-parva = rkauf-kokrs.     APPEND ld_addit_param.
  ld_addit_param-parid = 'KVS'.
  ld_addit_param-parva = rkauf-versn.     APPEND ld_addit_param.
  ld_addit_param-parid = 'VPE'.
  ld_addit_param-parva = rkauf-from.      APPEND ld_addit_param.
  ld_addit_param-parid = 'BPE'.
  ld_addit_param-parva = rkauf-to.        APPEND ld_addit_param.
  ld_addit_param-parid = 'GJR'.
  ld_addit_param-parva = rkauf-gjahr.     APPEND ld_addit_param.

* Pop-Up bringen
  CALL FUNCTION 'K_USER_VALUES_SAVE'
    EXPORTING
      kuvs_dynnr       = gd-dynnr_sub
      kuvs_report      = gd-repid_sub
      kuvs_langu       = sy-langu
      kuvs_with_dialog = 'X'
    TABLES
      kuvs_exception   = ld_exception
      kuvs_addit_param = ld_addit_param.

ENDFORM.                               " SET_USER_PARAMETERS

*&---------------------------------------------------------------------*
*&      Form  INIT_GD-SINGLE_GJAHR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* set GJAHR_BIS if not entered by user
*----------------------------------------------------------------------*
FORM init_gd-single_gjahr.
  IF    gd-single_gjahr EQ true
     OR rkauf-gjahr_bis IS INITIAL.
    rkauf-gjahr_bis = rkauf-gjahr.
  ENDIF.

ENDFORM.                               " INIT_GD-SINGLE_GJAHR

*&---------------------------------------------------------------------*
*&      Form  INIT_GD-SINGLE_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* set RKAUF-TO if not entered by user
*----------------------------------------------------------------------*
FORM init_gd-single_period.
  IF NOT gd-single_period IS INITIAL.
    rkauf-to  =  rkauf-from.
  ENDIF.

ENDFORM.                               " INIT_GD-SINGLE_PERIOD

*&---------------------------------------------------------------------*
*&      Form  MODIFY_PARAMETER_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_parameter_screen.

  DATA: kokrs LIKE tka01-kokrs.
  TABLES: evvs.

  IF NOT gd-modify_parasc IS INITIAL.
    LOOP AT SCREEN.
      IF gd-modify_parasc CS '001' AND screen-group1 = '001'.
        screen-active = 0.
      ENDIF.
      IF gd-modify_parasc CS '002' AND screen-group2 = '002'.
        screen-active = 0.
      ENDIF.
      IF gd-modify_parasc CS '003' AND screen-group3 = '003'.
        screen-active = 0.
      ENDIF.
      IF gd-modify_parasc CS '004' AND screen-group4 = '004'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
  IF gd-act_log EQ true.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  GET PARAMETER ID 'VERSN_EV' FIELD rpsco_x-versn_ev.   "Earned Value
  IF NOT rpsco_x-versn_ev IS INITIAL.
    GET PARAMETER ID 'CAC' FIELD kokrs.                     "CAC
    SELECT * FROM  evvs WHERE kokrs = kokrs AND
                              versn_ev = rpsco_x-versn_ev.
    ENDSELECT.
    IF sy-subrc <> 0.
      MESSAGE i340(7a).
      SELECT SINGLE * FROM evvs WHERE kokrs = kokrs.
      rpsco_x-versn_ev = evvs-versn_ev.
    ENDIF.
  ENDIF.

*ENHANCEMENT-POINT MODIFY_PARAMETER_SCREEN_01 SPOTS ES_SAPLKAZB.
ENDFORM.                               " MODIFY_PARAMETER_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_PROCESSING_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* exclude flag for detail list
*----------------------------------------------------------------------*
FORM modify_processing_screen.

* exclude flag for detail list
  IF NOT gd-modify_procsc IS INITIAL.
    LOOP AT SCREEN.
      IF gd-modify_procsc CS '001' AND screen-group1 = '001'.
        screen-active = 0.
      ENDIF.
      IF gd-modify_procsc CS '002' AND screen-group2 = '002'.
        screen-active = 0.
      ENDIF.
      IF gd-modify_procsc CS '003' AND screen-group3 = '003'.
        screen-active = 0.
      ENDIF.
      IF gd-modify_procsc CS '004' AND screen-group4 = '004'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " MODIFY_PROCESSING_SCREEN

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*   initialize transaction
*----------------------------------------------------------------------*
FORM init.

  STATICS:  s_initialized(1) TYPE c.
*ENHANCEMENT-POINT INIT_01 SPOTS ES_SAPLKAZB.

*----------------------------------------------------------------------*
* init structure GD
*----------------------------------------------------------------------*
  PERFORM init_gd.
*----------------------------------------------------------------------*
* init TCODE specific parameters
*----------------------------------------------------------------------*
  PERFORM init_tcode USING sy-tcode.
*----------------------------------------------------------------------*
* ask for subscreen
*----------------------------------------------------------------------*
  IF gd-act_log IS INITIAL.
    CALL FUNCTION 'K_OBJECT_SELECTION_SCREEN_SET'
      EXPORTING
        selart     = gd-selart
        oneobject  = gd-oneobject
        allobjects = gd-allobjects  "4.6
        i_vrgng    = rkauf-vrgng
      IMPORTING
        repid_sub  = gd-repid_sub
        dynnr_sub  = gd-dynnr_sub
        selart_txt = gd-obart_txt.
* set correct texts
    IF gd-selart EQ con_selart-or1.
      IF gd-oneobject IS INITIAL.
        gd-obart_txt = text-ob2.
      ENDIF.
    ENDIF.
  ENDIF.
  gd-obart_txt = gd-obart_txt(40).
*----------------------------------------------------------------------*
* change selection screen if objects from action log are being
* processed
*----------------------------------------------------------------------*
  IF gd-act_log EQ true.
    gd-repid_sub = 'SAPLKAZB'.
    gd-dynnr_sub = dynpro_act_log.
  ENDIF.

*----------------------------------------------------------------------*
* init parameters for first screen
*----------------------------------------------------------------------*
* clear RKAUF-GJAHR_BIS after wrong input
  IF gd-single_gjahr EQ true.
    CLEAR rkauf-gjahr_bis.
  ENDIF.

  CHECK NOT s_initialized EQ true.
  s_initialized = true.

* init control parameters
  IF gd-act_log NE true.
    rkauf-test    = true.
    rkauf-act_log = false.
  ENDIF.
  rkauf-book    = true.
  rkauf-dialog  = false.
  IF gd-modify_procsc CS '001'.
    rkauf-list   = true.
  ENDIF.

*----------------------------------------------- UGL
* Initialize posting date to today's date        UGL
  if rkauf-budat is initial.                    "UGL
     rkauf-budat = sy-datum.                    "UGL
  endif.                                        "UGL
*----------------------------------------------- UGL


ENDFORM.                               " INIT
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTION_CRITERIA
*&---------------------------------------------------------------------*
*   get selection criteria for batch and list output
*----------------------------------------------------------------------*
FORM get_selection_criteria TABLES  pt_selpa STRUCTURE  kaba00.

  DATA: ld_variant    LIKE codia-variant.

  IF gd-act_log IS INITIAL.
*  for cost centers seltab is needed
*   if  gd-repid_btc = 'RKAZUB70'.
*      call function 'K_OBJECT_SELECTION_CRIT_GET'
*           importing
*                variant   = ld_variant
*           tables
*                it_seltab = pt_selpa.
*    else.
    CALL FUNCTION 'K_OBJECT_SELECTION_CRIT_GET'
      EXPORTING
        transparent       = 'X'
        short_field_names = off
      IMPORTING
        variant           = ld_variant
      TABLES
        it_seltab         = pt_selpa.
*   endif.
  ELSE.
    pt_selpa-option = 'EQ'.
    pt_selpa-field  = 'SELTAB'.
    pt_selpa-low    = 'SUBOBJTXT'.
    pt_selpa-low+10 = balsubt-subobjtxt.
    APPEND pt_selpa.
    pt_selpa-low    = 'EXTNUMBER'.
    pt_selpa-low+10 = balhdr-extnumber.
    APPEND pt_selpa.
  ENDIF.

ENDFORM.                               " GET_SELECTION_CRITERIA
*&---------------------------------------------------------------------*
*&      Form  POPUP_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popup_variant.

  DATA:   l_answer(1) TYPE c.

  CALL FUNCTION 'POPUP_TO_DECIDE_WITH_MESSAGE'
       EXPORTING
*         DEFAULTOPTION  = '1'
            diagnosetext1  = text-wr1
            diagnosetext2  = text-wr2
            diagnosetext3  = text-wr3
            textline1      = text-wr4
            textline2      = text-wr5
*         TEXTLINE3      =
            text_option1   = text-o03
            text_option2   = text-o04
            titel          = text-t01
*         START_COLUMN   = 25
*         START_ROW      = 6
*         CANCEL_DISPLAY = ' '
       IMPORTING
            answer         = l_answer.

  CASE l_answer.
*----------------------------------------------------------------------*
* ignore
*----------------------------------------------------------------------*
    WHEN '1'.
*----------------------------------------------------------------------*
* define variant
*----------------------------------------------------------------------*
    WHEN '2'.
      LEAVE TO TRANSACTION 'OKOV'.
*----------------------------------------------------------------------*
* abort
*----------------------------------------------------------------------*
    WHEN 'A'.
      LEAVE TO SCREEN 1000.
  ENDCASE.
ENDFORM.                               " POPUP_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FILL_PERIOD_FROM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_period_from.

  IF rkauf-vrgng EQ vrgng_kzri OR
     rkauf-vrgng EQ vrgng_pev1.
    rkauf-from = rkauf-to.
  ENDIF.
ENDFORM.                               " FILL_PERIOD_FROM

*&---------------------------------------------------------------------*
*&      Form  FCODE_SPECIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fcode_special USING p_fcode LIKE sy-ucomm.

  CASE p_fcode.
    WHEN 'RUNP'.                       "parallel run, online
      rkauf-para = true.
      p_fcode = 'RUN '.
    WHEN 'RUND'.
      rkauf-para  = true.
      rkauf-debug = true.
      p_fcode = 'RUN '.
    WHEN 'STNP'.
      rkauf-para = true.
      p_fcode = 'STNO'.
    WHEN 'STND'.
      rkauf-para  = true.
      rkauf-debug = true.
      p_fcode = 'STNO'.
    WHEN 'TRACE'.                      "trace
      rkauf-dialog = 'T'.              "trace
      p_fcode = 'RUN '.                "trace
    WHEN 'STRACE'.                     "trace reverse
      rkauf-dialog = 'T'.              "trace reverse
      p_fcode = 'STNO'.                "trace reverse
    WHEN 'PLAN'.                       "<<<note360512
      rkauf-dialog = 'P'.              "<<<note360512
      p_fcode = 'RUN'.                 "<<<note360512
    WHEN 'SPLAN'.                      "<<<note360512
      rkauf-dialog = 'P'.              "<<<note360512
      p_fcode = 'STNO'.                "<<<note360512
  ENDCASE.

ENDFORM.                               " FCODE_SPECIAL
*&---------------------------------------------------------------------*
*&      Form  INIT_BOOKFLG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM init_bookflg.

  IF rkauf-test EQ true.
    rkauf-book = false.
  ELSEIF rkauf-test EQ false.
    rkauf-book = true.
  ENDIF.

ENDFORM.                               " INIT_BOOKFLG

*&---------------------------------------------------------------------*
*&      Form  INIT_ACT_LOG_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_act_log_screen.

  CHECK balsubt-subobject IS INITIAL.
** fill object for output
*  BALHDR-OBJECT    = GD-OBJECT.
** fill object text for output
*  SELECT SINGLE * FROM BALOBJT WHERE SPRAS  EQ SY-LANGU
*                             AND OBJECT   EQ GD-OBJECT.
** fill subobject for output
  balhdr-subobject = gd-subobject.
* fill subobject text for output
  SELECT SINGLE * FROM balsubt WHERE spras     EQ sy-langu
                               AND   object    EQ gd-object
                               AND   subobject EQ gd-subobject.
* fill external number for output
  balhdr-extnumber = gd-extnumber.

ENDFORM.                               " INIT_ACT_LOG_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SET_CUA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_cua_status.

*  MOVE 'VARC' TO FUNCTAB-FCODE.
*  COLLECT FUNCTAB.
*  MOVE 'SUB0' TO FUNCTAB-FCODE.
*  COLLECT FUNCTAB.
  MOVE 'PROT' TO functab-fcode.
  COLLECT functab.
  IF gd-oneobject EQ true.
    MOVE 'VAR1' TO functab-fcode.
    COLLECT functab.
    MOVE 'VAR2' TO functab-fcode.
    COLLECT functab.
    MOVE 'VAR3' TO functab-fcode.
    COLLECT functab.
    MOVE 'SUB0' TO functab-fcode.
    COLLECT functab.
    MOVE 'SUB4' TO functab-fcode.
    COLLECT functab.
    MOVE 'VARI' TO functab-fcode.
    COLLECT functab.
    MOVE 'VARC' TO functab-fcode.
    COLLECT functab.
    MOVE 'ECSV' TO functab-fcode.
    COLLECT functab.
    IF gd-obart EQ objektart_or.
      MOVE 'EALB' TO functab-fcode.
      COLLECT functab.
      MOVE 'EALL' TO functab-fcode.
      COLLECT functab.
    ENDIF.
  ENDIF.
* no to-date for planned interest calc.
  IF rkauf-vrgng = vrgng_kzrp.
    MOVE 'BSDT' TO functab-fcode.
    COLLECT functab.
  ENDIF.
  IF gd-act_log NE true.
    SET PF-STATUS gd-pfkey EXCLUDING functab.
    SET TITLEBAR gd-titbar WITH gd-obart_txt(40).
  ELSE.
    SET PF-STATUS 'ACTL'.
    SET TITLEBAR 'ALG'.
  ENDIF.
* no posting-date and no value date on comittment processing
  IF rkauf-wrttp = wrttp_obli.
    MOVE 'POSTDATE' TO functab-fcode.
    COLLECT functab.
    MOVE 'WERT' TO functab-fcode.
    COLLECT functab.
  ENDIF.
* no posting-date in planning
  IF rkauf-wrttp = wrttp_plan.
    MOVE 'POSTDATE' TO functab-fcode.
    COLLECT functab.
  ENDIF.
* no display of user parameters for sales orders  "<<<note350955
  IF  ( gd-obart = objektart_vb ).     "<<<note350955
    MOVE 'PAR' TO functab-fcode.       "<<<note350955
    COLLECT functab.                   "<<<note350955
  ENDIF.                               "<<<note350955

  SET PF-STATUS gd-pfkey EXCLUDING functab.

ENDFORM.                               " SET_CUA_STATUS
*&---------------------------------------------------------------------*
*&      Form  GET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LD_VARIANT  text                                           *
*----------------------------------------------------------------------*
FORM get_variant TABLES   pt_seltab  STRUCTURE  cosel2
                 USING    p_variant  LIKE       rsvar-variant.

  CONSTANTS: con_variant(10)   TYPE c    VALUE 'VARIANT   '.

  READ TABLE pt_seltab WITH KEY low(10) = con_variant.
  CHECK sy-subrc EQ 0.
  p_variant = pt_seltab-low+10.
ENDFORM.                               " GET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_PARAMETERS
*&---------------------------------------------------------------------*
*       different screen for planned interest calculation:
*       no mandatory fields and set/get parameters for Kzrp =>
*       let those fields initial in subscreen and fill those fields
*       if vrgng <> kzrp dynamically.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_parameters.

  CASE gd-selart.
    WHEN 'PRC'.
      PERFORM prc_set_default_period.
    WHEN OTHERS.
      IF rkauf-vrgng <> vrgng_kzrp.
        LOOP AT SCREEN.
*** mandatory fields
          IF screen-group1 = 'REQ'.
            screen-required = 1.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
*** get parameters
        GET PARAMETER ID 'VPE' FIELD rkauf-from.
        GET PARAMETER ID 'GJR' FIELD rkauf-gjahr.
        GET PARAMETER ID 'BPE' FIELD rkauf-to.
        GET PARAMETER ID 'GJR' FIELD rkauf-gjahr_bis.
      ENDIF.
  ENDCASE.
ENDFORM.                               " SET_SCREEN_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  SAVE_SCREEN_PARAMETERS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_screen_parameters.

  IF rkauf-vrgng <> vrgng_kzrp.
*** set parameters
    SET PARAMETER ID 'VPE' FIELD rkauf-from.
    SET PARAMETER ID 'GJR' FIELD rkauf-gjahr.
    SET PARAMETER ID 'BPE' FIELD rkauf-to.
  ENDIF.

ENDFORM.                               " SAVE_SCREEN_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  ACTIVITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM activity.

  IF NOT rkauf-stno IS INITIAL.
    gd-activity = '85'.
  ELSE.
    gd-activity = '16'.
  ENDIF.

ENDFORM.                               " ACTIVITY
*&---------------------------------------------------------------------*
*&      Form  INIT_GD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_gd.

* general
  gd-repid          = 'SAPLKAZB'.
  gd-pfkey          = 'PPI '.
  gd-titbar         = 'PPI'.
* subscreen information
  gd-repid_sub      = 'SAPLKAZB'.
  gd-repid_sub1     = 'SAPLKAZB'.
  gd-repid_btc      = 'RKAZUB70'.
* message storing
  gd-msg_object_id  = 'COPF'.
  gd-msg_appl_id    = 'COPF'.
* action log
  gd-object         = 'COAC'.
  gd-exitprog       = 'SAPLKAZB'.
  gd-exitform       = 'ACTION_LOG_PROCESS'.

ENDFORM.                               " INIT_GD
*&---------------------------------------------------------------------*
*&      Form  projects_check_balance_act_per
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM projects_check_balance_act_per.
  DATA: ld_stat_0 LIKE stat-sal_0.

  IF ( rkauf-wrttp = wrttp_act ).
    IF (  gd-obart  = objektart_pr ) and
       NOT con_check_balance_act_per IS INITIAL.                "397611
      CALL FUNCTION 'K_OBJECT_CHECK_BALANCE_ACT_PER'
        EXPORTING
          i_perio            = rkauf-from
          i_gjahr            = rkauf-gjahr
          flg_commit_allowed = 'X'
        IMPORTING
          e_no_balance       = ld_stat_0
        TABLES
          i_objnr            = it_objnr
        EXCEPTIONS
          no_valid_objects   = 1
          OTHERS             = 2.

* statistics
      stat-sal_0 = stat-sal_0 + ld_stat_0.

      IF sy-subrc = 1.
* if no objects are surcharged, status for worklist has to be set here
        LOOP AT it_objnr.              "worklist
          PERFORM wl_object_status_set USING it_objnr       "worklist
                                 kpepw_objstat-ok.          "worklist
        ENDLOOP.
        IF gd-moni_save = true.        "worklist
          PERFORM monitor_close_record.
        ENDIF.
        IF sy-batch EQ true.
          PERFORM message USING gd-max_error
                                'KA' 'I' '460'
                                 ' ' ' ' ' ' ' '.
        ELSE.
          MESSAGE ID 'KA' TYPE 'S' NUMBER '460'.
          SET SCREEN dynpro.
          LEAVE SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " projects_check_balance_act_per
*&---------------------------------------------------------------------*
*&      Form  prod_ord_la__revaluation_check
*&---------------------------------------------------------------------*
*      no processing for production orders with update of output
*      deleted objects are in it_objnr_err
*----------------------------------------------------------------------*
FORM prod_ord_la_revaluation_check.

  DATA:  it_objnr_err LIKE jsto_pre  OCCURS 0 WITH HEADER LINE.
  DATA:
         ld_ident_objid LIKE  sy-msgv1,
         ld_ident_txt20  LIKE  tbo01-txt20.

 IF ( gd-selart = con_selart-or2 ) OR ( gd-selart = con_selart-or3 ) OR
    ( gd-selart = con_selart-hp )  OR                    "<<<note513792
    ( gd-selart = con_selart-or1 ).                      "<<<note675070
    CALL FUNCTION 'CKML_LA_REVALUATION_CHECK'
      TABLES
        it_objnr     = it_objnr
        it_periods   = i_periods
        et_objnr_err = it_objnr_err.

    LOOP AT it_objnr_err.
* worklists
    PERFORM wl_object_status_set USING it_objnr_err-objnr     "worklists
                        kpepw_objstat-not_relevant.           "worklists
* message handler
      CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
        EXPORTING
          objnr       = it_objnr_err-objnr
        IMPORTING
          ident_objid = ld_ident_objid
          ident_txt20 = ld_ident_txt20.
      PERFORM message_line_add_1 USING gd-mesg_line.
      PERFORM message_default
                         USING gd-mesg_line
                               'KA' 'S' '159' ld_ident_txt20
                                              ld_ident_objid
                                              ' '  ' '.
      PERFORM message USING gd-max_error
                            'KA' 'E' '465'
                            ' ' ' ' ' ' ' '.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " prod_ord_la_revaluation_check
*&---------------------------------------------------------------------*
*&      Form  fill_sel_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_sel_text TABLES   it_selpa STRUCTURE kaba00
                            ct_seltext STRUCTURE seltext
                   USING    id_obart LIKE ionr-obart.

  DATA: lt_selpa LIKE kaba00 OCCURS 0 WITH HEADER LINE.

  lt_selpa[] = it_selpa[].
* convert selpa to non-transparent form
* note: this is done only if the program was startet by transaction
*       code. If the batch report was used, then seltab is already in
*       intransparant form
  READ TABLE lt_selpa INDEX 1.
  IF NOT lt_selpa-field IS INITIAL. "I hope this is true for all columns
    PERFORM convert_seltab IN PROGRAM saplkass
                           TABLES lt_selpa
                         USING  space space.
  ENDIF.
  PERFORM fill_sel_text IN PROGRAM saplkass
                        TABLES lt_selpa
                               sel_text
                        USING  gd-obart.

ENDFORM.                               " fill_sel_text
