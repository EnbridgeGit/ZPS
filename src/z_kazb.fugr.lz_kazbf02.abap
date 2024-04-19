*-------------------------------------------------------------------
***INCLUDE LKAZBF02 .
*-------------------------------------------------------------------
* Corrections:
* P6BK002257: 11.06.2002 Note 521846
* P6BK088859: Note 633394 18.06.2003 FCL
* P6BK094729: 17.07.2003 Note 642462 FCL
*
*&---------------------------------------------------------------------*
*&      Form GET_GROUP_INFO
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM get_group_info TABLES pt_objnr STRUCTURE objnr
                    USING  p_groupsize LIKE sy-tabix
                           p_objanz    LIKE sy-tabix
                           p_groups    LIKE sy-tabix.

  DATA: l_number  LIKE sy-tabix.

  DESCRIBE TABLE pt_objnr LINES p_objanz.
  IF rkauf-dialog IS INITIAL.
    p_groupsize = c_groupsize.
    p_groups = ( p_objanz DIV p_groupsize ).
    l_number = p_groups * p_groupsize.
    IF l_number LT p_objanz.
      p_groups = p_groups + 1.
    ENDIF.
  ELSE.
    p_groups  = p_objanz.
    p_groupsize = 1.
  ENDIF.

ENDFORM.                               " GET_GROUP_INFO
*&---------------------------------------------------------------------*
*&      Form  MAKE_GROUPS
*&---------------------------------------------------------------------*
*       make groups of objects that are processed together
*       (reason: tables for master data are refreshed before
*                the start of the next group to avoid excessive
*                memory usage or swapping.
*                parallel processing uses another concept)
*----------------------------------------------------------------------*
FORM make_groups TABLES pt_objnr        STRUCTURE objnr
                        pt_objnr_group  STRUCTURE objnr
                 USING  l_groupsize     LIKE sy-index.

  CLEAR   pt_objnr.
  CLEAR   pt_objnr_group.
  REFRESH pt_objnr_group.
  CALL FUNCTION 'K_ORDER_BUFFER_REFRESH'.

  CHECK l_groupsize NE 0.

  APPEND LINES OF pt_objnr TO l_groupsize TO pt_objnr_group.
  DELETE pt_objnr TO l_groupsize.

ENDFORM.                               " MAKE_GROUPS

*&---------------------------------------------------------------------*
*       FORM  CHECK_TRANSACTIONT TABLES LT_OBJNR STRUCTURE OBJNR.
*&---------------------------------------------------------------------*
*       check whether activity ( given by TCODE ) is allowed
*----------------------------------------------------------------------*
FORM  check_transaction TABLES lt_objnr          STRUCTURE objnr
                               pt_prot           TYPE kazb_prot_tab
                        USING  p_rkauf           STRUCTURE rkauf
                               p_stat            STRUCTURE stat
                               p_identification  LIKE      sy-uzeit
                               p_mesg_line       LIKE      mesg-zeile
                               p_max_error       LIKE      sy-subrc
                               p_processed       TYPE      i.

  DATA:   ld_ionra           LIKE  ionra,
          ld_cobl            LIKE  cobl,
*         LD_IDENTIFICATION  LIKE  SY-MSGV1,
          ld_ident_objid     LIKE  sy-msgv1,
          ld_ident_txt20     LIKE  tbo01-txt20,
          ld_subrc           LIKE  sy-subrc,
          l_statistical(1)   TYPE  c,
          ld_stat_not_relevant LIKE dcobjdef-name,          "note318485
          ld_stat_error LIKE dcobjdef-name,                 "note318485
*{   DELETE         KI4K061113                                        4
*\        ld_stat like DCOBJDEF-NAME.                        "note318485
*}   DELETE
*{   INSERT         KI4K061113                                        3
          ld_stat LIKE dcobjdef-name,                       "note318485
          ld_objnr LIKE coiob-objnr.                        "note318485+
*}   INSERT
  DATA: ld_cobk type cobk.                                   "note633394
*----------------------------------------------------------------------*
  LOOP AT lt_objnr.
*----------------------------------------------------------------------*
*   identify objects and add default line in message handler
*----------------------------------------------------------------------*
    CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
         EXPORTING
              objnr          = lt_objnr-objnr
         IMPORTING
              e_ionra        = ld_ionra
*              IDENTIFICATION = LD_IDENTIFICATION
              ident_objid    = ld_ident_objid
              ident_txt20    = ld_ident_txt20.

    CLEAR ld_cobl.
    PERFORM message_line_add_1 USING p_mesg_line.
    PERFORM message_default
                       USING p_mesg_line
                             'KA' 'S' '159' ld_ident_txt20
                                            ld_ident_objid
                                            ' '  ' '.
*----------------------------------------------------------------------*
*   aditional status check for worklists
*----------------------------------------------------------------------*
* in case of worklists check if activity is allowed
* because of the message handler the check has to be executed here
  IF gd-wl_active = 'X'.                                      "worklists
      PERFORM wl_additional_status_check USING lt_objnr-objnr ld_subrc.
    IF ld_subrc EQ 1.                                         "worklists
      CLEAR  pt_prot.                                         "worklists
        MOVE lt_objnr-objnr TO pt_prot-objnr.
      pt_prot-deleted = true.                                 "worklists
      pt_prot-status  = 'OTHS'.                               "worklists
      APPEND pt_prot.                                         "worklists
        PERFORM wl_object_status_set USING lt_objnr-objnr
                           kpepw_objstat-not_relevant.   "worklists
        CLEAR lt_objnr.
        MODIFY lt_objnr.
     p_stat-other_status = p_stat-other_status + 1.           "worklists
     CONTINUE.                                                "worklists
   ELSEIF ld_subrc = 2.                                       "worklists
        CLEAR  pt_prot.
        MOVE lt_objnr-objnr TO pt_prot-objnr.
        pt_prot-deleted = true.
        APPEND pt_prot.
        PERFORM wl_object_status_set USING lt_objnr-objnr
                               kpepw_objstat-error.           "worklists
        CLEAR lt_objnr.
        MODIFY lt_objnr.
        CONTINUE.
   ENDIF.                                                     "worklists
    ENDIF.
*----------------------------------------------------------------------*
*   fill parameters for check
*----------------------------------------------------------------------*
    IF     ld_ionra-obart EQ objektart_or.
*     orders
      MOVE-CORRESPONDING ld_ionra TO ld_cobl.
    ELSEIF ld_ionra-obart EQ objektart_pr.
*     PSP-elements
      MOVE ld_ionra-pspnr         TO ld_cobl-ps_psp_pnr.
    ELSEIF ld_ionra-obart EQ objektart_np.
*     network
      IF p_rkauf-sign_ap EQ con_plan.
*       no surcharges for network activity for plan data
*       done by calculation
*       PERFORM MESSAGE USING 'KA' 'E' '413' p_rkauf-VRGNG LD_IDENTIF
*                              ' ' ' '.
        CLEAR  pt_prot.
        MOVE lt_objnr TO pt_prot-objnr.
        pt_prot-deleted = true.
        pt_prot-vrgng   = p_rkauf-vrgng.
        APPEND pt_prot.
        CLEAR lt_objnr.
        MODIFY lt_objnr.
        CONTINUE.
      ENDIF.
      MOVE ld_ionra-aufnr         TO  ld_cobl-nplnr.
    ELSEIF ld_ionra-obart EQ objektart_nv.
*     network activity
      IF p_rkauf-sign_ap EQ con_plan.
*       no surcharges for network activity for plan data
*       done by calculation
*        PERFORM MESSAGE USING 'KA' 'E' '413' p_rkauf-VRGNG LD_IDENTIF
*                              ' ' ' '.
        CLEAR  pt_prot.
        MOVE lt_objnr TO pt_prot-objnr.
        pt_prot-deleted = true.
        pt_prot-vrgng   = p_rkauf-vrgng.
        APPEND pt_prot.
        CLEAR lt_objnr.
        MODIFY lt_objnr.
        CONTINUE.
      ENDIF.
      MOVE ld_ionra-aufpl         TO  ld_cobl-aufpl.
      MOVE ld_ionra-aplzl         TO  ld_cobl-aplzl.

      call function 'CO_SF_GET_AUFNR_FROM_AUFPL'          "<<<note642462
         exporting                                        "<<<note642462
              aufpl_imp     = LD_COBL-AUFPL               "<<<note642462
         importing                                        "<<<note642462
              aufnr_exp     = LD_COBL-NPLNR               "<<<note642462
         exceptions                                       "<<<note642462
              error_message = 1                           "<<<note642462
              others        = 2.                          "<<<note642462

       IF SY-SUBRC <> 0.                                  "<<<note642462
          sy-msgid = 'KOSA'.                              "<<<note642462
          sy-msgty = 'E'.                                 "<<<note642462
          sy-msgno = '007'.                               "<<<note642462
          sy-msgv1 = 'CO_SF_GET_AUFNR_FROM_AUFPL'.        "<<<note642462
          sy-msgv2 = 'LD_COBL_AUFPL'.                     "<<<note642462
          perform message using p_max_error               "<<<note642462
                  sy-MSGID sy-MSGTY sy-MSGNO              "<<<note642462
                  sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4. "<<<note642462
       ENDIF.                                             "<<<note642462


    ELSEIF ld_ionra-obart EQ objektart_hp.
*     cost object
      MOVE ld_ionra-kstrg         TO  ld_cobl-kstrg.
    ENDIF.

*----------------------------------------------------------------------*
* set global parameters for check
*----------------------------------------------------------------------*
    ld_cobl-kokrs = p_rkauf-kokrs.
    ld_cobl-gjahr = p_rkauf-gjahr.
    IF p_rkauf-wrttp = wrttp_obli.
      CLEAR ld_cobl-monat.
      ld_cobl-budat = sy-datum.
    ELSE.
      ld_cobl-monat = p_rkauf-from.
* if K_PERIOD_LIMITS_GET was already called take values from buffer
      IF p_rkauf-gjahr = buffer_gjahr
         AND p_rkauf-from = buffer_from
         AND p_rkauf-kokrs = buffer_kokrs.
        ld_cobl-budat = buffer_budat.
      ELSE.
        CALL FUNCTION 'K_PERIOD_LIMITS_GET'                 "ALRK151981
            EXPORTING                                       "ALRK151981
                 gjahr                = p_rkauf-gjahr       "ALRK151981
                 perio                = p_rkauf-from        "ALRK151981
                 kokrs                = p_rkauf-kokrs       "ALRK151981
            IMPORTING                                       "ALRK151981
                 pbegdat              = ld_cobl-budat.      "ALRK151981
        IF sy-subrc <> 0.
        ENDIF.
        buffer_gjahr = p_rkauf-gjahr.
        buffer_from = p_rkauf-from.
        buffer_kokrs = p_rkauf-kokrs.
        buffer_budat = ld_cobl-budat.
      ENDIF.
    ENDIF.
    ld_cobl-vorgn = p_rkauf-vrgng.
*----------------------------------------------------------------------*
* count number of messages before check
*----------------------------------------------------------------------*
    PERFORM messages_count USING vorher.
*----------------------------------------------------------------------*
* check transaction
*----------------------------------------------------------------------*
    MOVE-CORRESPONDING p_rkauf TO ld_cobk.                   "note633394

    CALL FUNCTION 'K_COBL_CHECK'
      EXPORTING
        i_cobl        = ld_cobl
        I_COBK        = LD_COBK                              "note633394
      IMPORTING
        e_cobl        = ld_cobl
      EXCEPTIONS
        error_message = 1.
    IF sy-subrc EQ 1.
      PERFORM message USING p_max_error
                    sy-msgid sy-msgty sy-msgno
                    sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
    ENDIF.
*----------------------------------------------------------------------*
* check whether errors occured during check
*----------------------------------------------------------------------*
    PERFORM messages_check USING ld_subrc
                                 vorher
                                 nachher.
    IF ld_subrc NE 0.
      CLEAR l_statistical.
      PERFORM messages_clean USING p_stat
                                   p_identification
                                   p_mesg_line
                                   p_max_error
                                   l_statistical
                                   ld_subrc.
*     store object number in table for put to action log
      IF ld_subrc EQ 0.
        APPEND lt_objnr TO it_objnr_log.
      ENDIF.
*     exclude object from further processing if an error occured
      CLEAR  pt_prot.
      MOVE lt_objnr TO pt_prot-objnr.
      pt_prot-deleted = true.
      pt_prot-statistical = l_statistical.
      pt_prot-vrgng   = p_rkauf-vrgng.
      APPEND pt_prot.
* set object status of excluded objects for worklist and pop up in list
* statistical objects are not relevant
      CALL FUNCTION 'KALV_SET_ROLLNAMES'                    "note318485
         IMPORTING                                          "note318485
            e_stat_not_relevant_roll = ld_stat_not_relevant "note318485
            e_stat_error_roll = ld_stat_error.              "note318485
      IF l_statistical = 'X'.
        PERFORM wl_object_status_set USING lt_objnr       "worklists
                              kpepw_objstat-not_relevant. "worklists
        ld_stat = ld_stat_not_relevant.                     "note318485
      ELSE.
        PERFORM wl_object_status_set USING lt_objnr       "worklists
                               kpepw_objstat-error.       "worklists
        ld_stat = ld_stat_error.                            "note318485
      ENDIF.
*{   INSERT         KI4K055510                                        1
      ld_objnr = lt_objnr.
      CALL FUNCTION 'K_OBJECT_STORE'                        "note318485
            EXPORTING                                       "note318485
               i_objnr    = ld_objnr                        "note318485
               i_status   = ld_stat.                        "note318485
      CLEAR ld_objnr.
*}   INSERT
*{   DELETE         KI4K055510                                        2
*\    CALL FUNCTION 'K_OBJECT_STORE'                       "note318485
*\          EXPORTING                                      "note318485
*\             I_OBJNR    = LT_OBJNR                       "note318485
*\             I_STATUS   = ld_stat.                       "note318485
*}   DELETE
      CLEAR  lt_objnr.
      MODIFY lt_objnr.
    ENDIF.

  ENDLOOP.
*---------------------------------------------------------------------*
* clear object list
*---------------------------------------------------------------------*
  DELETE lt_objnr WHERE objnr IS initial.
*---------------------------------------------------------------------*
* get number of objects processed
*---------------------------------------------------------------------*
  DESCRIBE TABLE lt_objnr LINES p_processed.
ENDFORM.                               " CHECK_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  CHECK_OBJECT_STATUS
*&---------------------------------------------------------------------*
*       check  for object status excluding further processing
*----------------------------------------------------------------------*
FORM check_object_status TABLES pt_objnr STRUCTURE  objnr
                                pt_prot  TYPE       kazb_prot_tab
                         USING  p_rkauf  LIKE       rkauf
                                p_obart  LIKE       ionr-obart
                                p_stat   STRUCTURE  stat.

  DATA: ld_subrc   LIKE sy-subrc,
*{   DELETE         KI4K061113                                       12
*\      LD_stat_not_relevant_state like dcobjdef-name.       "note318485
*}   DELETE
*{   INSERT         KI4K055510                                        7
    ld_stat_not_relevant_state LIKE dcobjdef-name,         "note318485+
    ld_objnr LIKE coiob-objnr.                             "note318485+
*}   INSERT
  DATA: ld_portion like groupsize,                        "<<<note421823
        ld_portion_begin TYPE i VALUE 1,                  "<<<note421823
        ld_portion_end like groupsize,                    "<<<note421823
        anz_obj TYPE p,                                   "<<<note421823
        anz_runs TYPE f.                                  "<<<note421823
  DATA: BEGIN OF lt_objnr OCCURS 0,                       "<<<note421823
        objnr LIKE coss-objnr,                            "<<<note421823
        END OF lt_objnr,                                  "<<<note421823
        glt_objnr LIKE lt_objnr OCCURS 0.                 "<<<note421823

*----------------------------------------------------------------------*
  CHECK p_obart NE objektart_ks
    AND p_obart NE objektart_kl
    AND p_obart NE objektart_bp.

  CHECK gd-processor NE 5.             " Don't do it for project progr.
*----------------------------------------------------------------------*
LD_PORTION = GROUPSIZE.                                   "<<<note421823
LD_PORTION_END = GROUPSIZE.                               "<<<note421823
REFRESH lt_objnr.                                         "<<<note421823
REFRESH glt_objnr.                                        "<<<note421823
DESCRIBE TABLE pt_objnr LINES anz_obj.                    "<<<note421823
anz_runs = anz_obj / ld_portion.                          "<<<note421823
anz_runs = ceil( anz_runs ).                              "<<<note421823

DO anz_runs TIMES.                                        "<<<note421823
REFRESH lt_objnr.                                         "<<<note421823
CLEAR lt_objnr.                                           "<<<note421823
APPEND LINES OF pt_objnr FROM ld_portion_begin            "<<<note421823
                         TO ld_portion_end TO lt_objnr.   "<<<note421823

    CALL FUNCTION 'STATUS_BUFFER_REFRESH'.
    CALL FUNCTION 'STATUS_PRE_READ'
      TABLES
        jsto_pre_tab = lt_objnr.                          "<<<note421823
  CALL FUNCTION 'KALV_SET_ROLLNAMES'                        "note318485
   IMPORTING                                                "note318485
    e_stat_not_relev_state_roll = ld_stat_not_relevant_state."note318485

    LOOP AT lt_objnr.
* check whether status is...
**  'EROF' ?
      IF p_rkauf-sign_ap EQ con_act.
        PERFORM check_status USING lt_objnr-objnr         "<<<note421823
                                   'I0001'
                                   ld_subrc.
        IF ld_subrc EQ 0.
          CLEAR  pt_prot.
          MOVE lt_objnr TO pt_prot-objnr.                "<<<note421823
          pt_prot-deleted = true.
          pt_prot-status  = 'EROF'.
          APPEND pt_prot.
* set object status of excluded objects for worklist          "worklists
        PERFORM wl_object_status_set USING pt_objnr           "worklists
                                 kpepw_objstat-not_relevant.  "worklists
* set object status for pop up on list
*{   INSERT         KI4K055510                                        1
          ld_objnr = lt_objnr.                           "<<<note421823
        CALL FUNCTION 'K_OBJECT_STORE'                      "note318485
              EXPORTING                                     "note318485
                 i_objnr    = ld_objnr                      "note318485
                 i_status   = ld_stat_not_relevant_state.   "note318485
          CLEAR ld_objnr.
*}   INSERT
*{   DELETE         KI4K055510                                        2
*\   CALL FUNCTION 'K_OBJECT_STORE'                          "note318485
*\     EXPORTING                                             "note318485
*\       I_OBJNR                 = PT_OBJNR                  "note318485
*\       I_STATUS                = ld_stat_not_relevant_state."ote318485
*}   DELETE
          CLEAR lt_objnr.                                 "<<<note421823
          MODIFY lt_objnr.                                "<<<note421823
          p_stat-erof = p_stat-erof + 1.
          CONTINUE.
        ENDIF.
      ENDIF.
*   'LÖVM' ?
      PERFORM check_status USING lt_objnr-objnr           "<<<note421823
                                 'I0076'
                                 ld_subrc.
      IF ld_subrc EQ 0.
        CLEAR  pt_prot.
        MOVE lt_objnr TO pt_prot-objnr.                   "<<<note421823
        pt_prot-deleted = true.
        pt_prot-status  = 'LOVM'.
        APPEND pt_prot.
* set object status of excluded objects for worklist          "worklists
        PERFORM wl_object_status_set USING pt_objnr      "worklists
                            kpepw_objstat-not_relevant.  "worklists
* set object status for pop up on list
*{   INSERT         KI4K055510                                        3
        ld_objnr = lt_objnr.                              "<<<note421823
        CALL FUNCTION 'K_OBJECT_STORE'                      "note318485
              EXPORTING                                     "note318485
                 i_objnr    = ld_objnr                      "note318485
                 i_status   = ld_stat_not_relevant_state.   "note318485
        CLEAR ld_objnr.
*}   INSERT
*{   DELETE         KI4K055510                                        9
*\  CALL FUNCTION 'K_OBJECT_STORE'                           "note318485
*\    EXPORTING                                              "note318485
*\      I_OBJNR                 = PT_OBJNR                   "note318485
*\      I_STATUS                = ld_stat_not_relevant_state."note318485
*}   DELETE
        CLEAR lt_objnr.                                   "<<<note421823
        MODIFY lt_objnr.                                  "<<<note421823
        p_stat-lovm = p_stat-lovm + 1.
        CONTINUE.
      ENDIF.
*   'LÖKZ' ?
      PERFORM check_status USING lt_objnr-objnr           "<<<note421823
                                 'I0013'
                                 ld_subrc.
      IF ld_subrc EQ 0.
        CLEAR  pt_prot.
        MOVE lt_objnr TO pt_prot-objnr.                  "<<<note421823
        pt_prot-deleted = true.
        pt_prot-status  = 'LOKZ'.
        APPEND pt_prot.
* set object status of excluded objects for worklist          "worklists
        PERFORM wl_object_status_set USING pt_objnr      "worklists
                           kpepw_objstat-not_relevant.   "worklists
* set object status for pop up on list
*{   INSERT         KI4K055510                                        4
        ld_objnr = lt_objnr.                              "<<<note421823
        CALL FUNCTION 'K_OBJECT_STORE'                      "note318485
              EXPORTING                                     "note318485
                 i_objnr    = ld_objnr                      "note318485
                 i_status   = ld_stat_not_relevant_state.   "note318485
        CLEAR ld_objnr.
*}   INSERT
*{   DELETE         KI4K055510                                       10
*\ CALL FUNCTION 'K_OBJECT_STORE'                                      \
*\                                "note318485
*\   EXPORTING                                              "note318485
*\     I_OBJNR                 = PT_OBJNR                   "note318485
*\     I_STATUS                = ld_stat_not_relevant_state."note318485
*}   DELETE
        CLEAR lt_objnr.                                  "<<<note421823
        MODIFY lt_objnr.                                 "<<<note421823
        p_stat-lokz = p_stat-lokz + 1.
        CONTINUE.
      ENDIF.
*   'ABGS' ?
      PERFORM check_status USING lt_objnr-objnr          "<<<note421823
                                 'I0046'
                                 ld_subrc.
      IF ld_subrc EQ 0.
        CLEAR  pt_prot.
        MOVE lt_objnr TO pt_prot-objnr.                  "<<<note421823
        pt_prot-deleted = true.
        pt_prot-status  = 'ABGS'.
        APPEND pt_prot.
* set object status of excluded objects for worklist          "worklists
        PERFORM wl_object_status_set USING pt_objnr      "worklists
                          kpepw_objstat-not_relevant.    "worklists
* set object status for pop up on list
*{   INSERT         KI4K055510                                        5
        ld_objnr = lt_objnr.                             "<<<note421823
        CALL FUNCTION 'K_OBJECT_STORE'                      "note318485
              EXPORTING                                     "note318485
                 i_objnr    = ld_objnr                      "note318485
                 i_status   = ld_stat_not_relevant_state.   "note318485
        CLEAR ld_objnr.
*}   INSERT
*{   DELETE         KI4K055510                                       11
*\ CALL FUNCTION 'K_OBJECT_STORE'                                      \
*\                                 "note318485
*\   EXPORTING                                               "note318485
*\     I_OBJNR                 = PT_OBJNR                    "note318485
*\     I_STATUS                = ld_stat_not_relevant_state. "note318485
*}   DELETE
        CLEAR lt_objnr.                                   "<<<note421823
        MODIFY lt_objnr.                                  "<<<note421823
        p_stat-abgs = p_stat-abgs + 1.
        CONTINUE.
      ENDIF.
*   'TABG' ?
      IF p_rkauf-sign_ap EQ con_plan.
        PERFORM check_status USING lt_objnr-objnr         "<<<note421823
                                   'I0045'
                                   ld_subrc.
        IF ld_subrc EQ 0.
          CLEAR  pt_prot.
          MOVE lt_objnr TO pt_prot-objnr.                 "<<<note421823
          pt_prot-deleted = true.
          pt_prot-status  = 'ABGS'.
          APPEND pt_prot.
* set object status of excluded objects for worklist          "worklists
          PERFORM wl_object_status_set USING pt_objnr      "worklists
                             kpepw_objstat-not_relevant.   "worklists
* set object status for pop up on list
*{   INSERT         KI4K055510                                        6
          ld_objnr = lt_objnr.
        CALL FUNCTION 'K_OBJECT_STORE'                      "note318485
              EXPORTING                                     "note318485
                 i_objnr    = ld_objnr                      "note318485
                 i_status   = ld_stat_not_relevant_state.   "note318485
          CLEAR ld_objnr.
*}   INSERT
*{   DELETE         KI4K055510                                        8
*\  CALL FUNCTION 'K_OBJECT_STORE'                                     \
*\                                 "note318485
*\    EXPORTING                                              "note318485
*\      I_OBJNR                 = PT_OBJNR                   "note318485
*\      I_STATUS                = ld_stat_not_relevant_state."note318485
*}   DELETE
          CLEAR lt_objnr.                                 "<<<note421823
          MODIFY lt_objnr.                                "<<<note421823
          p_stat-tabg = p_stat-tabg + 1.
          CONTINUE.
        ENDIF.
      ENDIF.

*****************************NOTE 521846*******************************

*   'SPER' ?
    PERFORM CHECK_STATUS USING LT_OBJNR-OBJNR
                               'I0043'
                               LD_SUBRC.
    IF LD_SUBRC EQ 0.
      CLEAR  PT_PROT.
      MOVE LT_OBJNR TO PT_PROT-OBJNR.
      PT_PROT-DELETED = TRUE.
      PT_PROT-STATUS  = 'SPER'.
      APPEND PT_PROT.
* set object status of excluded objects for worklist          "worklists
             PERFORM WL_OBJECT_STATUS_SET USING LT_OBJNR      "worklists
                               KPEPW_OBJSTAT-not_relevant.    "worklists
* set object status for pop up on list
 ld_objnr = lt_objnr.
 CALL FUNCTION 'K_OBJECT_STORE'
     EXPORTING
       I_OBJNR                 = ld_OBJNR
       I_STATUS                = ld_stat_not_relevant_state.
      clear ld_objnr.
      CLEAR LT_OBJNR.
      MODIFY LT_OBJNR.
      P_STAT-OTHER_STATUS = P_STAT-OTHER_STATUS + 1.
      CONTINUE.
    ENDIF.

*****************************NOTE 521846*******************************


    ENDLOOP.
   append lines of lt_objnr to glt_objnr.                "<<<note421823
   ld_portion_begin = ld_portion_begin + ld_portion.     "<<<note421823
   ld_portion_end = ld_portion_end + ld_portion.         "<<<note421823
 enddo.                                                  "<<<note421823
 pt_objnr[] = glt_objnr[].                               "<<<note421823

    DELETE pt_objnr WHERE objnr IS initial.

  ENDFORM.                               " CHECK_OBJECT_STATUS
*&---------------------------------------------------------------------*
*&      Form  ABORT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM abort.

  PERFORM message_default
                  USING gd-mesg_line
                        'KA' 'S' '403' ' '  ' '  ' '  ' '.
  PERFORM message USING gd-max_error 'KA' 'E' '404' ' '  ' '  ' '  ' '.
  PERFORM message_handler_deactivate.

  IF sy-batch EQ true.
    PERFORM display_error_list IN PROGRAM saplkalv.
  ELSE.
    READ TABLE functab WITH KEY fcode = 'PROT'.
    CHECK sy-subrc = 0.                                     "ALR153534
    DELETE functab INDEX sy-tabix.
    SET PF-STATUS gd-pfkey EXCLUDING functab.
  ENDIF.
  PERFORM message USING gd-max_error 'KA' 'E' '404' ' '  ' '  ' '  ' '.

ENDFORM.                               " ABORT
*&---------------------------------------------------------------------*
*&      Form  CHECK_STATUS
*&---------------------------------------------------------------------*
*       text
**----------------------------------------------------------------------
*
FORM check_status USING p_objnr  LIKE coss-objnr
                        p_status LIKE jest-stat
                        p_subrc  LIKE sy-subrc.

  CALL FUNCTION 'STATUS_CHECK'
       EXPORTING
*             BYPASS_BUFFER     = ' '
            client            = sy-mandt
            objnr             = p_objnr
            status            = p_status
       EXCEPTIONS
*           OBJECT_NOT_FOUND  = 1
            status_not_active = 2
            OTHERS            = 3.

  p_subrc = sy-subrc.

ENDFORM.                               " STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  CLEAN_OBJECT_LIST
*&---------------------------------------------------------------------*
*       exclude undesired objects from further processing
*----------------------------------------------------------------------*
FORM clean_object_list.

  CASE gd-processor.
    WHEN 1.
      LOOP AT it_objnr.
        IF it_objnr(2) EQ objektart_op.
          DELETE it_objnr.
*           if  gd-wl_active = 'X'.                            "worklist
          PERFORM wl_object_status_set USING it_objnr       "worklist
                             kpepw_objstat-not_relevant.    "worklist
*           endif.                                             "worklist
        ENDIF.
        IF (    it_objnr(2) EQ objektart_np
             OR it_objnr(2) EQ objektart_nv )
           AND rkauf-sign_ap EQ con_plan.
          DELETE it_objnr.
        ENDIF.
      ENDLOOP.
    WHEN 3.
      LOOP AT it_objnr.
        IF it_objnr(2) EQ objektart_op.
          DELETE it_objnr.
*     DELETE IT_OBJNR WHERE OBJNR(2) = OBJEKTART_OP.
*          if  gd-wl_active = 'X'.                             "worklist
          PERFORM wl_object_status_set USING it_objnr       "worklist
                       kpepw_objstat-not_relevant.          "worklist
*          endif.                                              "worklist
        ENDIF.
      ENDLOOP.
  ENDCASE.

ENDFORM.                               " CLEAN_OBJECT_LIST
*&---------------------------------------------------------------------*
*&      Form  WL_OBJECT_STATUS_SET
*&---------------------------------------------------------------------*
*  set object status of excluded objects for worklist
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM wl_object_status_set USING l_objnr STRUCTURE objnr       "worklists
                          id_status TYPE kpep_wl_objstat.     "worklists
  "worklists
DATA: wl_objnr TYPE kpep_wl_objectid,                         "worklists
      lt_sdr_mesg LIKE mesg OCCURS 0 WITH HEADER LINE,        "worklists
      ld_status TYPE kpep_wl_objstat,
      ld_count LIKE sy-tabix,
      ld_zeile TYPE msgzeile,
      off TYPE i VALUE 12,
      len TYPE i VALUE 10.

  PERFORM messages_count USING ld_count.

  WRITE ld_count TO ld_zeile+off(len).
  "worklists
CALL FUNCTION 'MESSAGES_GIVE'                                 "worklists
     EXPORTING                                                "worklists
          i_zeile      = ld_zeile                             "worklists
          i_incl_title = ' '                                  "worklists
     TABLES                                                   "worklists
          t_mesg       = lt_sdr_mesg.                         "worklists
  "worklists
  wl_objnr = l_objnr.                                     "worklists

  IF gd-processor    = 4                   "interest calculation
     AND wl_objnr(2) = objektart_pd.        "projects are always
    ld_status = kpepw_objstat-not_relevant. "not relevant
  ELSE.
    ld_status = id_status.
  ENDIF.

  IF ld_status = kpepw_objstat-error.
    CALL FUNCTION 'KPEP_WLA_SET_OBJECT_STATUS'          "worklists
         EXPORTING                                      "worklists
              object        = wl_objnr                  "worklists
              status        = ld_status                 "worklists
         TABLES                                         "worklists
              smsg_messages = lt_sdr_mesg.              "worklists
*                   CMFE_MESSAGES =                           "worklists
  ELSE.
    CALL FUNCTION 'KPEP_WLA_SET_OBJECT_STATUS'          "worklists
               EXPORTING                                      "worklists
                    object        = wl_objnr                  "worklists
                    status        = ld_status.                "worklists
  ENDIF.
ENDFORM.                                          "WL_OBJECT_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  WL_additional_status_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_OBJNR  text
*----------------------------------------------------------------------*
FORM wl_additional_status_check USING pt_objnr STRUCTURE objnr
                                      ld_subrc LIKE sy-subrc.
DATA: ld_vrgng LIKE rkauf-vrgng.                              "worklists
  ld_vrgng = rkauf-vrgng.                                 "worklists
DATA: wl_objnr TYPE jsto-objnr.                               "worklists

  DATA: ld_ident_objid LIKE sy-msgv1.
  DATA: ld_ident_txt20 LIKE  tbo01-txt20.

  wl_objnr = pt_objnr.
  CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
    EXPORTING
      objnr       = wl_objnr
    IMPORTING
      ident_objid = ld_ident_objid
      ident_txt20 = ld_ident_txt20.
  PERFORM message_line_add_1 USING gd-mesg_line.
  PERFORM message_default
                     USING gd-mesg_line
                           'KA' 'S' '159' ld_ident_txt20
                                          ld_ident_objid
                                          ' '  ' '.
* Deactivate message handler (otherwise messages are automatically
* stored and no exception is given)
  CALL FUNCTION 'MESSAGES_STOP'                               "worklist
       EXCEPTIONS                                             "worklist
            OTHERS = 0.                                       "worklist
  wl_objnr = pt_objnr.                                        "worklists
* Check if status allows activity
  CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'                   "worklist
       EXPORTING                                               "worklist
           check_only           = 'X'                         "worklist
           objnr                 = wl_objnr                    "worklist
           vrgng                = ld_vrgng                    "worklist
       EXCEPTIONS                                              "worklist
            activity_not_allowed = 1                           "worklist
            OTHERS               = 2.                          "worklist
  ld_subrc = sy-subrc.                                         "worklist

* Activate message handler again
  CALL FUNCTION 'MESSAGES_INITIALIZE'                         "worklists
       EXPORTING                                              "worklists
            reset            = space                          "worklists
            i_identification = gd-identification.
  "worklists* Write messages
  IF ld_subrc = 2.
    PERFORM message USING gd-max_error
                  sy-msgid sy-msgty sy-msgno
                  sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
  ENDIF.
ENDFORM.                    " WL_additional_status_check
