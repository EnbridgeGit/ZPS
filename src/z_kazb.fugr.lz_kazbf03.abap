*-------------------------------------------------------------------
***INCLUDE LKAZBF03 .
*-------------------------------------------------------------------
* changing history
* ----------------
* 4.6A
* SCT ALRK136242 031198 accrual calculation connected
* ----------------------------------------------------------------------
* PL0K016206: Note 505841 21.03.2002 FCL

*&---------------------------------------------------------------------*
*&      Form  LIST_RESULTS
*&---------------------------------------------------------------------*
*       show list
*----------------------------------------------------------------------*
FORM list_results TABLES p_xcoep    STRUCTURE coep
                         p_xcoeja   STRUCTURE coeja
                         p_xcoaib   STRUCTURE coaib
                  USING  p_rkauf    STRUCTURE rkauf
                         p_gd       STRUCTURE gd
                         p_tka01    STRUCTURE tka01
                         p_tka07    STRUCTURE tka07         "ALRK136242
                         p_tkt09    STRUCTURE tkt09
                         p_stat     STRUCTURE stat.

* DATA: LD_STAT       LIKE  STAT.
  DATA: ld_processed  TYPE  i.
  DATA: ld_count      LIKE  sy-tabix.

  DATA: l_belnr_ab    LIKE  cobk-belnr,
        l_belnr_bis   LIKE  cobk-belnr.
  DATA: L_CON_VERSN_000  LIKE   TKA09-VERSN VALUE '000'.  "<<<note505841

*----------------------------------------------------------------------*
  p_gd-list = true.

*----------------------------------------------------------------------*
* fill statistics information
*----------------------------------------------------------------------*
  IF p_rkauf-dialog NE true.                     "dialog diasplay is off
    ld_processed = p_gd-processed - p_stat-kalsm.
  ELSE.                                            "dialog display is on
    IF NOT p_stat IS INITIAL.
      ld_processed = 0.               "Status incorrect or KALSM missing
    ELSE.
      IF p_gd-processed = 0.                              "Error occured
        ld_processed =  0.
        p_gd-mesg_line = 1.
      ELSE.
        ld_processed =  1.                         "one object processed
        p_gd-mesg_line = 1.                            "no error occured
      ENDIF.
    ENDIF.
   IF NOT p_gd-processed = 0.      "Dialog Display shows one object max.
      p_gd-processed = p_gd-processed - 1.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'MESSAGES_COUNT'
    EXPORTING
      only_for_actual_line = 'X'
    IMPORTING
      count                = ld_count.

* if no error message is found for the last object
  IF ld_count EQ 0.
    IF  p_gd-mesg_line NE p_gd-mesg_line_sav.
*      AND NOT RKAUF-DIALOG EQ TRUE.
      p_gd-mesg_line = p_gd-mesg_line - 1.
      p_gd-mesg_line_sav = p_gd-mesg_line.
*     LD_PROCESSED =  LD_PROCESSED -  P_GD-MESG_LINE.   "<<<note318961

    ENDIF.
  ENDIF.

* restrict detail list of revaluation to version 0
  IF gd-processor = 3.                                 "<<<note505841
    LOOP AT p_xcoeja WHERE versn <> l_con_versn_000.   "<<<note505841
      DELETE p_xcoeja.                                 "<<<note505841
    ENDLOOP.                                           "<<<note505841
  ENDIF.                                               "<<<note505841


  CALL FUNCTION 'KALV_CO_OUTPUT_LIST'
    EXPORTING
      i_selected     = p_gd-selected
      i_processed    = ld_processed
      i_mesg_line    = p_gd-mesg_line
      i_erof         = p_stat-erof
      i_abgs         = p_stat-abgs
      i_tabg         = p_stat-tabg
      i_sal_0        = p_stat-sal_0                         "balance 0
      i_other_status = p_stat-other_status          "worklists
      i_lovm         = p_stat-lovm
      i_lokz         = p_stat-lokz
      i_kalsm        = p_stat-kalsm
      i_stat         = p_stat-stat
      i_rkauf        = p_rkauf
      i_obart        = p_gd-obart
      i_obart_txt    = p_gd-obart_txt
      i_processor    = p_gd-processor
      i_belnr_ab     = l_belnr_ab
      i_belnr_bis    = l_belnr_bis
      i_repid        = gd-repid_btc
      i_schedman_key = gd-key
      i_tka01        = p_tka01
      i_tka07        = p_tka07
      i_tkt09        = p_tkt09
    TABLES
      i_coep         = p_xcoep
      i_coeja        = p_xcoeja
      i_coaib        = p_xcoaib.

  CLEAR p_stat.                                                    "RR
ENDFORM.                               " LIST_RESULTS

*&---------------------------------------------------------------------*
*&      Form  FILL_STATISTICS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_statistics USING p_stat   LIKE stat
                           p_objnr  LIKE coss-objnr .

  READ TABLE gt_prot WITH KEY objnr = p_objnr.

  IF sy-subrc EQ 0.
    IF gt_prot-statistical EQ true.
      p_stat-stat = 1.
    ELSE.
      CASE gt_prot-status.
        WHEN 'EROF'.
          p_stat-erof = 1.
        WHEN 'ABGS'.
          p_stat-abgs = 1.
        WHEN 'TABG'.
          p_stat-tabg = 1.
        WHEN 'LOVM'.
          p_stat-lovm = 1.
        WHEN 'LOKZ'.
          p_stat-lokz = 1.
        WHEN 'OTHS'.                                          "worklists
          p_stat-other_status = 1.                            "worklists
        WHEN OTHERS.
          IF gt_prot-no_kalsm EQ true.
            p_stat-kalsm = 1.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.                               " FILL_STATISTICS
