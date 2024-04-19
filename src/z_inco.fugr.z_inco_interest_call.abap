function Z_INCO_INTEREST_CALL.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_RKAUF) LIKE  RKAUF STRUCTURE  RKAUF
*"     REFERENCE(I_GD) TYPE  KAZB_GD
*"     REFERENCE(I_STAT) TYPE  KAZB_STAT_LOG
*"  EXPORTING
*"     REFERENCE(E_GD) TYPE  KAZB_GD
*"     REFERENCE(E_STAT) TYPE  KAZB_STAT_LOG
*"  TABLES
*"      I_OBJNR STRUCTURE  JSTO_PRE
*"      I_PROT TYPE  KAZB_PROT_TAB
*"      I_SEL_TEXT STRUCTURE  SEL_TEXT_VERZINSUNG
*"--------------------------------------------------------------------
  data:  ld_progress_all like sy-tabix,
         ld_val type schedman_ext_val,
         ld_subrc        like sy-subrc,
         ld_period  type to_p.                                  "842117

  clear: gt_intps, gt_intps[],         "protocol interest calc. (I)
         gt_intpb, gt_intpb[],         "protocol interest calc. (II)
         gt_rsthie[], gt_depobj_prot[],
         gt_index[],
         gt_failed_pd[].

* copy structures to global memory
  it_objnr[] = i_objnr[].
  gt_prot[]  = i_prot[].
  rkauf      = i_rkauf.
  gd         = i_gd.
  stat       = i_stat.
  sel_text[] = i_sel_text[].
  gd_todate  = rkauf-todate.

  perform set_todate
            changing
                rkauf.

* Start of note 842117
  ld_period = rkauf-periods - rkauf-anzsp.
  if gd_todate is initial and
     rkauf-to le ld_period.
    gd_todate = rkauf-todate.
  endif.
* End of note 842117

* check period blocking (CO)
  PERFORM INTEREST_PERIOD_BLOCKING_CHECK USING LD_SUBRC.
  CHECK LD_SUBRC IS INITIAL.

* check if worklists are active
  call function 'KPEP_WLA_CHECK_ACTIVE'
       importing
            is_active = gd-wl_active.

* get hierarchy
  perform get_hierarchie
                   tables
                      it_objnr
                      gt_rsthie
                   using
                      gd-selected.

* fill counter of processed objects
  describe table gt_rsthie lines ld_progress_all.

  if not rkauf-para is initial.
*   parallel processing
    perform interest_parallel.
  else.
*   sequential processing
    perform interest_sequentially
                      using
                         ld_progress_all.
  endif.

* close recording for SchedMan monitor
  if not gd-protocol is initial.
    perform save_extract_to_indx tables
                                       gt_depobj_prot
                                       gt_intprot
                                       gt_intps
                                       gt_intpb
                                 using ld_val.
    perform monitor_record_close using ld_val.
  endif.

*--------------------------------------------------------
* list results
*--------------------------------------------------------
  perform interest_list_results tables gt_depobj_prot
                                       gt_intprot
                                       gt_intps
                                       gt_intpb.

endfunction.
