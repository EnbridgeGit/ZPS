REPORT  zbpci007_cogactual MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for COG actuals for                  *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date     Issue  By      Description                                   *
*10/07/12   995  M Khan  Change C: to H: drive (Citrix Problem).       *
*01/13/14 61903  G Ymana Added two new activity types.                 *
*12/09/18  AKMADASU    Consider all records except if the amount equal *
*                      to ZERO                                         *
************************************************************************
TABLES: coep, coepr, ce11100, faglflext.

TYPES:  BEGIN OF ty_coep,
          perio   LIKE coep-perio,
          gjahr   LIKE coep-gjahr,
          bukrs   LIKE coep-bukrs,
          versn   LIKE coep-versn,
          wrttp   LIKE coep-wrttp,
          objnr   LIKE coep-objnr,
          kstar   LIKE coep-kstar,
          wogbtr  LIKE coep-wogbtr,
          belnr   LIKE coep-belnr,
          kokrs   LIKE coep-kokrs,
          del_rec TYPE char01,
        END OF ty_coep,

         BEGIN OF ty_coepr,
          perio   LIKE coepr-perio,
          gjahr   LIKE coepr-gjahr,
          versn   LIKE coepr-versn,
          wrttp   LIKE coepr-wrttp,
          objnr   LIKE coepr-objnr,
          stagr   LIKE coepr-stagr,
          smebtr  LIKE coepr-smebtr,
        END OF ty_coepr,

         BEGIN OF ty_coepl,
          perio   LIKE coepl-perio,
          gjahr   LIKE coepl-gjahr,
          versn   LIKE coepl-versn,
          wrttp   LIKE coepl-wrttp,
          objnr   LIKE coepl-objnr,
          lstbtr  LIKE coepl-lstbtr,
        END OF ty_coepl.


DATA: lv_local      TYPE integer,
      lv_perio      TYPE ce11100-perio,
      wa_coep       LIKE coep,
      s_coep        TYPE ty_coep,
      t_cobk        TYPE TABLE OF cobk,
      s_cobk        TYPE cobk,
      t_coep        LIKE TABLE OF s_coep,
      t_coep_sum    LIKE TABLE OF s_coep,
      s_ce11100     LIKE ce11100,
      t_ce11100     LIKE TABLE OF s_ce11100,
      t_ce11100_sum LIKE TABLE OF s_ce11100,
      s_coepr       TYPE ty_coepr,
      t_coepr       LIKE TABLE OF s_coepr,
      t_coepr_sum   LIKE TABLE OF s_coepr,
      s_coepl       TYPE ty_coepl,
      t_coepl       LIKE TABLE OF s_coepl,
      t_coepl_sum   LIKE TABLE OF s_coepl,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec.


DATA: msg(80)           TYPE c,
      lv_account(11)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(9)     TYPE c,
      lv_entity(15)     TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(11)   TYPE c,
      lv_dso(6)         TYPE c,
      lv_service(10)    TYPE c,
      lv_paths(11)      TYPE c,
      lv_projectst(12)  TYPE c,
      lv_amount(20)     TYPE c.

.

DATA: tuchfile          LIKE rfpdo-rfbifile.


CONSTANTS: delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.


PARAMETERS:
p_perio   LIKE ce11100-perio OBLIGATORY.

SELECT-OPTIONS:
s_objnr   FOR coep-objnr    OBLIGATORY,
s_wrttp   FOR coep-wrttp    OBLIGATORY DEFAULT '4',
s_vrgar   FOR ce11100-vrgar OBLIGATORY,
s_kstar   FOR coep-kstar,
s_versn   FOR coep-versn,
s_stagr   FOR coepr-stagr.
SELECTION-SCREEN END OF BLOCK a1.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\COGActual.csv',
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.
SELECTION-SCREEN END OF BLOCK b1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/' INTO csvfile.


*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  IF p_local = 'X'.
    PERFORM check_file_path.
  ENDIF.
*End of TR995 changes
*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' p_perio(4) '-' p_perio+5(2) '-COG.csv' INTO csvfile.

  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.


  PERFORM get_db_data.
  PERFORM sumarize_data.
  PERFORM print_report.
  IF lv_local = 0.
    PERFORM create_touch_file.
  ENDIF.


*----------------------------------------------------------------------*
FORM get_db_data.

  CLEAR: t_cobk.

  SELECT perio objnr gjahr wrttp kstar versn wogbtr bukrs
         belnr kokrs
    INTO CORRESPONDING FIELDS OF TABLE t_coep
    FROM coep
    WHERE perio =  p_perio+4(3)
      AND objnr IN s_objnr
      AND gjahr =  p_perio(4)
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND versn IN s_versn.
  IF t_coep[] IS NOT INITIAL.
    SELECT * FROM cobk INTO TABLE t_cobk
      FOR ALL ENTRIES IN t_coep
      WHERE kokrs = t_coep-kokrs
        AND belnr = t_coep-belnr.
  ENDIF.

  SELECT perio objnr gjahr wrttp stagr versn smebtr
    INTO CORRESPONDING FIELDS OF TABLE t_coepr
    FROM coepr
    WHERE perio = p_perio+4(3)
      AND gjahr = p_perio(4)
      AND stagr IN s_stagr
  .

  SELECT perio objnr gjahr wrttp versn lstbtr
    INTO CORRESPONDING FIELDS OF TABLE t_coepl
    FROM coepl
    WHERE perio = p_perio+4(3)
      AND gjahr = p_perio(4)
  .

  SELECT gjahr perde werks  wwrat wwser wwprg wwsld vvcgg vvalb vvtrc vvtrd vvtrf
    INTO CORRESPONDING FIELDS OF TABLE t_ce11100
    FROM ce11100
    WHERE paledger  = '01'
      AND vrgar     IN s_vrgar
      AND versi     = ''
      AND perio     = p_perio
      AND artnr     = 'NATGAS'
      AND wwprg     = 'SY'
  .
ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.

  CONSTANTS: lc_390686 TYPE coep-kstar VALUE '0000390686'.
  DATA: ls_cobk TYPE cobk.

  SORT t_coep ASCENDING BY gjahr perio kstar.

  LOOP AT t_coep INTO s_coep.
    IF s_coep-kstar = lc_390686.
      CLEAR ls_cobk.
      READ TABLE t_cobk INTO ls_cobk WITH KEY kokrs = s_coep-kokrs
                                              belnr = s_coep-belnr.
      IF ls_cobk-blart = 'WE'.
        s_coep-del_rec = 'X'.
        MODIFY t_coep FROM s_coep TRANSPORTING del_rec.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE t_coep WHERE del_rec = 'X'.
  SORT t_coep ASCENDING BY gjahr perio kstar.
  LOOP AT t_coep INTO s_coep.
*     IF s_coep-kstar = lc_390686.
*       CLEAR ls_cobk.
*       READ TABLE t_cobk INTO ls_cobk with key kokrs = s_coep-kokrs
*                                               belnr = s_coep-belnr.
*        CASE ls_cobk-blart.
*            WHEN 'WE'.
*              CONTINUE.
*            WHEN OTHERS.
*              "Do Nothing.
*        ENDCASE.
*      ENDIF.
*Last sorted field is kstar
    AT END OF kstar.
      SUM.
      APPEND s_coep TO t_coep_sum.
    ENDAT.
  ENDLOOP.

  SORT t_coepr ASCENDING BY gjahr perio stagr.

  LOOP AT t_coepr INTO s_coepr.
    AT END OF stagr.
      SUM.
      APPEND s_coepr TO t_coepr_sum.
    ENDAT.
  ENDLOOP.

  SORT t_coepl ASCENDING BY gjahr perio objnr.

  LOOP AT t_coepl INTO s_coepl.
    AT END OF objnr.
      SUM.
      APPEND s_coepl TO t_coepl_sum.
    ENDAT.
  ENDLOOP.

  SORT t_ce11100 ASCENDING BY gjahr perde werks wwrat wwser wwprg wwsld.

  LOOP AT t_ce11100 INTO s_ce11100.
*Last sorted field is wwsld
    AT END OF wwsld.
      SUM.
      APPEND s_ce11100 TO t_ce11100_sum.
    ENDAT.
  ENDLOOP.


ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.

  CONSTANTS: lc_390686 TYPE coep-kstar VALUE '0000390686'.

  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011 text-012 text-013
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.



*Set the hard-coded values.

  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_COG'.
  lv_rptcurrency  = 'LC'.
  lv_customer     = 'No_Customer'.
  lv_dso          = 'No_DSO'.
  lv_service      = 'No_Service'.
  lv_projectst    = 'No_ProjectST'.


  DATA: lv_poski(16) TYPE c.

  LOOP AT t_coep_sum INTO s_coep.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_paths, lv_intco, lv_time, lv_amount.

    lv_account = s_coep-kstar+4.

    IF s_coep-objnr(2) = 'KS' OR s_coep-objnr(2) = 'KL'.
      lv_entity = s_coep-objnr+6(10).
      lv_paths  = 'No_COGPaths'.
      SHIFT lv_entity LEFT DELETING LEADING '0'.
    ELSEIF s_coep-objnr(2) = 'OR'.
      lv_entity = 'UGL'.
      CONCATENATE 'IO_' s_coep-objnr+8 INTO lv_paths.
      SHIFT lv_paths LEFT DELETING LEADING '0'.
    ELSE.
      lv_entity = 'UGL'.
      lv_paths  = 'No_COGPaths'.
    ENDIF.
    IF s_coep-kstar = lc_390686.
      lv_customer  = 'COGYCR'.
      lv_service   = 'YCR_Serv'.
      lv_paths     = 'No_PATHS'.
    ELSE.
      lv_customer     = 'No_Customer'.
      lv_service      = 'No_Service'.
    ENDIF.

    SELECT SINGLE affil
      INTO lv_intco
      FROM zacctnew
      WHERE cocode  = s_coep-bukrs
        AND glacct  = s_coep-kstar
      .

    IF lv_intco = ''.
      SELECT SINGLE affil
        INTO lv_intco
        FROM zacctnew
        WHERE cocode  = ''
          AND glacct  = s_coep-kstar
        .
      IF lv_intco = ''.
        lv_intco = 'No_IntCo'.
      ELSE.
        CONCATENATE 'IC_' lv_intco INTO lv_intco.
      ENDIF.
    ELSE.
      CONCATENATE 'IC_' lv_intco INTO lv_intco.
    ENDIF.

    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO lv_time.

    IF s_coep-wogbtr < 0.
      "If negative convert to positive and add '-' infront.
      s_coep-wogbtr = s_coep-wogbtr * -1.
      lv_amount = s_coep-wogbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_coep-wogbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.


    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY delimtr.

    IF s_coep-wogbtr <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.

  ENDLOOP.


  LOOP AT t_coepr_sum INTO s_coepr.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_paths, lv_intco, lv_time, lv_amount.

    lv_rptcurrency  = 'GJ'.
    CASE s_coepr-stagr.
      WHEN 'SPPVAC'.
        lv_account = 'COG_SUPPLY'. "'SPPVAC'.
      WHEN 'NPPVAC'.
        lv_account = 'COG_SUPPLY'. "'NPPVAC'.
      WHEN OTHERS.
        IF S_COEPR-OBJNR+11(2) = '78'.   "Fuel cost center
          lv_account = 'SKF'.
        ELSE.
          lv_account = 'MISADJ'.
        ENDIF.
    ENDCASE.

    IF s_coepr-objnr(2) = 'KS' OR s_coepr-objnr(2) = 'KL'.
      lv_entity = s_coepr-objnr+6(10).
      lv_paths  = 'No_COGPaths'.
      SHIFT lv_entity LEFT DELETING LEADING '0'.
    ELSEIF s_coepr-objnr(2) = 'OR'.
      lv_entity = 'UGL'.
      CONCATENATE 'IO_' s_coepr-objnr+8 INTO lv_paths.
      SHIFT lv_paths LEFT DELETING LEADING '0'.
    ELSE.
      lv_entity = 'UGL'.
      lv_paths  = 'No_COGPaths'.
    ENDIF.


    lv_intco = 'No_IntCo'.


    CONCATENATE s_coepr-gjahr s_coepr-perio+1(2) '00' INTO lv_time.

    IF s_coepr-smebtr < 0.
      "If negative convert to positive and add '-' infront.
      s_coepr-smebtr = s_coepr-smebtr * -1.
      lv_amount = s_coepr-smebtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_coepr-smebtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.


    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY delimtr.

    IF s_coepr-smebtr <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.

  ENDLOOP.

  LOOP AT t_coepl_sum INTO s_coepl.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_paths, lv_intco, lv_time, lv_amount.

    lv_rptcurrency  = 'GJ'.
    CASE s_coepl-objnr+16(6).
      WHEN 'WCGDMD'.
        lv_account = 'WCGDMD'.
      WHEN 'WCGCMM'.
        lv_account = 'WCGCMM'.
      WHEN 'WCGTFL'.
        lv_account = 'WCGTFL'.
      WHEN 'STSINC'.
        lv_account = 'STSINC'.
      WHEN 'STSIND'.
        lv_account = 'STSIND'.
      WHEN 'STSWDD'.
        lv_account = 'STSWDD'.
      WHEN 'STSWDC'.
        lv_account = 'STSWDC'.
      WHEN 'BTCONS'.                                        "SDP61903
        lv_account = 'BTCONS'.                              "SDP61903
      WHEN 'BTDELV'.                                        "SDP61903
        lv_account = 'BTDELV'.                              "SDP61903
      WHEN OTHERS.
        "Ignore these items
        CONTINUE.
    ENDCASE.

    IF s_coepl-objnr(2) = 'KS' OR s_coepl-objnr(2) = 'KL'.
      lv_entity = s_coepl-objnr+6(10).
      lv_paths  = 'No_COGPaths'.
      SHIFT lv_entity LEFT DELETING LEADING '0'.
    ELSEIF s_coepl-objnr(2) = 'OR'.
      lv_entity = 'UGL'.
      CONCATENATE 'IO_' s_coepl-objnr+8 INTO lv_paths.
      SHIFT lv_paths LEFT DELETING LEADING '0'.
    ELSE.
      lv_entity = 'UGL'.
      lv_paths  = 'No_COGPaths'.
    ENDIF.


    lv_intco = 'No_IntCo'.


    CONCATENATE s_coepl-gjahr s_coepl-perio+1(2) '00' INTO lv_time.

    IF s_coepl-lstbtr < 0.
      "If negative convert to positive and add '-' infront.
      s_coepl-lstbtr = s_coepl-lstbtr * -1.
      lv_amount = s_coepl-lstbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_coepl-lstbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.


    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY delimtr.

    IF s_coepl-lstbtr <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.

  ENDLOOP.
************************
** Append CE111100 Data.
************************
*Set the hard-coded values.
  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_COG'.
  lv_entity       = 'UGL'.
  lv_intco        = 'No_IntCo'.
  lv_rptcurrency  = 'LC'.
  "lv_customer     = 'No_Customer'.
  "lv_dso          = 'No_DSO'.
  "lv_service      = 'No_Service'.
  "lv_paths        = 'No_COGPaths'.
  lv_projectst    = 'No_ProjectST'.



  LOOP AT t_ce11100_sum INTO s_ce11100.

    CLEAR: lv_account, lv_time, lv_customer, lv_dso, lv_service, lv_paths, lv_amount.

    CONCATENATE s_ce11100-gjahr s_ce11100-perde+1(2) '00' INTO lv_time.

    "Account 302002/302003
    IF s_ce11100-werks = 'GSTH' OR s_ce11100-werks = 'GNTH'.
      "Logic from manual extract
      CASE s_ce11100-wwrat.
        WHEN '1' OR '01' OR '10' OR 'M1' OR 'M2'.
          CASE s_ce11100-wwser.
            WHEN 'CM' OR 'IN' OR 'RS' OR 'TB'.
              lv_customer = s_ce11100-wwser.
            WHEN OTHERS.
              lv_customer = 'No_Customer_GS'.
          ENDCASE.
        WHEN OTHERS.
          CASE s_ce11100-wwser.
            WHEN ''.
              lv_customer = 'No_Customer_GS'.
            WHEN OTHERS.
              lv_customer = 'No_Customer_Cont'.
          ENDCASE.
      ENDCASE.
    ELSE.
      lv_customer = s_ce11100-wwser.
    ENDIF.

    lv_dso = s_ce11100-wwprg.

    CASE s_ce11100-wwrat.
      WHEN '01' OR '1'.
        lv_service = 'R01'.
      WHEN '10' OR '20' OR '25' OR '30' OR '100'.
        CONCATENATE 'R' s_ce11100-wwrat INTO lv_service.
        CONDENSE lv_service NO-GAPS.
*        lv_service = 'R10'.
      WHEN ''.
        lv_service = 'No_Service_GS'.
      WHEN 'M5A'.
        lv_service = 'M5'.
      WHEN OTHERS.
        lv_service = s_ce11100-wwrat.
    ENDCASE.

    CONCATENATE 'DA_' s_ce11100-wwsld INTO lv_paths.

    IF s_ce11100-werks = 'GSTH'.
      lv_account = '302002'.
      lv_entity  = '77550'.
    ELSEIF s_ce11100-werks = 'GNTH'.
      lv_account = '302003'.
      lv_entity  = '77552'.
    ELSEIF s_ce11100-werks = 'GNTE'.
      lv_account = '302006'.
      lv_entity  = '77551'.
    ELSE.
      CONCATENATE 'UNK_' s_ce11100-werks INTO lv_account.
    ENDIF.

    IF s_ce11100-vvcgg < 0.
      "If negative convert to positive and add '-' infront.
      s_ce11100-vvcgg = s_ce11100-vvcgg * -1.
      lv_amount = s_ce11100-vvcgg.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_ce11100-vvcgg.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.

    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY delimtr.
**-- START OF CHNAGES BY AKMADASU CHG0123406
*    IF lv_amount <> 0.
    IF lv_amount EQ '0.00' OR lv_amount eq '0'.
      " DO NOTHING
    ELSE.
**-- END OF CHNAGES BY AKMADASU CHG0123406
      APPEND st_datarec TO t_data.
    ENDIF.



*    "ADD ALB
*    lv_account = 'ALB_BRD_PRC'.
*    IF s_ce11100-vvalb < 0.
*      "If negative convert to positive and add '-' infront.
*      s_ce11100-vvalb = s_ce11100-vvalb * -1.
*      lv_amount = s_ce11100-vvalb.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*      CONCATENATE '-' lv_amount INTO lv_amount.
*    ELSE.
*      lv_amount = s_ce11100-vvalb.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*    ENDIF.
*
*    CONCATENATE lv_account lv_category lv_datasrc lv_entity
*                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
*                lv_service lv_paths lv_projectst lv_amount
*                INTO st_datarec SEPARATED BY delimtr.
*
*    if lv_amount <> 0.
*      APPEND st_datarec TO t_data.
*    endif.
*
*
*
*    "ADD DMND
*    lv_account = 'TRAN_DMND'.
*    IF s_ce11100-vvtrc < 0.
*      "If negative convert to positive and add '-' infront.
*      s_ce11100-vvtrc = s_ce11100-vvtrc * -1.
*      lv_amount = s_ce11100-vvtrc.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*      CONCATENATE '-' lv_amount INTO lv_amount.
*    ELSE.
*      lv_amount = s_ce11100-vvtrc.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*    ENDIF.
*
*    CONCATENATE lv_account lv_category lv_datasrc lv_entity
*                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
*                lv_service lv_paths lv_projectst lv_amount
*                INTO st_datarec SEPARATED BY delimtr.
*
*    if lv_amount <> 0.
*      APPEND st_datarec TO t_data.
*    endif.
*
*
*
*    "ADD COMM
*    lv_account = 'TRN_COMM'.
*    IF s_ce11100-vvtrd < 0.
*      "If negative convert to positive and add '-' infront.
*      s_ce11100-vvtrd = s_ce11100-vvtrd * -1.
*      lv_amount = s_ce11100-vvtrd.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*      CONCATENATE '-' lv_amount INTO lv_amount.
*    ELSE.
*      lv_amount = s_ce11100-vvtrd.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*    ENDIF.
*
*    CONCATENATE lv_account lv_category lv_datasrc lv_entity
*                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
*                lv_service lv_paths lv_projectst lv_amount
*                INTO st_datarec SEPARATED BY delimtr.
*
*    if lv_amount <> 0.
*      APPEND st_datarec TO t_data.
*    endif.
*
*
*
*    "ADD FUEL
*    lv_account = 'TRN_FUEL'.
*    IF s_ce11100-vvtrf < 0.
*      "If negative convert to positive and add '-' infront.
*      s_ce11100-vvtrf = s_ce11100-vvtrf * -1.
*      lv_amount = s_ce11100-vvtrf.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*      CONCATENATE '-' lv_amount INTO lv_amount.
*    ELSE.
*      lv_amount = s_ce11100-vvtrf.
*      SHIFT lv_amount LEFT DELETING LEADING ' '.
*    ENDIF.
*
*    CONCATENATE lv_account lv_category lv_datasrc lv_entity
*                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
*                lv_service lv_paths lv_projectst lv_amount
*                INTO st_datarec SEPARATED BY delimtr.
*
*    if lv_amount <> 0.
*      APPEND st_datarec TO t_data.
*    endif.

  ENDLOOP.


  IF lv_local = 0.

    PERFORM open_csvfile.

    LOOP AT t_data INTO st_datarec.
      TRANSFER st_datarec TO csvfile.
    ENDLOOP.

    PERFORM close_csvfile.
    WRITE: 'File Outputed Successfully to: ', csvfile.

  ELSE.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_file
      TABLES
        data_tab                = t_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: 'File Outputed Successfully to: ', p_file.
  ENDIF.



ENDFORM.                    "print_report



*----------------------------------------------------------------------*
FORM open_csvfile.
  OPEN DATASET csvfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "OPEN_CSVFILE


*----------------------------------------------------------------------*
FORM close_csvfile.
  CLOSE DATASET csvfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES


*&---------------------------------------------------------------------*
*Create Touch File.
*----------------------------------------------------------------------*
FORM create_touch_file.
  OPEN DATASET tuchfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH tuchfile msg.
    STOP.
  ENDIF.

  TRANSFER 'This is a touch file' TO tuchfile.

  CLOSE DATASET tuchfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' tuchfile msg.
    STOP.
  ENDIF.

ENDFORM.                    "CREATE_TOUCH_FILE

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
