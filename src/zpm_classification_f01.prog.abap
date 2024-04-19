*&---------------------------------------------------------------------*
*&  Include           ZPM_CLASSIFICATION_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  CLEAR : gt_kssk,gt_tplnr,gt_equnr,gt_iflo,gt_vequi,gt_final,gt_cabn.
  CLEAR : gs_final,gt_status,gs_status,gs_cabn.
  DATA  : lv_objek TYPE equnr.
** Begin of changes - D30K932262 for REDIGDS
  DATA: lt_usrstat TYPE TABLE OF bapi_itob_status,
        lt_sysstat TYPE TABLE OF bapi_itob_status,
        ls_status TYPE bapi_itob_status,
        ls_status1 TYPE bapi_itob_status,
        ls_objek TYPE gty_objnr-objek,
        lv_equip TYPE bapi_itob_parms-equipment,
        lv_atflv_i TYPE f,                        " D30K932308 for REDIGDS
        lv_atflv_o(16) TYPE c,
        lv_in TYPE string VALUE 'INLET_MOP_1',
        lv_on TYPE string VALUE 'OUTLET_MOP_1'.
  " D30K932308 for REDIGDS
** End of changes - D30K932262 for REDIGDS
  IF p_eqp IS NOT INITIAL.
    LOOP AT s_objek.
      CLEAR : lv_objek.
      lv_objek = s_objek-low.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_objek
        IMPORTING
          output = lv_objek.
      s_objek-low = lv_objek.
      MODIFY s_objek INDEX sy-tabix TRANSPORTING low.
    ENDLOOP.
  ENDIF.

* Begin of changes - D30K932308 for REDIGDS
* Fetching data from table cabn ausp kssk_inob.
  SELECT a~atnam a~atfor a~msehi b~objek b~atflv c~inob_objek FROM cabn AS a
   INNER JOIN ausp AS b ON b~atinn = a~atinn
    INNER JOIN kssk_inob AS c ON c~objek = b~objek
    INTO TABLE gt_cabn WHERE a~atnam IN s_atnam AND b~klart = p_klart
     AND c~inob_objek IN s_objek AND atfor = 'NUM'.
  IF sy-subrc = 0.
    DELETE gt_cabn WHERE atfor NE 'NUM'.
    SORT gt_cabn BY objek inob_objek.
    DELETE ADJACENT DUPLICATES FROM gt_cabn COMPARING objek inob_objek.
  ENDIF.
* End of changes - D30K932308 for REDIGDS
  SELECT a~atnam b~objek b~atwrt c~inob_objek FROM cabn AS a
  INNER JOIN ausp AS b ON b~atinn = a~atinn
  INNER JOIN kssk_inob AS c ON c~objek = b~objek
    INTO TABLE gt_kssk
    WHERE a~atnam IN s_atnam AND
          b~klart = p_klart AND
          c~inob_objek IN s_objek.

  IF gt_kssk IS NOT INITIAL.
    MOVE gt_kssk TO gt_tplnr.
    MOVE gt_kssk TO gt_tplnr1.
    MOVE gt_kssk TO gt_equnr.
    MOVE gt_kssk TO gt_equnr1.
    IF p_fun IS NOT INITIAL.
      SORT gt_tplnr1 BY inob_objek.
      DELETE ADJACENT DUPLICATES FROM gt_tplnr1 COMPARING inob_objek.

      SELECT tplnr objnr eqart FROM iflo INTO TABLE gt_iflo
        FOR ALL ENTRIES IN gt_tplnr1
        WHERE tplnr = gt_tplnr1-inob_objek.
      IF gt_iflo IS NOT INITIAL.
        LOOP AT gt_tplnr INTO gs_tplnr.
          READ TABLE gt_tplnr1 INTO gs_tplnr1 WITH KEY inob_objek = gs_tplnr-inob_objek.
          SORT gt_iflo BY inob_objek.
          READ TABLE gt_iflo INTO gs_iflo
               WITH KEY inob_objek = gs_tplnr-inob_objek BINARY SEARCH.

          IF sy-subrc = 0.
            gs_objnr-objnr = gs_iflo-objnr.
            gs_objnr-objek = gs_iflo-inob_objek.
            gs_objnr-atnam = gs_tplnr-atnam.
            gs_objnr-atwrt = gs_tplnr-atwrt.
            gs_objnr-eqart = gs_iflo-eqart.
            APPEND gs_objnr TO gt_objnr.
            CLEAR : gs_objnr,gs_tplnr.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      SORT gt_equnr1 BY inob_objek.
      DELETE ADJACENT DUPLICATES FROM gt_equnr1 COMPARING inob_objek.
      SELECT equnr eqart objnr FROM v_equi INTO TABLE gt_vequi
      FOR ALL ENTRIES IN gt_equnr1
      WHERE equnr = gt_equnr1-inob_objek AND
            datbi = '99991231'.
      IF gt_vequi IS NOT INITIAL.
        LOOP AT gt_equnr INTO gs_equnr.
          SORT gt_vequi BY inob_objek.
          READ TABLE gt_vequi INTO gs_vequi
               WITH KEY inob_objek = gs_equnr-inob_objek BINARY SEARCH.
          IF sy-subrc = 0.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = gs_vequi-inob_objek
              IMPORTING
                output = gs_vequi-inob_objek.
            gs_objnr-objnr = gs_vequi-objnr.
            gs_objnr-objek = gs_vequi-inob_objek.
            gs_objnr-atnam = gs_equnr-atnam.
            gs_objnr-atwrt = gs_equnr-atwrt.
            gs_objnr-eqart = gs_vequi-eqart.
            APPEND gs_objnr TO gt_objnr.
            CLEAR : gs_objnr,gs_equnr.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  SORT gt_objnr BY objnr objek atnam atwrt.

  LOOP AT gt_objnr INTO gs_objnr.
    CLEAR : gt_status,gs_status.
    ls_objek = gs_objnr-objek.
    lv_equip = gs_objnr-objek.
    AT NEW objnr.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          objnr       = gs_objnr-objnr
          only_active = 'X'
        TABLES
          status      = gt_status.

      READ TABLE gt_status INTO gs_status WITH KEY stat = gc_i0320.
      IF sy-subrc = 0.
        DELETE gt_objnr WHERE objnr = gs_objnr-objnr.
        CONTINUE.
      ELSE.
        READ TABLE gt_status INTO gs_status WITH KEY stat = gc_i1105.
        IF sy-subrc = 0.
          DELETE gt_objnr WHERE objnr = gs_objnr-objnr.
          CONTINUE.
        ELSE.
          READ TABLE gt_status INTO gs_status WITH KEY stat = gc_i0076.
          IF sy-subrc = 0.
            DELETE gt_objnr WHERE objnr = gs_objnr-objnr.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
** Begin of changes - D30K932262 for REDIGDS
      CLEAR: ls_status, ls_status1.
      IF p_fun IS NOT INITIAL.
        CALL FUNCTION 'BAPI_FUNCLOC_GETSTATUS'
          EXPORTING
            functlocation = ls_objek
          TABLES
            system_status = lt_sysstat
            user_status   = lt_usrstat.
        IF sy-subrc = 0 AND
         ( lt_usrstat IS NOT INITIAL OR lt_sysstat IS NOT INITIAL ).
          READ TABLE lt_usrstat INTO ls_status INDEX 1.
          IF sy-subrc <> 0.
            CLEAR ls_status.
          ENDIF.
          READ TABLE lt_sysstat INTO ls_status1 INDEX 1.
          IF sy-subrc <> 0.
            CLEAR ls_status1.
          ENDIF.
        ENDIF.
      ELSEIF p_eqp IS NOT INITIAL.
        CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
          EXPORTING
            equipment     = lv_equip
          TABLES
            system_status = lt_sysstat
            user_status   = lt_usrstat.
        IF sy-subrc = 0 AND
         ( lt_usrstat IS NOT INITIAL OR lt_sysstat IS NOT INITIAL ).
          READ TABLE lt_usrstat INTO ls_status INDEX 1.
          IF sy-subrc <> 0.
            CLEAR ls_status.
          ENDIF.
          READ TABLE lt_sysstat INTO ls_status1 INDEX 1.
          IF sy-subrc <> 0.
            CLEAR ls_status1.
          ENDIF.
        ENDIF.
      ENDIF.
** End of changes - D30K932262 for REDIGDS
    ENDAT.

    MOVE-CORRESPONDING gs_objnr TO gs_final.
*    Begin of changes - D30K932308 for REDIGDS
    READ TABLE gt_cabn INTO gs_cabn WITH KEY inob_objek = gs_objnr-objek.
    IF sy-subrc = 0.
      lv_atflv_i = gs_cabn-atflv.
      CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
        EXPORTING
          i_number_of_digits = 2
          i_fltp_value       = lv_atflv_i
        IMPORTING
          e_char_field       = lv_atflv_o.
      CONDENSE lv_atflv_o.
      IF gs_cabn-atnam = lv_in.
        CONCATENATE lv_atflv_o gs_cabn-msehi INTO gs_final-inlet_mop_1 SEPARATED BY space .
      ELSEIF gs_cabn-atnam = lv_on.
        CONCATENATE lv_atflv_o gs_cabn-msehi INTO gs_final-outlet_mop_1 SEPARATED BY space .
      ENDIF.
    ENDIF.
*    End of changes - D30K932308 for REDIGDS

* Begin of changes - D30K932262 for REDIGDS
    gs_final-sstat = ls_status1-text.
    gs_final-ustat = ls_status-text.

* End of changes - D30K932262 for REDIGDS
    APPEND gs_final TO gt_final.

    CLEAR : gs_final,gs_cabn.

  ENDLOOP.

  SORT gt_final BY objek atnam atwrt.
  IF gt_final IS INITIAL.
    MESSAGE text-t01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .

  CLEAR : gr_stat[],gs_stat.

  gs_stat-sign = 'I'.
  gs_stat-option = 'EQ'.
  gs_stat-low = 'I0320'.
  APPEND gs_stat TO gr_stat.
  CLEAR : gs_stat.

  gs_stat-sign = 'I'.
  gs_stat-option = 'EQ'.
  gs_stat-low = 'I1105'.
  APPEND gs_stat TO gr_stat.
  CLEAR : gs_stat.

  gs_stat-sign = 'I'.
  gs_stat-option = 'EQ'.
  gs_stat-low = 'I0076'.
  APPEND gs_stat TO gr_stat.
  CLEAR : gs_stat.

  CONCATENATE text-t05 sy-sysid text-t06 '.csv' INTO p_path.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .

  DATA : lv_name TYPE string,
         lv_pos TYPE i,
         lv_count TYPE char2.
  CLEAR : gv_count,gv_pos,gt_dyn_fcat,gs_dyn_fcat,lv_count.
  CLEAR : lv_pos,gt_alv_fieldcat,gs_alv_fieldcat,gt_temp.

  gv_pos = gv_pos + 1.
  gs_dyn_fcat-fieldname = 'OBJEK'.
  gs_dyn_fcat-outputlen = 30.
  gs_dyn_fcat-tabname   = 'GT_FINAL'.
  gs_dyn_fcat-coltext   = 'OBJECT'.
  gs_dyn_fcat-col_pos   = gv_pos.
  gs_dyn_fcat-key       = 'X'.
  APPEND gs_dyn_fcat TO gt_dyn_fcat.
  CLEAR gs_dyn_fcat.

  gt_temp = gt_final.
  SORT gt_temp BY atnam.
  LOOP AT gt_temp INTO gs_temp.
    gv_pos = gv_pos + 1.
    READ TABLE gt_dyn_fcat INTO gs_dyn_fcat WITH KEY fieldname = gs_temp-atnam.
    IF sy-subrc NE 0.
      CONDENSE lv_name.
      gs_dyn_fcat-fieldname = gs_temp-atnam.
      gs_dyn_fcat-outputlen = 30.
      gs_dyn_fcat-tabname   = 'GT_FINAL'.
      gs_dyn_fcat-coltext   = gs_temp-atnam.
      gs_dyn_fcat-col_pos   = gv_pos.
      APPEND gs_dyn_fcat TO gt_dyn_fcat.
      CLEAR gs_dyn_fcat.
    ENDIF.
  ENDLOOP.

* Begin of changes - D30K932262 for REDIGDS
  gv_pos = gv_pos + 1.
  gs_dyn_fcat-fieldname = 'EQART'.
  gs_dyn_fcat-outputlen = 10.
  gs_dyn_fcat-tabname   = 'GT_FINAL'.
  gs_dyn_fcat-coltext   = 'Object Type'.
  gs_dyn_fcat-col_pos   = gv_pos.
  APPEND gs_dyn_fcat TO gt_dyn_fcat.
  CLEAR gs_dyn_fcat.

  gv_pos = gv_pos + 1.
  gs_dyn_fcat-fieldname = 'USTAT'.
  gs_dyn_fcat-outputlen = 15.
  gs_dyn_fcat-tabname   = 'GT_FINAL'.
  gs_dyn_fcat-coltext   = 'User Status'.
  gs_dyn_fcat-col_pos   = gv_pos.
  APPEND gs_dyn_fcat TO gt_dyn_fcat.
  CLEAR gs_dyn_fcat.

  gv_pos = gv_pos + 1.
  gs_dyn_fcat-fieldname = 'SSTAT'.
  gs_dyn_fcat-outputlen = 15.
  gs_dyn_fcat-tabname   = 'GT_FINAL'.
  gs_dyn_fcat-coltext   = 'System Status'.
  gs_dyn_fcat-col_pos   = gv_pos.
  APPEND gs_dyn_fcat TO gt_dyn_fcat.
  CLEAR gs_dyn_fcat.

  gv_pos = gv_pos + 1.
  gs_dyn_fcat-fieldname = 'RDATE'.
  gs_dyn_fcat-outputlen = 10.
  gs_dyn_fcat-tabname   = 'GT_FINAL'.
  gs_dyn_fcat-coltext   = 'Run Date'.
  gs_dyn_fcat-col_pos   = gv_pos.
  APPEND gs_dyn_fcat TO gt_dyn_fcat.
  CLEAR gs_dyn_fcat.

* End of changes - D30K932262 for REDIGDS
  LOOP AT gt_dyn_fcat INTO gs_dyn_fcat.
    lv_pos = lv_pos + 1.
    gs_alv_fieldcat-fieldname     = gs_dyn_fcat-fieldname.
    gs_alv_fieldcat-tabname       = gs_dyn_fcat-tabname.
    gs_alv_fieldcat-seltext_l     = gs_dyn_fcat-coltext.
    gs_alv_fieldcat-outputlen     = gs_dyn_fcat-outputlen.
    gs_alv_fieldcat-col_pos       = lv_pos.
    gs_alv_fieldcat-key           = gs_dyn_fcat-key.
    gs_alv_fieldcat-no_out        = gs_dyn_fcat-no_out.
    APPEND gs_alv_fieldcat TO gt_alv_fieldcat.
    CLEAR gs_alv_fieldcat.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  CREATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_table .

  DATA : lv_index TYPE i.
  CLEAR : gt_dyn_table,gs_final,lv_index.

  PERFORM build_fieldcatalog.

** Create a dynamic internal table with the fieldcatalog structure.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_dyn_fcat
    IMPORTING
      ep_table                  = gt_dyn_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.

  IF sy-subrc EQ 0.
* Assign the new table to field symbol
    ASSIGN gt_dyn_table->* TO <gfs_dyn_table>.
* Create dynamic work area for the dynamic table
    CREATE DATA gs_line LIKE LINE OF <gfs_dyn_table>.
    CREATE DATA gs_line1 LIKE LINE OF <gfs_dyn_table>.
    ASSIGN gs_line->* TO <gfs_line>.
    ASSIGN gs_line1->* TO <gfs_line1>.
  ENDIF.

* Populate the dynamic table
  LOOP AT gt_final INTO gs_final.
* Avoid duplicate entries for key field part.
    READ TABLE <gfs_dyn_table> INTO <gfs_line1> WITH KEY ('OBJEK') = gs_final-objek.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    ASSIGN COMPONENT 'OBJEK' OF STRUCTURE <gfs_line> TO <fs1>.
    <fs1> = gs_final-objek.
    UNASSIGN <fs1>.

    LOOP AT gt_dyn_fcat INTO gs_dyn_fcat.
      IF gs_dyn_fcat-fieldname = 'OBJEK'.
        CONTINUE.
      ENDIF.
      READ TABLE gt_final INTO gs_final1 WITH KEY objek = gs_final-objek
                                                  atnam = gs_dyn_fcat-fieldname.
      IF sy-subrc = 0.
        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
        <fs1> = gs_final1-atwrt.
        UNASSIGN <fs1>.
      ENDIF.
      CLEAR : gs_final1.
* Begin of changes - D30K932262 for REDIGDS
      IF gs_dyn_fcat-fieldname = 'EQART'.
        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
        <fs1> = gs_final-eqart.
        UNASSIGN <fs1>.
      ELSEIF gs_dyn_fcat-fieldname = 'USTAT'.
        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
        <fs1> = gs_final-ustat.
        UNASSIGN <fs1>.
      ELSEIF gs_dyn_fcat-fieldname = 'SSTAT'.
        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
        <fs1> = gs_final-sstat.
        UNASSIGN <fs1>.
      ELSEIF gs_dyn_fcat-fieldname = 'INLET_MOP_1'." Changes for D30K932308 REDIGDS
        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
        <fs1> = gs_final-inlet_mop_1.
        UNASSIGN <fs1>.
      ELSEIF gs_dyn_fcat-fieldname = 'OUTLET_MOP_1'." Changes for D30K932308 REDIGDS
        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
        <fs1> = gs_final-outlet_mop_1.
        UNASSIGN <fs1>.
      ELSEIF gs_dyn_fcat-fieldname = 'RDATE'.
        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
        <fs1> = sy-datum.
        UNASSIGN <fs1>.
      ENDIF.
* End of changes - D30K932262 for REDIGDS
    ENDLOOP.
    APPEND <gfs_line> TO <gfs_dyn_table>.
    CLEAR: <gfs_line>.
    CLEAR: gs_final, gs_final1.
  ENDLOOP.

ENDFORM.                    " CREATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  DATA : lwa_variant TYPE disvariant.
  CLEAR : lwa_variant.

  lwa_variant-report = sy-repid.
  lwa_variant-username = sy-uname.
  lwa_variant-variant = p_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = gt_alv_fieldcat
      i_default          = 'X'
      i_save             = 'A'
      is_variant         = lwa_variant
    TABLES
      t_outtab           = <gfs_dyn_table>.



ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  WRITE_APPL_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_appl_server .

  DATA: lv_path      TYPE string,
        lv_fieldname TYPE string,
        lv_line      TYPE string,
        lv_text      TYPE char50,
        lv_msg       TYPE text80.
  CLEAR : gs_dyn_fcat,lv_fieldname,lv_line,lv_text.

  lv_path = p_path.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_msg.
  IF sy-subrc = 0.
    LOOP AT gt_dyn_fcat INTO gs_dyn_fcat.
      IF lv_fieldname IS INITIAL.
        lv_fieldname = gs_dyn_fcat-coltext.
      ELSE.
        CONCATENATE lv_fieldname ',' gs_dyn_fcat-coltext INTO lv_fieldname.
      ENDIF.
    ENDLOOP.

    TRANSFER lv_fieldname TO lv_path.

    LOOP AT <gfs_dyn_table> INTO <gfs_line>.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <gfs_line> TO <fs1>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF lv_line IS INITIAL.
          lv_line = <fs1>.
        ELSE.
          lv_text = <fs1>.
          CONCATENATE lv_line ',' lv_text INTO lv_line.
        ENDIF.
        CLEAR : lv_text.
      ENDDO.
      TRANSFER lv_line TO lv_path.
      CLEAR: lv_line.
    ENDLOOP.

    CLOSE DATASET lv_path.
    MESSAGE text-t04 TYPE 'S'.
  ELSE.
    WRITE: text-t03, lv_msg.
    STOP.
  ENDIF.

ENDFORM.                    " WRITE_APPL_SERVER
*&---------------------------------------------------------------------*
*&      Form  F4_VARIANT_INPUTHELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LAYOUT  text
*----------------------------------------------------------------------*
FORM f4_variant_inputhelp USING uv_variant TYPE slis_vari.
  DATA: lv_exit,
        lt_objects    TYPE TABLE OF vanz,
        lt_valutab    TYPE TABLE OF rsparams,
        ls_objects    TYPE vanz,
        ls_variant    TYPE disvariant,
        ls_variant_x  TYPE disvariant.

  CLEAR: ls_variant,
         ls_variant_x.
  ls_variant-report =   sy-repid.
  ls_variant-username = sy-uname.
  ls_variant-variant  = uv_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_variant
      i_save        = 'A'
    IMPORTING
      e_exit        = lv_exit
      es_variant    = ls_variant_x
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc IS INITIAL AND lv_exit IS INITIAL.
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = sy-repid
        variant              = sy-slset
      TABLES
        valutab              = lt_valutab
        objects              = lt_objects
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE lt_objects INTO ls_objects WITH KEY name = 'P_LAYOUT'.
    IF NOT sy-subrc IS INITIAL.
      READ TABLE lt_objects INTO ls_objects WITH KEY name = 'SP_VARI'.
    ENDIF.
    IF ls_objects-protected IS INITIAL.
      ls_variant-variant = ls_variant_x-variant.
      uv_variant         = ls_variant_x-variant.
    ENDIF.
  ELSE.
    MESSAGE ID sy-msgid TYPE 'S'
                 NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F4_VARIANT_INPUTHELP
*&---------------------------------------------------------------------*
*&      Form  GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_path .

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_path
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_PATH
*&---------------------------------------------------------------------*
*&      Form  CREATE_REDIGDS_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CREATE_REDIGDS_TABLE .
*
*  DATA : lv_pos TYPE i.
*
*  CLEAR : gt_dyn_table,gs_final, GV_POS.
*
*  gv_pos = gv_pos + 1.
*  gs_dyn_fcat-fieldname = 'OBJEK'.
*  gs_dyn_fcat-outputlen = 30.
*  gs_dyn_fcat-tabname   = 'GT_FINAL'.
*  gs_dyn_fcat-coltext   = 'OBJECT'.
*  gs_dyn_fcat-col_pos   = gv_pos.
*  gs_dyn_fcat-key       = 'X'.
*  APPEND gs_dyn_fcat TO gt_dyn_fcat.
*  CLEAR gs_dyn_fcat.
*
*  gv_pos = gv_pos + 1.
*  gs_dyn_fcat-fieldname = 'EQART'.
*  gs_dyn_fcat-outputlen = 10.
*  gs_dyn_fcat-tabname   = 'GT_FINAL'.
*  gs_dyn_fcat-coltext   = 'Object Type'.
*  gs_dyn_fcat-col_pos   = gv_pos.
**  gs_dyn_fcat-key       = 'X'.
*  APPEND gs_dyn_fcat TO gt_dyn_fcat.
*  CLEAR gs_dyn_fcat.
*
*  gv_pos = gv_pos + 1.
*  gs_dyn_fcat-fieldname = 'USTAT'.
*  gs_dyn_fcat-outputlen = 8.
*  gs_dyn_fcat-tabname   = 'GT_FINAL'.
*  gs_dyn_fcat-coltext   = 'UsrStatus'.
*  gs_dyn_fcat-col_pos   = gv_pos.
**  gs_dyn_fcat-key       = 'X'.
*  APPEND gs_dyn_fcat TO gt_dyn_fcat.
*  CLEAR gs_dyn_fcat.
*
*  gv_pos = gv_pos + 1.
*  gs_dyn_fcat-fieldname = 'SSTAT'.
*  gs_dyn_fcat-outputlen = 8.
*  gs_dyn_fcat-tabname   = 'GT_FINAL'.
*  gs_dyn_fcat-coltext   = 'SysStatus'.
*  gs_dyn_fcat-col_pos   = gv_pos.
**  gs_dyn_fcat-key       = 'X'.
*  APPEND gs_dyn_fcat TO gt_dyn_fcat.
*  CLEAR gs_dyn_fcat.
*
*  LOOP AT gt_dyn_fcat INTO gs_dyn_fcat.
*    lv_pos = lv_pos + 1.
*    gs_alv_fieldcat-fieldname     = gs_dyn_fcat-fieldname.
*    gs_alv_fieldcat-tabname       = gs_dyn_fcat-tabname.
*    gs_alv_fieldcat-seltext_l     = gs_dyn_fcat-coltext.
*    gs_alv_fieldcat-outputlen     = gs_dyn_fcat-outputlen.
*    gs_alv_fieldcat-col_pos       = lv_pos.
*    gs_alv_fieldcat-key           = gs_dyn_fcat-key.
*    gs_alv_fieldcat-no_out        = gs_dyn_fcat-no_out.
*    APPEND gs_alv_fieldcat TO gt_alv_fieldcat.
*    CLEAR gs_alv_fieldcat.
*  ENDLOOP.
*
*** Create a dynamic internal table with the fieldcatalog structure.
*  CALL METHOD cl_alv_table_create=>create_dynamic_table
*    EXPORTING
*      it_fieldcatalog           = gt_dyn_fcat
*    IMPORTING
*      ep_table                  = gt_dyn_table
*    EXCEPTIONS
*      generate_subpool_dir_full = 1
*      OTHERS                    = 2.
*
*  IF sy-subrc EQ 0.
** Assign the new table to field symbol
*    ASSIGN gt_dyn_table->* TO <gfs_dyn_table>.
** Create dynamic work area for the dynamic table
*    CREATE DATA gs_line LIKE LINE OF <gfs_dyn_table>.
*    CREATE DATA gs_line1 LIKE LINE OF <gfs_dyn_table>.
*    ASSIGN gs_line->* TO <gfs_line>.
*    ASSIGN gs_line1->* TO <gfs_line1>.
*  ENDIF.
*  SORT gt_final by objek eqart.
*  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING objek eqart.
** Populate the dynamic table
*  LOOP AT gt_final INTO gs_final.
*
*    ASSIGN COMPONENT 'OBJEK' OF STRUCTURE <gfs_line> TO <fs1>.
*    <fs1> = gs_final-objek.
*    UNASSIGN <fs1>.
*
*    ASSIGN COMPONENT 'EQART' OF STRUCTURE <gfs_line> TO <fs1>.
*    <fs1> = gs_final-EQART.
*    UNASSIGN <fs1>.
*
*    ASSIGN COMPONENT 'USTAT' OF STRUCTURE <gfs_line> TO <fs1>.
*    <fs1> = gs_final-ustat.
*    UNASSIGN <fs1>.
*
*    ASSIGN COMPONENT 'SSTAT' OF STRUCTURE <gfs_line> TO <fs1>.
*    <fs1> = gs_final-sstat.
*    UNASSIGN <fs1>.
*
**    LOOP AT gt_dyn_fcat INTO gs_dyn_fcat.
**      IF gs_dyn_fcat-fieldname = 'OBJEK'.
**        CONTINUE.
**      ENDIF.
**      READ TABLE gt_final INTO gs_final1 WITH KEY objek = gs_final-objek
**                                                  atnam = gs_dyn_fcat-fieldname.
**      IF sy-subrc = 0.
**        ASSIGN COMPONENT gs_dyn_fcat-fieldname OF STRUCTURE <gfs_line> TO <fs1>.
**        <fs1> = gs_final1-atwrt.
**        UNASSIGN <fs1>.
**      ENDIF.
**      CLEAR : gs_final1.
**    ENDLOOP.
*    APPEND <gfs_line> TO <gfs_dyn_table>.
*    CLEAR: <gfs_line>.
*    CLEAR: gs_final, gs_final1.
*
*  ENDLOOP.
*ENDFORM.                    " CREATE_REDIGDS_TABLE
