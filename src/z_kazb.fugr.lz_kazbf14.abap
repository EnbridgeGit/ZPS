*----------------------------------------------------------------------*
*   INCLUDE LKAZBF14                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PRC_SET_DEFAULT_PERIOD
*&---------------------------------------------------------------------*
*       set period default values (values not necessary for CTC)
*----------------------------------------------------------------------*
FORM prc_set_default_period.

  DATA: l_tka01 LIKE tka01.

  IF NOT rkauf-kokrs IS INITIAL.
    CALL FUNCTION 'K_KOKRS_READ'
         EXPORTING
              kokrs   = rkauf-kokrs
         IMPORTING
              e_tka01 = l_tka01.

    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
         EXPORTING
              i_date  = sy-datlo
              i_periv = l_tka01-lmona
         IMPORTING
              e_buper = rkauf-from
              e_gjahr = rkauf-gjahr.
    LOOP AT SCREEN.
      IF screen-group2 = '002'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " PRC_SET_DEFAULT_PERIOD
*&---------------------------------------------------------------------*
*&      Form  PRC_SET_TODATE
*&---------------------------------------------------------------------*
*       set key date as the last day of the previous period
*----------------------------------------------------------------------*
FORM prc_set_todate.

  DATA: l_tka01 LIKE tka01,
        l_from  LIKE rkauf-from,
        l_bdatj LIKE rkauf-gjahr.

*--- don't change an entered keydate
  CHECK rkauf-todate IS INITIAL.

  IF NOT rkauf-kokrs IS INITIAL.
    CALL FUNCTION 'K_KOKRS_READ'
         EXPORTING
              kokrs   = rkauf-kokrs
         IMPORTING
              e_tka01 = l_tka01.
    l_from = rkauf-from.
    IF l_from IS INITIAL.
      CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
           EXPORTING
                i_date         = sy-datlo
                i_periv        = l_tka01-lmona
           IMPORTING
                e_buper        = l_from
                e_gjahr        = l_bdatj
           EXCEPTIONS
                input_false    = 1
                t009_notfound  = 2
                t009b_notfound = 3.
      CHECK sy-subrc IS INITIAL.
    ENDIF.
    IF NOT rkauf-gjahr IS INITIAL.
      l_bdatj = rkauf-gjahr.
    ENDIF.
    CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
         EXPORTING
              i_gjahr = l_bdatj
              i_periv = l_tka01-lmona
              i_poper = l_from
         IMPORTING
              e_date  = rkauf-todate.
*--- set posting date for period end partner
    rkauf-budat = rkauf-todate.
  ENDIF.

ENDFORM.                               " PRC_SET_TODATE
*&---------------------------------------------------------------------*
*&      Form  PRC_TODATE
*&---------------------------------------------------------------------*
*       process popup for key date
*----------------------------------------------------------------------*
FORM prc_todate.
*----------------------------------------------------------------------*
  DATA: ld_tka01 LIKE tka01,
        ld_return(1),
        lt_fields LIKE sval OCCURS 0 WITH HEADER LINE.

  IF NOT rkauf-kokrs IS INITIAL.
    CALL FUNCTION 'K_KOKRS_READ'
         EXPORTING
              kokrs   = rkauf-kokrs
         IMPORTING
              e_tka01 = ld_tka01.

    CLEAR: lt_fields,  lt_fields[].
    lt_fields-tabname   = 'RKAUF'.
    lt_fields-fieldname = 'TODATE'.

    IF rkauf-todate IS INITIAL.
      PERFORM prc_set_todate.
    ENDIF.

    lt_fields-value     = rkauf-todate.
    APPEND lt_fields.

    CALL FUNCTION 'POPUP_GET_VALUES'
         EXPORTING
              no_value_check = space
              popup_title    = text-008
              start_column   = '25'
              start_row      = '7'
         IMPORTING
              returncode     = ld_return
         TABLES
              fields         = lt_fields.

    IF ld_return IS INITIAL.
      READ TABLE lt_fields WITH KEY tabname   = 'RKAUF'
                                    fieldname = 'TODATE'.
      IF sy-subrc IS INITIAL.
        rkauf-todate = lt_fields-value.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                               " PRC_TODATE
*&---------------------------------------------------------------------*
*&      Form  CTC_GET_ALV_VARIANT
*&---------------------------------------------------------------------*
*       Gets the ALV display variant and writes it to RKAUF
*       for hierarchical lists
*----------------------------------------------------------------------*
FORM ctc_get_alv_variant.

  DATA: lt_fields       LIKE sval   OCCURS 0 WITH HEADER LINE,
        ls_variant      LIKE disvariant,
        returncode(1)   TYPE c,
        popup_title(30) TYPE c.

* initialize
  ls_variant-report    = gd-report_alv.
  ls_variant-username  = sy-uname.
  popup_title          = text-po1.
  lt_fields-tabname    = 'DISVARIANT'.
  lt_fields-fieldname  = 'VARIANT'.
  lt_fields-value      = rkauf-variant.
  lt_fields-field_obl  = 'X'.          "Muß-Feld
  APPEND lt_fields.

* get default variant
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            i_save     = 'A'
       CHANGING
            cs_variant = ls_variant
       EXCEPTIONS
            not_found  = 2.

* display variant in popup
  CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
       EXPORTING
*           F1_FORMNAME               = ' '
*           F1_PROGRAMNAME            = ' '
            f4_formname               = 'CTC_F4_ALV_VARIANT'
            f4_programname            = 'SAPLKAZB'
            formname                  = 'OR_CHECK_ALV_VARIANT'
            popup_title               =  popup_title
            programname               = 'SAPLKAZB'
       IMPORTING
            returncode                = returncode
       TABLES
            fields                    = lt_fields.

  IF NOT returncode = 'A'.
    READ TABLE lt_fields WITH KEY tabname    = 'DISVARIANT'
                               fieldname  = 'VARIANT'.
    IF sy-subrc IS INITIAL.
      rkauf-variant = lt_fields-value.
    ENDIF.
  ELSE.
    rkauf-variant = ls_variant-variant.
  ENDIF.

ENDFORM.                               " CTC_GET_ALV_VARIANT

*&---------------------------------------------------------------------*
*&      Form  CTC_F4_ALV_VARIANT
*&---------------------------------------------------------------------*
*       Gets the ALV display variant and writes it to RKAUF.
*----------------------------------------------------------------------*
FORM ctc_f4_alv_variant USING tabname fieldname display
                        CHANGING returncode VALUE.

  DATA: ls_variant      LIKE disvariant,
        ld_exit(1)      TYPE c.

* initialize
  ls_variant-report   = gd-report_alv.
  ls_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant       = ls_variant
            i_tabname_header = gd-tabname
            i_tabname_item   = gd-tabname_item
            i_save           = 'A'
       IMPORTING
            e_exit           = ld_exit
            es_variant       = ls_variant
       EXCEPTIONS
            not_found        = 1.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF ld_exit IS INITIAL.
      CLEAR returncode.
      value   = ls_variant-variant.
    ELSE.
      returncode = 'A'.
      CLEAR value.
    ENDIF.
  ENDIF.

ENDFORM.                               " CTC_F4_ALV_VARIANT
*&---------------------------------------------------------------------*
*&      Form  CTC_CHECK_ALV_VARIANT
*&---------------------------------------------------------------------*
*       Gets the ALV display variant and writes it to RKAUF.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM ctc_check_alv_variant TABLES   fields STRUCTURE sval
                           CHANGING error  STRUCTURE svale.

  DATA: ls_variant LIKE disvariant.

* initialize
  ls_variant-report   = gd-report_alv.
  ls_variant-username = sy-uname.
  READ TABLE fields WITH KEY tabname    = 'DISVARIANT'
                             fieldname  = 'VARIANT'.
  IF    sy-subrc IS INITIAL
   AND  NOT ( fields-value IS INITIAL ).
    ls_variant-variant = fields-value.
  ELSE.
    error-msgid       = 'OK'.
    error-msgty       = 'E'.
    error-msgno       = '204'.
    error-msgv1       = ' '.
    error-msgv2       = ' '.
    error-msgv3       = ' '.
    error-msgv4       = ' '.
    error-errortab    = 'DISVARIANT'.
    error-errorfield  = 'VARIANT'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
       EXPORTING
            i_save     = 'A'
       CHANGING
            cs_variant = ls_variant
       EXCEPTIONS
            not_found  = 1.
  IF NOT sy-subrc IS INITIAL.
    error-msgid       = 'OK'.
    error-msgty       = 'E'.
    error-msgno       = '204'.
    error-msgv1       = ' '.
    error-msgv2       = ' '.
    error-msgv3       = ' '.
    error-msgv4       = ' '.
    error-errortab    = 'DISVARIANT'.
    error-errorfield  = 'VARIANT'.
  ENDIF.

ENDFORM.                               " CTC_CHECK_ALV_VARIANT
*&---------------------------------------------------------------------*
*&      Module  RPSCO_X_VERSN_CTC  INPUT
*&---------------------------------------------------------------------*
MODULE rpsco_x_versn_ctc INPUT.

  DATA: l_display,
        l_selectfield  LIKE help_info-fieldname.

  DATA: BEGIN OF l_fieldt OCCURS 0.
          INCLUDE STRUCTURE help_value.
  DATA: END OF l_fieldt.

  DATA:  BEGIN OF l_values OCCURS 0,
           line(45),
         END OF l_values.

  DATA: BEGIN OF l_t_versn OCCURS 0,
          versi LIKE tkvs-versi,
        END OF l_t_versn.

  DATA: BEGIN OF l_t_txt OCCURS 0,
          versi LIKE tkvst-versi,
          vtext LIKE tkvst-vtext,
        END OF l_t_txt.

  REFRESH: l_t_versn, l_fieldt, l_t_txt.

  l_display = space.

  SELECT versi FROM tkvs  INTO CORRESPONDING FIELDS OF TABLE l_t_versn
                           WHERE exuvs = '04'.

  IF NOT l_t_versn[] IS INITIAL.
    SELECT versi vtext FROM tkvst INTO CORRESPONDING FIELDS OF
                                TABLE l_t_txt
                                FOR ALL ENTRIES IN l_t_versn
                                WHERE spras = sy-langu    AND
                                      versi = l_t_versn-versi.
  ENDIF.

  LOOP AT l_t_txt.
    MOVE l_t_txt-versi TO l_values-line.
    APPEND l_values.
    MOVE l_t_txt-vtext TO l_values-line.
    APPEND l_values.
  ENDLOOP.

  l_fieldt-tabname   = 'TKVST'.
  l_fieldt-fieldname = 'VERSI'.
  l_fieldt-selectflag = 'X'.
  APPEND l_fieldt.

  l_fieldt-tabname   = 'TKVST'.
  l_fieldt-fieldname = 'VTEXT'.
  l_fieldt-selectflag = space.
  APPEND l_fieldt.

  l_selectfield = 'VERSI'.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
      EXPORTING
           selectfield                  = l_selectfield
*          titel                        = l_title
*          title_in_values_list         = yx
**      importing
**           ind                          = l_ind
      TABLES
           fields                       = l_fieldt
           full_table                   = l_t_txt
      EXCEPTIONS
           OTHERS                       = 1.

ENDMODULE.                             " RPSCO_X_VERSN_CTC  INPUT
