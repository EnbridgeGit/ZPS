*&---------------------------------------------------------------------*
*& Subroutine Pool   ZPURCHASE01_SCRIPT_SUB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 03/11/2020    KMB          D30K930735 CHG0197719 Return Year End     *
*                                       Message to Vendors on PO       *
*                            D30K930737                                *
*&---------------------------------------------------------------------*

PROGRAM  zpurchase01_script_sub.

*&---------------------------------------------------------------------*
*&      Form  f_yearend_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM f_yearend_check TABLES in_par STRUCTURE itcsy
                            out_par STRUCTURE itcsy.

  CONSTANTS : gc_ptype LENGTH 8  TYPE c VALUE 'ZMPUM001',
              gc_stype LENGTH 11 TYPE c VALUE 'ZPURCHASE01',
              gc_key   LENGTH 13 TYPE c VALUE 'YEAREND_CHECK',
              gc_key2  LENGTH 4  TYPE c VALUE 'TEXT'.

  DATA: gv_value1 TYPE c,
        gv_text   TYPE string,
        gt_param  TYPE TABLE OF zfit_xparam,
        gs_param  TYPE zfit_xparam.

  FIELD-SYMBOLS : <out_par> TYPE itcsy.

  CLEAR: gt_param[], gs_param.

  SELECT *
    INTO TABLE gt_param
    FROM zfit_xparam
    WHERE paramtype = gc_ptype
    AND   subtype   = gc_stype
    AND   key1   = gc_key.
  IF sy-subrc = 0.
    READ TABLE gt_param INTO gs_param WITH KEY paramtype = gc_ptype subtype = gc_stype key1 = gc_key.
    IF sy-subrc = 0.
      READ TABLE out_par ASSIGNING <out_par> WITH KEY name = 'GV_CHECK'.
      CHECK <out_par> IS ASSIGNED.
      <out_par>-value = gs_param-value1.
      CLEAR gs_param.
    ENDIF.
    READ TABLE gt_param INTO gs_param WITH KEY paramtype = gc_ptype subtype = gc_stype key1 = gc_key key2 = gc_key2.
    IF sy-subrc = 0.
      READ TABLE out_par ASSIGNING <out_par> WITH KEY name = 'GV_TEXT'.
      CHECK <out_par> IS ASSIGNED.
      <out_par>-value = gs_param-value1.
      CLEAR gs_param.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_yearend_check
