*-------------------------------------------------------------------
***INCLUDE LKAZBI01 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
module  fcode  input.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
  perform fcode.
endmodule.                             " FCODE  INPUT

*&---------------------------------------------------------------------*
module  ecode  input.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
  perform ecode.
endmodule.                             " ECODE  INPUT

*&---------------------------------------------------------------------*
module  check_kokrs  input.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
  perform check_kokrs.
endmodule.                             " CHECK_KOKRS  INPUT

*&---------------------------------------------------------------------*
module  check_versn  input.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
  perform check_versn.
endmodule.                             " CHECK_VERSN  INPUT
*ENHANCEMENT-POINT LKAZBI01_01 SPOTS ES_SAPLKAZB STATIC.

*&---------------------------------------------------------------------*
module  check_period  input.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
  perform check_period.
endmodule.                             " CHECK_PERIOD  INPUT

*&---------------------------------------------------------------------*
module  check_perio_block  input.
*&---------------------------------------------------------------------*
*       check period block for activities
*----------------------------------------------------------------------*
  perform check_period_blocking.
endmodule.                             " CHECK_PERIO_BLOCK  INPUT

*&---------------------------------------------------------------------*
module  auth  input.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
  perform auth.
endmodule.                             " AUTH  INPUT

*&---------------------------------------------------------------------*
module  init_gd-single_gjahr  input.
*&---------------------------------------------------------------------*
* set GJAHR_BIS if not entered by user
*----------------------------------------------------------------------*
  perform init_gd-single_gjahr.
endmodule.                             " INIT_GD-SINGLE_GJAHR  INPUT

*&---------------------------------------------------------------------*
module  init_gd-single_period  input.
*----------------------------------------------------------------------*
* set RKAUF-TO if not entered by user
*----------------------------------------------------------------------*
  perform init_gd-single_period.
endmodule.                             " INIT_GD-SINGLE_PERIOD  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_GJAHR  INPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
module check_gjahr input.
  perform check_gjahr.
endmodule.                             " CHECK_GJAHR  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       no dialog processing in batch
*----------------------------------------------------------------------*
module check_processing input.
  perform check_processing.
endmodule.                             " CHECK_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_PERIOD_FROM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module fill_period_from input.
  perform fill_period_from.
endmodule.                             " FILL_PERIOD_FROM  INPUT
*&---------------------------------------------------------------------*
*&      Module  MESSAGES_INITIALIZE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module messages_initialize input.

  perform messages_initialize  using gd-identification
                                     gd-mesg_line
                                     vorher
                                     nachher.

endmodule.                             " MESSAGES_INITIALIZE  INPUT
*&---------------------------------------------------------------------*
*&      Module  SAVE_SCREEN_PARAMETERS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module save_screen_parameters input.
  perform save_screen_parameters.
endmodule.                             " SAVE_SCREEN_PARAMETERS  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_VERSN_RKAUF  INPUT
*&---------------------------------------------------------------------*
module set_versn_rkauf input.

  rkauf-versn = rpsco_x-versn_ctc.

endmodule.                             " SET_VERSN_RKAUF  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_VERSN_EV_RKAUF INPUT
*&---------------------------------------------------------------------*
module set_versn_ev_rkauf input.

  rkauf-versn = rpsco_x-versn_ev.
  set parameter id 'VERSN_EV' field rpsco_x-versn_ev.  "Earned Value

endmodule.                             " SET_VERSN_EV_RKAUF INPUT
*ENHANCEMENT-POINT LKAZBI01_02 SPOTS ES_SAPLKAZB STATIC.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VERSN_EVA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_versn_eva input.
* check ev-version
  perform check_versn_eva.
endmodule.                             " CHECK_VERSN_EVA  INPUT
*&---------------------------------------------------------------------*
*&      Module  BATCH_FLG_SET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module batch_flg_set input.

  call function 'K_OBJECT_BATCH_FLAG_SET'
       exporting
            i_batch_flg = rkauf-batch.

endmodule.                             " BATCH_FLG_SET  INPUT
*&---------------------------------------------------------------------*
*&      Module  RPSCO_X_VERSN_EVA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module rpsco_x_versn_eva input.
  perform rpsco_x_versn_eva.
endmodule.                             " RPSCO_X_VERSN_EVA  INPUT

*-------------------------------------------------------------------UGL
*  MODULE Z_CHECK_BUDAT     INPUT                                   UGL
*-------------------------------------------------------------------UGL
*  This module is required to check posting date and prevent it     UGL
*  from being in future AND to make sure that posting period        UGL
*  is open for the 2 accounts                                       UGL
*-------------------------------------------------------------------UGL
MODULE Z_CHECK_BUDAT INPUT.                                        "UGL
  perform z_check_budat.                                           "UGL
ENDMODULE.                                                         "UGL
*-------------------------------------------------------------------UGL
