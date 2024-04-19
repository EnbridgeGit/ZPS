*-------------------------------------------------------------------
***INCLUDE LKAZBO01 .
*-------------------------------------------------------------------
* changing history
* ----------------
* SCT ALRK136242 151098 accrual calculation connected
* ------------------------------------------------------------------

*&---------------------------------------------------------------------*
MODULE  SET_KOKRS  OUTPUT.

  CALL FUNCTION 'K_KOKRS_SET'
       IMPORTING
            E_KOKRS = RKAUF-KOKRS.

* accrual calculation needs text of controlling area for
* fcode 'ASCA' already in PBO:
  CALL FUNCTION 'K_KOKRS_READ'                              "ALRK136242
       EXPORTING
            KOKRS    =  RKAUF-KOKRS
       IMPORTING
            E_TKA01  =  TKA01.

ENDMODULE.                             " SET_KOKRS  OUTPUT

*&---------------------------------------------------------------------*
MODULE  SET_CUA  OUTPUT.
*&---------------------------------------------------------------------*
*       set CUA status for first screen
*----------------------------------------------------------------------*
 PERFORM SET_CUA_STATUS.
ENDMODULE.                             " SET_CUA  OUTPUT

*&---------------------------------------------------------------------*
*       MODULE  INIT  OUTPUT.
*&---------------------------------------------------------------------*
*       initialize transaction
*----------------------------------------------------------------------*
MODULE  INIT  OUTPUT.
  PERFORM INIT.
ENDMODULE.                             " INIT  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_PARAMETER_SCREEN OUTPUT.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
  PERFORM MODIFY_PARAMETER_SCREEN.
ENDMODULE.                             " MODIFY_PARAMETER_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_PROCESSING_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
* exclude flag for detail list
*----------------------------------------------------------------------*
MODULE MODIFY_PROCESSING_SCREEN OUTPUT.
  PERFORM MODIFY_PROCESSING_SCREEN.
ENDMODULE.                 " MODIFY_PROCESSING_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_ACT_LOG_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_ACT_LOG_SCREEN OUTPUT.
  PERFORM INIT_ACT_LOG_SCREEN.
ENDMODULE.                             " INIT_ACT_LOG_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN_PARAMETERS  OUTPUT
*&---------------------------------------------------------------------*
*       different screen for planned interest calculation:
*       no mandatory fields and set/get parameters for Kzrp =>
*       let those fields initial in subscreen and fill those fields
*       if vrgng <> kzrp dynamically.
*----------------------------------------------------------------------*
MODULE SET_SCREEN_PARAMETERS OUTPUT.
  PERFORM SET_SCREEN_PARAMETERS.
ENDMODULE.                 " SET_SCREEN_PARAMETERS  OUTPUT
