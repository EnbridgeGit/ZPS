*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPSDEV
*   generation date: 03.02.2003 at 08:53:26 by user ASCHERBI
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPSDEV             .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
