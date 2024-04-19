*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.02.2003 at 08:53:28 by user ASCHERBI
*---------------------------------------------------------------------*
*...processing: ZPSDIVGR........................................*
DATA:  BEGIN OF STATUS_ZPSDIVGR                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPSDIVGR                      .
CONTROLS: TCTRL_ZPSDIVGR
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZPSDIVGR                      .
TABLES: ZPSDIVGR                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
