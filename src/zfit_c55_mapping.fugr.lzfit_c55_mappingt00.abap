*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_C55_MAPPING................................*
DATA:  BEGIN OF STATUS_ZFIT_C55_MAPPING              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_C55_MAPPING              .
CONTROLS: TCTRL_ZFIT_C55_MAPPING
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_C55_MAPPING              .
TABLES: ZFIT_C55_MAPPING               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
