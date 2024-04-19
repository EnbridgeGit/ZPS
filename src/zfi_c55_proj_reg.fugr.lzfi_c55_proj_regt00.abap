*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_C55_PROJ_REG................................*
DATA:  BEGIN OF STATUS_ZFI_C55_PROJ_REG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_C55_PROJ_REG              .
CONTROLS: TCTRL_ZFI_C55_PROJ_REG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_C55_PROJ_REG              .
TABLES: ZFI_C55_PROJ_REG               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
