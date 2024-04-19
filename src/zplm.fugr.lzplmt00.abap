*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPLM_ACTION_TYPE................................*
DATA:  BEGIN OF STATUS_ZPLM_ACTION_TYPE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPLM_ACTION_TYPE              .
CONTROLS: TCTRL_ZPLM_ACTION_TYPE
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZPLM_DISTRICT...................................*
DATA:  BEGIN OF STATUS_ZPLM_DISTRICT                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPLM_DISTRICT                 .
CONTROLS: TCTRL_ZPLM_DISTRICT
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZPLM_DIVISION...................................*
DATA:  BEGIN OF STATUS_ZPLM_DIVISION                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPLM_DIVISION                 .
CONTROLS: TCTRL_ZPLM_DIVISION
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZPLM_ORG........................................*
DATA:  BEGIN OF STATUS_ZPLM_ORG                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPLM_ORG                      .
CONTROLS: TCTRL_ZPLM_ORG
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZPLM_QAPARTNER..................................*
DATA:  BEGIN OF STATUS_ZPLM_QAPARTNER                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPLM_QAPARTNER                .
CONTROLS: TCTRL_ZPLM_QAPARTNER
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZPLM_TOWN.......................................*
DATA:  BEGIN OF STATUS_ZPLM_TOWN                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPLM_TOWN                     .
CONTROLS: TCTRL_ZPLM_TOWN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPLM_ACTION_TYPE              .
TABLES: *ZPLM_DISTRICT                 .
TABLES: *ZPLM_DIVISION                 .
TABLES: *ZPLM_ORG                      .
TABLES: *ZPLM_QAPARTNER                .
TABLES: *ZPLM_TOWN                     .
TABLES: ZPLM_ACTION_TYPE               .
TABLES: ZPLM_DISTRICT                  .
TABLES: ZPLM_DIVISION                  .
TABLES: ZPLM_ORG                       .
TABLES: ZPLM_QAPARTNER                 .
TABLES: ZPLM_TOWN                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
