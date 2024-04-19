***INCLUDE MIOLLSEL .
***  Selections-options für Report RIIFLO20                        ***
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
 tplnr      FOR        iflo-tplnr      NO-DISPLAY                     ,
 strno      FOR       iflos-strno      MATCHCODE OBJECT iflm          .

*--- partner function, partner
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(9) text-001  FOR FIELD dy_parvw            .
SELECTION-SCREEN POSITION 10.
PARAMETERS:
 dy_parvw   LIKE       ihpa-parvw       AS LISTBOX VISIBLE LENGTH 22  ,
 dy_parnr   LIKE       ihpa-parnr                                     .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
*--- selection scheme for status
SELECTION-SCREEN COMMENT 1(30)          FOR FIELD selschem            .
SELECTION-SCREEN POSITION 33.
PARAMETERS:
 selschem   LIKE      tj48t-selid                                     .
*... adress selection
SELECTION-SCREEN PUSHBUTTON 58(12) text-005 USER-COMMAND addr.
PARAMETERS:
 dy_adrfl                               NO-DISPLAY                    .
*SELECTION-SCREEN COMMENT 71(30)         ad_icon.              "1307770
SELECTION-SCREEN COMMENT 71(31)         ad_icon.               "1307770
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK block1.

* SELECTION-SCREEN Block zur Klassifizierung
SELECTION-SCREEN BEGIN OF BLOCK clse WITH FRAME TITLE text-500.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30)          FOR FIELD dy_klart            .
SELECTION-SCREEN POSITION 33.
PARAMETERS:
 dy_klart   LIKE clselinput-klart                                     .
SELECTION-SCREEN POSITION 50.
PARAMETERS:
 dy_subcl   LIKE      rihea-subcl       AS CHECKBOX                   .
SELECTION-SCREEN COMMENT 53(27)         FOR FIELD dy_subcl            .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30)          FOR FIELD dy_class            .
SELECTION-SCREEN POSITION 33.
PARAMETERS:
 dy_class   LIKE clselinput-class                                     .
SELECTION-SCREEN PUSHBUTTON 58(12) text-501 USER-COMMAND comw         .
SELECTION-SCREEN COMMENT 71(35)         cl_icon                       .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK clse.

ENHANCEMENT-POINT EHP605_MIOLLSEL_01 SPOTS ZES_RIIFLO20 STATIC .


SELECTION-SCREEN BEGIN OF BLOCK miollsel_1 WITH FRAME TITLE text-f01.
SELECT-OPTIONS:
 pltxt      FOR        iflo-pltxt                                     ,
 iwerk      FOR        iflo-iwerk                                     ,
 ingrp      FOR        iflo-ingrp                                     ,
 submt      FOR        iflo-submt       MATCHCODE OBJECT mat1         ,
 rbnr       FOR        iflo-rbnr                                      ,
 begru      FOR        iflo-begru                                     ,
 gsber      FOR        iflo-gsber                                     ,
 gewrk      FOR     rihiflo-gewrk       MATCHCODE OBJECT cram         ,
 trpnr      FOR        iflo-trpnr       MATCHCODE OBJECT irlm         ,
 fltyp      FOR        iflo-fltyp                                     ,
 sogen      FOR        ihsg-pmsog                                     .
SELECTION-SCREEN END OF BLOCK miollsel_1.

SELECTION-SCREEN BEGIN OF BLOCK miollsel_2 WITH FRAME TITLE text-f02.
SELECT-OPTIONS:
 swerk      FOR        iflo-swerk                                     ,
 stort      FOR        iflo-stort                                     ,
 msgrp      FOR        iflo-msgrp                                     ,
 beber      FOR        iflo-beber                                     ,
 arbpl      FOR     rihiflo-arbpl       MATCHCODE OBJECT cram         ,
 abckz      FOR        iflo-abckz                                     ,
 eqfnr      FOR        iflo-eqfnr                                     ,
 kokrs      FOR        iflo-kokrs                                     ,
 bukrs      FOR        iflo-bukrs                                     ,
 kostl      FOR        iflo-kostl       MATCHCODE OBJECT kost         ,
 proid      FOR        iflo-proid       NO-DISPLAY                    ,
 proid_e    FOR        prps-posid       MATCHCODE OBJECT prpm         ,
 anlnr      FOR        iflo-anlnr       MATCHCODE OBJECT aanl         ,
 daufn      FOR        iflo-daufn       MATCHCODE OBJECT ordp         ,
 aufnr      FOR        iflo-aufnr       MATCHCODE OBJECT ordp         ,
 vkorg      FOR        iflo-vkorg                                     ,
 vtweg      FOR        iflo-vtweg                                     ,
 spart      FOR        iflo-spart                                     .
SELECTION-SCREEN END OF BLOCK miollsel_2.

SELECTION-SCREEN BEGIN OF BLOCK miollsel_3 WITH FRAME TITLE text-f04.
SELECT-OPTIONS:
 eqart      FOR        iflo-eqart                                     ,
 invnr      FOR        iflo-invnr                                     ,
 groes      FOR        iflo-groes                                     ,
 brgew      FOR        iflo-brgew                                     ,
 gewei      FOR        iflo-gewei                                     ,
 ansdt      FOR        iflo-ansdt                                     ,
 answt      FOR        iflo-answt                                     ,
 waers      FOR        iflo-waers                                     ,
 herst      FOR        iflo-herst                                     ,
 herld      FOR        iflo-herld                                     ,
 baujj      FOR        iflo-baujj                                     ,
 typbz      FOR        iflo-typbz                                     ,
 serge      FOR        iflo-serge                                     ,
 mapar      FOR        iflo-mapar                                     .
SELECTION-SCREEN END OF BLOCK miollsel_3.

SELECTION-SCREEN BEGIN OF BLOCK miollsel_4 WITH FRAME TITLE text-f03.
SELECT-OPTIONS:
 ernam      FOR        iflo-ernam       MATCHCODE OBJECT user_addr    ,
 erdat      FOR        iflo-erdat                                     ,
 aenam      FOR        iflo-aenam       MATCHCODE OBJECT user_addr    ,
 aedat      FOR        iflo-aedat                                     ,
 stai1      FOR       rihea-i_pstatin   MATCHCODE OBJECT i_status     ,
 stae1      FOR       rihea-i_pstatex   MATCHCODE OBJECT i_status     .
SELECTION-SCREEN END OF BLOCK miollsel_4.

SELECTION-SCREEN BEGIN OF BLOCK miollsel_5 WITH FRAME TITLE text-son.
PARAMETERS:
 variant    LIKE disvariant-variant                                   .
SELECTION-SCREEN END OF BLOCK miollsel_5.
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-101.
PARAMETERS : p_path TYPE localfile DEFAULT '/usr/sap/'.
SELECTION-SCREEN END OF BLOCK blk3.

*--- invisible select options
SELECT-OPTIONS:
 s_comw     FOR          sy-lisel       NO-DISPLAY                    .

PARAMETERS:
 dy_selm                                DEFAULT '0' NO-DISPLAY        ,
 dy_tcode   LIKE         sy-tcode       NO-DISPLAY                    ,
 dy_msgty   LIKE         sy-msgty       DEFAULT 'I' NO-DISPLAY        .
