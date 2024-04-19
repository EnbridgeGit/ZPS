*&---------------------------------------------------------------------*
*&  Include           ZPM_CLASSIFICATION_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_objek FOR inob-objek.
PARAMETERS : p_klart TYPE inob-klart OBLIGATORY.
SELECT-OPTIONS : s_atnam FOR cabn-atnam OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
PARAMETERS : p_fun RADIOBUTTON GROUP rd1 DEFAULT 'X',
             p_eqp RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF BLOCK blk2.
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-003.
PARAMETERS : p_alv AS CHECKBOX DEFAULT 'X',
             p_pth AS CHECKBOX USER-COMMAND ucom,
             p_path TYPE localfile DEFAULT '/usr/sap/' MODIF ID m1.
SELECTION-SCREEN END OF BLOCK blk3.
PARAMETERS : p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK blk1.
