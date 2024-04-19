REPORT ZPCREW00 USING DATABASE PNP MESSAGE-ID PN LINE-SIZE 180.

TABLES: PERNR.

TABLES: T001P,                         "Personnel Sub Area
        T501,                          "Employee Group
        T501T,                         "Employee Group Text
        T503,                          "Employee Sub Group
        T503T,                         "Employee Sub Group Text
        T552A.                         "Work Schedules

INFOTYPES: 0000,                       "Event
           0001,                       "Organizational Assignment
           0007.                       "Planned Working Time

SELECTION-SCREEN BEGIN OF BLOCK FRM1 WITH FRAME TITLE TEXT-LST.

PARAMETERS: OVT_FACT(2) TYPE N.

SELECTION-SCREEN END OF BLOCK FRM1.

DATA: BEGIN OF TAB1 OCCURS 10,
        PERNR LIKE P0001-PERNR,
        PLANT LIKE P0001-WERKS,
        PLSEC LIKE P0001-BTRTL,
        PERSG LIKE P0001-PERSG,
        PERSK LIKE P0001-PERSK,
        ZEITY LIKE T503-ZEITY,
        MOFID LIKE T001P-MOFID,
        MOSID LIKE T001P-MOSID,
        SHIFT LIKE P0007-SCHKZ,
      END OF TAB1.

DATA: BEGIN OF TAB2 OCCURS 10,
        PLANT LIKE P0001-WERKS,
        PLSEC LIKE P0001-BTRTL,
        PERSG LIKE P0001-PERSG,
        PERSK LIKE P0001-PERSK,
        HRS01(4) TYPE P DECIMALS 2,
        OVT01(4) TYPE P DECIMALS 2,
        HRS02(4) TYPE P DECIMALS 2,
        OVT02(4) TYPE P DECIMALS 2,
        HRS03(4) TYPE P DECIMALS 2,
        OVT03(4) TYPE P DECIMALS 2,
        HRS04(4) TYPE P DECIMALS 2,
        OVT04(4) TYPE P DECIMALS 2,
        HRS05(4) TYPE P DECIMALS 2,
        OVT05(4) TYPE P DECIMALS 2,
        HRS06(4) TYPE P DECIMALS 2,
        OVT06(4) TYPE P DECIMALS 2,
        HRS07(4) TYPE P DECIMALS 2,
        OVT07(4) TYPE P DECIMALS 2,
        HRS08(4) TYPE P DECIMALS 2,
        OVT08(4) TYPE P DECIMALS 2,
        HRS09(4) TYPE P DECIMALS 2,
        OVT09(4) TYPE P DECIMALS 2,
        HRS10(4) TYPE P DECIMALS 2,
        OVT10(4) TYPE P DECIMALS 2,
        HRS11(4) TYPE P DECIMALS 2,
        OVT11(4) TYPE P DECIMALS 2,
        HRS12(4) TYPE P DECIMALS 2,
        OVT12(4) TYPE P DECIMALS 2,
      END OF TAB2.

DATA: COUNT(3) TYPE N VALUE 0.
DATA: PLT_CNT(3) TYPE N VALUE 0.

INITIALIZATION.
  RP-SEL-EIN-AUS.


START-OF-SELECTION.

GET PERNR.
  PERFORM LOAD_TAB1.

END-OF-SELECTION.

  PERFORM LOAD_TAB2.
  PERFORM OVERTIME.
  PERFORM TOTAL.

*---------------------------------------------------------------------*
*       FORM LOAD_TAB1                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM LOAD_TAB1.
  RP-PROVIDE-FROM-LAST P0001 SPACE PN-BEGDA PN-ENDDA.
  TAB1-PERNR = P0001-PERNR.
  TAB1-PLANT = P0001-WERKS.
  TAB1-PLSEC = P0001-BTRTL.
  TAB1-PERSG = P0001-PERSG.
  TAB1-PERSK = P0001-PERSK.

  RP-PROVIDE-FROM-LAST P0007 SPACE PN-BEGDA PN-ENDDA.
  TAB1-SHIFT = P0007-SCHKZ.

  PERFORM RE001P.
  PERFORM RE503.
  APPEND TAB1.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM LOAD_TAB2                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM LOAD_TAB2.
  SORT TAB1 BY PLSEC PERSK.
  LOOP AT TAB1.
    PERFORM RE552A.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM OVERTIME                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM OVERTIME.
  LOOP AT TAB2.
    TAB2-OVT01 = TAB2-HRS01 * OVT_FACT / 100.
    TAB2-OVT02 = TAB2-HRS02 * OVT_FACT / 100.
    TAB2-OVT03 = TAB2-HRS03 * OVT_FACT / 100.
    TAB2-OVT04 = TAB2-HRS04 * OVT_FACT / 100.
    TAB2-OVT05 = TAB2-HRS05 * OVT_FACT / 100.
    TAB2-OVT06 = TAB2-HRS06 * OVT_FACT / 100.
    TAB2-OVT07 = TAB2-HRS07 * OVT_FACT / 100.
    TAB2-OVT08 = TAB2-HRS08 * OVT_FACT / 100.
    TAB2-OVT09 = TAB2-HRS09 * OVT_FACT / 100.
    TAB2-OVT10 = TAB2-HRS10 * OVT_FACT / 100.
    TAB2-OVT11 = TAB2-HRS11 * OVT_FACT / 100.
    TAB2-OVT12 = TAB2-HRS12 * OVT_FACT / 100.
    MODIFY TAB2.
  ENDLOOP.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM TOTAL                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOTAL.
  DATA: COUNTER TYPE I.
  SORT TAB2 BY PLSEC PERSK.
  LOOP AT TAB2.
    PERFORM GET_TEXT.
    PERFORM RE503T.
    COUNT = COUNT + 1.
    PLT_CNT = PLT_CNT + 1.

    AT NEW PLSEC.
      WRITE: / 'Source of Hours for: ', T001P-BTEXT.
    ENDAT.

    AT END OF PERSK.
      SUM.
      WRITE: / T503T-PTEXT,
              35 'Regular',
              48 COUNT NO-ZERO,
              55 TAB2-HRS01,
              65 TAB2-HRS02,
              75 TAB2-HRS03,
              85 TAB2-HRS04,
              95 TAB2-HRS05,
             105 TAB2-HRS06,
             115 TAB2-HRS07,
             125 TAB2-HRS08,
             135 TAB2-HRS09,
             145 TAB2-HRS10,
             155 TAB2-HRS11,
             165 TAB2-HRS12.
      WRITE: /35 'Overtime',
              55 TAB2-OVT01,
              65 TAB2-OVT02,
              75 TAB2-OVT03,
              85 TAB2-OVT04,
              95 TAB2-OVT05,
             105 TAB2-OVT06,
             115 TAB2-OVT07,
             125 TAB2-OVT08,
             135 TAB2-OVT09,
             145 TAB2-OVT10,
             155 TAB2-OVT11,
             165 TAB2-OVT12.
      SKIP 1.
      COUNT = 0.
      PERFORM RE503T.
    ENDAT.

    AT END OF PLSEC.
      SUM.
      WRITE: / 'Total for Plant ', T001P-BTEXT,
             35 'Regular',
             48 PLT_CNT NO-ZERO,
             55 TAB2-HRS01,
             65 TAB2-HRS02,
             75 TAB2-HRS03,
             85 TAB2-HRS04,
             95 TAB2-HRS05,
            105 TAB2-HRS06,
            115 TAB2-HRS07,
            125 TAB2-HRS08,
            135 TAB2-HRS09,
            145 TAB2-HRS10,
            155 TAB2-HRS11,
            165 TAB2-HRS12.
      WRITE: /35 'Overtime',
              55 TAB2-OVT01,
              65 TAB2-OVT02,
              75 TAB2-OVT03,
              85 TAB2-OVT04,
              95 TAB2-OVT05,
             105 TAB2-OVT06,
             115 TAB2-OVT07,
             125 TAB2-OVT08,
             135 TAB2-OVT09,
             145 TAB2-OVT10,
             155 TAB2-OVT11,
             165 TAB2-OVT12.
      PLT_CNT = 0.
      SKIP 3.
      PERFORM GET_TEXT.
    ENDAT.

  ENDLOOP.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM RE001P                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RE001P.
  SELECT SINGLE * FROM T001P WHERE WERKS EQ P0001-WERKS
                               AND BTRTL EQ P0001-BTRTL.
  MOVE T001P-MOSID TO TAB1-MOSID.
  MOVE T001P-MOFID TO TAB1-MOFID.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM RE503                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RE503.
  SELECT SINGLE * FROM T503 WHERE PERSG EQ P0001-PERSG
                            AND   PERSK EQ P0001-PERSK.
  MOVE T503-ZEITY TO TAB1-ZEITY.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM RE501T                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RE501T.
  SELECT * FROM T501T WHERE SPRSL EQ SY-LANGU
                        AND PERSG EQ P0001-PERSG.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    T501T-PTEXT = '???'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM RE503T                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RE503T.
  SELECT * FROM T503T WHERE SPRSL EQ SY-LANGU
                        AND PERSK EQ TAB2-PERSK.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    T503T-PTEXT = 'Unknown Employee SubGroup'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_TEXT                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_TEXT.
  SELECT SINGLE * FROM T001P WHERE WERKS EQ TAB2-PLANT
                               AND BTRTL EQ TAB2-PLSEC.
  IF SY-SUBRC NE 0.
    T001P-BTEXT = 'Unknown Personnel SubGroup'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM RE552A                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RE552A.

  DATA: TAB2FLD1(10) VALUE 'tab2-hrs'.
  FIELD-SYMBOLS: <F1>.

  CLEAR: TAB2.
  SELECT * FROM T552A WHERE ZEITY EQ TAB1-ZEITY
                        AND MOSID EQ TAB1-MOSID
                        AND MOFID EQ TAB1-MOFID
                        AND SCHKZ EQ TAB1-SHIFT
                        AND KJAHR EQ SY-DATUM(4).
    TAB2FLD1+8(2) = T552A-MONAT.
    ASSIGN (TAB2FLD1) TO <F1>.
    <F1> = T552A-SOLST.
    IF ( T552A-MONAT = '12' ).
      MOVE-CORRESPONDING TAB1 TO TAB2.
      APPEND TAB2.
      CLEAR TAB2.
    ENDIF.
  ENDSELECT.
ENDFORM.
