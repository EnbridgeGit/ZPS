REPORT ZPPMR009 NO STANDARD PAGE HEADING LINE-SIZE 250
                LINE-COUNT 65 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    ZPPMR009
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       March 1998.
*
*   The purpose of this program is to report on the actual monthly
*   expenditures for the various projects.  The report will list all
*   WBS elements with a status of technically complete or closed, the
*   option is available on the selection screen to override this.  The
*   report will list all WBS elements with the status above.  If there
*   is an opening balance or if the project is a major one then all
*   amounts prior to the inservice date will be rolled into the amount
*   corresponding to the inservice month.  Overhead costs have also been
*   allocated based on values contained in a user maintained table.
*
************************************************************************
* 99/01/28 mdemeest #---  Deal with activation date nnnn/01/01
************************************************************************

TABLES:   TJ02T,                        " object status
          JEST,                         " object status
          PRPS,                         " WBS element master data
          COSS,                         " CO object: internal postings
          COSP,                         " CO object: external postings
          COBRA,                        " Settlement rules by object nbr
          ZPWBS,                        " WBS element by asset class
          ANKT.                         " Asset class description

FIELD-SYMBOLS: <F1> .

DATA:     NEWBS(1)      TYPE C VALUE 'Y',               " new wbs ind
          PROJECT(9)    TYPE C,                         " rpt project
          WBS_ELE(4)    TYPE C,                         " rpt WBS elemnt
          WBS_DSC(40)   TYPE C,                         " rpt WBS desc
          AMT_001(11)   TYPE C,                         " rpt amt per 01
          AMT_002(11)   TYPE C,                         " rpt amt per 02
          AMT_003(11)   TYPE C,                         " rpt amt per 03
          AMT_004(11)   TYPE C,                         " rpt amt per 04
          AMT_005(11)   TYPE C,                         " rpt amt per 05
          AMT_006(11)   TYPE C,                         " rpt amt per 06
          AMT_007(11)   TYPE C,                         " rpt amt per 07
          AMT_008(11)   TYPE C,                         " rpt amt per 08
          AMT_009(11)   TYPE C,                         " rpt amt per 09
          AMT_010(11)   TYPE C,                         " rpt amt per 10
          AMT_011(11)   TYPE C,                         " rpt amt per 11
          AMT_012(11)   TYPE C,                         " rpt amt per 12
          AMT_TOT(11)   TYPE C,                         " rpt amt total
          DIV_LO(3)     TYPE C,                         " rpt div range
          DIV_HI(3)     TYPE C,                         " rpt div range
          WRK_AMT       LIKE COSS-WKG001,               " work field
          OPEN_AMT      LIKE COSS-WKG001,               " opening balanc
          WBSPROJ       LIKE PRPS-POSID,                " wbs project
          IDX(3)        TYPE N,                         " work index
          MTH(3)        TYPE N,                         " inservic month
          YEAR          LIKE COSS-GJAHR,                " inservice year
          FLD(14)       TYPE C,                         " insv amt field
          PASS1(1)      TYPE C VALUE 'Y',               " first pass ind
          DST_RAT       TYPE P DECIMALS 8,              " dist oh rate
          GSO_RAT       TYPE P DECIMALS 8,              " GSO oh rate
          NGV_RAT       TYPE P DECIMALS 8,              " NGV oh rate
          ANG_RAT       TYPE P DECIMALS 8,              " A & G oh rate
          OH_RATE       TYPE P DECIMALS 8,              " general oh rat
          TOT_TYP(3)    TYPE C,                         " total type ind
          INSERVDT      LIKE COBRA-BZDAT,         "special date nnnn0101
          BZDAT         LIKE COBRA-BZDAT.

DATA:     BEGIN OF WBSTAB OCCURS 1000,                  " WBS fields
            VERNR       LIKE PRPS-VERNR,                " division
            POSID       LIKE PRPS-POSID,                " WBS element Id
            PRART       LIKE PRPS-POSID,                " project type
            POST1       LIKE PRPS-POST1,                " WBS desc
            OBJNR       LIKE PRPS-OBJNR,                " object number
            TXT04       LIKE TJ02T-TXT04,               " status
          END OF WBSTAB.

DATA:     BEGIN OF REPTAB OCCURS 1000,                  " report data
            ANLKL       LIKE ZPWBS-ANLKL,               " asset class
            WBS(4)      TYPE C,                         " wbs element
            POSID       LIKE PRPS-POSID,                " project id
            POST1       LIKE PRPS-POST1,                " WBS desc
            VERNR       LIKE PRPS-VERNR,                " division
            TXT04       LIKE TJ02T-TXT04,               " status
            BZDAT       LIKE COBRA-BZDAT,               " inservice date
            MAJOR(1)    TYPE C,                         " major project
            AMT_001     LIKE COSS-WKG001,               " rpt amt per 01
            AMT_002     LIKE COSS-WKG002,               " rpt amt per 02
            AMT_003     LIKE COSS-WKG003,               " rpt amt per 03
            AMT_004     LIKE COSS-WKG004,               " rpt amt per 04
            AMT_005     LIKE COSS-WKG005,               " rpt amt per 05
            AMT_006     LIKE COSS-WKG006,               " rpt amt per 06
            AMT_007     LIKE COSS-WKG007,               " rpt amt per 07
            AMT_008     LIKE COSS-WKG008,               " rpt amt per 08
            AMT_009     LIKE COSS-WKG009,               " rpt amt per 09
            AMT_010     LIKE COSS-WKG010,               " rpt amt per 10
            AMT_011     LIKE COSS-WKG011,               " rpt amt per 11
            AMT_012     LIKE COSS-WKG012,               " rpt amt per 12
            AMT_TOT     LIKE COSS-WKG013,               " rpt amt total
          END OF REPTAB.

DATA:     BEGIN OF FILINE,                              " output file
            VERNR(9)    TYPE C,                          " division
            SPAC01(2)   TYPE C,                         " spaces
            ANLKL(8)    TYPE C,                         " asset class
            SPAC02(2)   TYPE C,                         " spaces
            WBS(4)      TYPE C,                         " wbs element
            SPAC03(2)   TYPE C,                         " spaces
            POST1(40)   TYPE C,                         " WBS desc
            SPAC04(2)   TYPE C,                         " spaces
            PROJECT(9)  TYPE C,                         " project Id
            SPAC05(2)   TYPE C,                         " spaces
            TXT04(4)    TYPE C,                         " status
            SPAC06(2)   TYPE C,                         " spaces
            BZDAT(10)   TYPE C,                         " inservice date
            SPAC07(2)   TYPE C,                         " spaces
            AMT_001(17) TYPE C,                         " rpt amt per 01
            SPAC08(2)   TYPE C,                         " spaces
            AMT_002(17) TYPE C,                         " rpt amt per 02
            SPAC09(2)   TYPE C,                         " spaces
            AMT_003(17) TYPE C,                         " rpt amt per 03
            SPAC10(2)   TYPE C,                         " spaces
            AMT_004(17) TYPE C,                         " rpt amt per 04
            SPAC11(2)   TYPE C,                         " spaces
            AMT_005(17) TYPE C,                         " rpt amt per 05
            SPAC12(2)   TYPE C,                         " spaces
            AMT_006(17) TYPE C,                         " rpt amt per 06
            SPAC13(2)   TYPE C,                         " spaces
            AMT_007(17) TYPE C,                         " rpt amt per 07
            SPAC14(2)   TYPE C,                         " spaces
            AMT_008(17) TYPE C,                         " rpt amt per 08
            SPAC15(2)   TYPE C,                         " spaces
            AMT_009(17) TYPE C,                         " rpt amt per 09
            SPAC16(2)   TYPE C,                         " spaces
            AMT_010(17) TYPE C,                         " rpt amt per 10
            SPAC17(2)   TYPE C,                         " spaces
            AMT_011(17) TYPE C,                         " rpt amt per 11
            SPAC18(2)   TYPE C,                         " spaces
            AMT_012(17) TYPE C,                         " rpt amt per 12
            SPAC19(2)   TYPE C,                         " spaces
            AMT_TOT(17) TYPE C,                         " rpt amt total
          END OF FILINE.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(35) TEXT-003.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
   SELECT-OPTIONS S_VERNR     FOR PRPS-VERNR.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-005.
      PARAMETERS: P_YEAR      LIKE COSS-GJAHR
                              OBLIGATORY MODIF ID ABC.
   SELECTION-SCREEN END OF LINE.
   SELECT-OPTIONS S_VERSN     FOR COSP-VERSN
                              OBLIGATORY MODIF ID ABC.
   SELECT-OPTIONS S_TXT04     FOR TJ02T-TXT04 NO INTERVALS
                              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
   SELECT-OPTIONS S_POSID     FOR PRPS-POSID.
   SELECT-OPTIONS S_SHADOW    FOR PRPS-POSID.
SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-029.
      PARAMETERS: P_DST       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 35(14) TEXT-030.
      PARAMETERS: P_DSTRAT(10) TYPE C
                              DEFAULT '0.25122550'.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 31.
      PARAMETERS: P_GSO       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 35(14) TEXT-031.
      PARAMETERS: P_GSORAT(10) TYPE C
                              DEFAULT '0.02214530'.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 31.
      PARAMETERS: P_NGV       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 35(14) TEXT-032.
      PARAMETERS: P_NGVRAT(10) TYPE C
                              DEFAULT '0.09732140'.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 31.
      PARAMETERS: P_ANG       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 35(14) TEXT-033.
      PARAMETERS: P_ANGRAT(10) TYPE C
                              DEFAULT '0.16481040'.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN ULINE.
   SELECT-OPTIONS S_KSTAR     FOR COSS-KSTAR
                              DEFAULT '491001' TO '491002'.
SELECTION-SCREEN END OF BLOCK BOX5.

SELECTION-SCREEN BEGIN OF BLOCK BOX6 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-026.
      PARAMETERS: P_FILE      AS CHECKBOX.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-027.
      PARAMETERS: P_OUTFIL    LIKE FILENAME-FILEEXTERN DEFAULT
      '/usr/sap/interfaces/P01/outbound/PS/projactx.dat'.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX6.

SELECTION-SCREEN BEGIN OF BLOCK BOX7 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(72) TEXT-035.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX7.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* initialize rates
MOVE:  P_DSTRAT TO DST_RAT
     , P_GSORAT TO GSO_RAT
     , P_NGVRAT TO NGV_RAT
     , P_ANGRAT TO ANG_RAT.
IF P_ANG NE SPACE.
  DST_RAT = DST_RAT + ANG_RAT.
  GSO_RAT = GSO_RAT + ANG_RAT.
  NGV_RAT = NGV_RAT + ANG_RAT.
ENDIF.

* clear tables
REFRESH: WBSTAB, REPTAB.
CLEAR:   WBSTAB, REPTAB.

* populate the version, status and date on the selection screen
AT SELECTION-SCREEN OUTPUT.
  IF PASS1 = 'Y'.
    PASS1 = 'N'.
    REFRESH S_VERSN.
    REFRESH S_TXT04.
    P_YEAR = SY-DATUM(4) - 1.
    S_VERSN-LOW    = '00'.
    S_VERSN-HIGH   = SPACE.
    S_VERSN-OPTION = 'EQ'.
    S_VERSN-SIGN   = 'I'.
    APPEND S_VERSN.
    CLEAR S_VERSN.
    S_VERSN-LOW    = '10'.
    S_VERSN-HIGH   = SPACE.
    S_VERSN-OPTION = 'EQ'.
    S_VERSN-SIGN   = 'I'.
    APPEND S_VERSN.
    CLEAR S_VERSN.
    S_TXT04-LOW    = 'TECO'.
    S_TXT04-HIGH   = SPACE.
    S_TXT04-OPTION = 'EQ'.
    S_TXT04-SIGN   = 'I'.
    APPEND S_TXT04.
    CLEAR S_TXT04.
    S_TXT04-LOW    = 'CLSD'.
    S_TXT04-HIGH   = SPACE.
    S_TXT04-OPTION = 'EQ'.
    S_TXT04-SIGN   = 'I'.
    APPEND S_TXT04.
    CLEAR S_TXT04.
*   loop at screen.
*     check screen-group1 = 'ABC'.
      MODIFY SCREEN.
*   endloop.
  ENDIF.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.
* set up the special inservice date - Jan 1 with the parameter year
  INSERVDT = P_YEAR.
  INSERVDT+4(4) = '0101'.

* open file and insert heading line if required
  IF P_FILE = 'X'.
    OPEN DATASET P_OUTFIL FOR OUTPUT IN TEXT MODE.
    IF SY-SUBRC NE '0'.
      MESSAGE E699 WITH 'Could not open output file'.
    ELSE.
      MOVE TEXT-001   TO FILINE-VERNR.                  " division
      MOVE TEXT-034   TO FILINE-ANLKL.                  " asset class
      MOVE TEXT-006   TO FILINE-WBS.                    " wbs element
      MOVE TEXT-028   TO FILINE-POST1.                  " WBS desc
      MOVE TEXT-007   TO FILINE-PROJECT.                " project id
      MOVE TEXT-008   TO FILINE-TXT04.                  " status
      MOVE TEXT-009   TO FILINE-BZDAT.                  " inservice date
      MOVE TEXT-010   TO FILINE-AMT_001.                " rpt amt per 01
      MOVE TEXT-011   TO FILINE-AMT_002.                " rpt amt per 02
      MOVE TEXT-012   TO FILINE-AMT_003.                " rpt amt per 03
      MOVE TEXT-013   TO FILINE-AMT_004.                " rpt amt per 04
      MOVE TEXT-014   TO FILINE-AMT_005.                " rpt amt per 05
      MOVE TEXT-015   TO FILINE-AMT_006.                " rpt amt per 04
      MOVE TEXT-016   TO FILINE-AMT_007.                " rpt amt per 07
      MOVE TEXT-017   TO FILINE-AMT_008.                " rpt amt per 08
      MOVE TEXT-018   TO FILINE-AMT_009.                " rpt amt per 09
      MOVE TEXT-019   TO FILINE-AMT_010.                " rpt amt per 10
      MOVE TEXT-020   TO FILINE-AMT_011.                " rpt amt per 11
      MOVE TEXT-021   TO FILINE-AMT_012.                " rpt amt per 12
      MOVE TEXT-022   TO FILINE-AMT_TOT.                " rpt amt total
      TRANSFER FILINE TO P_OUTFIL.
      CLEAR FILINE.
    ENDIF.
  ENDIF.

* -------------------------------------------------------------------- *
* get the WBS elements with an active status of technically complete
* (TECO-I0045) or closed (CLSD-I0046) or as per selection screen.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Reading active WBS elements'
       EXCEPTIONS
            OTHERS = 1.

  SELECT * FROM PRPS                              " getWBS elements
  WHERE VERNR IN S_VERNR
  AND   BELKZ EQ 'X'.

    SELECT * FROM JEST                            " get the status
    WHERE OBJNR EQ PRPS-OBJNR
    AND   INACT EQ SPACE.

      CLEAR TJ02T.
      SELECT SINGLE * FROM TJ02T                  " is status required
      WHERE ISTAT EQ JEST-STAT
      AND   SPRAS EQ 'E'.

      IF TJ02T-TXT04 IN S_TXT04.

        WBSTAB-VERNR = PRPS-VERNR.
        WBSTAB-POSID = PRPS-POSID.
        WBSTAB-PRART = PRPS-PRART.
        WBSTAB-POST1 = PRPS-POST1.
        WBSTAB-OBJNR = PRPS-OBJNR.
        WBSTAB-TXT04 = TJ02T-TXT04.
        APPEND WBSTAB.
        CLEAR WBSTAB.

      ENDIF.
    ENDSELECT.
  ENDSELECT.
  COMMIT WORK.                                    " release buffers

  SORT WBSTAB BY VERNR POSID.                     " sort by division

* -------------------------------------------------------------------- *
* get all associated costs for each WBS element.  First determine the
* low and high values of the division to appear on the heading. If the
* inservice year is equal to the fiscal year on the selection screen
* the amounts are retrieved for the currant year and all prior years,
* otherwise only amounts for the currant year are required.  NOTE that
* only periods one through twelve appear on the report, thus any amounts
* in periods thirteen through fifteen will appear in the ttoal column
* only.  Please check periods 13-16 for any discrepancies between
* period 1-12 amounts and the total amount.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Getting actual expenditures by WBS element'
       EXCEPTIONS
            OTHERS = 1.

  PASS1 = 'Y'.
  LOOP AT WBSTAB.

    IF PASS1 = 'Y'.
      DIV_LO = WBSTAB-VERNR+5(3).
      PASS1 = 'N'.
    ENDIF.
    DIV_HI = WBSTAB-VERNR+5(3).

    YEAR = SPACE.
    CLEAR BZDAT.                                              "99/01/27
    SELECT SINGLE * FROM COBRA
    WHERE  OBJNR EQ WBSTAB-OBJNR.
    IF SY-SUBRC = 0.
      YEAR = COBRA-BZDAT(4).
      BZDAT = COBRA-BZDAT.                                    "99/01/27
    ENDIF.

    REPTAB-VERNR = WBSTAB-VERNR.
    REPTAB-WBS   = WBSTAB-POSID+7(4).
    REPTAB-POST1 = WBSTAB-POST1.
    REPTAB-POSID = WBSTAB-POSID.
    REPTAB-TXT04 = WBSTAB-TXT04.
    REPTAB-BZDAT = COBRA-BZDAT.
    OPEN_AMT     = 0.

    SELECT * FROM COSS                            " internal postings
    WHERE  OBJNR EQ WBSTAB-OBJNR
    AND    VERSN IN S_VERSN
    AND    WRTTP EQ '04'
    AND    NOT KSTAR IN S_KSTAR.
      IF COSS-GJAHR = P_YEAR.
        REPTAB-AMT_001 = REPTAB-AMT_001 + COSS-WKG001.
        REPTAB-AMT_002 = REPTAB-AMT_002 + COSS-WKG002.
        REPTAB-AMT_003 = REPTAB-AMT_003 + COSS-WKG003.
        REPTAB-AMT_004 = REPTAB-AMT_004 + COSS-WKG004.
        REPTAB-AMT_005 = REPTAB-AMT_005 + COSS-WKG005.
        REPTAB-AMT_006 = REPTAB-AMT_006 + COSS-WKG006.
        REPTAB-AMT_007 = REPTAB-AMT_007 + COSS-WKG007.
        REPTAB-AMT_008 = REPTAB-AMT_008 + COSS-WKG008.
        REPTAB-AMT_009 = REPTAB-AMT_009 + COSS-WKG009.
        REPTAB-AMT_010 = REPTAB-AMT_010 + COSS-WKG010.
        REPTAB-AMT_011 = REPTAB-AMT_011 + COSS-WKG011.
        REPTAB-AMT_012 = REPTAB-AMT_012 + COSS-WKG012.
        REPTAB-AMT_TOT = REPTAB-AMT_TOT
            + COSS-WKG001 + COSS-WKG002 + COSS-WKG003 + COSS-WKG004
            + COSS-WKG005 + COSS-WKG006 + COSS-WKG007 + COSS-WKG008
            + COSS-WKG009 + COSS-WKG010 + COSS-WKG011 + COSS-WKG012
            + COSS-WKG013 + COSS-WKG014 + COSS-WKG015 + COSS-WKG016.
      ELSEIF COSS-GJAHR < P_YEAR
*     and year = p_year.                                    "99/01/27
      AND BZDAT <> INSERVDT.                                "99/01/27
        OPEN_AMT = OPEN_AMT
            + COSS-WKG001 + COSS-WKG002 + COSS-WKG003 + COSS-WKG004
            + COSS-WKG005 + COSS-WKG006 + COSS-WKG007 + COSS-WKG008
            + COSS-WKG009 + COSS-WKG010 + COSS-WKG011 + COSS-WKG012
            + COSS-WKG013 + COSS-WKG014 + COSS-WKG015 + COSS-WKG016.
      ENDIF.
    ENDSELECT.

    SELECT * FROM COSP                            " external postings
    WHERE  OBJNR EQ WBSTAB-OBJNR
    AND    VERSN IN S_VERSN
    AND    WRTTP EQ '04'
    AND    NOT KSTAR IN S_KSTAR.
      IF COSP-GJAHR = P_YEAR.
        REPTAB-AMT_001 = REPTAB-AMT_001 + COSP-WKG001.
        REPTAB-AMT_002 = REPTAB-AMT_002 + COSP-WKG002.
        REPTAB-AMT_003 = REPTAB-AMT_003 + COSP-WKG003.
        REPTAB-AMT_004 = REPTAB-AMT_004 + COSP-WKG004.
        REPTAB-AMT_005 = REPTAB-AMT_005 + COSP-WKG005.
        REPTAB-AMT_006 = REPTAB-AMT_006 + COSP-WKG006.
        REPTAB-AMT_007 = REPTAB-AMT_007 + COSP-WKG007.
        REPTAB-AMT_008 = REPTAB-AMT_008 + COSP-WKG008.
        REPTAB-AMT_009 = REPTAB-AMT_009 + COSP-WKG009.
        REPTAB-AMT_010 = REPTAB-AMT_010 + COSP-WKG010.
        REPTAB-AMT_011 = REPTAB-AMT_011 + COSP-WKG011.
        REPTAB-AMT_012 = REPTAB-AMT_012 + COSP-WKG012.
        REPTAB-AMT_TOT = REPTAB-AMT_TOT
            + COSP-WKG001 + COSP-WKG002 + COSP-WKG003 + COSP-WKG004
            + COSP-WKG005 + COSP-WKG006 + COSP-WKG007 + COSP-WKG008
            + COSP-WKG009 + COSP-WKG010 + COSP-WKG011 + COSP-WKG012
            + COSP-WKG013 + COSP-WKG014 + COSP-WKG015 + COSP-WKG016.
      ELSEIF COSP-GJAHR < P_YEAR
*     and year = p_year.                                    "99/01/27
      AND BZDAT <> INSERVDT.                                "99/01/27
        OPEN_AMT = OPEN_AMT
            + COSP-WKG001 + COSP-WKG002 + COSP-WKG003 + COSP-WKG004
            + COSP-WKG005 + COSP-WKG006 + COSP-WKG007 + COSP-WKG008
            + COSP-WKG009 + COSP-WKG010 + COSP-WKG011 + COSP-WKG012
            + COSP-WKG013 + COSP-WKG014 + COSP-WKG015 + COSP-WKG016.
      ENDIF.
    ENDSELECT.

    CONCATENATE WBSTAB-POSID(7) '0000' INTO WBSPROJ.
    IF OPEN_AMT <> 0                              " opening balance ?
    OR WBSPROJ IN S_POSID.                        " major project ?
      IF YEAR <> SPACE.                           " no inservice year
        IF YEAR = P_YEAR.                         " insrv year = rpt yr
          REPTAB-AMT_TOT = REPTAB-AMT_TOT + OPEN_AMT.
          PERFORM SHIFT_AMOUNTS.
        ENDIF.
      ELSEIF WBSPROJ IN S_POSID.                  " no inservice year
        REPTAB-MAJOR = '*'.
      ENDIF.
    ENDIF.

    CLEAR ZPWBS.
    SELECT SINGLE * FROM ZPWBS                    " get the asset class
    WHERE  WBSEL EQ REPTAB-WBS
    AND    PRART EQ WBSTAB-PRART.
    REPTAB-ANLKL = ZPWBS-ANLKL.

    IF ( REPTAB-AMT_TOT <> 0                      " is there an amount
    OR WBSPROJ IN S_SHADOW )                      " shadow project ?
    AND ( P_DST = 'X'                             " are overheads reqd
    OR P_GSO = 'X'
    OR P_NGV = 'X'
    OR P_ANG = 'X' ).
      PERFORM ALLOCATE_OVERHEADS.
    ENDIF.

    IF REPTAB-AMT_TOT <> 0                        " is there an amount
    OR WBSPROJ IN S_SHADOW.                       " shadow project ?
      APPEND REPTAB.
    ENDIF.
    CLEAR REPTAB.

  ENDLOOP.
  COMMIT WORK.                                    " release buffers

* -------------------------------------------------------------------- *
* sort the table for the report
  SORT REPTAB BY ANLKL WBS POSID.

* -------------------------------------------------------------------- *
* output the report

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Creating the actual expenditure report'
       EXCEPTIONS
            OTHERS = 1.

  LOOP AT REPTAB.
    AT NEW ANLKL.
      CLEAR ANKT.
      SELECT SINGLE * FROM ANKT
      WHERE  SPRAS EQ 'E'
      AND    ANLKL EQ REPTAB-ANLKL.
      NEW-PAGE.
    ENDAT.
    AT NEW WBS.
      NEWBS   = 'Y'.
      WBS_ELE = REPTAB-WBS.
    ENDAT.
    IF NEWBS  = 'Y'.                              " reqd since descript
      NEWBS   = 'N'.                              " chg for same wbs's
      WBS_DSC = REPTAB-POST1.
    ENDIF.
    PERFORM WRITE_DETAIL.
    WBS_ELE = SPACE.
    WBS_DSC = SPACE.
    AT END OF WBS.
      SUM.
      TOT_TYP = 'WBS'.
      PERFORM WRITE_TOTAL.
    ENDAT.
    AT END OF ANLKL.
      SUM.
      TOT_TYP = 'GRP'.
      PERFORM WRITE_TOTAL.
    ENDAT.
    AT LAST.
      SUM.
      TOT_TYP = 'RPT'.
      PERFORM WRITE_TOTAL.
    ENDAT.
  ENDLOOP.

  IF P_FILE = 'X'.
    CLOSE DATASET P_OUTFIL.
  ENDIF.

END-OF-SELECTION.

************************************************************************
*  This section moves all amounts prior to the inservice month to the
*  inservice month amount and sets them to zero.  the opening balance
*  is also added to the inservice month amount.
************************************************************************
FORM SHIFT_AMOUNTS.
  CONCATENATE '0' REPTAB-BZDAT+4(2) INTO MTH.
  FLD(11)   = 'REPTAB-AMT_'.
  FLD+11(3) = MTH.
  ASSIGN (FLD) TO <F1>.

  CLEAR WRK_AMT.
  MTH = MTH - 1.
  ADD REPTAB-AMT_001 FROM 1 TO MTH GIVING WRK_AMT.
  ADD OPEN_AMT TO WRK_AMT.
  ADD WRK_AMT TO <F1>.

  IDX = 0.
  DO MTH TIMES.
    IDX = IDX + 1.
    FLD+11(3) = IDX.
    ASSIGN (FLD) TO <F1>.
    <F1> = 0.
  ENDDO.
ENDFORM.

************************************************************************
*  This section allocates an overheads to the amounts in the report.
*  The amounts are multiplied by the rate (depending on the overhead
*  type) and added to the amount.
************************************************************************
FORM ALLOCATE_OVERHEADS.
  IF ZPWBS-OHTYP = 'DST'
  AND P_DST = 'X'.
    OH_RATE = DST_RAT.
  ELSEIF ZPWBS-OHTYP = 'GSO'
  AND P_GSO = 'X'.
    OH_RATE = GSO_RAT.
  ELSEIF ZPWBS-OHTYP = 'NGV'
  AND P_NGV = 'X'.
    OH_RATE = NGV_RAT.
  ELSEIF ZPWBS-OHTYP = 'ANG'
  AND P_ANG = 'X'.
    OH_RATE = ANG_RAT.
  ELSE.
    OH_RATE = 0.
  ENDIF.

  WRK_AMT        = REPTAB-AMT_001.
  REPTAB-AMT_001 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_002.
  REPTAB-AMT_002 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_003.
  REPTAB-AMT_003 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_004.
  REPTAB-AMT_004 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_005.
  REPTAB-AMT_005 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_006.
  REPTAB-AMT_006 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_007.
  REPTAB-AMT_007 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_008.
  REPTAB-AMT_008 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_009.
  REPTAB-AMT_009 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_010.
  REPTAB-AMT_010 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_011.
  REPTAB-AMT_011 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_012.
  REPTAB-AMT_012 = WRK_AMT + ( WRK_AMT * OH_RATE ).
  WRK_AMT        = REPTAB-AMT_TOT.
  REPTAB-AMT_TOT = WRK_AMT + ( WRK_AMT * OH_RATE ).

ENDFORM.

************************************************************************
*  This section outputs the report headings
************************************************************************
FORM WRITE_HEADER.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 115 TEXT-001, DIV_LO, '-', DIV_HI
            , 239 TEXT-002,  SY-PAGNO
            , 250 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 088 TEXT-003, TEXT-036
            ,     SY-REPID UNDER TEXT-002
            , 250 SY-VLINE.
     WRITE:   /01 SY-VLINE
            , 003 TEXT-004, REPTAB-ANLKL, '-', ANKT-TXK50
            , 115 TEXT-005, P_YEAR
            , 250 SY-VLINE.
     WRITE:  /01 SY-VLINE, SY-MANDT, SY-SYSID, 250 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-006
            , 051 TEXT-007
            , 063 TEXT-008
            , 070 TEXT-009
            , 083 TEXT-010
            , 096 TEXT-011
            , 109 TEXT-012
            , 122 TEXT-013
            , 135 TEXT-014
            , 148 TEXT-015
            , 161 TEXT-016
            , 174 TEXT-017
            , 187 TEXT-018
            , 200 TEXT-019
            , 213 TEXT-020
            , 226 TEXT-021
            , 239 TEXT-022.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

************************************************************************
*  This section outputs the detail lines to the report
************************************************************************
FORM WRITE_DETAIL.
     CONCATENATE REPTAB-POSID+0(02) '-'
                 REPTAB-POSID+2(02) '-'
                 REPTAB-POSID+4(03) INTO PROJECT.
     IF P_FILE = 'X'.
       PERFORM OUTPUT_FILE_REC.
     ENDIF.
     PERFORM MOVE_AMTS_TO_CHAR.
     WRITE: /     WBS_ELE          UNDER TEXT-006
            ,     WBS_DSC
            ,     PROJECT          UNDER TEXT-007
            ,     REPTAB-TXT04     UNDER TEXT-008
            ,     REPTAB-BZDAT     UNDER TEXT-009
            , 080 REPTAB-MAJOR
            ,     AMT_001          UNDER TEXT-010
            ,     AMT_002          UNDER TEXT-011
            ,     AMT_003          UNDER TEXT-012
            ,     AMT_004          UNDER TEXT-013
            ,     AMT_005          UNDER TEXT-014
            ,     AMT_006          UNDER TEXT-015
            ,     AMT_007          UNDER TEXT-016
            ,     AMT_008          UNDER TEXT-017
            ,     AMT_009          UNDER TEXT-018
            ,     AMT_010          UNDER TEXT-019
            ,     AMT_011          UNDER TEXT-020
            ,     AMT_012          UNDER TEXT-021
            ,     AMT_TOT          UNDER TEXT-022.
     PERFORM SHOWVLINE.
ENDFORM.

************************************************************************
*  This section outputs the total lines for the report.  Totals are
*  produced at the WBS element level, the WBS grouping and final totals
*  at the end of the report.
************************************************************************
FORM WRITE_TOTAL.
     PERFORM MOVE_AMTS_TO_CHAR.
     FORMAT INTENSIFIED OFF.
     CASE TOT_TYP.
       WHEN 'WBS'.
         WRITE: / SY-VLINE.
         ULINE AT 49.
         WRITE: / TEXT-023         UNDER TEXT-006.
       WHEN 'GRP'.
         WRITE: / TEXT-024         UNDER TEXT-006.
       WHEN 'RPT'.
         ULINE.
         WRITE: / TEXT-025         UNDER TEXT-006.
     ENDCASE.
     WRITE:       AMT_001          UNDER TEXT-010
            ,     AMT_002          UNDER TEXT-011
            ,     AMT_003          UNDER TEXT-012
            ,     AMT_004          UNDER TEXT-013
            ,     AMT_005          UNDER TEXT-014
            ,     AMT_006          UNDER TEXT-015
            ,     AMT_007          UNDER TEXT-016
            ,     AMT_008          UNDER TEXT-017
            ,     AMT_009          UNDER TEXT-018
            ,     AMT_010          UNDER TEXT-019
            ,     AMT_011          UNDER TEXT-020
            ,     AMT_012          UNDER TEXT-021
            ,     AMT_TOT          UNDER TEXT-022.
     PERFORM SHOWVLINE_TOT.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

************************************************************************
*  This section divides all amount field by 1,000 and then converts
*  them to character format.  This is to remove punctuation and reduce
*  the size so that all of the fields can be accomodated in the report.
************************************************************************
FORM MOVE_AMTS_TO_CHAR.
     WRK_AMT  = REPTAB-AMT_001 / 1000.
     WRITE: WRK_AMT TO AMT_001 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_002 / 1000.
     WRITE: WRK_AMT TO AMT_002 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_003 / 1000.
     WRITE: WRK_AMT TO AMT_003 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_004 / 1000.
     WRITE: WRK_AMT TO AMT_004 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_005 / 1000.
     WRITE: WRK_AMT TO AMT_005 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_006 / 1000.
     WRITE: WRK_AMT TO AMT_006 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_007 / 1000.
     WRITE: WRK_AMT TO AMT_007 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_008 / 1000.
     WRITE: WRK_AMT TO AMT_008 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_009 / 1000.
     WRITE: WRK_AMT TO AMT_009 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_010 / 1000.
     WRITE: WRK_AMT TO AMT_010 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_011 / 1000.
     WRITE: WRK_AMT TO AMT_011 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_012 / 1000.
     WRITE: WRK_AMT TO AMT_012 USING EDIT MASK 'RR_______.__V'.
     WRK_AMT  = REPTAB-AMT_TOT / 1000.
     WRITE: WRK_AMT TO AMT_TOT USING EDIT MASK 'RR_______.__V'.
ENDFORM.

************************************************************************
*  This section writes a record to the output file so that it can be
*  manipulated with a tool such as Excel.
************************************************************************
FORM OUTPUT_FILE_REC.
     MOVE:  REPTAB-VERNR   TO FILINE-VERNR
          , REPTAB-ANLKL   TO FILINE-ANLKL
          , REPTAB-WBS     TO FILINE-WBS
          , REPTAB-POST1   TO FILINE-POST1
          , PROJECT        TO FILINE-PROJECT
          , REPTAB-TXT04   TO FILINE-TXT04
          , REPTAB-BZDAT   TO FILINE-BZDAT.
     WRITE: REPTAB-AMT_001 TO FILINE-AMT_001
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_002 TO FILINE-AMT_002
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_003 TO FILINE-AMT_003
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_004 TO FILINE-AMT_004
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_005 TO FILINE-AMT_005
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_006 TO FILINE-AMT_006
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_007 TO FILINE-AMT_007
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_008 TO FILINE-AMT_008
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_009 TO FILINE-AMT_009
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_010 TO FILINE-AMT_010
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_011 TO FILINE-AMT_011
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_012 TO FILINE-AMT_012
                        USING EDIT MASK 'RRV_____________.__'
          , REPTAB-AMT_TOT TO FILINE-AMT_TOT
                        USING EDIT MASK 'RRV_____________.__'.
    TRANSFER FILINE TO P_OUTFIL.
    CLEAR FILINE.
ENDFORM.

************************************************************************
*  These sections write vertical lines between the columns of the report
************************************************************************
FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 049 SY-VLINE
            , 061 SY-VLINE
            , 068 SY-VLINE
            , 081 SY-VLINE
            , 094 SY-VLINE
            , 107 SY-VLINE
            , 120 SY-VLINE
            , 133 SY-VLINE
            , 146 SY-VLINE
            , 159 SY-VLINE
            , 172 SY-VLINE
            , 185 SY-VLINE
            , 198 SY-VLINE
            , 211 SY-VLINE
            , 224 SY-VLINE
            , 237 SY-VLINE
            , 250 SY-VLINE.
ENDFORM.

FORM SHOWVLINE_TOT.
     WRITE:   001 SY-VLINE
            , 081 SY-VLINE
            , 094 SY-VLINE
            , 107 SY-VLINE
            , 120 SY-VLINE
            , 133 SY-VLINE
            , 146 SY-VLINE
            , 159 SY-VLINE
            , 172 SY-VLINE
            , 185 SY-VLINE
            , 198 SY-VLINE
            , 211 SY-VLINE
            , 224 SY-VLINE
            , 237 SY-VLINE
            , 250 SY-VLINE.
ENDFORM.

************************************************************************
*  This is the end, my freind
************************************************************************
