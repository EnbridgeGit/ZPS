REPORT ZPPMR043.
TYPE-POOLS: SLIS.

************************************************************************
* PROGRAM: ZPPMR043.
* Issue Number: TR855.
* Date:  September 7, 2010.
* Description:
*          THIS PROGRAM IS A COPY OF PROGRAM ZKPSR005 AND FOLLOWING
*          ARE MADE: 1- Cost Element Group is added to the selection
*          screen and it's corresponding changes are made in program.
*          2- Hard coded Cost elements are removed from the program but
*             can be added using selection screen.
*          3- It will produce ALV eport instead of legacy report.
*          4- Selection screen period wilol default from 1 to 12.
*          5- Selection screen year will default to current year.
*          6- If no matching data is found, a message will be displayed
*             and slection screen will remain in front of user.
*
***********************************************************************
* Detailed Warehouse Activity Report: PROJECTS
* AUTHOR : NESH N. LAURENCIC   - OMNILOGIC SYSTEMS GROUP
*          MARVIN J. RADSMA    - OMNILOGIC SYSTEMS GROUP
***********************************************************************
* 00/11/15 mdemeest 4.6B Changed REFBZ to ZLENR for proper indexing into
*                        BSEG.
*
* 98/09/24 md7140   #--- Quantity field is always being processed as a
*                        positive.  Should be +/-
************************************************************************

TABLES: PROJ,                          "Project definition
        PRPS,                          "WBS element description
        COBK,                          "CO Object: Document header
        COEP,                          "CO object:  period-related line
        MSEG,                          "Document Segment: Material
        MKPF,                          "Header: Material Document
        BKPF,                          "Accounting document header
        BSEG,                          "Accounting document segment
        MAKT,                          "Material Descriptions
        SKAT,                          "G/L account master record
        EKPO.                          "Purchasing Document Item

RANGES: WBS_RNG FOR PRPS-POSID.        " WBS element range

DATA: FLAG TYPE C,                     " 1 time run subroutine flag
      PSPID LIKE MSEG-PROJN,           " Because of diff. in length
      I_AWORG(10) TYPE C,              " Function AC_document_record
      TEXT(40) TYPE C,                 " report save area
      UOM(5) TYPE C,                   " report save area
      CURRENCY(3) TYPE C,              " report save area
      POST1 LIKE PRPS-POST1,           " report save area
      ERRORFLAG TYPE C,                " error flag
      LIN TYPE D,                      " lines in table
      TMPMEINS LIKE BSEG-MEINS,        " To avoid stars when do the sum
      LINEITEM LIKE MSEG-BUZEI,        " Diff. format
      BSEGITEM LIKE MSEG-BUZEI,        " Line item in accounting doc
      MSEGITEM LIKE MSEG-BUZEI,        " This one is guesing ...?
      FDATE LIKE SY-DATUM,
      LDATE LIKE SY-DATUM,
      W_HEAD01(60) TYPE C,
      W_HEAD02(60) TYPE C,
      W_HEAD03(60) TYPE C,
      W_HEAD04(60) TYPE C,
      ES_VARIANT LIKE DISVARIANT,
      IS_VARIANT LIKE DISVARIANT.


* EXPORT TABLE FROM FUNCTION
DATA: BEGIN OF T_DOCUMENTS OCCURS 100.
        INCLUDE STRUCTURE ACC_DOC.     " Used by function
DATA: END OF T_DOCUMENTS.

DATA:   BEGIN OF NAME_TAB OCCURS 10.   " Cost Elements
           INCLUDE STRUCTURE RGSB4.
DATA:   END OF NAME_TAB.

DATA: BEGIN OF ITAB OCCURS 500,
        PSPID LIKE PROJ-PSPID,         " Project
        POSKI LIKE PRPS-POSKI,         " WBS ELEMENT
        MATNR LIKE MSEG-MATNR,         " Material number
        BELNR LIKE BSEG-BELNR,         " Document number
        KTEXT LIKE AUFK-KTEXT,         " Line item desc or G/L desc
        POST1 LIKE PRPS-POST1,         " WBS element description
        WERKS LIKE MSEG-WERKS,         " Plant
        LGORT LIKE MSEG-LGORT,         " Storage location
        MENGE LIKE BSEG-MENGE,         " Quantity issued or ordered
        MEINS LIKE BSEG-MEINS,         " Unit of measurment
        PSWBT LIKE BSEG-PSWBT,         " Value in CND
        PSWSL LIKE BSEG-PSWSL,         " Currency
        HKONT LIKE BSEG-HKONT,         " G/L number
        BUDAT LIKE BKPF-BUDAT,         " Document date
        USNAM LIKE BKPF-USNAM,         " User name
        BELNR_TEMP LIKE BSEG-BELNR,    " Always accounting doc. number
        BUZEI LIKE      BSEG-BUZEI,    " Line item number acc doc
      END OF ITAB.
DATA: TEMP_ITAB LIKE ITAB.

DATA: BEGIN OF IPRPS OCCURS 300,       " PRPS/COBK/COEP structure
        PSPID LIKE PROJ-PSPID,
        PSPNR LIKE PRPS-PSPNR,
        POSID LIKE PRPS-POSID,
        OBJNR LIKE PRPS-OBJNR,
        POSKI LIKE PRPS-POSKI,
        PKOKR LIKE PRPS-PKOKR,
        PBUKR LIKE PRPS-PBUKR,
        BELNR LIKE COEP-BELNR,
        POST1 LIKE PRPS-POST1,         "TR855 NEW
        BUZEI LIKE COEP-BUZEI,
        ZLENR LIKE COEP-ZLENR,
        REFBN LIKE COBK-REFBN,
        AWTYP LIKE COBK-AWTYP,
      END OF IPRPS.

DATA: BEGIN OF IMSEG,                  " MSEG structure
        MBLNR LIKE MSEG-MBLNR,
        MJAHR LIKE MSEG-MJAHR,
        MATNR LIKE MSEG-MATNR,
        WERKS LIKE MSEG-WERKS,
        LGORT LIKE MSEG-LGORT,
        DMBTR LIKE MSEG-DMBTR,
        EBELN LIKE MSEG-EBELN,
        EBELP LIKE MSEG-EBELP,
        SAKTO LIKE MSEG-SAKTO,
      END OF IMSEG.

DATA: BEGIN OF IBSEG,                  " BSEG structure
        BELNR LIKE BSEG-BELNR,
        BUZEI LIKE BSEG-BUZEI,
        SHKZG LIKE BSEG-SHKZG,
        PSWBT LIKE BSEG-PSWBT,
        PSWSL LIKE BSEG-PSWSL,
        SGTXT LIKE BSEG-SGTXT,
        HKONT LIKE BSEG-HKONT,
        WERKS LIKE BSEG-WERKS,
        MENGE LIKE BSEG-MENGE,
        MEINS LIKE BSEG-MEINS,
      END OF IBSEG.

DATA: BEGIN OF IMKPF,                  " MKPF structure
        BUDAT LIKE MKPF-BUDAT,
        USNAM LIKE MKPF-USNAM,
        AWSYS LIKE MKPF-AWSYS,
      END OF IMKPF.

DATA: BEGIN OF IBKPF ,                  " BKPF structure
        BUDAT LIKE BKPF-BUDAT,
        USNAM LIKE BKPF-USNAM,
      END OF IBKPF.

RANGES: R_KSTAR FOR COEP-KSTAR.
*************************   Selection Screen ************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
  SELECT-OPTIONS: SPSPID FOR PROJ-PSPID
                   MATCHCODE OBJECT PRSM OBLIGATORY.
SELECTION-SCREEN SKIP 1.
  PARAMETERS: P_CEGRP LIKE KKB0-KST. "Cost Element Group
SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(2) TEXT-012.
   SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS: S_KSTAR FOR COEP-KSTAR.

SELECTION-SCREEN SKIP 1.
  PARAMETERS: FPOPER LIKE T009B-POPER DEFAULT '01' OBLIGATORY. "From per
    PARAMETERS:  LPOPER LIKE T009B-POPER DEFAULT '12'.         "To perio
* Fiscal variant is hidden, default value can be easily changed
PARAMETERS: BDATJ LIKE T009B-BDATJ DEFAULT SY-DATUM(4)
                                            OBLIGATORY, "Year
            PERIV LIKE T009B-PERIV DEFAULT 'K1'
                                       NO-DISPLAY. "Fiscal year variant
SELECTION-SCREEN SKIP 1.
    PARAMETERS: P_SUMRPT(1)  DEFAULT 'X' NO-DISPLAY.
    PARAMETERS PVARIANT LIKE DISVARIANT-VARIANT.        "Display Variant
SELECTION-SCREEN END OF BLOCK B1.

*********************END-of-selection screen****************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CEGRP.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0H'
            FIELD_NAME         = 'KSTAR'
*            SEARCHFLD          = P_SEARCH
            SEARCHFLD_REQUIRED = SPACE
            START_COLUMN       = 10
            START_ROW          = 5
            TABLE              = 'CCSS'
            TYPELIST           = 'BS'
       IMPORTING
            SET_NAME           = P_CEGRP
       EXCEPTIONS
            NO_SET_PICKED      = 1
            OTHERS             = 2.

*Select Display variant
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZPPMR043'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = IS_VARIANT
*           I_TABNAME_HEADER    =
*           I_TABNAME_ITEM      =
*           IT_DEFAULT_FIELDCAT =
            I_SAVE              = 'A'
        IMPORTING
*           E_EXIT              =
            ES_VARIANT          = ES_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1
            PROGRAM_ERROR       = 2
            OTHERS              = 3.
  PVARIANT = ES_VARIANT-VARIANT.

* Either a cost element group is entered or at least one cost element
AT SELECTION-SCREEN.
  IF ( S_KSTAR-LOW  IS INITIAL   AND  P_CEGRP IS INITIAL ) OR
     ( S_KSTAR-LOW(1) <> SPACE   AND  P_CEGRP(1) <> SPACE ).
     MESSAGE E001(U2).
  ENDIF.

*&---------------------------------------------------------------------*
*&                 START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF P_CEGRP(1) <> SPACE.
     PERFORM GET_COST_ELEMENTS USING P_CEGRP.
  ELSE.
     R_KSTAR[] = S_KSTAR[].
  ENDIF.
  PERFORM FIRST_AND_LAST_DAY_IN_PERIOD.
  MOVE BDATJ TO I_AWORG.

* Assign last period in that fiscal year
  IF LPOPER EQ SPACE.
    IF LDATE(4) EQ FDATE(4).
      MOVE  LDATE+4(2) TO LPOPER.
    ELSE.
      LPOPER = '12' + LDATE+4(2).
    ENDIF.
  ENDIF.

* get all required projects/WBS elements
  SELECT PSPID
  FROM   PROJ
  INTO   CORRESPONDING FIELDS OF IPRPS
  WHERE  PSPID IN SPSPID
  AND    LOEVM EQ SPACE.

    CONCATENATE IPRPS-PSPID(7) '0000' INTO WBS_RNG-LOW.
    CONCATENATE IPRPS-PSPID(7) '9999' INTO WBS_RNG-HIGH.

*$    SELECT PSPNR POSID OBJNR POSKI PKOKR PBUKR POST1
    SELECT PSPNR POSID OBJNR POSKI PKOKR PBUKR POST1
    FROM   PRPS
    INTO   CORRESPONDING FIELDS OF IPRPS
    WHERE  POSID GE WBS_RNG-LOW
    AND    POSID LE WBS_RNG-HIGH
    AND    BELKZ EQ 'X'.

      SELECT BELNR BUZEI ZLENR
        FROM   COEP
        INTO   CORRESPONDING FIELDS OF IPRPS
        WHERE  OBJNR EQ IPRPS-OBJNR
        AND    KSTAR IN R_KSTAR
        AND    GJAHR EQ BDATJ
        AND    PERIO GE FPOPER
        AND    KOKRS EQ IPRPS-PKOKR
        AND    BUKRS EQ IPRPS-PBUKR
        AND    OBJNR EQ IPRPS-OBJNR
        AND    WRTTP EQ '04'
        AND    PERIO LE LPOPER.

        SELECT SINGLE REFBN AWTYP
        FROM   COBK
        INTO   CORRESPONDING FIELDS OF IPRPS
        WHERE  KOKRS EQ IPRPS-PKOKR
        AND    BELNR EQ IPRPS-BELNR.

        APPEND IPRPS.

      ENDSELECT.
    ENDSELECT.
  ENDSELECT.
  COMMIT WORK.

* Process all data currently selected
  LOOP AT IPRPS.
    LINEITEM = IPRPS-BUZEI.                       " CO doc line item
*    BSEGITEM = IPRPS-REFBZ.   "4.6B mdemeest     " BSEG line item
    bsegitem = iprps-zlenr.    "4.6B mdemeest
    MSEGITEM = LINEITEM + 1.                      " when items not match
    CASE IPRPS-AWTYP.
      WHEN 'MKPF '.
        CLEAR: MSEG, IMSEG.
        SELECT SINGLE MBLNR MJAHR MATNR WERKS LGORT
                      DMBTR EBELN EBELP SAKTO
        INTO   IMSEG
        FROM   MSEG
        WHERE  MBLNR EQ IPRPS-REFBN
        AND    MJAHR EQ BDATJ
        AND    ZEILE EQ LINEITEM
        AND    PS_PSP_PNR EQ IPRPS-PSPNR.
        IF SY-SUBRC <> 0.
          SELECT SINGLE MBLNR MJAHR MATNR WERKS LGORT
                        DMBTR EBELN EBELP SAKTO
          INTO   IMSEG
          FROM   MSEG
          WHERE  MBLNR EQ IPRPS-REFBN
          AND    MJAHR EQ BDATJ
          AND    ZEILE EQ MSEGITEM
          AND    PS_PSP_PNR EQ IPRPS-PSPNR.
        ENDIF.
        CHECK SY-SUBRC = 0.

        CLEAR: MKPF, IMKPF.
        SELECT SINGLE BUDAT USNAM AWSYS
        INTO   IMKPF
        FROM   MKPF
        WHERE  MBLNR EQ IMSEG-MBLNR
        AND    MJAHR EQ IMSEG-MJAHR.
        CHECK SY-SUBRC = 0.

        REFRESH T_DOCUMENTS.
        FLAG = 1.
        CALL FUNCTION 'AC_DOCUMENT_RECORD'
             EXPORTING
                  I_AWTYP      = 'MKPF '
                  I_AWREF      = IMSEG-MBLNR     " Material document nbr
                  I_AWORG      = I_AWORG "Year
                  I_AWSYS      = IMKPF-AWSYS
*                 I_AWTYP_INCL = ' '
*                 I_AWTYP_EXCL = ' '
                  I_BUKRS      = IPRPS-PBUKR
                  X_DIALOG     = ' '
             TABLES
                  T_DOCUMENTS  = T_DOCUMENTS
             EXCEPTIONS
                  NO_REFERENCE = 1
                  NO_DOCUMENT  = 2
                  OTHERS       = 3.

        IF SY-SUBRC = 0.
          LOOP AT T_DOCUMENTS
            WHERE AWTYP EQ 'BKPF'
            AND   BUKRS EQ IPRPS-PBUKR.
          ENDLOOP.
          IF SY-SUBRC = 0.
            PERFORM MAT_AND_AC_DOCUMENTS.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.
      WHEN 'BKPF '.
        PERFORM FIND_REST_OF_AC_DOCUMENTS.
    ENDCASE.

    CLEAR: MSEG, FLAG, LINEITEM, MSEGITEM, BSEGITEM.
  ENDLOOP.

  DESCRIBE TABLE ITAB LINES LIN.
  IF LIN = 0.
     CALL FUNCTION 'C14A_POPUP_NO_HITS'.                   "TR855
     STOP.
*   MESSAGE E999(AT) WITH TEXT-887.
  ENDIF.

* Check currency and conver into canadian dollars
  LOOP AT ITAB.
    IF ITAB-PSWSL NE 'CAD'.
      PERFORM CONVERT_INTO_CANADIAN.
    ENDIF.
  ENDLOOP.

  SORT ITAB BY PSPID POSKI MATNR BELNR.

  PERFORM DISPLAY_ALV_GRID_DATA.
***************************FORMS****************************************
FORM GET_COST_ELEMENTS USING SETNAME.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0H'
            SETNR         = SETNAME
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = NAME_TAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.

  REFRESH R_KSTAR.
  LOOP AT NAME_TAB.
       CLEAR:  R_KSTAR-LOW, R_KSTAR-HIGH.
       R_KSTAR-SIGN = 'I'.
       R_KSTAR-OPTION = 'BT'.
       MOVE NAME_TAB-FROM TO R_KSTAR-LOW.
       MOVE NAME_TAB-TO   TO R_KSTAR-HIGH.
       APPEND R_KSTAR.
       CLEAR  R_KSTAR.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MAT_AND_AC_DOCUMENTS
*&---------------------------------------------------------------------*
*       Finding material document and assoc.  accounting document      *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAT_AND_AC_DOCUMENTS.
  ITAB-PSPID = IPRPS-PSPID.            " PROJECT
  ITAB-POSKI = IPRPS-POSKI.            " WBS ELEMENT
  ITAB-MATNR = IMSEG-MATNR.            " Material number
  ITAB-POST1 = IPRPS-POST1.             "PS: Short description
* Material description from material master or purchase order.
  IF ITAB-MATNR NE SPACE.              " If there is material number
    SELECT SINGLE * FROM MAKT
    WHERE  MATNR EQ ITAB-MATNR
    AND    SPRAS EQ SY-LANGU.
    ITAB-KTEXT = MAKT-MAKTX.
  ELSE.                         " No matnr number , I am taking from PO
    SELECT SINGLE * FROM EKPO
    WHERE  EBELN EQ IMSEG-EBELN
    AND    EBELP EQ IMSEG-EBELP.
    ITAB-KTEXT = EKPO-TXZ01.
  ENDIF.
  ITAB-WERKS = IMSEG-WERKS.            " Plant
  ITAB-LGORT = IMSEG-LGORT.            " Storage location
  ITAB-BELNR = IMSEG-MBLNR.            " Material document number

  SELECT SINGLE * FROM BSEG
  WHERE  BUKRS EQ IPRPS-PBUKR
  AND    BELNR EQ T_DOCUMENTS-DOCNR
  AND    GJAHR EQ BDATJ
  AND    BUZEI EQ BSEGITEM
  AND    PSWBT EQ IMSEG-DMBTR
  AND    HKONT EQ IMSEG-SAKTO
  AND    MATNR EQ IMSEG-MATNR
  AND    PROJK EQ IPRPS-PSPNR.
  CHECK SY-SUBRC = 0.

  ITAB-HKONT = BSEG-HKONT.             " G/L number
  ITAB-MEINS = BSEG-MEINS.             " Unit od measurment
  CASE BSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = BSEG-PSWBT.         " Value in CAD
      ITAB-MENGE = BSEG-MENGE.         " Quantity        MD7140 98/09/24
    WHEN 'H'.
      ITAB-PSWBT = BSEG-PSWBT * ( -1 )." Credit
      ITAB-MENGE = BSEG-MENGE * ( -1 )."                 MD7140 98/09/24
  ENDCASE.
  ITAB-PSWSL = BSEG-PSWSL.             " Currency
  ITAB-BELNR_TEMP = BSEG-BELNR.        " Accounting document number
  ITAB-BUZEI = BSEG-BUZEI.             " Line item
  ITAB-BUDAT = IMKPF-BUDAT.            "Posting date
  ITAB-USNAM = IMKPF-USNAM.            "User name
  APPEND ITAB.
  CLEAR ITAB.

ENDFORM.                               " MAT_AND_AC_DOCUMENTS

*&---------------------------------------------------------------------*
*&      Form  FIND_REST_OF_AC_DOCUMENTS
*&---------------------------------------------------------------------*
*       Find rest of acc docs                                          *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_REST_OF_AC_DOCUMENTS.
* Just valid documents, for enetered period
  SELECT SINGLE BUDAT USNAM
  INTO   IBKPF
  FROM   BKPF
  WHERE  BUKRS EQ IPRPS-PBUKR
  AND    BELNR EQ IPRPS-REFBN
  AND    GJAHR EQ BDATJ
  AND    BUDAT BETWEEN FDATE AND LDATE
  AND    BSTAT EQ SPACE.
  CHECK SY-SUBRC = 0.

  SELECT SINGLE BELNR BUZEI SHKZG PSWBT PSWSL
                SGTXT HKONT WERKS MENGE MEINS
  INTO   IBSEG
  FROM   BSEG
  WHERE  BUKRS EQ IPRPS-PBUKR
  AND    BELNR EQ IPRPS-REFBN
  AND    GJAHR EQ BDATJ
  AND    BUZEI EQ BSEGITEM
  AND    PROJK EQ IPRPS-PSPNR.
  CHECK SY-SUBRC = 0.

  CLEAR ITAB.
  ITAB-PSPID = IPRPS-PSPID.            " PROJECT
  ITAB-POSKI = IPRPS-POSKI.            " WBS ELEMENT
  ITAB-POST1 = IPRPS-POST1.            "PS: Short description
  ITAB-BELNR = IPRPS-BELNR.            " Acc doc #
  ITAB-BELNR_TEMP = IBSEG-BELNR.       " Acc doc #
  ITAB-HKONT = IBSEG-HKONT.            " G/L number
  CASE IBSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = IBSEG-PSWBT.        " Value in CAD
    WHEN 'H'.
      ITAB-PSWBT = IBSEG-PSWBT * ( -1 ) .
  ENDCASE.
  ITAB-PSWSL = IBSEG-PSWSL.            " Currency
  ITAB-MENGE = IBSEG-MENGE.            " Quantity
  ITAB-MEINS = IBSEG-MEINS.            " Unit of measure
  ITAB-WERKS = IBSEG-WERKS.            " Plant
  ITAB-BUZEI = IBSEG-BUZEI.            " Line itam in acc. document
  ITAB-BUDAT = IBKPF-BUDAT.            " Document date
  ITAB-USNAM = IBKPF-USNAM.            " User name
* Description for G/L
  IF IBSEG-SGTXT EQ SPACE.
    SELECT SINGLE * FROM SKAT
    WHERE  SPRAS EQ SY-LANGU
    AND    KTOPL EQ 'COAT'
    AND    SAKNR EQ ITAB-HKONT.
    ITAB-KTEXT = SKAT-TXT20.
  ELSE.
    ITAB-KTEXT = IBSEG-SGTXT.
  ENDIF.

  MOVE ITAB TO TEMP_ITAB.              " Temp stored header-line.

  CLEAR ITAB.
  READ TABLE ITAB WITH KEY
       PSPID      = TEMP_ITAB-PSPID
       POSKI      = TEMP_ITAB-POSKI
       HKONT      = TEMP_ITAB-HKONT
       BELNR_TEMP = TEMP_ITAB-BELNR_TEMP
       BUZEI      = TEMP_ITAB-BUZEI.

  IF SY-SUBRC <> 0.
    MOVE TEMP_ITAB TO ITAB.
    APPEND ITAB.
  ENDIF.
  CLEAR ITAB.
  CLEAR IBSEG.
ENDFORM.                               " FIND_REST_OF_AC_DOCUMENTS

*&---------------------------------------------------------------------*
*&      Form  FIRST_AND_LAST_DAY_IN_PERIOD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIRST_AND_LAST_DAY_IN_PERIOD.
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR        = BDATJ
*         I_MONMIT       = 00
            I_PERIV        = PERIV
            I_POPER        = FPOPER
       IMPORTING
            E_DATE         = FDATE
       EXCEPTIONS
            INPUT_FALSE    = 1
            T009_NOTFOUND  = 2
            T009B_NOTFOUND = 3
            OTHERS         = 4.
  IF SY-SUBRC NE 0.
    MESSAGE E019(ZS) WITH TEXT-999.
  ENDIF.
  IF LPOPER NE SPACE.
    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
         EXPORTING
              I_GJAHR        = BDATJ
*         I_MONMIT       = 00
              I_PERIV        = PERIV
              I_POPER        = LPOPER
         IMPORTING
              E_DATE         = LDATE
         EXCEPTIONS
              INPUT_FALSE    = 1
              T009_NOTFOUND  = 2
              T009B_NOTFOUND = 3
              OTHERS         = 4.
    IF SY-SUBRC <> 0.
      MESSAGE E019(ZS) WITH TEXT-999.
    ENDIF.
  ELSE.
    CALL FUNCTION 'LAST_DAY_IN_YEAR_GET'
         EXPORTING
              I_DATE         = FDATE
              I_PERIV        = PERIV
         IMPORTING
              E_DATE         = LDATE
         EXCEPTIONS
              INPUT_FALSE    = 1
              T009_NOTFOUND  = 2
              T009B_NOTFOUND = 3
              OTHERS         = 4.

  ENDIF.
ENDFORM.                               " FIRST_AND_LAST_DAY_IN_PERIOD

*&---------------------------------------------------------------------*
*&      Form  CONVERT_INTO_CANADIAN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONVERT_INTO_CANADIAN.
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            DATE             = ITAB-BUDAT
            FOREIGN_AMOUNT   = ITAB-PSWBT
            FOREIGN_CURRENCY = ITAB-PSWSL
            LOCAL_CURRENCY   = 'CAD'
*         RATE             = 0
            TYPE_OF_RATE     = 'M'
       IMPORTING
*          exchange_rate    =
*          foreign_factor   =
             LOCAL_AMOUNT     = ITAB-PSWBT
*          local_factor     =
*          exchange_ratex   =
       EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            OTHERS           = 4.
  IF SY-SUBRC = 0.
    ITAB-PSWSL = 'CAD'.
    MODIFY ITAB.
  ELSE.
    EXIT.
  ENDIF.
ENDFORM.                               " CONVERT_INTO_CANADIAN

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
*                 DISPLAY_ALV_GRID_DATA
**----------------------------------------------------------------------
 FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV,
      W_FIELD(4).

 IF SPSPID-HIGH = SPACE.
    CONCATENATE TEXT-001 SPSPID-LOW(2) TEXT-DSH SPSPID-LOW+2(2)
          TEXT-DSH SPSPID-LOW+4(3) INTO W_HEAD01 SEPARATED BY SPACE.
 ELSE.
    CONCATENATE TEXT-001 SPSPID-LOW(2) TEXT-DSH SPSPID-LOW+2(2)
    TEXT-DSH SPSPID-LOW+4(3) 'To' SPSPID-HIGH(2) TEXT-DSH
    SPSPID-HIGH+2(2) TEXT-DSH SPSPID-HIGH+4(3)
                      INTO W_HEAD01 SEPARATED BY SPACE.
 ENDIF.

  CONCATENATE LPOPER ',' INTO W_FIELD.
  CONCATENATE TEXT-980 FPOPER TEXT-991 W_FIELD BDATJ INTO W_HEAD02
                                               SEPARATED BY SPACE.
 IF P_CEGRP <> SPACE.
    CONCATENATE TEXT-981 P_CEGRP INTO W_HEAD03 SEPARATED BY SPACE.
 ENDIF.

  MOVE TEXT-CLT  TO W_HEAD04+0(7).
  MOVE SY-SYSID  TO W_HEAD04+8(5).
  MOVE SY-MANDT  TO W_HEAD04+14(4).
  MOVE TEXT-DTE  TO W_HEAD04+21(5).
  WRITE SY-DATUM TO W_HEAD04+27(10).
  MOVE TEXT-TME  TO W_HEAD04+40(5).
  WRITE SY-UZEIT TO W_HEAD04+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.
  CLEAR:  FIELDCAT, FC_STR.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'ITAB'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'PSPID'.
           FC_STR-NO_OUT = 'X'.                " hide column
     WHEN 'POSKI'.
          FC_STR-SELTEXT_L = TEXT-C01.         " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'BELNR'.
           FC_STR-KEY    = ' '.                " Key columns-not first
     WHEN 'POST1'.
          FC_STR-SELTEXT_L = TEXT-C02.         " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'PSWBT'.
          FC_STR-DO_SUM  = 'X'.                " Do Sum
     WHEN 'BELNR_TEMP'.
           FC_STR-TECH   = 'X'.   " Technical field- can't be displayed
     WHEN 'BUZEI'.
           FC_STR-NO_OUT = 'X'.                " hide column
     WHEN OTHERS.

ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = ITAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*4- ACTION LINE:  TYPE A
IF P_CEGRP <> SPACE.
   CLEAR LS_LINE.
   LS_LINE-TYP   = 'A'.
   LS_LINE-KEY   = ''.
   LS_LINE-INFO = W_HEAD03.
   APPEND LS_LINE TO LT_TOP_OF_PAGE.
ENDIF.

*5- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD04.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
