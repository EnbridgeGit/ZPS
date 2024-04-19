*----------------------------------------------------------------------*
*   INCLUDE LKAZBF97                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_ACTIVATE
*&---------------------------------------------------------------------*
*       activate action log for object COAC
*----------------------------------------------------------------------*
FORM ACTION_LOG_ACTIVATE.

  DATA: L_TCODE LIKE BALHDRI-ALTCODE,
        L_REPID LIKE BALHDRI-ALPROG.

  CLEAR GD-SUBOBJECT.
  CLEAR GD-EXTNUMBER.
  CHECK NOT RKAUF-ACT_LOG IS INITIAL.

* set parameters for message handler
  GD-MSG_OBJECT_ID  = '00  '.
  GD-MSG_APPL_ID    = 'COAC'.

* set subobject
  IF RKAUF-STNO EQ TRUE.
    CONCATENATE RKAUF-VRGNG '_REVS' INTO GD-SUBOBJECT.
  ELSE.
    CONCATENATE RKAUF-VRGNG '_EXEC' INTO GD-SUBOBJECT.
  ENDIF.
* set external number
  IF RKAUF-TO EQ RKAUF-FROM.
    CONCATENATE RKAUF-FROM '/' RKAUF-GJAHR
                GD-OBART_TXT
                INTO GD-EXTNUMBER SEPARATED BY ' '.
  ELSE.
    CONCATENATE RKAUF-FROM '/' RKAUF-GJAHR
                RKAUF-TO   '/' RKAUF-GJAHR_BIS
                GD-OBART_TXT
                INTO GD-EXTNUMBER SEPARATED BY ' '.
  ENDIF.

  TRANSLATE GD-EXTNUMBER TO UPPER CASE.     "#EC SYNTCHAR
  WRITE GD-PFLID TO GD-EXTNUMBER+80.

  L_TCODE = SY-TCODE.
  L_REPID = GD-REPID.
  CALL FUNCTION 'CO_APPL_LOG_INIT'
       EXPORTING
            SUBOBJECT = GD-SUBOBJECT
            EXTNUMBER = GD-EXTNUMBER
            EXITPROG  = GD-EXITPROG
            EXITFORM  = GD-EXITFORM
            ANW_TCODE = L_TCODE
            ANW_REPID = L_REPID.
*       TABLES
*            PARAMETER = LT_SPAR.

ENDFORM.                               " ACTION_LOG_ACTIVATE
*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ACTION_LOG_DELETE.

  TABLES: CMFK.

  DATA: L_DDATE    LIKE BALHDR-ALDATE,
        L_ANSWER   TYPE C,
        L_COUNT    LIKE SY-TABIX,
        L_FIELD    LIKE SVAL,
        LT_FIELDS  LIKE L_FIELD OCCURS 0.
  DATA: LD_SUBOBJECT LIKE  BALHDRI-SUBOBJECT,
        LT_CMFK    LIKE  CMFK  OCCURS 0 WITH HEADER LINE.

* set default values
  L_DDATE = SY-DATLO - 1.

* get selection date
  MOVE: 'SYST'   TO L_FIELD-TABNAME,   "Bis-Datum
        'DATLO'  TO L_FIELD-FIELDNAME,
        TEXT-AL1 TO L_FIELD-FIELDTEXT,
        L_DDATE  TO L_FIELD-VALUE.
  APPEND L_FIELD TO LT_FIELDS.

  CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
       EXPORTING
            POPUP_TITLE = TEXT-AL2
       IMPORTING
            RETURNCODE  = L_ANSWER
       TABLES
            FIELDS      = LT_FIELDS.
  CHECK L_ANSWER NE 'A'.

  READ TABLE LT_FIELDS INTO L_FIELD    "Bis-Datum
       WITH KEY FIELDNAME = 'DATLO'.
  CHECK SY-SUBRC = 0.
  MOVE L_FIELD-VALUE TO L_DDATE.
* set application specific parameters
  CONCATENATE RKAUF-VRGNG '*' INTO LD_SUBOBJECT.
  CONCATENATE '*' GD-OBART_TXT '*' INTO GD-EXTNUMBER.
  TRANSLATE GD-EXTNUMBER TO UPPER CASE.    "#EC SYNTCHAR

* delete action logs
  CALL FUNCTION 'CO_APPL_LOG_DELETE'
       EXPORTING
            SUBOBJECT              = LD_SUBOBJECT
            EXTNUMBER              = GD-EXTNUMBER
            DATE_TO                = L_DDATE
            LOG_CLASS              = '1'
       IMPORTING
            NUMBER_OF_DELETED_LOGS = L_COUNT.
* IF SY-SUBRC = 0.
*   MESSAGE I517(C$) WITH L_COUNT.
**********************************************************************
* ACHTUNG: Diese Nachricht (selbsterklärend) ist nicht mehr vorhanden,
* der Text lautete folgendermaßen:
* Anzahl Löschungen in der Protokolldatei: &
**********************************************************************
* ENDIF.
* delete associated messages
  SELECT * FROM CMFK INTO TABLE LT_CMFK
                     WHERE APLID EQ CON_APPL_ID
                       AND DATUM LE L_DDATE.
  LOOP AT LT_CMFK.
    CALL FUNCTION 'CM_F_DELETE_LOG_ON_DB'
         EXPORTING
              APLID  = CON_APPL_ID
              CMF_NR = LT_CMFK-NR.
  ENDLOOP.

ENDFORM.                               " ACTION_LOG_DELETE
*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ACTION_LOG_PROCESS TABLES PT_SPAR STRUCTURE SPAR
                               PT_OBJ  STRUCTURE COAPPLOBJ.

 PERFORM ACTION_LOG_CALL TABLES PT_SPAR
                                PT_OBJ.

ENDFORM.                               " ACTION_LOG_PROCESS

*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_STORE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ACTION_LOG_STORE.

  CHECK NOT RKAUF-ACT_LOG IS INITIAL.

  FIELD-SYMBOLS <FIELD>.

  DATA: LT_SPAR     LIKE   SPAR  OCCURS 0 WITH HEADER LINE,
        LT_NAMETAB  LIKE   DNTAB OCCURS 0 WITH HEADER LINE,
        L_MSG_STORE_NR  LIKE  CMFP-NR.
*----------------------------------------------------------------------*
* store messages on data base
*----------------------------------------------------------------------*
  PERFORM MESSAGES_STORE_ON_DB USING L_MSG_STORE_NR
                                     GD-MSG_OBJECT_ID
                                     GD-MSG_APPL_ID.


  MOVE L_MSG_STORE_NR TO GD-MSG_STORE_NR.
*----------------------------------------------------------------------*
* loop structure RKAUF to table SPAR
*----------------------------------------------------------------------*
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            TABNAME = 'RKAUF'
       TABLES
            NAMETAB = LT_NAMETAB.

  CLEAR RKAUF-PARA.
  LOOP AT LT_NAMETAB.
    ASSIGN COMPONENT LT_NAMETAB-FIELDNAME OF STRUCTURE RKAUF TO <FIELD>.
    LT_SPAR-VALUE = <FIELD>.
    LT_SPAR-PARAM = LT_NAMETAB-FIELDNAME.
    APPEND LT_SPAR.
  ENDLOOP.

  LT_SPAR-PARAM = 'PROCESSOR'.
  LT_SPAR-VALUE = GD-PROCESSOR.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'PROTOCOL'.
  LT_SPAR-VALUE = GD-PROTOCOL.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'REPID_SUB'.
  LT_SPAR-VALUE = GD-REPID_SUB.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'DYNNR_SUB'.
  LT_SPAR-VALUE = GD-DYNNR_SUB.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'REPID_SUB1'.
  LT_SPAR-VALUE = GD-REPID_SUB1.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'DYNNR_SUB1'.
  LT_SPAR-VALUE = GD-DYNNR_SUB1.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'REPID_BTC'.
  LT_SPAR-VALUE = GD-REPID_BTC.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'PERIODS'.
  LT_SPAR-VALUE = GD-PERIODS.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'MODIFY_PARASC'.
  LT_SPAR-VALUE = GD-MODIFY_PARASC.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'MODIFY_PROCSC'.
  LT_SPAR-VALUE = GD-MODIFY_PROCSC.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'SINGLE_PERIOD'.
  LT_SPAR-VALUE = GD-SINGLE_PERIOD.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'SINGLE_GJAHR'.
  LT_SPAR-VALUE = GD-SINGLE_GJAHR.
  APPEND LT_SPAR.

  LT_SPAR-PARAM = 'GD-SUBOBJECT'.
  LT_SPAR-VALUE = GD-SUBOBJECT.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'GD-EXTNUMBER'.
  LT_SPAR-VALUE = GD-EXTNUMBER.
  APPEND LT_SPAR.
  LT_SPAR-PARAM = 'MSG_STORE_NR'.
  LT_SPAR-VALUE = L_MSG_STORE_NR.
  APPEND LT_SPAR.

  CALL FUNCTION 'CO_APPL_LOG_PUT'
       TABLES
            PARAMETER = LT_SPAR.

ENDFORM.                               " ACTION_LOG_STORE
*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_APPEND_OBJNR_SINGLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ACTION_LOG_APPEND_OBJNR_SINGLE USING P_OBJNR LIKE OBJNR.

  CHECK NOT RKAUF-ACT_LOG IS INITIAL.
  CALL FUNCTION 'CO_APPL_LOG_APPEND_OBJNR'
       EXPORTING
            OBJNR = P_OBJNR-OBJNR.

ENDFORM.                               " ACTION_LOG_APPEND_OBJNR_SINGLE

*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_APPEND_OBJNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ACTION_LOG_APPEND_OBJNR TABLES P_OBJNR STRUCTURE OBJNR.

  CHECK NOT RKAUF-ACT_LOG IS INITIAL.
  LOOP AT P_OBJNR.
    CALL FUNCTION 'CO_APPL_LOG_APPEND_OBJNR'
         EXPORTING
              OBJNR = P_OBJNR-OBJNR.
  ENDLOOP.

ENDFORM.                               " ACTION_LOG_APPEND_OBJNR
*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_PROCESS_RESTART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ACTION_LOG_PROCESS_RESTART TABLES PT_SPAR STRUCTURE SPAR
                                       PT_OBJ  STRUCTURE COAPPLOBJ.

  FIELD-SYMBOLS <FIELD>.
  DATA: LD_FIELD(50) TYPE C.

  DATA: LT_NAMETAB  LIKE   DNTAB   OCCURS 0 WITH HEADER LINE.

*     loop structure RKAUF to table SPAR
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            TABNAME = 'RKAUF'
       TABLES
            NAMETAB = LT_NAMETAB.

  LOOP AT PT_SPAR.
    READ TABLE LT_NAMETAB WITH KEY FIELDNAME = PT_SPAR-PARAM.
    IF SY-SUBRC EQ 0.
      CONCATENATE 'RKAUF-' LT_NAMETAB-FIELDNAME INTO LD_FIELD.
      ASSIGN (LD_FIELD) TO <FIELD>.
      WRITE PT_SPAR-VALUE TO <FIELD>.
    ENDIF.
  ENDLOOP.

  READ TABLE PT_SPAR WITH KEY PARAM = 'PROCESSOR'.
  IF SY-SUBRC EQ 0.
    GD-PROCESSOR       = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'PROTOCOL'.
  IF SY-SUBRC EQ 0.
    GD-PROTOCOL         = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'REPID_SUB'.
  IF SY-SUBRC EQ 0.
    GD-REPID_SUB     = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'DYNNR_SUB'.
  IF SY-SUBRC EQ 0.
    GD-DYNNR_SUB     = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'REPID_SUB1'.
  IF SY-SUBRC EQ 0.
    GD-REPID_SUB1     = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'REPID_SUB1'.
  IF SY-SUBRC EQ 0.
    GD-REPID_SUB1     = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'REPID_BTC'.
  IF SY-SUBRC EQ 0.
    GD-REPID_BTC      = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'PERIODS'.
  IF SY-SUBRC EQ 0.
    GD-PERIODS        = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'MODIFY_PARASC'.
  IF SY-SUBRC EQ 0.
    GD-MODIFY_PARASC  = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'MODIFY_PROCSC'.
  IF SY-SUBRC EQ 0.
    GD-MODIFY_PROCSC  = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'SINGLE_PERIOD'.
  IF SY-SUBRC EQ 0.
    GD-SINGLE_PERIOD  = PT_SPAR-VALUE.
  ENDIF.
  READ TABLE PT_SPAR WITH KEY PARAM = 'SINGLE_GJAHR'.
  IF SY-SUBRC EQ 0.
    GD-SINGLE_GJAHR   = PT_SPAR-VALUE.
  ENDIF.

  READ TABLE PT_SPAR WITH KEY PARAM = 'GD-SUBOBJECT'.
  IF SY-SUBRC EQ 0.
    GD-SUBOBJECT     = PT_SPAR-VALUE.
    BALHDR-SUBOBJECT = PT_SPAR-VALUE.
  ENDIF.

  READ TABLE PT_SPAR WITH KEY PARAM = 'GD-EXTNUMBER'.
  IF SY-SUBRC EQ 0.
    GD-EXTNUMBER      = PT_SPAR-VALUE.
    BALHDR-EXTNUMBER = PT_SPAR-VALUE.
  ENDIF.

  REFRESH IT_OBJNR.
  LOOP AT PT_OBJ.
    MOVE PT_OBJ-OBJNR TO IT_OBJNR.
    APPEND IT_OBJNR.
  ENDLOOP.

  IF NOT IT_OBJNR[] IS INITIAL.
    CALL SCREEN 1000.
  ELSE.
    MESSAGE S451(KA).
  ENDIF.
ENDFORM.                               " ACTION_LOG_PROCESS_RESTART

*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_PROCESS_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ACTION_LOG_PROCESS_MESSAGES TABLES   PT_SPAR STRUCTURE SPAR.

  DATA: LT_CMFMSG       LIKE  CMFMSG  OCCURS 0 WITH HEADER LINE,
        L_MSG_STORE_NR  LIKE  CMFP-NR.

  READ TABLE PT_SPAR WITH KEY PARAM = 'MSG_STORE_NR'.
  IF SY-SUBRC EQ 0.
    L_MSG_STORE_NR = PT_SPAR-VALUE.

    CALL FUNCTION 'MESSAGE_GET_FROM_DB'
         EXPORTING
              I_APLID          = 'COAC'
              I_OBJECT_ID      = '00  '
              I_CMF_NR         = L_MSG_STORE_NR
         IMPORTING
              E_IDENTIFICATION = GD-IDENTIFICATION
              E_LINE           = GD-MESG_LINE.

    CLEAR VORHER.
    CLEAR NACHHER.

  ENDIF.

ENDFORM.                               " ACTION_LOG_PROCESS_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_BATCH_START
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM ACTION_LOG_BATCH_START TABLES PT_SELTAB STRUCTURE COSEL2.

  LOOP AT PT_SELTAB WHERE LOW(10) EQ 'OBJNR     '.
    MOVE PT_SELTAB-LOW+10 TO IT_OBJNR.
    APPEND IT_OBJNR.
    DELETE PT_SELTAB.
  ENDLOOP.

  IF NOT IT_OBJNR IS INITIAL.
    GD-ACT_LOG = TRUE.
  ENDIF.

ENDFORM.                               " ACTION_LOG_BATCH_START
*&---------------------------------------------------------------------*
*&      Form  ACTION_LOG_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_SPAR  text
*      -->P_STRUCTURE  text
*      -->P_SPAR  text
*      -->P_PT_OBJ  text
*      -->P_STRUCTURE  text
*      -->P_COAPPLOBJ  text
*----------------------------------------------------------------------*
FORM ACTION_LOG_CALL  TABLES PT_SPAR STRUCTURE SPAR
                             PT_OBJ  STRUCTURE COAPPLOBJ.

  DATA: LD_HEADER(1)    TYPE  C.

*----------------------------------------------------------------------*

  GD-ACT_LOG = TRUE.
  READ TABLE PT_SPAR WITH KEY '%%HEADER'.
  IF SY-SUBRC EQ 0.
    IF PT_SPAR-VALUE EQ SPACE.
      LD_HEADER = FALSE.
    ELSE.
      LD_HEADER = TRUE.
    ENDIF.
  ELSE.
    LD_HEADER = UNDEFINED.
  ENDIF.

  CASE LD_HEADER.
    WHEN TRUE.  "aus dem Kopfbild heraus auf einen Lauf geklickt
*----------------------------------------------------------------------*
*     Nachbearbeitung der Objektliste
*----------------------------------------------------------------------*
      PERFORM ACTION_LOG_PROCESS_MESSAGES TABLES PT_SPAR.
      PERFORM ACTION_LOG_PROCESS_RESTART TABLES PT_SPAR
                                                PT_OBJ.


    WHEN FALSE. "aus dem Detailbild eines Laufs auf Nachricht geklickt
*----------------------------------------------------------------------*
*     Anzeigen des Protokolls zum Lauf
*----------------------------------------------------------------------*
      PERFORM ACTION_LOG_PROCESS_MESSAGES TABLES PT_SPAR.
      CALL FUNCTION 'MESSAGES_SHOW'.

    WHEN UNDEFINED.
*     Alter Arbeitsvorrat -> nicht verarbeitbar -> nichts tun

  ENDCASE.

ENDFORM.                               " ACTION_LOG_CALL
*&---------------------------------------------------------------------*
*&      Form  APPL_LOG_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPL_LOG_DISPLAY using ld_subobject.                "ALR

 CONCATENATE RKAUF-VRGNG '*' INTO LD_SUBOBJECT.         "ALR
 CALL FUNCTION 'CO_APPL_LOG_DISPLAY'                    "ALR
      EXPORTING                                         "ALR
           SUBOBJECT = LD_SUBOBJECT                     "ALR
           EXTNUMBER = '*'.                             "ALR
 LEAVE TO TRANSACTION SY-TCODE.                         "ALR

endform.                                                "ALR
