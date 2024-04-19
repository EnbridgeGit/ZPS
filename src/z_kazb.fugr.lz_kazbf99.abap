*-------------------------------------------------------------------
***INCLUDE LKAZBF99 .
*-------------------------------------------------------------------
* Corrections:
* AL0K108990: Note 697270 15.01.2004 FCL
*---------------------------------------------------------------------*
*  FORM  MESSAGE
*                USING MSGID MSGTY MSGNO
*                      PAR1  PAR2  PAR3  PAR4.
*----------------------------------------------------------------------*
FORM  MESSAGE
              USING P_MAX_ERROR
                    MSGID MSGTY MSGNO
                    PAR1  PAR2  PAR3  PAR4.

  DATA: TXTNR        LIKE MESG-TXTNR,
        LD_SEVERITY  LIKE SY-SUBRC.

*.fill table stat_error
  TXTNR = MSGNO.
  CALL FUNCTION 'MESSAGE_STORE'
       EXPORTING
            ARBGB                   = MSGID
            MSGTY                   = MSGTY
            TXTNR                   = TXTNR
            MSGV1                   = PAR1
            MSGV2                   = PAR2
            MSGV3                   = PAR3
            MSGV4                   = PAR4
            EXCEPTION_IF_NOT_ACTIVE = SPACE
       IMPORTING
            ACT_SEVERITY            = LD_SEVERITY.
  IF LD_SEVERITY > P_MAX_ERROR.
    P_MAX_ERROR = LD_SEVERITY.
  ENDIF.

ENDFORM.                               " MESSAGE
*---------------------------------------------------------------------*
*  FORM  MESSAGE_DEFAULT
*                USING MSGID MSGTY MSGNO
*                      PAR1  PAR2  PAR3  PAR4.
*----------------------------------------------------------------------*
FORM  MESSAGE_DEFAULT
              USING P_MESG_LINE
                    MSGID MSGTY MSGNO
                    PAR1  PAR2  PAR3  PAR4.

  DATA: TXTNR        LIKE MESG-TXTNR.

*.fill table stat_error
  TXTNR = MSGNO.
  CALL FUNCTION 'MESSAGE_SET_DEFAULTLINE'
       EXPORTING
            ARBGB  = MSGID
            MSGTY  = MSGTY
            TXTNR  = TXTNR
            MSGV1  = PAR1
            MSGV2  = PAR2
            MSGV3  = PAR3
            MSGV4  = PAR4
            ZEILE  = P_MESG_LINE
            MODIFY = 'X'.

ENDFORM.                               " MESSAGE_DEFAULT
*eject
*&---------------------------------------------------------------------*
*       FORM MESSAGE_STORE.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM MESSAGE_STORE USING P_MAX_ERROR.

  PERFORM MESSAGE USING
                    P_MAX_ERROR
                    SY-MSGID
                    SY-MSGTY
                    SY-MSGNO
                    SY-MSGV1
                    SY-MSGV2
                    SY-MSGV3
                    SY-MSGV4.
ENDFORM.                               "MESSAGE_STORE

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_HANDLER_DEACTIVATE
*&---------------------------------------------------------------------*
*       temporary deactivation of message handler                      *
*----------------------------------------------------------------------*
FORM MESSAGE_HANDLER_DEACTIVATE.

  CALL FUNCTION 'MESSAGES_ACTIVE'
       EXCEPTIONS
            NOT_ACTIVE    = 1
            ERROR_MESSAGE = 2.
  IF SY-SUBRC = 0.
    MESSAGE_ACTIVE_FLAG = 'X'.         "Message-Handler aktiv
    CALL FUNCTION 'MESSAGES_STOP'.
  ELSEIF SY-SUBRC = 1.
    MESSAGE_ACTIVE_FLAG = ' '.         "Message-Handler nicht aktiv
  ELSE.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " MESSAGE_HANDLER_DEACTIVATE

*&---------------------------------------------------------------------*
*       Form  MESSAGE_HANDLER_AS_BEFORE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM MESSAGE_HANDLER_AS_BEFORE.

  IF MESSAGE_ACTIVE_FLAG = 'X'.
    CALL FUNCTION 'MESSAGES_INITIALIZE'
         EXPORTING
              RESET = ' '.
  ENDIF.

ENDFORM.                               " MESSAGE_HANDLER_AS_BEFORE

*---------------------------------------------------------------------*
*       FORM MESSAGES_CHECK.
*---------------------------------------------------------------------*
*        check whether errors occured
*---------------------------------------------------------------------*
FORM MESSAGES_CHECK USING LD_SUBRC  LIKE  SY-SUBRC
                          L_VORHER  LIKE  SY-TABIX
                          L_NACHHER LIKE  SY-TABIX.

  DATA:     IGNORE_WARNINGS,
            LD_FLAG,
            SEVERITY       LIKE  SY-SUBRC.   "Fehler-Schweregrad

  DATA:     DBG_FLG.
  DATA:     LT_MESG LIKE  MESG OCCURS 0 WITH HEADER LINE.
*---------------------------------------------------------------------*
  IGNORE_WARNINGS = ON.
  LD_SUBRC = 0.

*---------------------------------------------------------------------*
* count number of messages afterwards
*---------------------------------------------------------------------*

  CALL FUNCTION 'MESSAGES_COUNT'
       EXPORTING
            ONLY_FOR_ACTUAL_LINE = ON
       IMPORTING
            COUNT                = L_NACHHER
            MAX_SEVERITY         = SEVERITY.

IF DBG_FLG EQ TRUE.
CALL FUNCTION 'MESSAGES_GIVE'
     TABLES
          T_MESG = LT_MESG.
ENDIF.
*---------------------------------------------------------------------*
* react if messages occured
*---------------------------------------------------------------------*
  IF L_VORHER <> L_NACHHER.
    IF SEVERITY > 8.
      LD_SUBRC = 1.                    "MESSAGES_STORED.
    ELSEIF IGNORE_WARNINGS = OFF.
      CLEAR LD_FLAG.
      CALL FUNCTION 'MESSAGES_SHOW'
           EXPORTING
                CORRECTIONS_OPTION = 'X'
           IMPORTING
                CORRECTIONS_WANTED = LD_FLAG.
*     abort on warning
      IF LD_FLAG = ON.
        PERFORM OK_LEAVE.              "CORRECTIONS_WANTED.
      ELSE.
        CALL FUNCTION 'MESSAGES_INITIALIZE'.
        L_NACHHER = 0.
      ENDIF.
    ENDIF.
    L_VORHER = L_NACHHER.
  ENDIF.

ENDFORM.                               "MESSAGES_CHECK

*&---------------------------------------------------------------------*
*&      Form  OK_LEAVE
*&---------------------------------------------------------------------*
FORM OK_LEAVE.

  IF SY-DYNNR = DYNPRO.
    LEAVE TO TRANSACTION SY-TCODE.
  ELSE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                               " OK_LEAVE

*&---------------------------------------------------------------------*
*&      Form  MESSAGES_COUNT
*&---------------------------------------------------------------------*
*    Count previous error messages are counted.
*    Supplies parameter VORHER for check in MESSAGES_CHECK.
*----------------------------------------------------------------------*
FORM MESSAGES_COUNT USING L_COUNT.

  CALL FUNCTION 'MESSAGES_COUNT'
       EXPORTING
            ONLY_FOR_ACTUAL_LINE = ON
       IMPORTING
            COUNT                = L_COUNT.
*            MAX_SEVERITY         = SEVERITY.

ENDFORM.                               " MESSAGES_COUNT

*&---------------------------------------------------------------------*
*&      FORM  MESSAGES_SHOW
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM MESSAGES_SHOW.

  CALL FUNCTION 'MESSAGES_SHOW'.

* PERFORM MESSAGES_STOP.

ENDFORM.                               " MESSAGES_SHOW

*&---------------------------------------------------------------------*
*&      Form  MESSAGES_STOP
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM MESSAGES_STOP.

  CALL FUNCTION 'MESSAGES_STOP'.
*      EXCEPTIONS
*          A_MESSAGE = 1
*           E_MESSAGE = 2
*           I_MESSAGE = 3
*           W_MESSAGE = 4
*           OTHERS    = 5.

ENDFORM.                               " MESSAGES_STOP
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_LINE_ADD_1
*&---------------------------------------------------------------------*
*       set message line
*----------------------------------------------------------------------*
FORM MESSAGE_LINE_ADD_1 USING P_MESG_LINE LIKE MESG-ZEILE.

  DATA: LD_COUNT  LIKE  SY-TABIX.

  CALL FUNCTION 'MESSAGES_COUNT'
       EXPORTING
            ONLY_FOR_ACTUAL_LINE = 'X'
       IMPORTING
            COUNT                = LD_COUNT.

  IF LD_COUNT GT 1.
    ADD 1 TO P_MESG_LINE.
    CALL FUNCTION 'MESSAGE_LINE_SET'
         EXPORTING
              ZEILE = P_MESG_LINE.
  ENDIF.

ENDFORM.                               " MESSAGE_LINE_ADD_1
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_LINE_SUB_1
*&---------------------------------------------------------------------*
*       set message line
*----------------------------------------------------------------------*
FORM MESSAGE_LINE_SUB_1 USING P_MESG_LINE LIKE MESG-ZEILE.

  SUBTRACT 1 FROM P_MESG_LINE.
  CALL FUNCTION 'MESSAGE_LINE_SET'
       EXPORTING
            ZEILE = P_MESG_LINE.

ENDFORM.                               " MESSAGE_LINE_SUB_1
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_DEFAULT_ADD USING P_OBJNR.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MESSAGE_DEFAULT_ADD USING P_OBJNR LIKE OBJNR
                               P_MESG_LINE LIKE MESG-ZEILE.

  DATA:        L_IDENT_OBJID    LIKE   SY-MSGV1,
               L_IDENT_TXT20    LIKE   TBO01-TXT20.

  CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
       EXPORTING
            OBJNR       = P_OBJNR-OBJNR
       IMPORTING
            IDENT_OBJID = L_IDENT_OBJID
            IDENT_TXT20 = L_IDENT_TXT20.

  PERFORM MESSAGE_LINE_ADD_1 USING P_MESG_LINE.
  PERFORM MESSAGE_DEFAULT
                  USING P_MESG_LINE
                        'KA' 'S' '159' L_IDENT_TXT20 L_IDENT_OBJID
                           ' '  ' '.

ENDFORM.                               " MESSAGE_DEFAULT_ADD
*&---------------------------------------------------------------------*
*&      Form  FORM PROGRESS_INDICATOR_TEXT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM PROGRESS_INDICATOR_TEXT USING P_TEXT_1 TYPE C
                                   P_TEXT_2 TYPE C.

  CONCATENATE P_TEXT_1 P_TEXT_2 INTO P_TEXT_1 SEPARATED BY SPACE.
  IF SY-BATCH EQ TRUE.
    MESSAGE ID 'KA' TYPE 'S' NUMBER '414' WITH P_TEXT_1.
  ELSE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              TEXT = P_TEXT_1.
  ENDIF.

ENDFORM.                               " FORM PROGRESS_INDICATOR_TEXT
*&---------------------------------------------------------------------*
*&      Form  FORM PROGRESS_INDICATOR_PERCENT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM PROGRESS_INDICATOR_PERCENT
                 USING VALUE(V_PROCESSED) LIKE SY-TABIX
                       VALUE(V_TOTAL) LIKE SY-TABIX.

  CALL FUNCTION 'K_PROGRESS_INDICATOR_PERCENT'
       EXPORTING
            I_PROCESSED = V_PROCESSED
            I_TOTAL     = V_TOTAL.
*         I_STEP_SIZE_ONLINE = 20
*         I_STEP_SIZE_BATCH  = 10
*         I_INITIALIZE       =
*     EXCEPTIONS
*         OTHERS             = 1.

ENDFORM.                               " FORM PROGRESS_INDICATOR_PERCENT
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MESSAGES_INITIALIZE USING P_IDENTIFICATION LIKE SY-UZEIT
                               P_MESG_LINE      LIKE MESG-ZEILE
                               P_VORHER         LIKE  SY-TABIX
                               P_NACHHER        LIKE  SY-TABIX.

  CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
           COLLECT_AND_SEND = ' '
           RESET            = 'X'
*        LINE_FROM        = ' '
*        LINE_TO          = ' '
           I_IDENTIFICATION = P_IDENTIFICATION
           CHECK_ON_COMMIT  = 'X'
      IMPORTING
           E_IDENTIFICATION = P_IDENTIFICATION.

  P_MESG_LINE = 1.
  CALL FUNCTION 'MESSAGE_LINE_SET'
       EXPORTING
            ZEILE = P_MESG_LINE.
  CLEAR P_VORHER.
  CLEAR P_NACHHER.

ENDFORM.                               " MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_STORE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MESSAGES_STORE TABLES PT_MESG STRUCTURE MESG
                    USING  P_MAX_ERROR.

  LOOP AT PT_MESG.
    PERFORM  MESSAGE USING P_MAX_ERROR
                           PT_MESG-ARBGB
                           PT_MESG-MSGTY
                           PT_MESG-TXTNR
                           PT_MESG-MSGV1  PT_MESG-MSGV2
                           PT_MESG-MSGV3  PT_MESG-MSGV4.

  ENDLOOP.

ENDFORM.                               " MESSAGES_STORE
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_COLLECT
*&---------------------------------------------------------------------*
*       collect messages from parallel tasks and
*       renumber according to the actual global counter
*----------------------------------------------------------------------*
FORM MESSAGES_COLLECT TABLES   PT_MESG STRUCTURE MESG
                      USING    P_MESG_LINE
                               P_MAX_ERROR.

  DATA: L_ZEILE_SAV LIKE MESG-ZEILE.
  LOOP AT PT_MESG.
    IF L_ZEILE_SAV NE PT_MESG-ZEILE.
      L_ZEILE_SAV = PT_MESG-ZEILE.
      PERFORM MESSAGE_LINE_ADD_1 USING P_MESG_LINE.
      PERFORM  MESSAGE_DEFAULT USING P_MESG_LINE
                                     PT_MESG-ARBGB
                                     PT_MESG-MSGTY
                                     PT_MESG-TXTNR
                                     PT_MESG-MSGV1  PT_MESG-MSGV2
                                     PT_MESG-MSGV3  PT_MESG-MSGV4.
    ELSE.
      PERFORM  MESSAGE USING P_MAX_ERROR
                             PT_MESG-ARBGB
                             PT_MESG-MSGTY
                             PT_MESG-TXTNR
                             PT_MESG-MSGV1  PT_MESG-MSGV2
                             PT_MESG-MSGV3  PT_MESG-MSGV4.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " MESSAGES_COLLECT
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_STORE_ON_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MESSAGES_STORE_ON_DB USING P_MSG_STORE_NR  LIKE  CMFP-NR
                                P_OBJECT_ID     LIKE TCMF5-OBJECT_ID
                                P_APPL_ID       LIKE TCMF6-APLID.

  CALL FUNCTION 'MESSAGE_STORE_ON_DB'
       EXPORTING
            I_APLID     = P_APPL_ID
            I_OBJECT_ID = P_OBJECT_ID
       IMPORTING
*            E_APLID     = P_APPL_ID
*            E_OBJECT_ID = P_OBJECT_ID
            E_CMF_NR    = P_MSG_STORE_NR.

ENDFORM.                               " MESSAGES_STORE_ON_DB
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_CLEAN
*&---------------------------------------------------------------------*
*       Die Routine löscht unerwünschte Meldungen aus dem
*       Protokoll des Messagehandlers
*----------------------------------------------------------------------*
FORM MESSAGES_CLEAN USING P_STAT           STRUCTURE STAT
                          P_IDENTIFICATION LIKE SY-UZEIT
                          P_MESG_LINE      LIKE MESG-ZEILE
                          P_MAX_ERROR      LIKE SY-SUBRC
                          P_STATISTICAL
                          P_SUBRC.

  STATICS: L_MESG_LOADED TYPE C.
  STATICS: LT_MESG_DEL LIKE MESG OCCURS 0 WITH HEADER LINE.

  DATA: L_SUBRC      LIKE SY-SUBRC,
        L_SUBRC_MESG LIKE SY-SUBRC.

  DATA: LT_MESG     LIKE MESG OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*

  CLEAR P_SUBRC.
  IF L_MESG_LOADED NE TRUE.
*   get messages that should not be listed in the protokoll
    L_MESG_LOADED = TRUE.
    PERFORM GET_UNDESIRED_MESSAGES(SAPLKAZB) TABLES LT_MESG_DEL.
  ENDIF.

* get actual messages
  CALL FUNCTION 'MESSAGES_GIVE'
       TABLES
            T_MESG = LT_MESG
       EXCEPTIONS
            OTHERS = 0.

* check for statistical objects
  LOOP AT LT_MESG_DEL.
    READ TABLE LT_MESG WITH KEY ZEILE = P_MESG_LINE
                                TXTNR = LT_MESG_DEL-TXTNR
                                ARBGB = LT_MESG_DEL-ARBGB.
    IF SY-SUBRC EQ 0.
      MOVE LT_MESG_DEL-MSGV1 TO L_SUBRC_MESG.
      IF L_SUBRC_MESG GT L_SUBRC.
        L_SUBRC = LT_MESG_DEL-MSGV1.
        IF LT_MESG_DEL-MSGV2 EQ 'S'.
          P_STAT-STAT = P_STAT-STAT + 1.
          P_STATISTICAL = TRUE.
         ENDIF.
      ENDIF.
      MOVE LT_MESG_DEL-MSGV3 TO L_SUBRC_MESG.
      IF L_SUBRC_MESG GT P_SUBRC.
        P_SUBRC = LT_MESG_DEL-MSGV3.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF L_SUBRC NE 0.
    CALL FUNCTION 'MESSAGES_INITIALIZE'
         EXPORTING
              COLLECT_AND_SEND = ' '
              RESET            = 'X'
              LINE_FROM        = P_MESG_LINE
              LINE_TO          = P_MESG_LINE
              I_IDENTIFICATION = P_IDENTIFICATION
              CHECK_ON_COMMIT  = 'X'
              I_RESET_LINE     = SPACE                      "note697270
         IMPORTING
              E_IDENTIFICATION = P_IDENTIFICATION.
  ENDIF.
  IF L_SUBRC EQ 1.
    LOOP AT LT_MESG WHERE ZEILE = P_MESG_LINE.
      READ TABLE LT_MESG_DEL WITH KEY TXTNR = LT_MESG-TXTNR
                                      ARBGB = LT_MESG-ARBGB
                                      MSGV1 = '1'.
      IF SY-SUBRC NE 0.
        IF LT_MESG-MSGTY IS INITIAL.
          PERFORM MESSAGE_DEFAULT USING P_MESG_LINE
                                        LT_MESG-ARBGB
                                        'S'
                                        LT_MESG-TXTNR
                                        LT_MESG-MSGV1  LT_MESG-MSGV2
                                        LT_MESG-MSGV3  LT_MESG-MSGV4.
        ELSE.
          PERFORM MESSAGE USING P_MAX_ERROR
                                LT_MESG-ARBGB
                                LT_MESG-MSGTY
                                LT_MESG-TXTNR
                                LT_MESG-MSGV1  LT_MESG-MSGV2
                                LT_MESG-MSGV3  LT_MESG-MSGV4.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " MESSAGES_CLEAN
*&---------------------------------------------------------------------*
*&      Form  RPSCO_X_VERSN_EVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RPSCO_X_VERSN_EVA.
  DATA: l_display,
        l_selectfield  LIKE help_info-fieldname.

  DATA: BEGIN OF l_fieldt OCCURS 0.
          INCLUDE STRUCTURE help_value.
  DATA: END OF l_fieldt.

  DATA:  BEGIN OF l_values OCCURS 0,
           line(45),
         END OF l_values.

  DATA: BEGIN OF l_t_versn OCCURS 0,
          versn LIKE tka09-versn,
        END OF l_t_versn.

  DATA: BEGIN OF l_t_txt OCCURS 0,
          versn LIKE tkt09-versn,
          txt   LIKE tkt09-txt,
        END OF l_t_txt.

  l_display = space.

  SELECT versn FROM tka09  INTO CORRESPONDING FIELDS OF TABLE l_t_versn
                           WHERE kokrs = rkauf-kokrs AND
                                 exuvs = '03'.    "PROGRESS VERSION

  IF NOT l_t_versn[] IS INITIAL.
    SELECT versn txt FROM tkt09 INTO CORRESPONDING FIELDS OF
                                TABLE l_t_txt
                                FOR ALL ENTRIES IN l_t_versn
                                WHERE langu = sy-langu    AND
                                      versn = l_t_versn-versn.
  ENDIF.

  LOOP AT l_t_txt.
    MOVE l_t_txt-versn TO l_values-line.
    APPEND l_values.
    MOVE l_t_txt-txt TO l_values-line.
    APPEND l_values.
  ENDLOOP.

  l_fieldt-tabname   = 'TKT09'.
  l_fieldt-fieldname = 'VERSN'.
  l_fieldt-selectflag = 'X'.
  APPEND l_fieldt.

  l_fieldt-tabname   = 'TKT09'.
  l_fieldt-fieldname = 'TXT'.
  l_fieldt-selectflag = space.
  APPEND l_fieldt.

  l_selectfield = 'VERSN'.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
      EXPORTING
           selectfield                  = l_selectfield
*           titel                        = l_title
*           title_in_values_list         = yx
*      importing
*           ind                          = l_ind
      TABLES
           fields                       = l_fieldt
           full_table                   = l_t_txt
      EXCEPTIONS
           OTHERS                       = 1.


ENDFORM.                    " RPSCO_X_VERSN_EVA

*------------------------------------------------------- UGL
*  FORM Z_CHECK_BUDAT                                    UGL
*--------------------------------------------------------UGL
*                                                        UGL
* --> p1    text                                         UGL
* <-- p2    text                                         UGL
*--------------------------------------------------------UGL
*  Checks for posting date                               UGL
*  1.  Check if posting date is not in future            UGL
*  2.  Check the posting period for a/c numbers for      UGL
*      interest are open                                 UGL
*                                                        UGL
FORM Z_CHECK_BUDAT.                                     "UGL
  if rkauf-budat > sy-datum.                            "UGL
     message  e398(00) with text-z01.                   "UGL
  endif.                                                "UGL
*                                                        UGL
*  Check if the posting period is open                   UGL
*                                                        UGL
  ztemp_gjahr = rkauf-budat(4).                         "UGL
  ztemp_monat = rkauf-budat+4(2).                       "UGL
                                                        "UGL
  call function 'FI_PERIOD_CHECK'                       "UGL
          exporting                                     "UGL
             ibukrs           = 'UGL'                   "UGL
             igjahr           = ztemp_gjahr             "UGL
             i_koart          = 'S'                     "UGL
             i_konto          = '491003'                "UGL
             i_monat          = ztemp_monat             "UGL
          exceptions                                    "UGL
             error_period     = 1                       "UGL
             error_period_acc = 2                       "UGL
             others           = 3.                      "UGL
  if sy-subrc <> 0.                                     "UGL
     message id sy-msgid type sy-msgty number sy-msgno  "UGL
       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.        "UGL
  endif.                                                "UGL
*                                                       "UGL
  call function 'FI_PERIOD_CHECK'                       "UGL
          exporting                                     "UGL
             ibukrs           = 'UGL'                   "UGL
             igjahr           = ztemp_gjahr             "UGL
             i_koart          = 'S'                     "UGL
             i_konto          = '115900'                "UGL
             i_monat          = ztemp_monat             "UGL
          exceptions                                    "UGL
             error_period     = 1                       "UGL
             error_period_acc = 2                       "UGL
             others           = 3.                      "UGL
  if sy-subrc <> 0.                                     "UGL
     message id sy-msgid type sy-msgty number sy-msgno  "UGL
       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.        "UGL
  endif.                                                "UGL
endform.                                                "UGL
