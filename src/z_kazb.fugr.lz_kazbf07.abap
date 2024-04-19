

*----------------------------------------------------------------------*
***INCLUDE LKAZBF07 .
*----------------------------------------------------------------------*
* Corrections:
* P6BK111909: Note 671903 17.10.2003 FCL

*&---------------------------------------------------------------------*
*&      Form  MONITOR_SELCRIT_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONITOR_SELCRIT_FILL  TABLES  SELPA  STRUCTURE  KABA00.

  LOOP AT SELPA.
    MOVE SELPA-LOW(10) TO GT_SELCRIT-FIELD.
    IF GT_SELCRIT-FIELD NE 'VARIANT'.
      GT_SELCRIT-STRUCTURE = 'LKO74'.
    ELSE.
      GT_SELCRIT-STRUCTURE = 'CODIA'.
    ENDIF.
    MOVE SELPA-LOW+10  TO GT_SELCRIT-LOW.
    MOVE SELPA-HIGH+10 TO GT_SELCRIT-HIGH.
    MOVE SELPA-OPTION  TO GT_SELCRIT-OPTIO.
    MOVE SELPA-SIGN    TO GT_SELCRIT-SIGN.
    IF    NOT GT_SELCRIT-LOW  IS INITIAL
       OR NOT GT_SELCRIT-HIGH IS INITIAL.
      APPEND GT_SELCRIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " MONITOR_SELCRIT_FILL
*&---------------------------------------------------------------------*
*&      Form  MONITOR_INIT_RECORD
*&---------------------------------------------------------------------*
*       Initialize Schedule Manager
*----------------------------------------------------------------------*
*      -->P_P_TCODE  text
*----------------------------------------------------------------------*
FORM MONITOR_INIT_RECORD USING VALUE(P_TCODE).
  DATA: LS_DETAIL LIKE SCHEDMAN_DETAIL_USER,
        LS_COOM LIKE SCHEDMAN_SPECIFIC_COOM,
        LS_PS   LIKE SCHEDMAN_SPECIFIC_PS,
        LS_COPC LIKE SCHEDMAN_SPECIFIC_COPC,
        LS_WITEM TYPE SCMA_WITEM,
        LT_PARAM   LIKE  SCHEDMAN_SELKRIT OCCURS 0 WITH HEADER LINE.
*****************Note 671903 Begin*****************
  field-symbols: <ls_appl>. "field symbol for ls_coom,ls_ps,ls_copc
  data: ld_tcode type sy-tcode,
        ld_subrc type sy-subrc,
        ld_application type schedman_detail_user-application,
        ls_tka00 type tka00,
        ls_tka01 type tka01.
  ld_tcode = sy-cprog+4(4).
*****************Note 671903 End******************
*----------------------------------------------------------------------*
  CHECK NOT GD-PROTOCOL IS INITIAL.
* set activity
  PERFORM ACTIVITY.
* fill ls_coom, ls_ps, ls_pc

* add to gt_selcrit
* period from, period to
  GT_SELCRIT-STRUCTURE = 'RKAUF'.
  GT_SELCRIT-FIELD     = 'FROM'.
  GT_SELCRIT-LOW = RKAUF-FROM.
  IF NOT RKAUF-TO EQ RKAUF-FROM.
    GT_SELCRIT-HIGH = RKAUF-TO.
    GT_SELCRIT-OPTIO = 'BT'.
  ELSE.
    GT_SELCRIT-OPTIO = 'EQ'.
  ENDIF.
  GT_SELCRIT-SIGN = 'I'.
  APPEND GT_SELCRIT.

* year from, year to
  GT_SELCRIT-STRUCTURE = 'RKAUF'.
  GT_SELCRIT-FIELD     = 'GJAHR'.
  GT_SELCRIT-LOW = RKAUF-GJAHR.
  IF NOT RKAUF-GJAHR_BIS EQ RKAUF-GJAHR.
    GT_SELCRIT-HIGH = RKAUF-GJAHR_BIS.
    GT_SELCRIT-OPTIO = 'BT'.
  ELSE.
    GT_SELCRIT-OPTIO = 'EQ'.
  ENDIF.
  GT_SELCRIT-SIGN = 'I'.
  APPEND GT_SELCRIT.

* fill ls_detail
  LS_DETAIL-FUNCTION = RKAUF-VRGNG.
  IF GD-REPID_BTC = 'RKAZUB70'.
    CLEAR LS_DETAIL-REPID.
  ELSE.
    LS_DETAIL-REPID = GD-REPID_BTC.
  ENDIF.
  LS_DETAIL-TCODE = P_TCODE.
  LS_DETAIL-TESTFLAG = RKAUF-TEST.
  ls_detail-application = ld_application. "from init_tcode  "note671903
  LS_DETAIL-ACTIVITY = GD-ACTIVITY.

*****************Note 671903 Begin*****************
* determine application for settlement
  call function 'MONI_DETERMINE_APPLICATION'
       exporting
            i_repid = sy-cprog
            i_tcode = ld_tcode
       importing
            e_appl  = ls_detail-application
            e_subrc = ld_subrc.

* organizations and dates
  if ls_detail-application = 'CO-OM-OPA'.
    move-corresponding rkauf to ls_coom.
    ls_coom-perio_from = rkauf-from.
    ls_coom-perio_to   = rkauf-to.
    ls_coom-gjahr_to   = rkauf-gjahr.
    ls_coom-budat      = rkauf-budat.
    call function 'K_KOKRS_READ'
         exporting
              kokrs   = rkauf-kokrs
         importing
              e_tka01 = ls_tka01
         exceptions
              others  = 0.
    ls_coom-lmona      = ls_tka01-lmona.
    assign ls_coom to <ls_appl>.
  elseif ls_detail-application(9) = 'CO-PC-OBJ'.
    move-corresponding rkauf to ls_copc.
    ls_copc-perio_from = rkauf-from.
    ls_copc-perio_to   = rkauf-to.
    ls_copc-gjahr_to   = rkauf-gjahr.
    assign ls_copc to <ls_appl>.
  elseif ls_detail-application = 'PS'.
    move-corresponding rkauf to ls_ps.
    ls_ps-perio_from = rkauf-from.
    ls_ps-perio_to   = rkauf-to.
    ls_ps-gjahr_to   = rkauf-gjahr.
    assign ls_ps to <ls_appl>.
  endif.
*****************Note 671903 End*****************

* flag delayed update
  if gd-processor  = 7.     "incoming orders
    if rkauf-para is initial.
*     sequential processing: use delayed update to improve performance
*     GD-DELAYED_UPDATE is on.
    else.
*     parallel processing: no delayed update (would mess up DB server)
      clear GD-DELAYED_UPDATE.
      set update task local.
    endif.
  endif.

* workflow
  LS_WITEM-WF_WITEM = RKAUF-WITEM.
  LS_WITEM-WF_WLIST = RKAUF-WORKLIST.

  CLEAR GD-COMMIT.
  CALL FUNCTION 'KPEP_MONI_INIT_RECORD'
       EXPORTING
            LS_DETAIL        = LS_DETAIL
            LS_WITEM         = LS_WITEM
            ls_appl          = <ls_appl>
            LD_WORKLIST_FLAG = GD-WL_ACTIVE
            LD_DELAYED_UPDATE = GD-DELAYED_UPDATE
       IMPORTING
            LS_KEY           = GD-KEY
            WILL_BE_SAVED    = GD-MONI_SAVE
       TABLES
            LT_SELKRIT       = GT_SELCRIT
            LT_PARAM         = LT_PARAM.

  COMMIT WORK.

ENDFORM.                               " MONITOR_INIT_RECORD
*&---------------------------------------------------------------------*
*&      Form  MONITOR_CLOSE_RECORD
*&---------------------------------------------------------------------*
*       Close Schedule Manager
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONITOR_CLOSE_RECORD.

  CHECK NOT GD-PROTOCOL IS INITIAL.
  DATA:   LD_BALOBJECT    TYPE  BALOBJ_D VALUE 'COAC',
          LD_BALSUBOBJECT TYPE  BALSUBOBJ,
          LD_LOG_HANDLE TYPE  BALLOGHNDL.

  DATA: LS_MESSAGE    LIKE SCHEDMAN_MESSAGE.
  DATA: L_SUBRC       LIKE SY-SUBRC.
  DATA: LD_APLSTAT    LIKE SMMAIN-APLSTAT.
  DATA: LS_SCMA_EVENT LIKE  SCMA_EVENT.

  IF NOT GD-MSG_STORE_NR IS INITIAL.
    LS_MESSAGE-NR        = GD-MSG_STORE_NR.
    LS_MESSAGE-OBJECT_ID = GD-MSG_OBJECT_ID.
    LS_MESSAGE-APLID     = GD-MSG_APPL_ID.
  ENDIF.

  CALL FUNCTION 'MESSAGES_COUNT'
       IMPORTING
            MAX_SEVERITY = L_SUBRC.

  IF ( L_SUBRC > 8 ).
    LD_APLSTAT = '4'.
  ELSE.
    IF L_SUBRC > 4.
      LD_APLSTAT = '2'.
    ELSE.
      LD_APLSTAT = '0'.
    ENDIF.
  ENDIF.
* save messages
* only if schedule manager saves the data as well
  IF NOT GD-MONI_SAVE IS INITIAL.
    IF RKAUF-STNO EQ TRUE.
      CONCATENATE RKAUF-VRGNG '_REVS' INTO LD_BALSUBOBJECT.
    ELSE.
      CONCATENATE RKAUF-VRGNG '_EXEC' INTO LD_BALSUBOBJECT.
    ENDIF.
    CALL FUNCTION 'MESSAGES_SAVE'
         EXPORTING
              I_BALOBJECT          = LD_BALOBJECT
              I_BALSUBOBJECT       = LD_BALSUBOBJECT
              I_IN_UPDATE_TASK     = ON
              I_IDENTIFICATION     = GD-IDENTIFICATION
         IMPORTING
              E_LOG_HANDLE         = LD_LOG_HANDLE
         EXCEPTIONS
              NO_MESSAGES          = 0
              NOT_ACTIVE           = 0
              PROGRAM_ERROR        = 0
              SAVE_NOT_ALLOWED     = 0
              WRONG_IDENTIFICATION = 0.
    LS_MESSAGE-LOG_HANDLE =  LD_LOG_HANDLE.
  ENDIF.

* event handling > workflow
  LS_SCMA_EVENT-WF_WITEM = RKAUF-WITEM.
  LS_SCMA_EVENT-WF_OKEY  = RKAUF-WF_OKEY.
  IF L_SUBRC >= 12.                    "error
    LS_SCMA_EVENT-WF_EVENT = CS_WF_EVENTS-ERROR.
  ELSE.
    LS_SCMA_EVENT-WF_EVENT = CS_WF_EVENTS-FINISHED.
  ENDIF.
* for worlists always event = finished
  IF NOT GD-WL_ACTIVE IS INITIAL.
    LS_SCMA_EVENT-WF_EVENT = CS_WF_EVENTS-FINISHED.
  ENDIF.

* number of postings is counted
  DO GD-COMMIT TIMES.
    CALL FUNCTION 'SCMA_UPD_LOG_COUNT_TASKS'.
  ENDDO.              .

  CALL FUNCTION 'KPEP_MONI_CLOSE_RECORD'
       EXPORTING
            LS_KEY        = GD-KEY
            LS_MESSAGE    = LS_MESSAGE
            LD_OBJECTS    = GD-SELECTED
            LS_SCMA_EVENT = LS_SCMA_EVENT
       CHANGING
            LD_APLSTAT    = LD_APLSTAT
       EXCEPTIONS
            NO_ID_GIVEN   = 1
            OTHERS        = 2.
  IF SY-SUBRC <> 0.
  ENDIF.

  COMMIT WORK.

ENDFORM.                               " MONITOR_CLOSE_RECORD

*&---------------------------------------------------------------------*
*&      Form  commit_document
*&---------------------------------------------------------------------*
*       Application performs commit_document
*       to report back the number of postings
*----------------------------------------------------------------------*
FORM COMMIT_DOCUMENT.

  GD-COMMIT = GD-COMMIT + 1.
  CALL FUNCTION 'SCMA_UPD_LOG_UPD_RECORD'
       EXPORTING
            IS_MONITOR_KEY = GD-KEY.

ENDFORM.                               " commit_document
