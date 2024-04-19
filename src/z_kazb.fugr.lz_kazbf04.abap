*-------------------------------------------------------------------
***INCLUDE LKAZBF04 .
*-------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form PARALLEL_TASK_END
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PARALLEL_TASK_END USING P_TASKNAME.

  PERFORM (GD_EOT-FORM) IN PROGRAM (GD_EOT-PROG)
     USING
        P_TASKNAME.

ENDFORM.                               " PARALLEL_TASK_END
*&---------------------------------------------------------------------*
*&      Form  PARALLEL_TASK_LIST_FILL
*&---------------------------------------------------------------------*
*       fill object list for next task
*----------------------------------------------------------------------*
FORM PARALLEL_TASK_LIST_FILL
                TABLES   PT_OBJNR       STRUCTURE JSTO_PRE
                USING    P_PARA_IMP     TYPE KKPA_T_PARA_IMP
                CHANGING P_PARA_EXP     TYPE KKPA_T_PARA_EXP
                         P_FAILED_OBJS  TYPE KKPA_T_FAILED_OBJECTS
                         P_PENDING_OBJS TYPE KKPA_T_PENDING_OBJECTS
                         P_USER_PARAM   TYPE T_USER_PARAM
                         P_EXIT         TYPE C.           "ALR 17.12.98

  DATA:   LD_PEND_OBJ     TYPE LINE OF KKPA_T_PENDING_OBJECTS,
          LD_FAIL_OBJ     TYPE LINE OF KKPA_T_FAILED_OBJECTS,
          LD_OBJNR        TYPE LINE OF T_USER_PARAM-OBJNR,
          LD_NR_ORDER     LIKE SY-INDEX,
          LD_PACKAGE_SIZE LIKE SY-INDEX.
*STATICS: LD_PROCESSED    LIKE SY-TABIX.
*----------------------------------------------------------------------*
  IF P_FAILED_OBJS[] IS INITIAL.
* Keine Objekte aus abgebrochenen Tasks übrig
* Paketstragetie: die Paketgröße wird derart gewählt, daß mit der
*                 Restmenge an Objekten alle Workprozesse noch 5 mal
*                 verwendet werden,
*                 die Mindestgröße pro Paket ist   20 Objekte
*                 die Maximalgröße pro Paket ist  200 Objekte

    DESCRIBE TABLE P_USER_PARAM-OBJNR LINES LD_NR_ORDER.

    LD_PACKAGE_SIZE = LD_NR_ORDER / P_PARA_IMP-NR_TOTAL_TASKS / 5.
    IF LD_PACKAGE_SIZE < 20.
      LD_PACKAGE_SIZE = 20.
    ELSEIF  LD_PACKAGE_SIZE > 200.
      LD_PACKAGE_SIZE = 200.
    ENDIF.

    LOOP AT P_USER_PARAM-OBJNR INTO LD_OBJNR
         FROM 1 TO LD_PACKAGE_SIZE.
      APPEND LD_OBJNR TO PT_OBJNR.
    ENDLOOP.                           "LOOP AT PT_OBJNR
    DELETE P_USER_PARAM-OBJNR FROM 1 TO LD_PACKAGE_SIZE.
*----------------------------------------------------------------------*
*  call progress indicator before starting first group
*----------------------------------------------------------------------*
* LD_PROCESSED = LD_PROCESSED + LD_PACKAGE_SIZE.
* PERFORM PROGRESS_INDICATOR_PERCENT USING LD_PROCESSED GD-SELECTED.

*----------------------------------------------------------------------*
* objects from aborted tasks are resubmitted
*----------------------------------------------------------------------*
  ELSE.
    READ TABLE P_FAILED_OBJS INTO LD_FAIL_OBJ INDEX 1.
    CHECK SY-SUBRC = 0.                                     "ALR153534
    PT_OBJNR = LD_FAIL_OBJ.
    DELETE P_FAILED_OBJS INDEX 1.
    APPEND PT_OBJNR.
  ENDIF.

*----------------------------------------------------------------------*
* no objects left in original object list
* ---> stop submitting of further tasks
*----------------------------------------------------------------------*
  READ TABLE P_USER_PARAM-OBJNR INDEX 1 INTO LD_OBJNR.
  IF SY-SUBRC <> 0 AND P_FAILED_OBJS[] IS INITIAL.
    P_PARA_EXP-ALL_SUBMIT = 'X'.
  ENDIF.

*----------------------------------------------------------------------*
* end of processing if no objects are left
*----------------------------------------------------------------------*
  READ TABLE PT_OBJNR INDEX 1.
  IF SY-SUBRC <> 0.
    P_PARA_EXP-RFC_SUBRC = KKPA_RFC_SUBRC-RFC_NOT_STARTED.
      P_EXIT = 'X'.                                       "ALR 17.12.98
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* add object list of actual task to list of pending objects
*----------------------------------------------------------------------*
  LOOP AT PT_OBJNR.
    LD_PEND_OBJ = PT_OBJNR-OBJNR.
    APPEND LD_PEND_OBJ TO P_PENDING_OBJS.
  ENDLOOP.
ENDFORM.                               " PARALLEL_TASK_LIST_FILL
*&---------------------------------------------------------------------*
*&      Form  CHECK_SERVER_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECK_SERVER_GROUP USING P_RFC_GRP LIKE RKAUF-RFC_GRP.
  IF P_RFC_GRP IS INITIAL.
    CALL FUNCTION 'CK_F_RFC_GROUP_SELECTION_LIST'
         IMPORTING
              RFC_GROUP_EXP = P_RFC_GRP.
    IF P_RFC_GRP IS INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_SERVER_GROUP
