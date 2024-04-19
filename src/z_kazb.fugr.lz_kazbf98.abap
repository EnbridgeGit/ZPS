*----------------------------------------------------------------------*
*   INCLUDE LKAZBF98                                                   *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  WORKFLOW_EVENTS
*&---------------------------------------------------------------------*
*       Ereignisse nach Status des Message-Handlers auslösen           *
*----------------------------------------------------------------------*
*  -->  VALUE(P_WF_OKEY)     Schlüssel (= Instanz) des Objektes
*----------------------------------------------------------------------*
FORM WORKFLOW_EVENTS USING VALUE(P_WF_OKEY)  LIKE SWOTOBJID-OBJKEY
                           VALUE(P_WF_WITEM) LIKE SCMA_WITEM-WF_WITEM.
* Vereinbarungen
  DATA: L_TEXT1(255)    TYPE C,
        L_TEXT2(255)    TYPE C,
        L_COUNT         LIKE SY-TABIX VALUE 99,
        L_MAX_SEVERITY  LIKE SY-SUBRC VALUE 16.

DATA: L_T_FIXED_VALUES LIKE DD07V            OCCURS 13 WITH HEADER LINE.

  CONSTANTS: LC_W_MESS  LIKE SY-SUBRC VALUE  8.

* (moegliche) Schweregrade der Messages
  "          lc_no_mess like sy-subrc value  0,
  "          lc_i_mess  like sy-subrc value  4,
  "          lc_e_mess  like sy-subrc value 12,
  "          lc_a_mess  like sy-subrc value 16,

* Protokoll des Message-Handlers auswerten
  PERFORM EVALUATE_MESSAGE_HANDLER USING L_COUNT
                                         L_MAX_SEVERITY.

* Ist ein (schwerwiegender) Fehler aufgetreten ?
  IF ( NOT ( L_COUNT        IS INITIAL   ) ) AND
     (     ( L_MAX_SEVERITY GE LC_W_MESS ) ).


    CASE GD-OBART.

***** Aufträge *********************************************************
      WHEN OBJEKTART_OR.

*        Sammelverarbeitung
        IF GD-ONEOBJECT EQ FALSE.

*           Texte der Domaenen-Festwerte zu den Auftragstypen
          PERFORM GET_DOMAIN_TEXT TABLES L_T_FIXED_VALUES
                                  USING  'AUFTYP'.

*           Text zum aktuellen Auftragstypen
          READ TABLE L_T_FIXED_VALUES
                     WITH KEY DOMVALUE_L = GD-SUBART
                                           TRANSPORTING DDTEXT.
          CHECK SY-SUBRC = 0.                               "ALR153534
*           variable Textfragmente für Text der Workflow-Aufgabe
          L_TEXT1 = L_T_FIXED_VALUES-DDTEXT.
          L_TEXT2 = SPACE.

*           Event ausloesen: Fehler aufgetreten
          PERFORM SWE_RAISE_EVENT_ERROR USING 'BPJOB_SMAN'
                                              'REPORTERROR'
                                               P_WF_OKEY
                                               P_WF_WITEM
                                               L_TEXT1
                                               L_TEXT2.
        ENDIF.


***** Projekte *********************************************************
      WHEN OBJEKTART_PD OR
           OBJEKTART_NP OR
           OBJEKTART_NV.

*        Sammelverarbeitung
        IF GD-ONEOBJECT EQ FALSE.

*           variable Textfragmente für Text der Workflow-Aufgabe
          L_TEXT1 = 'Sammelverarbeitung Zuschläge auf Projekt'(CM1).
          L_TEXT2 = SPACE.

*           Event ausloesen: Fehler aufgetreten
          PERFORM SWE_RAISE_EVENT_ERROR USING 'BPJOB_SMAN'
                                              'REPORTERROR'
                                              P_WF_OKEY
                                              P_WF_WITEM
                                              L_TEXT1
                                              L_TEXT2.
        ENDIF.
    ENDCASE.


  ELSE.                                "kein Fehler

*  Event ausloesen: keine Fehler aufgetreten
    PERFORM SWE_RAISE_EVENT_COMPL USING 'BPJOB_SMAN'
                                        'FINISHED'
                                        P_WF_WITEM
                                        P_WF_OKEY.
  ENDIF.

ENDFORM.                               " WORKFLOW_EVENTS


*&---------------------------------------------------------------------*
*&      Form  SWE_RAISE_EVENT_COMPL
*&---------------------------------------------------------------------*
*       publiziertes Ereignis für Workflow auslösen                    *
*----------------------------------------------------------------------*
*  -->  VALUE(P_OBJECTTYPE)  Objekttyp aus dem BOR
*  -->  VALUE(P_EVENT)       auszulösendes Ereignis des Objekttypen
*  -->  VALUE(P_WF_OKEY)     Schlüssel (= Instanz) des Objektes
*----------------------------------------------------------------------*
FORM SWE_RAISE_EVENT_COMPL
                   USING VALUE(P_OBJECTTYPE) LIKE SWETYPECOU-OBJTYPE
                           VALUE(P_EVENT)    LIKE SWETYPECOU-EVENT
                           VALUE(P_WF_WITEM) LIKE SCMA_WITEM-WF_WITEM
                           VALUE(P_WF_OKEY)  LIKE SWEINSTCOU-OBJKEY.

  CALL FUNCTION 'SCWF_EVENT_CREATE'
       EXPORTING
            I_WF_WITEM = P_WF_WITEM
            I_WF_OKEY  = P_WF_OKEY
            I_EVENT    = P_EVENT.

  IF NOT ( SY-SUBRC IS INITIAL ).
*  Fehler beim Auslösen des Ereignisses <p_event> vom Objekttyp
*  <p_objecttype> aufgetreten
    MESSAGE E430(KA) WITH P_EVENT P_OBJECTTYPE.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFORM.                               " SWE_RAISE_EVENT_COMPL


*&---------------------------------------------------------------------*
*&      Form  SWE_RAISE_EVENT_ERROR
*&---------------------------------------------------------------------*
*       publiziertes Ereignis für Workflow auslösen                    *
*----------------------------------------------------------------------*
*  -->  VALUE(P_OBJECTTYPE)  Objekttyp aus dem BOR
*  -->  VALUE(P_EVENT)       auszulösendes Ereignis des Objekttypen
*  -->  VALUE(P_WF_OKEY)     Schlüssel (= Instanz) des Objektes
*  -->  VALUE(P_TEXT1)       beliebiger Text
*  -->  VALUE(P_TEXT2)       beliebiger Text
*----------------------------------------------------------------------*
FORM SWE_RAISE_EVENT_ERROR
                     USING VALUE(P_OBJECTTYPE) LIKE SWETYPECOU-OBJTYPE
                           VALUE(P_EVENT)      LIKE SWETYPECOU-EVENT
                           VALUE(P_WF_OKEY)    LIKE SWEINSTCOU-OBJKEY
                           VALUE(P_WF_WITEM)   LIKE SCMA_WITEM-WF_WITEM
                           VALUE(P_TEXT1)      TYPE char255
                           VALUE(P_TEXT2)      TYPE char255.
* AC0K002172: TYPE CHAR255 instead of LIKE MESSAGEINF-MSGTX

* Vereinbarungen
***  DATA S_WF_USER LIKE SWHACTOR.

* Benutzername ermitteln (HR-ORG)
***  S_WF_USER-OTYPE = 'US'.
***  S_WF_USER-OBJID = SY-UNAME.

* Ereignis-Container anlegen
***  SWC_CONTAINER EVENT_CONTAINER.

*** Variante in Ereignis-Container schreiben
* SWC_CREATE_ELEMENT EVENT_CONTAINER 'Variant'.
*  SWC_SET_ELEMENT    EVENT_CONTAINER 'Variant'  SY-SLSET.

*** Text I in Ereignis-Container schreiben
*  SWC_CREATE_ELEMENT EVENT_CONTAINER 'Text1'.
*  SWC_SET_ELEMENT    EVENT_CONTAINER 'Text1'    P_TEXT1.

*** Text II in Ereignis-Container schreiben
*  SWC_CREATE_ELEMENT EVENT_CONTAINER 'Text2'.
*  SWC_SET_ELEMENT    EVENT_CONTAINER 'Text2'    P_TEXT2.

* Ereignis auslösen
***  CALL FUNCTION 'SWE_EVENT_CREATE'
***       EXPORTING
***            OBJTYPE              = P_OBJECTTYPE
***            OBJKEY               = P_WF_OKEY
***            EVENT                = P_EVENT
***            CREATOR              = S_WF_USER
***  "         START_WITH_DELAY     = ' '
***  "         START_RECFB_SYNCHRON = ' '
***                                       "    importing
*                                       "         event_id             =
***       TABLES
***            EVENT_CONTAINER      = EVENT_CONTAINER
***       EXCEPTIONS
***            OBJTYPE_NOT_FOUND    =  1
***            OTHERS               = 99.

  CALL FUNCTION 'SCWF_EVENT_CREATE'
       EXPORTING
            I_WF_WITEM = P_WF_WITEM
            I_WF_OKEY  = P_WF_OKEY
            I_EVENT    = P_EVENT.
  IF NOT ( SY-SUBRC IS INITIAL ).
*  Fehler beim Auslösen des Ereignisses <p_event> vom Objekttyp
*  <p_objecttype> aufgetreten
    MESSAGE E430(KA) WITH P_EVENT P_OBJECTTYPE.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFORM.                               " SWE_RAISE_EVENT_ERROR


*&---------------------------------------------------------------------*
*&      Form  GET_DOMAIN_TEXT
*&---------------------------------------------------------------------*
*       ermitteln von Texten zu Festwerten einer Domäne                *
*----------------------------------------------------------------------*
*  -->  P_DOMAIN          Name einer Domaene
*  <--  P_T_FIXED_VALUES  Eregbnistabelle
*----------------------------------------------------------------------*
FORM GET_DOMAIN_TEXT TABLES P_T_FIXED_VALUES STRUCTURE DD07V
                     USING  P_DOMAIN         LIKE      DD01D-DOMNAME.

  CALL FUNCTION 'DD_DOMVALUES_GET'
       EXPORTING
            DOMNAME       = P_DOMAIN
            TEXT          = 'X'
            LANGU         = SY-LANGU
            BYPASS_BUFFER = SPACE
       TABLES
            DD07V_TAB     = P_T_FIXED_VALUES.

ENDFORM.                               " GET_DOMAIN_TEXT

*&---------------------------------------------------------------------*
*&      Form  EVALUATE_MESSAGE_HANDLER
*&---------------------------------------------------------------------*
*       Protokoll des Message-Handlers auswerten
*----------------------------------------------------------------------*
*  <--  P_L_COUNT         Anzahl der gesammelten Nachrichten
*  <--  P_L_MAX_SEVERITY  maximal aufgetretener Schweregrad
*----------------------------------------------------------------------*
FORM EVALUATE_MESSAGE_HANDLER USING P_L_COUNT
                                    P_L_MAX_SEVERITY.

  CALL FUNCTION 'MESSAGES_COUNT'
       IMPORTING
            COUNT        = P_L_COUNT
            MAX_SEVERITY = P_L_MAX_SEVERITY.

ENDFORM.                               " EVALUATE_MESSAGE_HANDLER
