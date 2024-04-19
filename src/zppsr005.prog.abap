REPORT zppsr005 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
   MESSAGE-ID zp.

************************************************************************
*  Author:   MOHAMMAD KHAN
*  Date:     December, 2000.
*  Issue No: 821.
*  Brief Description:
*     - The purpose of this program is to produce a report listing
*       actual and plan values for selected projects and cost
*       elements groups.
************************************************************************
* CHANGES
*  Cathy McCoy - February 2002 - changes made using function calls to *
*  read set values for CO structure.

************************************************************************

TABLES:   proj,             "Project Definition
          prps,             "WBS (Work Breakdown Structure)
          cosp,             "CO Object: Cost total for External Postings
          coss,             "CO Object: Cost total for Internal Postings
          csku,             "Cost element text
          cska.             "Cost Elem.[Data dependent on Chart of accou


DATA:    BEGIN OF name_tab OCCURS 10.
        INCLUDE STRUCTURE rgsb4.
DATA:    END OF name_tab.

DATA:    BEGIN OF wa_setinfo  OCCURS 0.
        INCLUDE STRUCTURE rgsmh.   " cmccoy- was structure setinfo
DATA:    END OF wa_setinfo.

DATA: BEGIN OF project_tab OCCURS 100,
        vernr    LIKE proj-vernr,                           "Division
        pspid    LIKE proj-pspid,                           "Project #
        costgr   LIKE rgsb4-setnr,      "Cost Element Group
        kstar    LIKE coss-kstar,       "Cost Element
        pspnr    LIKE proj-pspnr,       "Project Object
        post1    LIKE proj-post1,       "Project Description
        posid    LIKE prps-posid,       "WBS
        grname   LIKE rgsb4-title,      "Cost Element Group Name
        objnr    LIKE coss-objnr,       "Internal Object Number
        descript LIKE name_tab-title,   "Cost Element Description
        pper     LIKE cosp-wkg001,      "Sum of Plans for the Year
        aper     LIKE cosp-wkg001,      "Sum of Actuals for Period
        aytd     LIKE cosp-wkg001,      "Sum of Actuals for YTD
        verna    LIKE proj-verna,       "Person Responsible
        ordernr  LIKE aufk-aufnr,
      END OF project_tab.

DATA:    post1          LIKE proj-post1,
         descript       LIKE project_tab-descript,
         delta1         LIKE cosp-wkg001,
         verna          LIKE proj-verna,
         vernr          LIKE proj-vernr,
         len   TYPE i,
         len2  TYPE i,
         p_start(2)     VALUE   '01',
         setid  LIKE sethier-setid.
**********************************************************************
* cmccoy
* new data types for function calls
*..... Control block (for sequential search within a set) ............ *
CONSTANTS: control_block LIKE sy-tabix VALUE 1.

*..... Set index for differentiation of concurrently processed sets . *
DATA: setindex         LIKE sy-tabix.

*..... Return code ................................................... *
DATA: subrc         LIKE sy-subrc.

*..... Auxiliary field for the search value .......................... *
DATA: svalue        LIKE rgsmv-from.

*..... Internal ID of cost element group  ............................ *
DATA: p_setid       LIKE sethier-setid.

*..... Type pool for set classes .................................... *
TYPE-POOLS: gsetc.


*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-000.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_pspid     FOR proj-pspid,
                s_vernr     FOR proj-vernr.
PARAMETERS:    p_costgr    LIKE    kkb0-kst      OBLIGATORY,
               p_vers      LIKE    tkvst-versi   DEFAULT '0',
               p_fyear     LIKE    cosp-gjahr    DEFAULT sy-datum+0(4),
               p_period    LIKE    sy-datum+4(2) DEFAULT '01',
               p_search LIKE tka01-ktopl MEMORY ID kpl.     "cmccoy
SELECTION-SCREEN END OF BLOCK box.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.                             "Plans
PARAMETER:      p_notot      RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 05(40) text-013.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.                             "Plans
PARAMETER:      p_tot      RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 05(40) text-012.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box2.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF s_pspid(3) = space AND s_vernr(3) = space.
    MESSAGE e100 WITH ' Enter either a project or a division'.
  ENDIF.

* cmccoy
* Get the internal Set-ID from the group name, chart of accounts
* and the setclass of cost element groups (see type pool GSETC).
*----------------------------------------------------------------------*
  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
       EXPORTING
            setclass  = gsetc_costelement_setclass
            shortname = p_costgr
            ktopl     = p_search
       IMPORTING
            setid     = p_setid.

* Check if set exists
  CALL FUNCTION 'G_SET_GET_INFO'
       EXPORTING
            no_set_title     = 'X'
            setname          = p_setid
            use_table_buffer = 'X'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_costgr.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
*            class              = '0H' - cmccoy- not required
            field_name         = 'KSTAR'
*            searchfld          = p_search
*            searchfld_required = space
*            start_column       = 10
*            start_row          = 5
*            table              = 'CCSS'
*            typelist           = 'BS'
       IMPORTING
            set_name           = p_costgr
            setid              = p_setid
       EXCEPTIONS
            no_set_picked      = 1
            OTHERS             = 2.

*-----------------------------------------------------------------------
TOP-OF-PAGE.
  WRITE: /1 text-rpt, sy-repid INTENSIFIED ON,
        105 text-dte, sy-datum, text-amp, sy-uzeit.

  WRITE: /1 text-clt, sy-mandt UNDER sy-repid, sy-sysid.

  WRITE: 45 text-ttl.
  WRITE: text-pge UNDER text-dte, sy-pagno.
  WRITE: / text-011 UNDER text-clt, p_vers UNDER sy-mandt.
  SKIP 1.
  ULINE.

  WRITE:  20 text-ceg, p_costgr.                    "Cost Element Group
  WRITE:  text-per, p_fyear NO-GAP, text-slh NO-GAP, p_period.
  ULINE.
  WRITE: /2 vernr,             30 verna.
  WRITE: /2 project_tab-pspid, 30 post1.
  FORMAT INTENSIFIED ON.
  PERFORM print_headings.
*-----------------------------------------------------------------------

START-OF-SELECTION.
  PERFORM get_project_info.
  PERFORM get_cost_range_name USING p_costgr.
  PERFORM assign_cost_element_to_project.

  PERFORM final_step.


************************************************************************
*                Subroutines used by program

*------------------------- GET_PROJECT_INFO ----------------------------
*   This form selects all projects and WBS elements based on entries
*   made in the variant.  Both planning & acct assignment WBS are
*   selected
*-----------------------------------------------------------------------
FORM get_project_info.
  SELECT * FROM proj                                 "Project Selection
     WHERE pspid IN s_pspid
       AND vernr IN s_vernr.
    IF proj-pspid+5(2) CO '0123456789'.              "Eliminate template
      SELECT * FROM prps
         WHERE psphi = proj-pspnr
           AND ( belkz = 'X' OR plakz = 'X' ).      "Lowest elements

        MOVE proj-pspnr  TO project_tab-pspnr.    "Project Object
        MOVE proj-pspid  TO project_tab-posid.    "WBS
        MOVE proj-post1  TO project_tab-post1.    "Project Descript
        MOVE prps-objnr  TO project_tab-objnr.    "WBS Internal Id
        MOVE proj-vernr  TO project_tab-vernr.              "Division
        MOVE proj-verna  TO project_tab-verna.    "Division Name
        MOVE proj-pspid  TO project_tab-pspid.              "Project #
        PERFORM get_associated_costs.

      ENDSELECT.
    ENDIF.
  ENDSELECT.
ENDFORM.

*--------------------  GET_ASSOCIATED_COSTS  ---------------------------
*  Using entries in PROJECT_TAB, pick up all associated costs from both
*  the COSP & the COSS tables for the appropriate periods.
*-----------------------------------------------------------------------
FORM get_associated_costs.
  SELECT * FROM cosp
     WHERE objnr = project_tab-objnr
       AND gjahr = p_fyear.
    CHECK cosp-wrttp = '01' OR cosp-wrttp = '04'.

    IF cosp-wrttp = '01'.                                   "Plans
      CHECK cosp-versn = p_vers.
      ADD cosp-wkg001 FROM 1 TO 12 GIVING project_tab-pper.
      MOVE cosp-kstar TO project_tab-kstar.
      COLLECT project_tab.
      CLEAR project_tab-pper.
    ENDIF.

    IF cosp-wrttp = '04'.                                   "Actuals
      CHECK cosp-versn = 0.
*                                                    "Actual for Period

      ADD cosp-wkg001 FROM p_period TO p_period GIVING
                                     project_tab-aper.

*                                                    "Actual YTD

      ADD cosp-wkg001 FROM p_start TO p_period GIVING
                                     project_tab-aytd.
      MOVE cosp-kstar TO project_tab-kstar.
      COLLECT project_tab.
      CLEAR: project_tab-aper, project_tab-aytd.
    ENDIF.
  ENDSELECT.

  SELECT * FROM coss
     WHERE objnr = project_tab-objnr
       AND gjahr = p_fyear.
    CHECK coss-wrttp = '01' OR coss-wrttp = '04'.

    IF coss-wrttp = '01'.                                   "Plans
      CHECK coss-versn = p_vers.
      MOVE coss-kstar TO project_tab-kstar.
      ADD coss-wkg001 FROM p_start TO p_period GIVING project_tab-pper.
      COLLECT project_tab.
      CLEAR project_tab-pper.
    ENDIF.

    IF coss-wrttp = '04'.                                   "Actuals
      CHECK coss-versn = 0.
      MOVE coss-kstar TO project_tab-kstar.
      ADD coss-wkg001 FROM p_period TO p_period GIVING project_tab-aper.
      ADD coss-wkg001 FROM p_start TO p_period GIVING project_tab-aper.
      COLLECT project_tab.
      CLEAR project_tab-aper.
    ENDIF.
  ENDSELECT.

  SORT project_tab BY pspnr  kstar.
ENDFORM.

*--------------------- GET_COST_RANGE_NAME  ----------------------------
*   This time the same (as before) function module is used to get cost
*   element ranges for a selected cost element group
*---------------------------------------------------------------------*
*  -->  SETNAME - name of cost element group                          *
*---------------------------------------------------------------------*
FORM get_cost_range_name USING setname.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            class         = '0H'
            setnr         = setname
            table         = 'CCSS'
       TABLES
            set_values    = name_tab
       EXCEPTIONS
            set_not_found = 1.

ENDFORM.

*-------------------  ASSIGN_COST_GROUP_TO_ORDERS ----------------------
*   Here project_tab table is checked aganist ALLCOST_TAB.  If cost
*   element from project_tab is not listed in ALLCOST_TAB, the entry is
*   deleted form project_tab.  If cost element from ORDER_TAB does exist
*   in ALLCOST_TAB, the description of cost element and the name of
*   cost group is entered into project_tab.
*-----------------------------------------------------------------------
FORM assign_cost_element_to_project.

  DATA: l_index   LIKE sy-subrc.

  LOOP AT project_tab.
    LOOP AT name_tab.
      IF ( project_tab-kstar GE name_tab-from AND
           project_tab-kstar LE name_tab-to ).
        MOVE name_tab-setnr    TO project_tab-costgr.
        PERFORM get_group_name.
        MOVE wa_setinfo-title  TO project_tab-descript.
        MODIFY project_tab.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

* remove all WBS whose cost element is not in the cost element group

  LOOP AT project_tab.
    IF project_tab-costgr = space.
      DELETE project_tab.
    ENDIF.
  ENDLOOP.

ENDFORM.

*--------------------------  FINAL_STEP  -------------------------------
*  This subroutine displayes info from project_tab in the form of the
*-----------------------------------------------------------------------
FORM final_step.

  DATA:  temp(12).

  SORT project_tab BY vernr pspid costgr kstar.

  LOOP AT project_tab.
    MOVE project_tab-post1    TO post1.
    MOVE project_tab-descript TO descript.
    MOVE project_tab-verna    TO verna.
    MOVE project_tab-vernr    TO vernr.


*    at new pspnr.
    AT NEW pspid.
      NEW-PAGE.                                             "Project
    ENDAT.


    AT END OF kstar.
      FORMAT INTENSIFIED OFF.
      SUM.
      delta1 = project_tab-aytd - project_tab-pper.
      PERFORM print_vert.
      SELECT SINGLE * FROM csku
         WHERE spras = sy-langu
           AND ktopl = p_search
           AND kstar = project_tab-kstar.
      WRITE: (6) project_tab-kstar UNDER text-002,
                  csku-ktext       UNDER text-003,     "CE Description
             (14) project_tab-aper UNDER text-004,     "Period Actuals
             (14) project_tab-aytd UNDER text-007,     "YTD Actuals
             (14) project_tab-pper UNDER text-005,          "Plans
             (14) delta1           UNDER text-006.          "Variance
      FORMAT INTENSIFIED ON.
    ENDAT.

    AT END OF costgr.
      PERFORM print_vert.
      WRITE: 6 sy-uline(127).
      SUM.
      delta1 = project_tab-aytd - project_tab-pper.
      PERFORM print_vert.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED.
      WRITE: descript UNDER text-003,
             (14) project_tab-aper UNDER text-004,
             (14) project_tab-aytd UNDER text-007,
             (14) project_tab-pper UNDER text-005,
             (14) delta1           UNDER text-006.
      FORMAT RESET.
      WRITE: /6 sy-uline(127).
    ENDAT.

*    at end of pspnr.
    AT END OF pspid.
      SUM.
      delta1 = project_tab-aytd - project_tab-pper.
      PERFORM print_vert.
      FORMAT COLOR COL_TOTAL INTENSIFIED.
      WRITE:  'Total'           UNDER text-003,
             (14) project_tab-aper UNDER text-004,
             (14) project_tab-aytd UNDER text-007,
             (14) project_tab-pper UNDER text-005,
             (14) delta1           UNDER text-006.
      FORMAT RESET.
      WRITE: /6 sy-uline(127).
    ENDAT.

    AT END OF vernr.
      IF p_tot = 'X'.
        SUM.
        delta1 = project_tab-aytd - project_tab-pper.
        PERFORM print_vert.
        FORMAT COLOR COL_KEY INTENSIFIED.
        WRITE:  'Division Total' UNDER text-003,
               (14) project_tab-aper UNDER text-004,
               (14) project_tab-aytd UNDER text-007,
               (14) project_tab-pper UNDER text-005,
               (14) delta1           UNDER text-006.
        FORMAT RESET.
        WRITE: /6 sy-uline(127).
      ENDIF.
    ENDAT.

    AT LAST.
      IF p_tot = 'X'.
        PERFORM print_vert.
        WRITE: 6 sy-uline(127).
        SUM.
        delta1 = project_tab-aytd - project_tab-pper.
        PERFORM print_vert.
        FORMAT COLOR COL_GROUP.
        WRITE: 'Report Total' UNDER text-003,
               (14) project_tab-aper UNDER text-004,
               (14) project_tab-aytd UNDER text-007,
               (14) project_tab-pper UNDER text-005,
               (14) delta1           UNDER text-006.
        FORMAT RESET.
        WRITE: /6 sy-uline(127).
      ENDIF.
    ENDAT.
  ENDLOOP.


ENDFORM.

*---------------------- PRINT_HEADINGS  --------------------------------
*   This subroutine prints columns headings.
*-----------------------------------------------------------------------
FORM print_headings.
  WRITE: /6(127) sy-uline.
  FORMAT INTENSIFIED OFF.
  PERFORM print_vert.
  WRITE:  7(125) text-002 COLOR COL_NORMAL ON,
          21(51) text-003 COLOR COL_NORMAL ON,
          62 text-004 COLOR COL_NORMAL ON,
          81 text-007 COLOR COL_NORMAL ON,
          101 text-005 COLOR COL_NORMAL ON,
          119 text-006 COLOR COL_NORMAL ON.
  FORMAT INTENSIFIED ON.
  WRITE: /6(127) sy-uline.
ENDFORM.

*------------------------  PRINT_VERT ----------------------------------
*   This is used to print vertical lines.
*---------------------------------------------------------------------*
FORM print_vert.
  WRITE: /6 sy-vline, 19 sy-vline, 58 sy-vline, 77 sy-vline,
         96 sy-vline, 115 sy-vline, 132 sy-vline.
ENDFORM.

*---------------------- GET_GROUP_NAME ---------------------------------
* This function call retrieves the Group's name
*-----------------------------------------------------------------------
FORM get_group_name.
*   move name_tab-setnr+8(12) to setid.
*   call function 'G_SET_GET_INFO'
*      exporting
*           class           = '0H'
*           setname         = setid
*           table           = 'CCSS'
*           fieldname       = 'KSTAR'
*           client          = sy-mandt
*      importing
*           info            = wa_setinfo
*      exceptions
*           set_not_found   = 1.

*  cmccoy- removal of above function call - replaced with the following
*
*..... Read set into the buffer of the set manager (library GSSM) ....*
*      (repeated call of G_SET_INSERT for the same set
*       will not lead to repeated database access)
  CALL FUNCTION 'G_SET_INSERT'
       EXPORTING
            setname = p_setid
       IMPORTING
            header  = wa_setinfo
            index   = setindex.

*..... Move cost element parameter to value field ....................*
  svalue = project_tab-kstar.

*..... Search first existence of the cost element in the hierarchy ...*
  CALL FUNCTION 'G_INTERVAL_FIND_VALUE'
       EXPORTING
            control_block    = control_block
            index            = setindex
            value            = svalue  "Search value
       IMPORTING
            header           = wa_setinfo
       EXCEPTIONS
            end_of_intervals = 1.
  subrc = sy-subrc.

*..... Reset this control block of the set manager ................... *
  CALL FUNCTION 'G_CONTROL_BLOCK_RESET'
       EXPORTING
            control_block = control_block.


ENDFORM.
















