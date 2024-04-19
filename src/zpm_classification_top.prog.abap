*&---------------------------------------------------------------------*
*&  Include           ZPM_CLASSIFICATION_TOP
*&---------------------------------------------------------------------*

TABLES : cabn,inob.
*&---------------------------------------------------------------------*
*&   Structure Declarations
*&---------------------------------------------------------------------*
TYPES : BEGIN OF gty_kssk,
        atnam TYPE atnam,
        objek TYPE objnum,
        atwrt TYPE atwrt,
        inob_objek TYPE cuobn,
        END OF gty_kssk.

TYPES : BEGIN OF gty_tplnr,
        atnam TYPE atnam,
        objek TYPE objnum,
        atwrt TYPE atwrt,
        inob_objek TYPE tplnr,
        END OF gty_tplnr.

TYPES : BEGIN OF gty_equnr,
        atnam TYPE atnam,
        objek TYPE objnum,
        atwrt TYPE atwrt,
        inob_objek TYPE equnr,
        END OF gty_equnr.

TYPES : BEGIN OF gty_iflo,
        inob_objek TYPE tplnr,
        objnr TYPE j_objnr,
        eqart TYPE eqart, "D30K932262 for REDIGDS
        END OF gty_iflo.

TYPES : BEGIN OF gty_vequi,
        inob_objek TYPE equnr,
        eqart TYPE eqart, "D30K932262 for REDIGDS
        objnr TYPE j_objnr,
        END OF gty_vequi.

TYPES : BEGIN OF gty_objnr,
        objnr TYPE j_objnr,
        objek TYPE char30,
        atnam TYPE atnam,
        atwrt TYPE atwrt,
        eqart TYPE eqart,
        END OF gty_objnr.

* Begin of changes D30K932308 for REDIGDS
TYPES : BEGIN OF gty_cabn,
        atnam TYPE atnam,
        atfor TYPE atfor,
        msehi TYPE msehi,
        objek TYPE objnum,
        atflv TYPE atflv,
        inob_objek TYPE cuobn,
        END OF gty_cabn.
*End of changes D30K932308 for REDIGDS

TYPES : BEGIN OF gty_final,
        objek TYPE char30,
        atnam TYPE atnam,
        atwrt TYPE atwrt,
        eqart TYPE eqart, "D30K932262 for REDIGDS
        ustat TYPE j_txt04,"D30K932262 for REDIGDS
        sstat TYPE j_txt04,"D30K932262 for REDIGDS
        inlet_mop_1 TYPE string,"D30K932308 for REDIGDS
        outlet_mop_1 TYPE string,"D30K932308 for REDIGDS
        END OF gty_final.

*&---------------------------------------------------------------------*
*&   Internal Table Declarations
*&---------------------------------------------------------------------*
DATA : gt_kssk TYPE TABLE OF gty_kssk,
       gt_tplnr TYPE TABLE OF gty_tplnr,
       gt_tplnr1 TYPE TABLE OF gty_tplnr,
       gt_equnr TYPE TABLE OF gty_equnr,
       gt_equnr1 TYPE TABLE OF gty_equnr,
       gt_iflo TYPE TABLE OF gty_iflo,
       gt_vequi TYPE TABLE OF gty_vequi,
       gt_objnr TYPE TABLE OF gty_objnr,
       gt_cabn TYPE TABLE OF gty_cabn,
       gt_final TYPE TABLE OF gty_final,
       gt_final1 TYPE TABLE OF gty_final,
       gt_temp TYPE TABLE OF gty_final,
       gt_status TYPE TABLE OF jstat,
       gt_dyn_fcat TYPE lvc_t_fcat,
       gt_alv_fieldcat TYPE slis_t_fieldcat_alv.

*&---------------------------------------------------------------------*
*&   Work area Declarations
*&---------------------------------------------------------------------*
DATA : gs_tplnr TYPE gty_tplnr,
       gs_tplnr1 TYPE gty_tplnr,  " changes
       gs_equnr TYPE gty_equnr,
       gs_iflo TYPE gty_iflo,
       gs_vequi TYPE gty_vequi,
       gs_objnr TYPE gty_objnr,
       gs_cabn TYPE  gty_cabn,
       gs_final TYPE gty_final,
       gs_final1 TYPE gty_final,
       gs_temp TYPE gty_final,
       gs_status TYPE jstat,
       gs_dyn_fcat  TYPE lvc_s_fcat,
       gs_alv_fieldcat TYPE slis_fieldcat_alv.

*&---------------------------------------------------------------------*
*&   Range Declaration
*&---------------------------------------------------------------------*
RANGES : gr_stat FOR jstat-stat.
DATA : gs_stat LIKE LINE OF gr_stat.

*&---------------------------------------------------------------------*
*&   Variables
*&---------------------------------------------------------------------*
DATA : gv_pos       TYPE i,
       gv_count     TYPE i.

*&---------------------------------------------------------------------*
*&   Constants
*&---------------------------------------------------------------------*
DATA : gc_i0320 TYPE j_status VALUE 'I0320',
       gc_i1105 TYPE j_status VALUE 'I1105',
       gc_i0076 TYPE j_status VALUE 'I0076'.

*&---------------------------------------------------------------------*
*&   Data References
*&---------------------------------------------------------------------*
DATA : gt_dyn_table TYPE REF TO data,
       gs_line      TYPE REF TO data,
       gs_line1     TYPE REF TO data.

*&---------------------------------------------------------------------*
*&   Field Symbols
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <gfs_line>,<gfs_line1>,
               <gfs_dyn_table> TYPE STANDARD TABLE,
               <fs1>.
