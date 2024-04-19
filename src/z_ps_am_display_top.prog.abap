*&---------------------------------------------------------------------*
*&  Include           Z_PS_AM_DISPLAY_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   Z_PS_AM_DISPLAY                               *
* Program Include    :   Z_PS_AM_DISPLAY_TOP                           *
* Author             :                                                 *
* Date               :   April,13, 2018                                *
* Technical Contact  :   Ashok kumar madasu                            *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :                                                 *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:T090NAZ,ANKA.
*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPES: BEGIN OF GTY_T090NAT,
       AFAPL TYPE AFAPL,
       AFASL TYPE AFASL,
       AFATXT TYPE AFATXT,
       END OF GTY_T090NAT,
       BEGIN OF GTY_T090NAZ,
       AFAPL TYPE AFAPL,
       AFASL TYPE AFASL,
       METSTU TYPE METSTU,
       END OF GTY_T090NAZ,
       BEGIN OF GTY_T090NS,
       AFAPL TYPE AFAPL,
       METSTU TYPE METSTU,
       AFPROZ TYPE AFPROZ,
       END OF GTY_T090NS,
       BEGIN OF GTY_ANKA,
       ANLKL TYPE ANLKL,
       XLOEV TYPE XLOEV_ANKA,
       XSPEA TYPE XSPEA_ANKA,
       END OF GTY_ANKA,
       BEGIN OF GTY_ANKB,
       ANLKL TYPE ANLKL,
       AFASL TYPE AFASL,
       END OF GTY_ANKB,
       BEGIN OF GTY_T090NA,
       AFAPL TYPE AFAPL,
       AFASL TYPE AFASL,
       XAKTIV TYPE XAKTIV_RSL,
       END OF GTY_T090NA,
       BEGIN OF GTY_FINAL,
         ANLKL TYPE ANLKL,
         AFASL TYPE AFASL,
         AFPROZ TYPE AFPROZ,
         AFATXT TYPE AFATXT,
         END OF GTY_FINAL.
*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================

DATA: GT_T090NAT TYPE TABLE OF GTY_T090NAT,
      GT_T090NA  TYPE TABLE OF GTY_T090NA,
      GT_T090NAZ TYPE TABLE OF GTY_T090NAZ,
      GT_ANKA    TYPE TABLE OF GTY_ANKA,
      GT_ANKB    TYPE TABLE OF GTY_ANKB,
      GT_T090NS  TYPE TABLE OF GTY_T090NS,
      GT_FINAL   TYPE TABLE OF GTY_FINAL,
      gt_fieldcatalog type slis_t_fieldcat_alv.
*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA:GS_T090NAT TYPE GTY_T090NAT,
     GS_T090NA  TYPE GTY_T090NA,
     GS_ANKA    TYPE GTY_ANKA,
     GS_ANKB    TYPE GTY_ANKB,
     GS_T090NAZ TYPE GTY_T090NAZ,
     GS_T090NS  TYPE GTY_T090NS,
     GS_FINAL   TYPE GTY_FINAL,
     gs_fieldcatalog like LINE OF gt_fieldcatalog.

*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS: GC_CAD TYPE CHAR3 VALUE 'CAD',
           GC_FLAG TYPE FLAG VALUE 'X'.
