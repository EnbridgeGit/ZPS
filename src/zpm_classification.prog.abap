*&---------------------------------------------------------------------*
*& Report  ZPM_CLASSIFICATION
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZPM_CLASSIFICATION                             *
* Author             :  Manvitha Dadi                                  *
* Date               :  21-02-2022                                     *
* Change Request     :  CHG0241440                                    *
* Purpose            :  SAP-PM integration into that effort to extract *
*                       the Station Class for the Lantern Project      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 21-Feb-2022  DADIM       D30K932004-Initial development              *
*----------------------------------------------------------------------*
* ---------------------------------------------------------------------*
* 21-Feb-2022  LAKKADIS    D30K932262- Extract for REDIGDS project     *
*----------------------------------------------------------------------*

REPORT  zpm_classification.

INCLUDE zpm_classification_top.
INCLUDE zpm_classification_sel.
INCLUDE zpm_classification_evt.
INCLUDE zpm_classification_f01.
