*&---------------------------------------------------------------------*
*& Report  ZPPMI022
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zppmi022.

TABLES: bpja.

TYPES: BEGIN OF typ_output,
         gjahr  TYPE gjahr,
         comma1 type c,
         poski  TYPE prps-poski,
         comma2 TYPE c,
         wtjhr  TYPE bp_wjt,
       END OF typ_output.
DATA:BEGIN OF gt_tab OCCURS 0,  "Text file format
           text1(208),
     END OF gt_tab.
CONSTANTS: gc_app     TYPE dxfields-location VALUE 'A'.
DATA: gt_output TYPE TABLE OF typ_output,
      gs_output TYPE typ_output,
      gt_bpja   TYPE TABLE OF bpja,
      gs_bpja   TYPE bpja,
      gv_path   TYPE dxfields-longpath.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_gjahr FOR bpja-gjahr, " OBLIGATORY,
                s_versn FOR bpja-versn, " OBLIGATORY,
                s_lednr FOR bpja-lednr,
                s_objnr FOR bpja-objnr NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_server RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmd,
            r_pc RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_file TYPE rfpdo-lboxfile. "  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.
***************
AT SELECTION-SCREEN output.
  perform PBO_data.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  IF r_pc = 'X'.
    p_file = 'h:\'.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        mask      = ',DAT File,*.dat'
        static    = 'X'
      CHANGING
        file_name = p_file.

  ELSE. "Application server

    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = gc_app
        i_server        = space
        i_path          = gv_path
      IMPORTING
        o_path          = gv_path
      EXCEPTIONS
        rfc_error       = 1
        error_with_gui  = 2
        OTHERS          = 3.
    IF sy-subrc EQ 0.
      p_file = gv_path.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  PERFORM selection_validation.
  CLEAR: gt_output,
         gt_bpja.

  PERFORM get_data.

  IF r_server = 'X'. "application server
    IF gt_output[] IS NOT INITIAL. " internal table for data
*Downloading of text file to Application server.
      PERFORM as_downloading_file.
    ENDIF.
  ELSE.  "Presentation Server
    IF gt_output IS NOT INITIAL.
      PERFORM pc_download_file.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  AS_DOWNLOADING_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM as_downloading_file .
  OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open text file for output., Check path/security'.
  ELSE.
    LOOP AT gt_output INTO gs_output.
      TRANSFER gs_output TO p_file.
    ENDLOOP.
    CLOSE DATASET p_file.
  ENDIF.
ENDFORM.                    " AS_DOWNLOADING_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_prps TYPE TABLE OF prps,
        ls_prps TYPE prps,
        lt_bpja TYPE TABLE OF bpja.

  CLEAR s_objnr.
  REFRESH s_objnr.

  s_objnr-sign = 'I'.
  s_objnr-option = 'CP'.
  s_objnr-low = 'PR*'.      "only projects
  APPEND s_objnr.

  SELECT * FROM bpja INTO TABLE gt_bpja
    WHERE lednr IN s_lednr
      AND objnr IN s_objnr
      AND gjahr IN s_gjahr
      AND versn IN s_versn.

  IF gt_bpja[] IS INITIAL.
    WRITE : / 'No data to output..'.
    STOP.
  ENDIF.

  lt_bpja[] = gt_bpja[].
  SORT lt_bpja BY objnr.
  DELETE ADJACENT DUPLICATES FROM lt_bpja COMPARING objnr.
  IF lt_bpja IS NOT INITIAL.
    SELECT * FROM prps INTO TABLE lt_prps
      FOR ALL ENTRIES IN lt_bpja
      WHERE objnr = lt_bpja-objnr.
  ENDIF.
  LOOP AT gt_bpja INTO gs_bpja.
    clear ls_prps.
    READ TABLE lt_prps INTO ls_prps
         with key objnr = gs_bpja-objnr.
    gs_output-gjahr  = gs_bpja-gjahr.
    gs_output-comma1 = ','.
    gs_output-poski  = ls_prps-poski.
    gs_output-comma2 = ','.
    gs_output-wtjhr  = gs_bpja-wtjhr.
    append gs_output to gt_output.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PC_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pc_download_file .

  DATA: lv_filename TYPE string.

*Download Canadian file
  MOVE p_file TO lv_filename.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_output
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24.
  IF sy-subrc <> 0.
    WRITE: / 'Error with downloading file at PC ', sy-subrc.
  ELSE.
    WRITE: / 'File successfully created at ', p_file.
  ENDIF.

ENDFORM.                    " PC_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  PBO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PBO_DATA .
IF r_pc = 'X'.
   p_file = 'h:\'.
else.
   clear p_file.
ENDIF.
ENDFORM.                    " PBO_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECTION_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECTION_VALIDATION .

if s_gjahr is INITIAL.
   write: / 'Enter Year on selection screen..'.
   stop.
endif.
if s_versn is INITIAL.
   write: / 'Enter Version on selection screen..'.
   stop.
endif.
IF p_file is INITIAL.
   write: / 'Enter File Name & Path on selection screen..'.
   stop.
ENDIF.
ENDFORM.                    " SELECTION_VALIDATION
