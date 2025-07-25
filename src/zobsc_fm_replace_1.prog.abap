*&---------------------------------------------------------------------*
*& Report ZOBSC_FM_REPLACE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZOBSC_FM_REPLACE_1.

TABLES : mara.

*-------------INTERNAL TABLE DECLARATION
TYPES : BEGIN OF itab,
          matnr TYPE mara-matnr,
          meins TYPE mara-meins,
          ernam TYPE mara-ernam,
          aenam TYPE mara-aenam,
          mtart TYPE mara-mtart,
        END OF itab.

DATA : it_tab TYPE TABLE OF itab .
DATA : v_file TYPE string.
DATA : t_file TYPE rlgrap-filename.

*-------------RETRIVE DATA FROM DATABASE
SELECT
matnr
meins ernam aenam mtart FROM mara
INTO TABLE
it_tab.

*-------------CALLING FUNCTION MODULES
CALL FUNCTION 'F4_FILENAME' "PASS THE FILE NAME AS U NEED.
  EXPORTING
    field_name = 'T_FILE'
  IMPORTING
    file_name  = t_file.

v_file = t_file. "STRING CONVERSION

*Begin of change by Haneya Tool
CALL METHOD cl_gui_frontend_services=>gui_download "Added by Haneya Tool => . "Added by Haneya Tool
*CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD"Commented by Haneya Tool
  EXPORTING
*   BIN_FILESIZE            =
    filename                = v_file
    filetype                = 'ASC'
*   APPEND                  = SPACE
*   WRITE_FIELD_SEPARATOR   = SPACE
*   HEADER                  = '00'
*   TRUNC_TRAILING_BLANKS   = SPACE
*   WRITE_LF                = 'X'
*   COL_SELECT              = SPACE
*   COL_SELECT_MASK         = SPACE
*   DAT_MODE                = SPACE
*   CONFIRM_OVERWRITE       = SPACE
*   NO_AUTH_CHECK           = SPACE
*   CODEPAGE                = SPACE
*   IGNORE_CERR             = ABAP_TRUE
*   REPLACEMENT             = '#'
*   WRITE_BOM               = SPACE
*   TRUNC_TRAILING_BLANKS_EOL = 'X'
*   WK1_N_FORMAT            = SPACE
*   WK1_N_SIZE              = SPACE
*   WK1_T_FORMAT            = SPACE
*   WK1_T_SIZE              = SPACE
*   SHOW_TRANSFER_STATUS    = 'X'
*   FIELDNAMES              =
*   WRITE_LF_AFTER_LAST_LINE = 'X'
*   VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*   FILELENGTH              =
  CHANGING
    data_tab                = it_tab
  EXCEPTIONS
    access_denied           = 1
    control_flush_error     = 2
    dataprovider_exception  = 3
    disk_full               = 4
    dp_error_create         = 5
    dp_error_send           = 6
    dp_error_write          = 7
    dp_out_of_memory        = 8
    dp_timeout              = 9
    error_no_gui            = 10
    filesize_not_allowed    = 11
    file_not_found          = 12
    file_write_error        = 13
    gui_refuse_filetransfer = 14
    header_not_allowed      = 15
    header_too_long         = 16
    invalid_type            = 17
    not_supported_by_gui    = 18
    no_authority            = 19
    no_batch                = 20
    separator_not_allowed   = 21
    unknown_dp_error        = 22
    unknown_error           = 23.

*CALL FUNCTION 'DOWNLOAD'
**exporting
***   BIN_FILESIZE                  = ' '
***   CODEPAGE                      = ' '
**  filename                      = v_file
**  filetype                      = 'ASC'
***   ITEM                          = ' '
***   MODE                          = ' '
***   WK1_N_FORMAT                  = ' '
***   WK1_N_SIZE                    = ' '
***   WK1_T_FORMAT                  = ' '
***   WK1_T_SIZE                    = ' '
***   FILEMASK_MASK                 = ' '
***   FILEMASK_TEXT                 = ' '
***   FILETYPE_NO_CHANGE            = ' '
***   FILEMASK_ALL                  = ' '
***   FILETYPE_NO_SHOW              = ' '
***   SILENT                        = 'S'
***   COL_SELECT                    = ' '
***   COL_SELECTMASK                = ' '
***   NO_AUTH_CHECK                 = ' '
*** IMPORTING
***   ACT_FILENAME                  =
***   ACT_FILETYPE                  =
***   FILESIZE                      =
***   CANCEL                        =
** tables
**   data_tab                      =  it_tab.
*End of change by Haneya Tool
*End of change by Haneya Tool
*   FIELDNAMES                    =
* EXCEPTIONS
*   INVALID_FILESIZE              = 1
*   INVALID_TABLE_WIDTH           = 2
*   INVALID_TYPE                  = 3
*   NO_BATCH                      = 4
*   UNKNOWN_ERROR                 = 5
*   GUI_REFUSE_FILETRANSFER       = 6
*   OTHERS                        = 7
.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
