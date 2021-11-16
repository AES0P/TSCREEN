INTERFACE zif_table_control
  PUBLIC .


  TYPES:
    BEGIN OF ty_empty_field,
      field TYPE fieldname,
    END OF ty_empty_field .
  TYPES:
    tty_empty_field TYPE STANDARD TABLE OF ty_empty_field WITH EMPTY KEY .
  TYPES:
    tty_dynpread TYPE STANDARD TABLE OF dynpread WITH EMPTY KEY .

  CONSTANTS c_filter_on TYPE abap_bool VALUE abap_true ##NO_TEXT.
  CONSTANTS c_filter_off TYPE abap_bool VALUE abap_false ##NO_TEXT.
  DATA program TYPE syrepid READ-ONLY .
  DATA dynnr TYPE sy-dynnr READ-ONLY .
  DATA tc_name TYPE char10 READ-ONLY .
  DATA prefix TYPE field_name READ-ONLY .
  DATA selbar TYPE field_name READ-ONLY .
  DATA in_filter_mode TYPE abap_bool READ-ONLY .
  DATA filter_column TYPE field_name READ-ONLY .
  DATA empty_fields TYPE tty_empty_field READ-ONLY .
  DATA ref_structure_name TYPE tabname READ-ONLY .
  DATA screen_lines TYPE sy-loopc READ-ONLY .

  METHODS pai_tc_line .
  METHODS pbo_tc_line .
  METHODS poh .
  METHODS pov .
  METHODS user_command .
  METHODS set_visibility
    IMPORTING
      !visibility TYPE abap_bool .
  METHODS scroll
    IMPORTING
      !ucomm TYPE sy-ucomm .
  METHODS mark_all_columns .
  METHODS demark_all_columns .
  METHODS mark_all_lines .
  METHODS demark_all_lines .
  METHODS convert_all_lines_mark .
  METHODS insert_line .
  METHODS delete_line
    RETURNING
      VALUE(deleted) TYPE abap_bool .
  METHODS sort
    IMPORTING
      !ucomm TYPE sy-ucomm .
  METHODS mass_column
    IMPORTING
      VALUE(copy_from_top)  TYPE abap_bool DEFAULT abap_false
      VALUE(paste_from_top) TYPE abap_bool DEFAULT abap_false .
  METHODS filter
    RETURNING
      VALUE(filter_columns) TYPE lvc_t_filt .
  METHODS unfilter .
  METHODS show_row_content
    IMPORTING
      !index TYPE sy-tabix .
  METHODS show_row_ddic_detail
    IMPORTING
      VALUE(row) TYPE i .
  METHODS hide_empty_columns .
ENDINTERFACE.
