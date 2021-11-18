interface ZIF_TABLE_CONTROL
  public .


  types:
    BEGIN OF ty_empty_field,
      field TYPE fieldname,
    END OF ty_empty_field .
  types:
    tty_empty_field TYPE STANDARD TABLE OF ty_empty_field WITH EMPTY KEY .
  types:
    tty_dynpread TYPE STANDARD TABLE OF dynpread WITH EMPTY KEY .

  constants C_FILTER_ON type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  constants C_FILTER_OFF type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data PROGRAM type SYREPID read-only .
  data DYNNR type SY-DYNNR read-only .
  data TC_NAME type CHAR10 read-only .
  data PREFIX type FIELD_NAME read-only .
  data SELBAR type FIELD_NAME read-only .
  data IN_FILTER_MODE type ABAP_BOOL read-only .
  data FILTER_COLUMN type FIELD_NAME read-only .
  data EMPTY_FIELDS type TTY_EMPTY_FIELD read-only .
  data REF_STRUCTURE_NAME type TABNAME read-only .
  data SCREEN_LINES type SY-LOOPC read-only .

  methods PAI_TC_LINE .
  methods PBO_TC_LINE .
  methods POH .
  methods POV .
  methods USER_COMMAND .
  methods SET_VISIBILITY
    importing
      !VISIBILITY type ABAP_BOOL .
  methods SCROLL
    importing
      !UCOMM type SY-UCOMM .
  methods MARK_ALL_COLUMNS .
  methods DEMARK_ALL_COLUMNS .
  methods MARK_ALL_LINES .
  methods DEMARK_ALL_LINES .
  methods CONVERT_ALL_LINES_MARK .
  methods INSERT_LINE .
  methods DELETE_LINE
    returning
      value(DELETED) type ABAP_BOOL .
  methods SORT
    importing
      !UCOMM type SY-UCOMM .
  methods MASS_COLUMN
    importing
      value(COPY_FROM_TOP) type ABAP_BOOL default ABAP_FALSE
      value(PASTE_FROM_TOP) type ABAP_BOOL default ABAP_FALSE .
  methods FILTER
    returning
      value(FILTER_COLUMNS) type LVC_T_FILT .
  methods UNFILTER .
  methods SHOW_ROW_CONTENT
    importing
      !INDEX type SY-TABIX .
  methods SHOW_ROW_DDIC_DETAIL
    importing
      value(ROW) type I .
  methods HIDE_EMPTY_COLUMNS .
endinterface.
