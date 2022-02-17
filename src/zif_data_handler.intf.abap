INTERFACE zif_data_handler
  PUBLIC .


  CLASS-METHODS export_data
    IMPORTING
      !data         TYPE REF TO data
      !fieldcatalog TYPE lvc_t_fcat .
  CLASS-METHODS import_data
    IMPORTING
      !fieldcatalog TYPE lvc_t_fcat
    CHANGING
      !data         TYPE REF TO data .
ENDINTERFACE.
