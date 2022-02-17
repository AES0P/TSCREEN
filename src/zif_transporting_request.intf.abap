INTERFACE zif_transporting_request
  PUBLIC .


  TYPES ty_e071 TYPE e071 .
  TYPES:
    tty_e071 TYPE STANDARD TABLE OF ty_e071 WITH DEFAULT KEY .
  TYPES ty_e071k TYPE e071k .
  TYPES:
    tty_e071k TYPE STANDARD TABLE OF ty_e071k WITH DEFAULT KEY .

  METHODS associate
    IMPORTING
      !order_type    TYPE trfunction DEFAULT 'K'
      !task_type     TYPE trfunction DEFAULT 'S'
      !category      TYPE e070-korrdev DEFAULT 'SYST'
    RETURNING
      VALUE(tr_tool) TYPE REF TO zif_transporting_request .
  METHODS entrys
    IMPORTING
      !table           TYPE table
      VALUE(ddic_type) TYPE trobj_name OPTIONAL          "#EC CI_VALPAR
    RETURNING
      VALUE(tr_tool)   TYPE REF TO zif_transporting_request .
  METHODS entry
    IMPORTING
      VALUE(objname) TYPE trobj_name                     "#EC CI_VALPAR
      VALUE(objfunc) TYPE objfunc DEFAULT 'K'
      VALUE(tabkey)  TYPE trobj_name                     "#EC CI_VALPAR
    RETURNING
      VALUE(tr_tool) TYPE REF TO zif_transporting_request .
  METHODS commit
    IMPORTING
      !show_error        TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(is_commited) TYPE abap_bool .
ENDINTERFACE.
