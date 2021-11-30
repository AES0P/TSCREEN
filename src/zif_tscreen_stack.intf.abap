INTERFACE zif_tscreen_stack
  PUBLIC .


  METHODS size
    RETURNING
      VALUE(size) TYPE i .
  METHODS is_empty
    RETURNING
      VALUE(is_empty) TYPE abap_bool .
  METHODS push
    IMPORTING
      !tscreen TYPE REF TO zif_tscreen .
  METHODS pop
    RAISING
      zcx_tscreen .
  METHODS top
    RETURNING
      VALUE(tscreen) TYPE REF TO zif_tscreen
    RAISING
      zcx_tscreen .
ENDINTERFACE.
