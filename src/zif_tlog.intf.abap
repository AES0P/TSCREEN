INTERFACE zif_tlog
  PUBLIC .


  METHODS clear
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS add_log
    IMPORTING
      !type           TYPE sy-msgty DEFAULT 'S'
      !content        TYPE string
      !dynnr          TYPE sy-dynnr DEFAULT sy-dynnr
      !ucomm          TYPE sy-ucomm DEFAULT sy-ucomm
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS save_log
    IMPORTING
      !guid           TYPE guid
      !symbol         TYPE icon_d OPTIONAL
      !if_commit      TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS show_log
    IMPORTING
      !style          TYPE c DEFAULT '1'
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS commit
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
ENDINTERFACE.
