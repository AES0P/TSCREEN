INTERFACE zif_text_editor
  PUBLIC .


  DATA text TYPE string READ-ONLY .

  METHODS set_status_text
    IMPORTING
      !title        TYPE c
    RETURNING
      VALUE(editor) TYPE REF TO zif_text_editor .
  METHODS set_toolbar_mode
    IMPORTING
      !mode         TYPE i
    RETURNING
      VALUE(editor) TYPE REF TO zif_text_editor .
  METHODS set_statusbar_mode
    IMPORTING
      !mode         TYPE i
    RETURNING
      VALUE(editor) TYPE REF TO zif_text_editor .
  METHODS set_text
    IMPORTING
      !text         TYPE string
    RETURNING
      VALUE(editor) TYPE REF TO zif_text_editor .
  METHODS save_text
    RETURNING
      VALUE(editor) TYPE REF TO zif_text_editor .
  METHODS get_text
    RETURNING
      VALUE(text) TYPE string .
ENDINTERFACE.