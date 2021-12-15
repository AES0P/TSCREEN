INTERFACE zif_tscreen_component
  PUBLIC .


  DATA id TYPE string READ-ONLY .
  DATA display_mode TYPE abap_bool READ-ONLY .
  DATA visibility TYPE abap_bool READ-ONLY .
  DATA call_attr_method_by_parent TYPE abap_bool READ-ONLY .
  CONSTANTS c_component_visible TYPE abap_bool VALUE abap_true ##NO_TEXT.
  CONSTANTS c_component_invisible TYPE abap_bool VALUE abap_false ##NO_TEXT.
  CONSTANTS c_component_editable TYPE abap_bool VALUE abap_true ##NO_TEXT.
  CONSTANTS c_component_display TYPE abap_bool VALUE abap_false ##NO_TEXT.
  CONSTANTS c_component_editor TYPE string VALUE 'EDITOR' ##NO_TEXT.
  CONSTANTS c_component_tc TYPE string VALUE 'TC' ##NO_TEXT.

  METHODS get_component
    RETURNING
      VALUE(component) TYPE REF TO zif_tscreen_component .
  METHODS initialize_by_tscreen
    IMPORTING
      !tscreen TYPE REF TO zcl_tscreen
    RAISING
      zcx_tscreen .
  METHODS change_visibility
    IMPORTING
      !visibility      TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(component) TYPE REF TO zif_tscreen_component .
  METHODS change_editable
    IMPORTING
      !display_mode    TYPE abap_bool
    RETURNING
      VALUE(component) TYPE REF TO zif_tscreen_component .
  METHODS set_component_attr_by_setting .
ENDINTERFACE.
