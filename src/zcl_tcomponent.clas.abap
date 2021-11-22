CLASS zcl_tcomponent DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_tscreen_component
      ABSTRACT METHODS change_editable
      change_visibility
      set_component_attr_by_setting
      initialize_by_tscreen
      DATA VALUES call_attr_method_by_parent = abap_true
      visibility = zif_tscreen_component=>c_component_visible .

    ALIASES call_attr_method_by_parent
      FOR zif_tscreen_component~call_attr_method_by_parent .
    ALIASES change_editable
      FOR zif_tscreen_component~change_editable .
    ALIASES change_visibility
      FOR zif_tscreen_component~change_visibility .
    ALIASES c_component_display
      FOR zif_tscreen_component~c_component_display .
    ALIASES c_component_editable
      FOR zif_tscreen_component~c_component_editable .
    ALIASES c_component_invisible
      FOR zif_tscreen_component~c_component_invisible .
    ALIASES c_component_visible
      FOR zif_tscreen_component~c_component_visible .
    ALIASES display_mode
      FOR zif_tscreen_component~display_mode .
    ALIASES get_component
      FOR zif_tscreen_component~get_component .
    ALIASES id
      FOR zif_tscreen_component~id .
    ALIASES set_component_attr_by_setting
      FOR zif_tscreen_component~set_component_attr_by_setting .
    ALIASES visibility
      FOR zif_tscreen_component~visibility .

    METHODS constructor
      IMPORTING
        !id TYPE string OPTIONAL
      RAISING
        cx_uuid_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TCOMPONENT IMPLEMENTATION.


  METHOD constructor.
    IF id IS NOT INITIAL.
      me->id = id.
    ELSE.
      me->id = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_tscreen_component~get_component.
    component = me.
  ENDMETHOD.
ENDCLASS.
