*MIT License
*
*Copyright (c) 2021 AES0P
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
CLASS zcl_tcomponent DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_tscreen_component
      ABSTRACT METHODS change_editable
      change_visibility
      set_component_attr_by_setting
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

    DATA parent TYPE REF TO zcl_tscreen .
    DATA tlog TYPE REF TO zif_tlog .
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


  METHOD zif_tscreen_component~initialize_by_tscreen.

    IF tscreen IS NOT BOUND.
      zcx_tscreen=>raise_text( 'Parent is not bound' ).
    ENDIF.

    me->parent  = tscreen.
    tlog        = tscreen->tlog.

  ENDMETHOD.
ENDCLASS.
