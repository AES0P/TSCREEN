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
