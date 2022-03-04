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
CLASS zcl_text_editor_tool DEFINITION
  PUBLIC
  INHERITING FROM zcl_text_editor
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      IMPORTING
        !id                         TYPE string OPTIONAL
        !container_name             TYPE c OPTIONAL
        !content                    TYPE string OPTIONAL
        VALUE(length_content)       TYPE i OPTIONAL
        VALUE(style)                TYPE i OPTIONAL
        !wordwrap_mode              TYPE i DEFAULT cl_gui_textedit=>wordwrap_at_fixed_position
        !length_line                TYPE i DEFAULT '132'
        !wordwrap_to_linebreak_mode TYPE i DEFAULT cl_gui_textedit=>false
        !filedrop_mode              TYPE i OPTIONAL
        VALUE(lifetime)             TYPE i OPTIONAL
        VALUE(name)                 TYPE string OPTIONAL
      RETURNING
        VALUE(editor)               TYPE REF TO zif_text_editor
      RAISING
        cx_uuid_error
        zcx_tscreen .

    METHODS free
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA instance TYPE REF TO zcl_text_editor_tool .
ENDCLASS.



CLASS ZCL_TEXT_EDITOR_TOOL IMPLEMENTATION.


  METHOD get_instance.

    IF instance IS NOT BOUND.

      CREATE OBJECT instance
        EXPORTING
          id                         = id
          container_name             = container_name
          content                    = content
          length_content             = length_content
          style                      = style
          wordwrap_mode              = wordwrap_mode
          length_line                = length_line
          wordwrap_to_linebreak_mode = wordwrap_to_linebreak_mode
          filedrop_mode              = filedrop_mode
          lifetime                   = lifetime
          name                       = name.

    ENDIF.

    editor = instance.

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD free.
    FREE instance.
    super->free( ).
  ENDMETHOD.
ENDCLASS.
