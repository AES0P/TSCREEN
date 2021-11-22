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
        !content                    TYPE string
        VALUE(length_content)       TYPE i OPTIONAL
        VALUE(style)                TYPE i OPTIONAL
        !wordwrap_mode              TYPE i DEFAULT cl_gui_textedit=>wordwrap_at_fixed_position
        !length_line                TYPE i DEFAULT 132
        !wordwrap_to_linebreak_mode TYPE i DEFAULT cl_gui_textedit=>false
        !filedrop_mode              TYPE i OPTIONAL
        VALUE(lifetime)             TYPE i OPTIONAL
        VALUE(name)                 TYPE string OPTIONAL
      RETURNING
        VALUE(editor)               TYPE REF TO zif_text_editor
      RAISING
        cx_uuid_error
        zcx_tscreen .
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

  ENDMETHOD.
ENDCLASS.
