class ZCL_TEXT_EDITOR_TOOL definition
  public
  inheriting from ZCL_TEXT_EDITOR
  final
  create private .

public section.

  class-methods GET_INSTANCE
    importing
      !ID type STRING optional
      !CONTAINER_NAME type C optional
      !CONTENT type STRING optional
      value(LENGTH_CONTENT) type I optional
      value(STYLE) type I optional
      !WORDWRAP_MODE type I default CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
      !LENGTH_LINE type I default '132'
      !WORDWRAP_TO_LINEBREAK_MODE type I default CL_GUI_TEXTEDIT=>FALSE
      !FILEDROP_MODE type I optional
      value(LIFETIME) type I optional
      value(NAME) type STRING optional
    returning
      value(EDITOR) type ref to ZIF_TEXT_EDITOR
    raising
      CX_UUID_ERROR
      ZCX_TSCREEN .

  methods FREE
    redefinition .
protected section.
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
