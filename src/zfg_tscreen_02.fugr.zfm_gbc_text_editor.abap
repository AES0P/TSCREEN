FUNCTION zfm_gbc_text_editor.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IO_EDITOR) TYPE REF TO  ZIF_TEXT_EDITOR
*"     REFERENCE(IV_READONLY) TYPE  XFELD
*"  CHANGING
*"     REFERENCE(TEXT) TYPE  ANY
*"----------------------------------------------------------------------

  editor      = io_editor.
  is_readonly = iv_readonly.

  ASSIGN text TO <text>.
  editor->set_text( <text> ).

  CALL SCREEN 9000.

ENDFUNCTION.
