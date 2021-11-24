*----------------------------------------------------------------------*
***INCLUDE LZFG_TSCREEN_02I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.

  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'DIS_MODE'.
      IF CAST zif_tscreen_component( editor )->display_mode = zif_tscreen_component=>c_component_editable.
        CAST zif_tscreen_component( editor )->change_editable( zif_tscreen_component=>c_component_display ).
      ELSE.
        CAST zif_tscreen_component( editor )->change_editable( zif_tscreen_component=>c_component_editable ).
      ENDIF.
    WHEN 'SAVE'.
      editor->save_text( ).
    WHEN 'BACK'.
      <text> = editor->get_text( ).
      CAST zcl_text_editor_tool( editor )->free( ).
      FREE editor.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.

ENDMODULE.
