*&---------------------------------------------------------------------*
*& Report ZAESOP_EDITOR_TOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_editor_tool.

DATA(abc123) = CONV string( 'abc123' ).
DATA(editor) = CAST zif_tscreen_component(
                    zcl_text_editor_tool=>get_instance( abc123  )->set_status_text( 'TEST abc123'
                                                                )->set_statusbar_mode( 1
                                                                )->set_toolbar_mode( 1 ) ).
CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS sy-dynnr.
  SET TITLEBAR  sy-dynnr.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA(ok_code) = sy-ucomm.
  CASE ok_code.
    WHEN 'DIS_MODE'.
      IF editor->display_mode = zif_tscreen_component=>c_component_editable.
        editor->change_editable( zif_tscreen_component=>c_component_display ).
      ELSE.
        editor->change_editable( zif_tscreen_component=>c_component_editable ).
      ENDIF.
    WHEN 'SAVE'.
      CAST zif_text_editor( editor )->save_text( ).
    WHEN 'BACK'.
      abc123 = CAST zif_text_editor( editor )->get_text( ).
      MESSAGE abc123 TYPE 'I'.
      CAST zcl_text_editor( editor )->free( ).
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.
