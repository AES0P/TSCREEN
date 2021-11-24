*&---------------------------------------------------------------------*
*& Report ZAESOP_EDITOR_TOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_editor_tool.

DATA abc TYPE string VALUE 'abc'.

CALL FUNCTION 'ZFM_GBC_TEXT_EDITOR'
  EXPORTING
    io_editor = zcl_text_editor_tool=>get_instance( )->set_status_text( 'abc test'
                                                    )->set_statusbar_mode( 1
                                                    )->set_toolbar_mode( 1 )
  CHANGING
    text      = abc.

WRITE abc.
