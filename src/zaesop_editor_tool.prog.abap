*&---------------------------------------------------------------------*
*& Report ZAESOP_EDITOR_TOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_editor_tool.

TRY.

    ##NEEDED    DATA abc TYPE string VALUE 'abc'.

    CALL FUNCTION 'ZFM_GBC_TEXT_EDITOR'
      EXPORTING
        io_editor = zcl_text_editor_tool=>get_instance( )->set_status_text( 'abc test'
                                                        )->set_statusbar_mode( 1
                                                        )->set_toolbar_mode( 1 )
      CHANGING
        text      = abc.

    WRITE abc.

  CATCH cx_uuid_error INTO DATA(lx_uuid_error) ##NEEDED.
    MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  CATCH zcx_tscreen INTO DATA(lx_tscreen) ##NEEDED.
    MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
