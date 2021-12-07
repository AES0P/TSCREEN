*&---------------------------------------------------------------------*
*& 包含               ZAESOP_TSCREEN_EVENT_INC
*&
*& 不要改动此include文件！！！
*& TSCREEN框架出厂自带事件在此处都写好了入口，如果对框架进行了扩展，把扩充的事件写到自己的程序里
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION."仅1000选择屏幕会触发
  lcl_prog=>push_view( ).
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'INIT' ).
    CATCH zcx_tscreen INTO DATA(gx_tscreen) ##NEEDED.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&　START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'EXE' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&  PBO
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT."所有带屏幕元素的选择屏幕会触发
  lcl_prog=>push_view( ).
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'PBO' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&  PAI
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'AUTH' ).
    CATCH zcx_tscreen INTO DATA(gx_tscreen).
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'PAI' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'EXIT' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&　　　　END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'SHOW' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'EXIT' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  lcl_prog=>push_view( ).
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'PBO' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module PBO_TC_LINE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_tc_line OUTPUT.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'PBO_TC_LINE' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'EXIT' ).
      LEAVE TO SCREEN 0.
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'PAI' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_TC_LINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_tc_line INPUT.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'PAI_TC_LINE' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  POH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE poh INPUT.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'POH' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  POV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pov INPUT.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'POV' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  TC_9000_01_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_user_command INPUT.
  TRY.
      zcl_tscreen_stack=>get_instance( )->current( )->handle_event( 'USER_COMMAND' ).
    CATCH zcx_tscreen INTO gx_tscreen.
      MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.
ENDMODULE.
