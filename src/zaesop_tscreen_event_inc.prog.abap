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
*&---------------------------------------------------------------------*
*& 包含               ZAESOP_TSCREEN_EVENT_INC
*&
*& 勿改！扩充的事件写到自己的程序里
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  INITIALIZATION/选择屏幕前初始化
*&---------------------------------------------------------------------*
INITIALIZATION."仅1000选择屏幕会触发
  lcl_prog=>push_view( ).
  lcl_prog=>event( 'INIT' ).

*&---------------------------------------------------------------------*
*&　START-OF-SELECTION/开始选择屏幕
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_prog=>event( 'EXE' ).

*&---------------------------------------------------------------------*
*&  PBO/选择屏幕输出
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT."所有带屏幕元素的选择屏幕会触发
  lcl_prog=>push_view( ).
  lcl_prog=>event( 'PBO' ).

*&---------------------------------------------------------------------*
*&  PAI/选择屏幕开始
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  lcl_prog=>event( 'AUTH' ).
  lcl_prog=>event( 'PAI' ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  lcl_prog=>event( 'EXIT' ).

*&---------------------------------------------------------------------*
*&  END-OF-SELECTION/结束选择屏幕（程序结束处理，输出等）
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  lcl_prog=>event( 'SHOW' ).
  lcl_prog=>event( 'EXIT' ).

*&---------------------------------------------------------------------*
*&  TOP-OF-PAGE
*&---------------------------------------------------------------------*
TOP-OF-PAGE.
  lcl_prog=>event( 'LINE_TOP' ).

*&---------------------------------------------------------------------*
*&  AT LINE-SELECTION
*&---------------------------------------------------------------------*
AT LINE-SELECTION.
  lcl_prog=>push_view( ).
  lcl_prog=>event( 'LINE_SEL' ).

*&---------------------------------------------------------------------*
*&  AT USER-COMMAND
*&---------------------------------------------------------------------*
AT USER-COMMAND.
  lcl_prog=>push_view( ).
  lcl_prog=>event( 'LINE_UCOMM' ).

*&---------------------------------------------------------------------*
*&  END-OF-PAGE
*&---------------------------------------------------------------------*
END-OF-PAGE.
  lcl_prog=>event( 'LINE_END' ).

*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  lcl_prog=>push_view( ).
  lcl_prog=>event( 'PBO' ).
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module PBO_TC_LINE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_tc_line OUTPUT.
  lcl_prog=>event( 'PBO_TC_LINE' ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  lcl_prog=>event( 'EXIT' ).
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.
  lcl_prog=>event( 'PAI' ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_TC_LINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_tc_line INPUT.
  lcl_prog=>event( 'PAI_TC_LINE' ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  POH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE poh INPUT.
  lcl_prog=>event( 'POH' ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  POV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pov INPUT.
  lcl_prog=>event( 'POV' ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  TC_9000_01_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_user_command INPUT.
  lcl_prog=>event( 'USER_COMMAND' ).
ENDMODULE.
