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
