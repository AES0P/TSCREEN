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
***INCLUDE LZFG_TSCREEN_00I02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CREATE_TS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_ts INPUT.
  IF ztscreen_manager-erdat < '19700101'.
*    AND ztscreen_manager-ertim IS INITIAL
*    AND ztscreen_manager-ernam IS INITIAL.
    ztscreen_manager-erdat = sy-datum.
    ztscreen_manager-ertim = sy-uzeit.
    ztscreen_manager-ernam = sy-uname.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHANGE_TS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_ts INPUT.
  ztscreen_manager-aedat = sy-datum.
  ztscreen_manager-aetim = sy-uzeit.
  ztscreen_manager-aenam = sy-uname.
ENDMODULE.
