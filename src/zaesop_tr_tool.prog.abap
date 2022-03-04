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
*& Report ZAESOP_TR_TOOL
*&---------------------------------------------------------------------*
*&  工作台请求和定制请求传输ZTABLE示例
*&---------------------------------------------------------------------*
REPORT zaesop_tr_tool.

"应用表 A    .
SELECT *
  FROM ztdynpro_attr
  INTO TABLE @DATA(lt_ztdynpro_attr).
IF sy-subrc = 0.
  NEW zcl_transporting_request( )->associate(  )->entrys( lt_ztdynpro_attr )->commit( ).
ENDIF.

"定制表 C    .
*SELECT *
*  FROM zgbct_cg_cont
*  INTO TABLE @DATA(lt_zgbct_cg_cont).
*
*zcl_tr_customizing=>get_instance( )->associate( )->entrys( lt_zgbct_cg_cont )->commit( ).
