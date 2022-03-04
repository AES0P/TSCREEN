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
***INCLUDE LZFG_TSCREEN_01I01.
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  check_repeat  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_repeat INPUT.

  IF ztdynpro_attr-tc_name = 'PF-STATUS' AND ztdynpro_attr-zmode = '*'.
    MESSAGE 'PF-STATUS控件只能按单独场景配置生效,不支持通用设置' TYPE 'E'.
  ENDIF.

  SELECT SINGLE @abap_false ##NEEDED
    FROM ztdynpro_attr
    INTO @DATA(lv_is) ##NEEDED
   WHERE cprog      = @ztdynpro_attr-cprog
     AND dynnr      = @ztdynpro_attr-dynnr
     AND tc_name    = @ztdynpro_attr-tc_name
     AND name       = @ztdynpro_attr-name.
  IF sy-subrc = 0.
    MESSAGE '已存在与' && ztdynpro_attr-name && '相同主键的配置项，程序只读取排名最前的组的配置。如果组名与元素实际组名不匹配，配置将不生效！' TYPE 'I'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_EMPTY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_empty INPUT.

  IF ztdynpro_attr-xoblg = abap_false AND ztdynpro_attr-xoptn = abap_false AND ztdynpro_attr-xnodi = abap_false AND ztdynpro_attr-xdisp = abap_false.
    MESSAGE '必须勾选其中一项！' TYPE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_input INPUT.

  ##NEEDED  ##DECL_MODUL
  DATA field TYPE string.
  GET CURSOR FIELD field.

  REPLACE 'ZTDYNPRO_ATTR-' WITH '' INTO field.
  CONDENSE field NO-GAPS.

  IF ztdynpro_attr-xoblg = abap_true AND field = 'XOBLG'.
    ztdynpro_attr-xoptn =  ztdynpro_attr-xnodi = ztdynpro_attr-xdisp = abap_false.
  ELSEIF ztdynpro_attr-xoptn = abap_true AND field = 'XOPTN'.
    ztdynpro_attr-xoblg =  ztdynpro_attr-xnodi = ztdynpro_attr-xdisp = abap_false.
  ELSEIF ztdynpro_attr-xnodi = abap_true AND field = 'XNODI'.
    ztdynpro_attr-xoblg =  ztdynpro_attr-xoptn = ztdynpro_attr-xdisp = abap_false.
  ELSEIF ztdynpro_attr-xdisp = abap_true AND field = 'XDISP'.
    ztdynpro_attr-xoblg =  ztdynpro_attr-xoptn = ztdynpro_attr-xnodi = abap_false.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CREATE_TS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_ts INPUT.
  IF ztdynpro_attr-erdat < '19700101'.
*    AND ztdynpro_attr-ertim IS INITIAL
*    AND ztdynpro_attr-ernam IS INITIAL.
    ztdynpro_attr-erdat = sy-datum.
    ztdynpro_attr-ertim = sy-uzeit.
    ztdynpro_attr-ernam = sy-uname.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHANGE_TS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_ts INPUT.
  ztdynpro_attr-aedat = sy-datum.
  ztdynpro_attr-aetim = sy-uzeit.
  ztdynpro_attr-aenam = sy-uname.
ENDMODULE.
