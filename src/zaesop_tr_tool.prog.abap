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
