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
