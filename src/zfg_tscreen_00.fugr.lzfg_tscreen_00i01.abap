*----------------------------------------------------------------------*
***INCLUDE LZFG_TSCREEN_00I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_REPTI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_repti INPUT.

  IF ztscreen_manager-repti IS INITIAL AND ztscreen_manager-cprog IS NOT INITIAL.
    SELECT SINGLE text
      FROM trdirt
      INTO ztscreen_manager-repti
     WHERE name  = ztscreen_manager-cprog
       AND sprsl = sy-langu.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.

  DATA current_line TYPE i.
  GET CURSOR LINE current_line.

  ASSIGN extract[ current_line ] TO FIELD-SYMBOL(<current_line>).
  CHECK sy-subrc = 0.

  CASE sy-ucomm.
    WHEN 'TC_0001_01_LT1'.

      TRY.
          CALL FUNCTION 'ZFM_GBC_TEXT_EDITOR'
            EXPORTING
              io_editor   = zcl_text_editor_tool=>get_instance( )->set_status_text( title = '用户黑名单（逗号间隔）' text = '用户黑名单（逗号间隔）'
                                                                )->set_statusbar_mode( 1
                                                                )->set_toolbar_mode( 1 )
              iv_readonly = ''
            CHANGING
              text        = <current_line>+188(1000).
          <current_line>+2221(1) = abap_false.
        CATCH cx_uuid_error INTO DATA(lx_uuid_error).
          MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        CATCH zcx_tscreen INTO DATA(lx_tscreen).
          MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    WHEN 'TC_0001_01_LT2'.
      TRY.
          CALL FUNCTION 'ZFM_GBC_TEXT_EDITOR'
            EXPORTING
              io_editor   = zcl_text_editor_tool=>get_instance( )->set_status_text( title = 'IP黑名单（逗号间隔）' text = 'IP黑名单（逗号间隔）'
                                                                )->set_statusbar_mode( 1
                                                                )->set_toolbar_mode( 1 )
              iv_readonly = ''
            CHANGING
              text        = <current_line>+1188(1000).
          <current_line>+2221(1) = abap_false.
        CATCH cx_uuid_error INTO lx_uuid_error.
          MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        CATCH zcx_tscreen INTO lx_tscreen.
          MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    WHEN 'TC_0001_01_LT3'.
      TRY.
          CALL FUNCTION 'ZFM_GBC_TEXT_EDITOR'
            EXPORTING
              io_editor   = zcl_text_editor_tool=>get_instance( )->set_status_text( title = '停用时放行列表' text = '停用时放行列表（例如：IP:XXX.XXX.XXX.XXX,USER:XXX'
                                                                )->set_statusbar_mode( 1
                                                                )->set_toolbar_mode( 1 )
              iv_readonly = ''
            CHANGING
              text        = <current_line>+2222(1333).
          <current_line>+3555(1) = abap_false.
        CATCH cx_uuid_error INTO lx_uuid_error.
          MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        CATCH zcx_tscreen INTO lx_tscreen.
          MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    WHEN OTHERS.
  ENDCASE.
  vim_modify_screen = abap_true.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_date INPUT.

  IF ( ztscreen_manager-zbdate IS NOT INITIAL AND ztscreen_manager-zsdate IS INITIAL )
    OR ( ztscreen_manager-zbdate IS INITIAL AND ztscreen_manager-zsdate IS NOT INITIAL ).
    MESSAGE '停用日期必须完整填写'(e01) TYPE 'E'.
  ELSEIF ztscreen_manager-zbdate > ztscreen_manager-zsdate.
    MESSAGE '上限比下限大'(e02) TYPE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_TIME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_time INPUT.
  IF ztscreen_manager-zbdate = ztscreen_manager-zsdate
    AND ztscreen_manager-zbtime > ztscreen_manager-zstime.
    MESSAGE '上限比下限大'(e02) TYPE 'E'.
  ENDIF.
ENDMODULE.
