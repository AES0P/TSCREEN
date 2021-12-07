*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_02
*&---------------------------------------------------------------------*
*&  将自建屏幕的各事件流抽象，并按照模板方法执行
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_02.

*&---------------------------------------------------------------------*
*&　　　　TABLES
*&---------------------------------------------------------------------*
TABLES ekko.

*&---------------------------------------------------------------------*
*&　　　　DATA define
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    CLASS-METHODS push_view.

    METHODS constructor.
    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS poh REDEFINITION.
    METHODS pov REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid ).

    DATA view TYPE REF TO zif_tscreen.
    CREATE OBJECT view TYPE lcl_prog.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( refresh_interval = '5' ).
    ##WARN_OK
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko.                  "#EC CI_SUBRC
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'DIS_MODE'.
        IF get_display_mode( ) = zcl_tscreen=>display_mode_modify.
          set_display_mode( zcl_tscreen=>display_mode_show ).
        ELSE.
          set_display_mode( zcl_tscreen=>display_mode_modify ).
        ENDIF.
    ENDCASE.
    CLEAR sy-ucomm.
  ENDMETHOD.

  METHOD poh.
    CASE cursor_filed.

*&---------------------------------------------------------------------*
*&        F1_EBELN
*&---------------------------------------------------------------------*
      WHEN 'EKKO-EBELN'.

        screen_util->call_transaction( tcode = 'ME23N' value = cursor_filed_value )."跳转平台

    ENDCASE.
  ENDMETHOD.

  METHOD pov.
    CASE cursor_filed.

*&---------------------------------------------------------------------*
*&        F4_BUKRS
*&---------------------------------------------------------------------*
      WHEN 'EKKO-BUKRS'.

        MESSAGE cursor_filed && ':' && cursor_filed_value TYPE 'I'."显示当前字段和值

*&---------------------------------------------------------------------*
*&        F4_WAERS
*&---------------------------------------------------------------------*
      WHEN 'EKKO-WAERS'.

        MESSAGE filedname TYPE 'I'."显示无前缀的字段名

*&---------------------------------------------------------------------*
*&        F4_ERNAM
*&---------------------------------------------------------------------*
      WHEN 'EKKO-ERNAM'.

        SELECT usr21~bname,adrp~name_text,adrp~persnumber
          FROM adrp
         INNER JOIN usr21
            ON adrp~persnumber = usr21~persnumber
          INTO TABLE @DATA(lt_users)
          ##NUMBER_OK
         UP TO 500 ROWS
         ORDER BY bname.                                  "#EC CI_SUBRC

        "获取选中值
        ekko-ernam = f4_event( key_field = 'BNAME' value_tab = lt_users ).

        "带出
        TRY.
            bring_out( EXPORTING source = 'NAME_TEXT'  CHANGING target = ekko-desp ).
            bring_out( EXPORTING source = 'PERSNUMBER' CHANGING target = ekko-unsez ).
          CATCH zcx_tscreen INTO DATA(lcx_tscreen).
            MESSAGE lcx_tscreen->get_text( ) TYPE 'A'.
        ENDTRY.

        "联动
        ##USER_OK
        IF ekko-ernam = sy-uname.
          ekko-zterm = '1'.
        ELSE.
          ekko-zterm = '0'.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　END-OF-SELECTION
*&---------------------------------------------------------------------*
##DUPL_EVENT
END-OF-SELECTION."重复事件以先后顺序执行
  CALL SCREEN 9000.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
