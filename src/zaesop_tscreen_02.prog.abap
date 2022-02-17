*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_02
*&---------------------------------------------------------------------*
*&  将报表程序执行事件等效为选择屏幕的事件流，通过处理每一个事件流方法，响应每一个报表事件
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_02.

*&---------------------------------------------------------------------*
*&　　　　PARAMETERS
*&---------------------------------------------------------------------*
PARAMETERS: p_ebeln TYPE ekko-ebeln ##NEEDED ,
            ##NEEDED p_bukrs TYPE ekko-bukrs DEFAULT '0000',
            ##NEEDED p_bstyp TYPE ekko-bstyp,
            ##NEEDED p_bsart TYPE ekko-bsart,
            ##NEEDED p_loekz TYPE ekko-loekz.

SELECTION-SCREEN SKIP 1.
*&---------------------------------------------------------------------*
*&　　　　SELECTION-SCREEN
*&---------------------------------------------------------------------*
TABLES ekko.
##NEEDED SELECT-OPTIONS: s_ernam FOR ekko-ernam.
##NEEDED SELECT-OPTIONS: s_aedat FOR ekko-aedat.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-METHODS push_view.

    METHODS constructor.

    METHODS initialize REDEFINITION.
    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS poh REDEFINITION.
    METHODS pov REDEFINITION.
    METHODS execute REDEFINITION.
    METHODS show REDEFINITION.
    METHODS exit REDEFINITION.
    METHODS check_authority REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT is_screen_exists( program = sy-repid ).

    DATA view TYPE REF TO zif_tscreen.
    CREATE OBJECT view TYPE lcl_prog.

  ENDMETHOD.

  METHOD constructor.
    zkey = 'test zkey'.
    super->constructor( ).
  ENDMETHOD.

  ##NEEDED
  METHOD initialize.
*    BREAK-POINT."INITIALIZATION事件里可通过断点验证事件是否触发
  ENDMETHOD.

  METHOD pbo.
    MESSAGE 'AT SELECTION-SCREEN OUTPUT' TYPE 'S'."PBO事件里消息不可使用弹窗形式
  ENDMETHOD.

  METHOD pai.
    MESSAGE 'AT SELECTION-SCREEN :' && cursor_filed && cursor_filed_value TYPE 'I'.
  ENDMETHOD.

  METHOD poh.
    MESSAGE 'AT SELECTION-SCREEN ON HELP-REQUEST :' && cursor_filed_value TYPE 'I'.
  ENDMETHOD.

  METHOD pov.
    MESSAGE 'AT SELECTION-SCREEN ON VALUE-REQUEST :' && cursor_filed_value TYPE 'I'.
  ENDMETHOD.

  METHOD execute.
    MESSAGE 'START-OF-SELECTION' TYPE 'I'.
  ENDMETHOD.

  METHOD show.
    MESSAGE 'END-OF-SELECTION' TYPE 'I'.
    TRY.
        zcx_tscreen=>raise_text( 'error' )."框架抛异常通用形式
      CATCH zcx_tscreen ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD check_authority.
    MESSAGE 'CHECK AUTHORITY' TYPE 'I'.
  ENDMETHOD.

  METHOD exit.
    MESSAGE 'EXIT' TYPE 'I'.
    super->exit( )."重写EXIT时，必须调用SUPER->EXIT，否则可能引发出栈异常
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&  POH
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR p_ebeln.
  lcl_prog=>event( 'POH' ).

*&---------------------------------------------------------------------*
*&  POV
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.
  lcl_prog=>event( 'POV' ).

  ##INCL_OK
  INCLUDE zaesop_tscreen_event_inc."通用EVENT include
