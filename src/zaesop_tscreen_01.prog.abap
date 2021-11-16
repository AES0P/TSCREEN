*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_01.

*&---------------------------------------------------------------------*
*&　　　　PARAMETERS
*&---------------------------------------------------------------------*
PARAMETERS: p_ebeln TYPE ekko-ebeln,
            p_bukrs TYPE ekko-bukrs DEFAULT '0000',
            p_bstyp TYPE ekko-bstyp,
            p_bsart TYPE ekko-bsart,
            p_loekz TYPE ekko-loekz.

SELECTION-SCREEN SKIP 1.
*&---------------------------------------------------------------------*
*&　　　　SELECTION-SCREEN
*&---------------------------------------------------------------------*
TABLES ekko.
SELECT-OPTIONS: s_ernam FOR ekko-ernam.
SELECT-OPTIONS: s_aedat FOR ekko-aedat.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport.

  PUBLIC SECTION.

    CLASS-METHODS push_view.

    METHODS initialize REDEFINITION.
    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS poh REDEFINITION.
    METHODS pov REDEFINITION.
    METHODS execute REDEFINITION.
    METHODS show REDEFINITION.
    METHODS check_authority REDEFINITION.

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

  METHOD initialize.
*    BREAK-POINT."INITIALIZATION事件里不可显示任何消息,可通过断点验证事件是否触发
  ENDMETHOD.

  METHOD pbo.
    MESSAGE 'AT SELECTION-SCREEN OUTPUT' TYPE 'S'."PBO事件里消息不可使用弹窗形式
  ENDMETHOD.

  METHOD pai.
    IF get_display_mode( ) = zcl_tscreen=>display_mode_modify.
      set_display_mode( zcl_tscreen=>display_mode_show ).
    ELSE.
      set_display_mode( zcl_tscreen=>display_mode_modify ).
    ENDIF.
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
  ENDMETHOD.

  METHOD check_authority.
    MESSAGE 'CHECK AUTHORITY' TYPE 'I'.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&  POH
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR p_ebeln.
  TRY.
      zcl_tscreen_stack=>get_instance( )->top( )->handle_event( 'POH' ).
    CATCH zcx_tscreen INTO DATA(lx_tscreen).
      MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&  POV
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.
  TRY.
      zcl_tscreen_stack=>get_instance( )->top( )->handle_event( 'POV' ).
    CATCH zcx_tscreen INTO DATA(lx_tscreen).
      MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
  ENDTRY.

INCLUDE zaesop_tscreen_event_inc."通用EVENT include
