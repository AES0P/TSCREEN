*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_09
*&---------------------------------------------------------------------*
*&  zcl_treport 作为报表，zcl_tscreen 作为选择屏幕+自建屏幕，通过1:1:N的搭配，示例其简单组合后的威力
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_09.

*&---------------------------------------------------------------------*
*&　　　　TABLES
*&---------------------------------------------------------------------*
TABLES ekko.

*&---------------------------------------------------------------------*
*&　　　　SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 9900 AS SUBSCREEN.
  PARAMETERS p_ebeln TYPE ekko-ebeln.
  SELECT-OPTIONS s_bukrs FOR ekko-bukrs.
SELECTION-SCREEN END OF SCREEN 9900.

*&---------------------------------------------------------------------*
*&　　　　DATA define
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-DATA view_prog_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_prefix(24) VALUE 'LCL_TSCREEN_09' READ-ONLY.
    CLASS-METHODS push_view.

    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_09_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_09_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS get_data .

ENDCLASS.

CLASS lcl_tscreen_09_v9900 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS pbo REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        CHECK NOT is_screen_exists( program = sy-repid ).
        DATA(class_name) = lcl_prog=>view_prog_prefix.
      WHEN OTHERS.
        CHECK NOT is_screen_exists( program = sy-repid dynnr_super = '9000' ).
        class_name = lcl_prog=>view_prefix && '_V' && sy-dynnr.
    ENDCASE.

    CREATE OBJECT view TYPE (class_name).

  ENDMETHOD.

  METHOD check_authority.
    MESSAGE 'CHECK_AUTHORITY' TYPE 'I'.
*    MESSAGE 'CHECK_AUTHORITY' TYPE 'S' DISPLAY LIKE 'E'.
*    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD show.
    check_authority( )."没有选择屏幕时，用户不能主动触发PAI，但在show里边校验权限即可
    CALL SCREEN 9000.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_09_v9000 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    display_mode = zcl_tscreen=>display_mode_modify.
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'EXECUTE'.
        TRY.
            CAST lcl_tscreen_09_v9001( lcl_prog=>get_screen( dynnr_super = '9000' dynnr = '9001' ) )->get_data( ).
          CATCH zcx_tscreen INTO DATA(lx_tscreen) ##NEEDED.
            MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
        ENDTRY.
      WHEN 'DIS_MODE'.
        IF get_display_mode( ) = zcl_tscreen=>display_mode_modify.
          set_display_mode( zcl_tscreen=>display_mode_show ).
        ELSE.
          set_display_mode( zcl_tscreen=>display_mode_modify ).
        ENDIF.
    ENDCASE.
    CLEAR sy-ucomm.
  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_09_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = '9000' ).
    get_data( ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD get_data.
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln  = p_ebeln
       AND bukrs IN s_bukrs.                              "#EC CI_SUBRC
  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_09_v9900 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = '9000' ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

ENDCLASS.
##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
