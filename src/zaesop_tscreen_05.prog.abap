*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_05
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_05.

*&---------------------------------------------------------------------*
*&　　　　TABLES
*&---------------------------------------------------------------------*
TABLES ekko.
TABLES ekpo.

*&---------------------------------------------------------------------*
*&　　　　DATA define
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*&　　　　PARAMETERS
*&---------------------------------------------------------------------*
PARAMETERS: p_ebeln TYPE ekko-ebeln.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport.

  PUBLIC SECTION.

    CLASS-DATA view_cls_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_05' READ-ONLY.
    CLASS-METHODS push_view.
    CLASS-METHODS get_parent_screen
      IMPORTING
                dynnr         TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING VALUE(parent) LIKE sy-dynnr.

    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_05_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_05_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_05_v9002 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid dynnr_super = get_parent_screen( sy-dynnr ) dynnr = sy-dynnr )."仅子屏幕没复用的情况可以不加上父屏幕号进行判断,否则一定要加上父屏幕号

    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        DATA(class_name) = lcl_prog=>view_cls_prefix.
      WHEN OTHERS.
        class_name = lcl_prog=>view_view_prefix && '_V' && sy-dynnr.
    ENDCASE.

    CREATE OBJECT view TYPE (class_name).

  ENDMETHOD.

  METHOD get_parent_screen.

    CASE dynnr.
      WHEN '9001' OR '9002'.
        parent = '9000'.
      WHEN OTHERS.
        CLEAR parent.
    ENDCASE.

  ENDMETHOD.

  METHOD pbo.
  ENDMETHOD.

  METHOD check_authority.
    MESSAGE 'CHECK AUTHORITY' TYPE 'S'.
  ENDMETHOD.

  METHOD show.
    CALL SCREEN 9000.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_05_v9000 IMPLEMENTATION.

  METHOD pbo.
  ENDMETHOD.

  METHOD pai.
    CASE ok_code.
      WHEN 'DIS_MODE'.
        IF get_display_mode( ) = zcl_tscreen=>display_mode_modify.
          set_display_mode( zcl_tscreen=>display_mode_show ).
        ELSE.
          set_display_mode( zcl_tscreen=>display_mode_modify ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_05_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = p_ebeln.
  ENDMETHOD.

  METHOD pbo.
  ENDMETHOD.

  METHOD poh.
    CASE filedname.

*&---------------------------------------------------------------------*
*&        F1_EBELN
*&---------------------------------------------------------------------*
      WHEN 'EBELN'.
        screen_util->call_transaction( tcode = 'ME23N' value = cursor_filed_value ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_05_v9002 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *
      FROM ekpo
      INTO CORRESPONDING FIELDS OF ekpo
     WHERE ebeln = p_ebeln
       AND ebelp = '10'.
  ENDMETHOD.

  METHOD pbo.
  ENDMETHOD.

  METHOD poh.
    CASE filedname.

*&---------------------------------------------------------------------*
*&        F1_EBELN
*&---------------------------------------------------------------------*
      WHEN 'EBELN'.
        screen_util->call_transaction( tcode = 'ME23N' value = cursor_filed_value ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

INCLUDE zaesop_tscreen_event_inc."通用EVENT include
