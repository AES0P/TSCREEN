*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_08
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_08.

*&---------------------------------------------------------------------*
*&　　　　TABLES
*&---------------------------------------------------------------------*
TABLES ekko.
TABLES ekpo.
TABLES eket.
TABLES ekbe.

*&---------------------------------------------------------------------*
*&　　　　DATA define
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*&　　　　PARAMETERS
*&---------------------------------------------------------------------*
CONSTANTS c_ebeln TYPE ekko-ebeln VALUE '4500000001'.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport.

  PUBLIC SECTION.

    CLASS-DATA view_cls_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_08' READ-ONLY.
    CLASS-METHODS push_view.
    CLASS-METHODS get_parent_screen
      IMPORTING
                dynnr         TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING VALUE(parent) LIKE sy-dynnr.

    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_08_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_08_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_08_v9002 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_08_v9003 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS pbo REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_08_v9004 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS pbo REDEFINITION.

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
      WHEN '9001' OR '9002' OR '9003' OR '9004'.
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
CLASS lcl_tscreen_08_v9000 IMPLEMENTATION.

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
CLASS lcl_tscreen_08_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = c_ebeln.
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
CLASS lcl_tscreen_08_v9002 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *
      FROM ekpo
      INTO CORRESPONDING FIELDS OF ekpo
     WHERE ebeln = c_ebeln
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

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_08_v9003 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *
      FROM eket
      INTO CORRESPONDING FIELDS OF eket
     WHERE ebeln = c_ebeln
       AND ebelp = '10'
       AND etenr = '1'.
  ENDMETHOD.

  METHOD pbo.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_08_v9004 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *
      FROM ekbe
      INTO CORRESPONDING FIELDS OF ekbe
     WHERE ebeln = c_ebeln
       AND ebelp = '10'.
  ENDMETHOD.

  METHOD pbo.
  ENDMETHOD.

ENDCLASS.

INCLUDE zaesop_tscreen_event_inc."通用EVENT include

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'ITEM_TABS'
CONSTANTS: BEGIN OF c_item_tabs,
             tab1 LIKE sy-ucomm VALUE 'ITEM_TABS_F01', "%%%约定，tab页签必须包含 _TABS_ 字符
             tab2 LIKE sy-ucomm VALUE 'ITEM_TABS_F02', "%%%约定，tab页签必须包含 _TABS_ 字符
             tab3 LIKE sy-ucomm VALUE 'ITEM_TABS_F03', "%%%约定，tab页签必须包含 _TABS_ 字符
           END OF c_item_tabs.
*&SPWIZARD: DATA FOR TABSTRIP 'ITEM_TABS'
CONTROLS:  item_tabs TYPE TABSTRIP.
DATA: BEGIN OF g_item_tabs,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE sy-repid,
        pressed_tab LIKE sy-ucomm VALUE c_item_tabs-tab1,
      END OF g_item_tabs.

*&SPWIZARD: OUTPUT MODULE FOR TS 'ITEM_TABS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE item_tabs_active_tab_set OUTPUT.
  item_tabs-activetab = g_item_tabs-pressed_tab.
  CASE g_item_tabs-pressed_tab.
    WHEN c_item_tabs-tab1.
      g_item_tabs-subscreen = '9002'.
    WHEN c_item_tabs-tab2.
      g_item_tabs-subscreen = '9003'.
    WHEN c_item_tabs-tab3.
      g_item_tabs-subscreen = '9004'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'ITEM_TABS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE item_tabs_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_item_tabs-tab1.
      g_item_tabs-pressed_tab = c_item_tabs-tab1.
    WHEN c_item_tabs-tab2.
      g_item_tabs-pressed_tab = c_item_tabs-tab2.
    WHEN c_item_tabs-tab3.
      g_item_tabs-pressed_tab = c_item_tabs-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
