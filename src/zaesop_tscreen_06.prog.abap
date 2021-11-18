*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_06
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_06.

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
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-DATA parent_dynnr TYPE sy-dynnr VALUE '9000'.
    CLASS-DATA view_cls_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_06' READ-ONLY.
    CLASS-METHODS push_view.

    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_06_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_06_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_06_v9002 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_06_v9003 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.
*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid dynnr_super = '' dynnr = sy-dynnr )."仅子屏幕没复用的情况可以不加上父屏幕号进行判断,否则一定要加上父屏幕号
        DATA(class_name) = lcl_prog=>view_cls_prefix.
      WHEN OTHERS.
        CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid dynnr_super = lcl_prog=>parent_dynnr dynnr = sy-dynnr )."仅子屏幕没复用的情况可以不加上父屏幕号进行判断,否则一定要加上父屏幕号
        class_name = lcl_prog=>view_view_prefix && '_V' && sy-dynnr.
    ENDCASE.

    CREATE OBJECT view TYPE (class_name).

  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD check_authority.
    MESSAGE 'CHECK AUTHORITY' TYPE 'S'.
  ENDMETHOD.

  METHOD show.
    lcl_prog=>parent_dynnr = '9000'.
    CALL SCREEN 9000.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_06_v9000 IMPLEMENTATION.

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
      WHEN 'CALL_9003'.
        lcl_prog=>parent_dynnr = '9003'.
        CALL SCREEN '9003'.
    ENDCASE.
    CLEAR sy-ucomm.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_06_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>parent_dynnr ).
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = p_ebeln.                               "#EC CI_SUBRC
    IF lcl_prog=>parent_dynnr = '9003'.
      ekko-ebeln += 1.
    ENDIF.
  ENDMETHOD.

  ##NEEDED
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
CLASS lcl_tscreen_06_v9002 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>parent_dynnr ).
    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekpo
      INTO CORRESPONDING FIELDS OF ekpo
     WHERE ebeln = p_ebeln
       AND ebelp = '10'.                                  "#EC CI_SUBRC
    IF lcl_prog=>parent_dynnr = '9003'.
      ekpo-ebeln += 1.
    ENDIF.
  ENDMETHOD.

  ##NEEDED
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

CLASS lcl_tscreen_06_v9003 IMPLEMENTATION.

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
      WHEN 'CALL_9000'.
        lcl_prog=>parent_dynnr = '9000'.
        TRY.
            exit( ).
          CATCH zcx_tscreen INTO DATA(gx_tscreen).
            MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
        ENDTRY.
        LEAVE TO SCREEN 0.
    ENDCASE.
    CLEAR sy-ucomm.
  ENDMETHOD.

ENDCLASS.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
