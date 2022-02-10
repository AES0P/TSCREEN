*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_04
*&---------------------------------------------------------------------*
*&  zcl_treport 作为报表，zcl_tscreen 作为自建屏幕，通过1:n的搭配，示例其简单组合后的威力
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_04.

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

    CLASS-DATA view_prog_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_prefix(24) VALUE 'LCL_TSCREEN_04' READ-ONLY.
    CLASS-METHODS push_view.

    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_04_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_04_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT is_screen_exists( program = sy-repid ).

    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        DATA(class_name) = lcl_prog=>view_prog_prefix.
      WHEN OTHERS.
        class_name = lcl_prog=>view_prefix && '_V' && sy-dynnr.
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
    CALL SCREEN 9000.
  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_04_v9000 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = p_ebeln.                               "#EC CI_SUBRC
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
        CALL SCREEN '9001'.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_04_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekpo
      INTO CORRESPONDING FIELDS OF ekpo
     WHERE ebeln = p_ebeln
       AND ebelp = '10'.                                  "#EC CI_SUBRC
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

ENDCLASS.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
