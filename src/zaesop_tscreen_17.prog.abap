*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_17
*&---------------------------------------------------------------------*
*&  zcl_treport 作为报表，zcl_tscreen 作为选择屏幕+自建屏幕，通过1:1:N的搭配，示例其简单组合后的威力
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_17.

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

DATA: po_items TYPE STANDARD TABLE OF zsekpo,
      po_item  LIKE LINE OF po_items.

CONTROLS tc_9001_01 TYPE TABLEVIEW USING SCREEN '9001'.
*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-DATA view_cls_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_17' READ-ONLY.
    CLASS-METHODS push_view.

    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_17_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_17_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_17_v9900 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS pbo REDEFINITION.

ENDCLASS.

CLASS lcl_tc_po_items DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_table_control FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        tscreen TYPE REF TO zif_tscreen
      RAISING
        cx_uuid_error.

    METHODS get_data.

    METHODS user_command_extend REDEFINITION.

    METHODS pai_tc_line REDEFINITION.
    METHODS pbo_tc_line REDEFINITION.
    METHODS poh REDEFINITION.
    METHODS pov REDEFINITION.

    METHODS is_line_insert REDEFINITION.
    METHODS is_deletion_confirmed REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid dynnr_super = '9000' ).

    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        DATA(class_name) = lcl_prog=>view_cls_prefix.
      WHEN OTHERS.
        class_name = lcl_prog=>view_view_prefix && '_V' && sy-dynnr.
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
CLASS lcl_tscreen_17_v9000 IMPLEMENTATION.

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
            DATA(v9001) = CAST lcl_tscreen_17_v9001(  zcl_tscreen_stack=>get_instance( )->current( dynnr_super = '9000' dynnr = '9001' ) ).
            CAST lcl_tc_po_items( v9001->get_component( group = zif_tscreen_component=>c_component_tc id = 'TC_9001_01' ) )->get_data( ).
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
CLASS lcl_tscreen_17_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = '9000' ).
    add_components( ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD add_components.

    TRY.
        NEW lcl_tc_po_items( me ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_17_v9900 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = '9000' ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tc_po_items IMPLEMENTATION.

  METHOD constructor.
    super->constructor( parent             = CAST zcl_tscreen( tscreen )
                        tc_name            = 'TC_9001_01'
                        data_source        = 'PO_ITEMS'
                        data_wa            = 'PO_ITEM'
                        ref_structure_name = 'ZSEKPO' ).

    get_data( ).
  ENDMETHOD.

  METHOD get_data.
    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT * ##TOO_MANY_ITAB_FIELDS
      FROM ekpo
     INNER JOIN ekko
        ON ekko~ebeln = ekpo~ebeln
      LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
       AND makt~spras = sy-langu
      INTO CORRESPONDING FIELDS OF TABLE po_items
     WHERE ekpo~ebeln = p_ebeln
       AND ekko~bukrs IN s_bukrs
     ORDER BY ebelp.                                      "#EC CI_SUBRC

  ENDMETHOD.

  METHOD user_command_extend .

    CASE ucomm.
      WHEN 'PO_ITEM-MATNR'."双击事件

        screen_util->call_transaction( tcode = 'MM03' value = parent->cursor_filed_value ).

      WHEN 'ELIKZ'.
        MESSAGE '点击了删除标识' TYPE 'S'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD pai_tc_line .

    SELECT SINGLE maktx
      FROM makt
      INTO po_item-maktx
     WHERE matnr = po_item-matnr
       AND spras = sy-langu.                              "#EC CI_SUBRC

    super->pai_tc_line( ).

  ENDMETHOD.

  METHOD pbo_tc_line .

    super->pbo_tc_line( ).

    CHECK parent->display_mode = zcl_tscreen=>display_mode_modify."修改模式才生效
    IF po_item-ebelp MOD '20' <> 0.
      set_cell_editable( 'PO_ITEM-MENGE' ).
    ELSE.
      set_cell_display( 'PO_ITEM-MENGE' ).
    ENDIF.

  ENDMETHOD.

  METHOD poh .

    CASE parent->cursor_filed.

*&---------------------------------------------------------------------*
*&        F1_EBELN
*&---------------------------------------------------------------------*
      WHEN 'PO_ITEM-EBELN'.

        screen_util->call_transaction( tcode = 'ME23N' value = get_cell_value_by_cursor( ) )."跳转平台

    ENDCASE.


  ENDMETHOD.

  METHOD pov .

    CASE parent->cursor_filed.

*&---------------------------------------------------------------------*
*&        F4_MATNR
*&---------------------------------------------------------------------*
      WHEN 'PO_ITEM-MATNR'.

        SELECT mara~matnr, makt~maktx                   "#EC CI_NOORDER
          FROM mara
         INNER JOIN makt
            ON makt~matnr = mara~matnr
           AND makt~spras = @sy-langu
          INTO TABLE @DATA(lt_mara)
         UP TO '100' ROWS.                                "#EC CI_SUBRC

        po_item-matnr = parent->f4_event( key_field = 'MATNR' value_tab = lt_mara ).

    ENDCASE.

    super->pov( ).

  ENDMETHOD.

  METHOD is_line_insert .

    is_line_insert = super->is_line_insert( )."添加空行

    DATA(index) = lines( po_items ).

    "根据最后一行的行号递增
    po_items[ index ]-ebeln = ekko-ebeln.
    IF po_items[ index ]-ebelp IS INITIAL.
      IF index = 1.
        po_items[ index ]-ebelp = 10.
      ELSE.
        po_items[ index ]-ebelp = po_items[ index - 1 ]-ebelp + 10.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD is_deletion_confirmed .

    DATA answer TYPE c.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = '删除行:' && po_items[ index ]-ebelp
      IMPORTING
        answer         = answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc = 0 AND answer = '1'.
      is_confirmed = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
