*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_10
*&---------------------------------------------------------------------*
*&  原生 TABLE CONTROL 控件
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_10.

*&---------------------------------------------------------------------*
*&　　　　TABLES
*&---------------------------------------------------------------------*
TABLES ekko.

*&---------------------------------------------------------------------*
*&　　　　DATA define
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

DATA: po_items TYPE STANDARD TABLE OF zsekpo,
      po_item  LIKE LINE OF po_items.

CONTROLS tc_9000_01 TYPE TABLEVIEW USING SCREEN 9000.
DATA g_tc_9000_01_lines TYPE sy-loopc.

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

    CLASS-DATA view_prog(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_10' READ-ONLY.

    CLASS-METHODS push_view.
    CLASS-METHODS tc_command.

    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_10_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_data
      IMPORTING
        ebeln TYPE ekko-ebeln.
    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS pov REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( sy-repid )."仅子屏幕没复用的情况可以不加上父屏幕号进行判断,否则一定要加上父屏幕号

    "此处创建屏幕对象
    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        DATA(class_name) = lcl_prog=>view_prog.
      WHEN OTHERS.
        class_name = lcl_prog=>view_view_prefix && '_V' && sy-dynnr.
    ENDCASE.

    CREATE OBJECT view TYPE (class_name).

    "此处为屏幕添加控件对象
    CASE sy-dynnr.
      WHEN 9000.
        TRY.
            NEW zcl_table_control( parent             = CAST zcl_tscreen( view )
                                   tc_name            = 'TC_9000_01'
                                   data_source        = 'PO_ITEMS'
                                   screen_lines_field = 'G_TC_9000_01_LINES'
                                   hide_empty_fields  = abap_true
                                   ref_structure_name = 'ZSEKPO' ).
          CATCH cx_uuid_error.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD tc_command.

    "如果控件的相关功能需要传入参数，则需要仿造此方式，如果不需要则直接使用event处理方式
*    DATA tc_group TYPE REF TO cl_object_collection_iterator.
*    tc_group = CAST zcl_tscreen_with_components( zcl_tscreen_stack=>get_instance( )->top( ) )->get_components_iterator( group = 'TC' ).
*
*    WHILE tc_group->has_next( ).
*      DATA(tc) = CAST zcl_table_control( tc_group->get_next( ) ).
*      tc->user_command( ).
*  ENDWHILE.

  ENDMETHOD.

  METHOD show.
    CALL SCREEN 9000.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_10_v9000 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    get_data( p_ebeln ).
  ENDMETHOD.

  METHOD get_data.

    SELECT SINGLE *
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = ebeln.

    "仅示例，行数据查询不应该在此处处理
    SELECT *
      FROM ekpo
      LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
       AND makt~spras = sy-langu
      INTO CORRESPONDING FIELDS OF TABLE po_items
     WHERE ebeln = ebeln
     ORDER BY ebelp.

  ENDMETHOD.

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

  METHOD pov.

    CASE filedname.

*&---------------------------------------------------------------------*
*&        F1_EBELN
*&---------------------------------------------------------------------*
      WHEN 'EBELN'.

        SELECT ebeln,bukrs,bstyp,bsart,bsakz,loekz,statu,aedat,ernam
          FROM ekko
          INTO TABLE @DATA(lt_ekko)
         UP TO 500 ROWS
         ORDER BY ebeln.

        ekko-ebeln = f4_event( key_field = 'EBELN' value_tab = lt_ekko ).
        CHECK ekko-ebeln IS NOT INITIAL.

        get_data( ekko-ebeln ).

    ENDCASE.
  ENDMETHOD.


ENDCLASS.

INCLUDE zaesop_tscreen_event_inc."通用EVENT include

*&---------------------------------------------------------------------*
*& Module TC_9000_01_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE tc_9000_01_change_tc_attr OUTPUT.
  DESCRIBE TABLE po_items LINES tc_9000_01-lines.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module TC_9000_01_GET_LINES OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE tc_9000_01_get_lines OUTPUT.
  g_tc_9000_01_lines = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_9000_01_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_9000_01_modify INPUT.

  MODIFY po_items
    FROM po_item
   INDEX tc_9000_01-current_line.

ENDMODULE.
