*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_10
*&---------------------------------------------------------------------*
*&  zcl_treport 作为报表，zcl_tscreen_with_components 作为自建屏幕，示例怎么使用原生 TABLE CONTROL 控件
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

CONTROLS tc_9000_01 TYPE TABLEVIEW USING SCREEN '9000'.

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
    CLASS-DATA view_prefix(24) VALUE 'LCL_TSCREEN_10' READ-ONLY.

    CLASS-METHODS push_view.

    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_10_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_data
      IMPORTING
        ebeln TYPE ekko-ebeln.
    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS pov REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT is_screen_exists( sy-repid )."仅子屏幕没复用的情况可以不加上父屏幕号进行判断,否则一定要加上父屏幕号

    "此处创建屏幕对象
    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        DATA(class_name) = lcl_prog=>view_prog_prefix.
      WHEN OTHERS.
        class_name = lcl_prog=>view_prefix && '_V' && sy-dynnr.
    ENDCASE.

    CREATE OBJECT view TYPE (class_name).

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
    "此处为屏幕添加控件对象
    add_components( ).
  ENDMETHOD.

  METHOD get_data.
    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = ebeln.                                 "#EC CI_SUBRC

    "仅示例，行数据查询不应该在此处处理
    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT *
      FROM ekpo
      LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
       AND makt~spras = sy-langu
      INTO CORRESPONDING FIELDS OF TABLE po_items  ##TOO_MANY_ITAB_FIELDS
     WHERE ebeln = ebeln
     ORDER BY ebelp.                                      "#EC CI_SUBRC

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

  METHOD pov.

    CASE filedname.

*&---------------------------------------------------------------------*
*&        F1_EBELN
*&---------------------------------------------------------------------*
      WHEN 'EBELN'.


        SELECT ebeln,bukrs,bstyp,bsart,bsakz,loekz,statu,aedat,ernam
          FROM ekko
          INTO TABLE @DATA(lt_ekko)
         UP TO '500' ROWS
         ORDER BY ebeln.                   "#EC CI_SUBRC #EC CI_NOWHERE

        ekko-ebeln = f4_event( key_field = 'EBELN' value_tab = lt_ekko ).
        CHECK ekko-ebeln IS NOT INITIAL.

        get_data( ekko-ebeln ).

    ENDCASE.
  ENDMETHOD.

  METHOD add_components.

    TRY.

        zcl_tcomponent_factory=>get_tcomponent(
                                  parent = me
                                  params = /ui2/cl_json=>serialize( VALUE zcl_tcomponent_factory=>ty_tc(
                                    group              = zif_tscreen_component=>c_component_tc
                                    id                 = 'TC_9000_01'
                                    data_source        = 'PO_ITEMS'
                                    data_wa            = 'PO_ITEM'
*                                    hide_empty_fields  = abap_true
                                    ref_structure_name = 'ZSEKPO'
                                    ddic_tabname       = 'EKPO' ) ) ).

        "  重写了控件就需要按下列方式直接new，参考后边demo
*          NEW zcl_table_control( parent             = me
*                                 tc_name            = 'TC_9000_01'
*                                 data_source        = 'PO_ITEMS'
*                                 data_wa            = 'PO_ITEM'
**                               hide_empty_fields  = abap_true
*                                 ref_structure_name = 'ZSEKPO'
*                                 ddic_tabname       = 'EKPO' ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      CATCH zcx_tscreen INTO DATA(lx_tscreen).
        MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
