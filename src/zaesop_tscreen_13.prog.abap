*&---------------------------------------------------------------------*
*& REPORT ZAESOP_TSCREEN_13
*&---------------------------------------------------------------------*
*&  子屏幕带table control
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_13.

*&---------------------------------------------------------------------*
*&　　　　TABLES
*&---------------------------------------------------------------------*
TABLES ekko.

*&---------------------------------------------------------------------*
*&　　　　DATA DEFINE
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

DATA: po_items        TYPE STANDARD TABLE OF zsekpo,
      po_items_filter TYPE STANDARD TABLE OF zsekpo,
      po_item         LIKE LINE OF po_items.

CONTROLS tc_9002_01 TYPE TABLEVIEW USING SCREEN 9002.
DATA g_tc_9002_01_lines TYPE sy-loopc.

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
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_13' READ-ONLY.

    CLASS-METHODS push_view.
    CLASS-METHODS get_parent_screen
      IMPORTING
                dynnr         TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING VALUE(parent) LIKE sy-dynnr.

    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_13_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.
  PUBLIC SECTION.
    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
ENDCLASS.

CLASS lcl_tscreen_13_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_data
      IMPORTING
        ebeln TYPE ekko-ebeln.
    METHODS pbo REDEFINITION.
    METHODS pov REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_13_v9002 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.

ENDCLASS.

CLASS lcl_tc_po_items DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_table_control.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        tscreen TYPE REF TO zif_tscreen
      RAISING
        cx_uuid_error.

    METHODS get_data
      IMPORTING
        ebeln TYPE ekko-ebeln.

    METHODS is_line_insert REDEFINITION.

    METHODS before_filter_table_writeback REDEFINITION.
    METHODS after_filter_table_writeback REDEFINITION.

ENDCLASS.
*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD get_parent_screen.

    CASE dynnr.
      WHEN '9001' OR '9002'.
        parent = '9000'.
      WHEN OTHERS.
        CLEAR parent.
    ENDCASE.

  ENDMETHOD.

  METHOD push_view.

    CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid dynnr_super = get_parent_screen( sy-dynnr ) dynnr = sy-dynnr )."仅子屏幕没复用的情况可以不加上父屏幕号进行判断,否则一定要加上父屏幕号

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
      WHEN 9002.
        TRY.
            NEW lcl_tc_po_items( view ).
          CATCH cx_uuid_error.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD show.
    CALL SCREEN 9000.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_13_v9000 IMPLEMENTATION.

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

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_13_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    get_data( p_ebeln ).
  ENDMETHOD.

  METHOD get_data.

    SELECT SINGLE *
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = ebeln.

  ENDMETHOD.

  METHOD pbo.
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

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_13_v9002 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
  ENDMETHOD.

  METHOD pbo.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tc_po_items IMPLEMENTATION.

  METHOD constructor.
    super->constructor( parent             = CAST zcl_tscreen( tscreen )
                        tc_name            = 'TC_9002_01'
                        data_source        = 'PO_ITEMS'
                        screen_lines_field = 'G_TC_9002_01_LINES'
                        hide_empty_fields  = abap_true
                        ref_structure_name = 'ZSEKPO' ).

    get_data( p_ebeln ).
  ENDMETHOD.

  METHOD get_data.

    SELECT *
      FROM ekpo
      LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
       AND makt~spras = sy-langu
      INTO CORRESPONDING FIELDS OF TABLE po_items
     WHERE ebeln = ebeln
     ORDER BY ebelp.

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

  METHOD before_filter_table_writeback .

    SELECT *
       FROM @po_items AS its
      WHERE filter = @abap_true
       INTO CORRESPONDING FIELDS OF TABLE @po_items_filter.
    CHECK sy-subrc = 0.

    DELETE po_items WHERE filter = abap_true.
    SORT po_items ASCENDING BY ebeln ebelp.

  ENDMETHOD.

  METHOD after_filter_table_writeback .

    CHECK po_items_filter IS NOT INITIAL.

    DATA po_item_filter LIKE LINE OF po_items_filter.
    po_item_filter-filter = abap_false.
    MODIFY po_items_filter FROM po_item_filter TRANSPORTING filter WHERE filter = abap_true.

    APPEND LINES OF po_items_filter TO po_items.
    SORT po_items ASCENDING BY ebeln ebelp.

    CLEAR po_items_filter.

  ENDMETHOD.

ENDCLASS.

INCLUDE zaesop_tscreen_event_inc."通用EVENT INCLUDE

*&---------------------------------------------------------------------*
*& MODULE TC_9002_01_GET_LINES OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE tc_9002_01_get_lines OUTPUT.
  g_tc_9002_01_lines = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  TC_9002_01_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE tc_9002_01_modify INPUT.

  MODIFY po_items
    FROM po_item
   INDEX tc_9002_01-current_line.

ENDMODULE.
