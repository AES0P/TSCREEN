*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_14
*&---------------------------------------------------------------------*
*&  zcl_treport 作为报表，zcl_tscreen&zcl_tscreen_with_components 作为主屏幕和子屏幕，并在子屏幕的多页签中使用 TABLE CONTROL 控件
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_14.

*&---------------------------------------------------------------------*
*&　　　　TABLES
*&---------------------------------------------------------------------*
TABLES ekko.

*&---------------------------------------------------------------------*
*&　　　　DATA define
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

DATA: po_items        TYPE STANDARD TABLE OF zsekpo,
      po_items_filter TYPE STANDARD TABLE OF zsekpo ##NEEDED,
      po_item         LIKE LINE OF po_items.

DATA: po_plans TYPE STANDARD TABLE OF eket,
      po_plan  LIKE LINE OF po_plans.

DATA: po_histories TYPE STANDARD TABLE OF ekbe,
      po_history   LIKE LINE OF po_histories.

CONTROLS tc_9002_01 TYPE TABLEVIEW USING SCREEN '9002'.

CONTROLS tc_9003_01 TYPE TABLEVIEW USING SCREEN '9003'.

CONTROLS tc_9004_01 TYPE TABLEVIEW USING SCREEN '9004'.
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

    CLASS-DATA view_cls_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_14' READ-ONLY.

    CLASS-METHODS push_view.
    CLASS-METHODS get_parent_screen
      IMPORTING
                dynnr         TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING VALUE(parent) LIKE sy-dynnr.

    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_14_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_14_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS poh REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_14_v9002 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_14_v9003 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_14_v9004 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

CLASS lcl_tc_po_items DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_table_control FINAL.

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

CLASS lcl_tc_po_plans DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_table_control FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        tscreen TYPE REF TO zif_tscreen
      RAISING
        cx_uuid_error.

    METHODS get_data
      IMPORTING
        ebeln TYPE ekko-ebeln.

ENDCLASS.

CLASS lcl_tc_po_histories DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_table_control FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        tscreen TYPE REF TO zif_tscreen
      RAISING
        cx_uuid_error.

    METHODS get_data
      IMPORTING
        ebeln TYPE ekko-ebeln.

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
CLASS lcl_tscreen_14_v9000 IMPLEMENTATION.

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

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_14_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = p_ebeln.                               "#EC CI_SUBRC
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
CLASS lcl_tscreen_14_v9002 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    "此处为屏幕添加控件对象
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
CLASS lcl_tscreen_14_v9003 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    "此处为屏幕添加控件对象
    add_components( ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD add_components.

    TRY.
        NEW lcl_tc_po_plans( me ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_14_v9004 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    "此处为屏幕添加控件对象
    add_components( ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD add_components.

    TRY.
        NEW lcl_tc_po_histories( me ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

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
                        data_wa            = 'PO_ITEM'
                        hide_empty_fields  = abap_true
                        ref_structure_name = 'ZSEKPO' ).

    get_data( p_ebeln ).
  ENDMETHOD.

  METHOD get_data.
    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT * ##TOO_MANY_ITAB_FIELDS
      FROM ekpo
      LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
       AND makt~spras = sy-langu
      INTO CORRESPONDING FIELDS OF TABLE po_items
     WHERE ebeln = ebeln
     ORDER BY ebelp.                                      "#EC CI_SUBRC

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

    SELECT *  ##ITAB_KEY_IN_SELECT
      FROM @po_items AS its
     WHERE filter = @abap_true
      INTO CORRESPONDING FIELDS OF TABLE @po_items_filter.
    CHECK sy-subrc = 0.

    DELETE po_items WHERE filter = abap_true.            "#EC CI_STDSEQ
    SORT po_items ASCENDING BY ebeln ebelp.

  ENDMETHOD.

  METHOD after_filter_table_writeback .

    CHECK po_items_filter IS NOT INITIAL.

    DATA po_item_filter LIKE LINE OF po_items_filter.
    po_item_filter-filter = abap_false.
    MODIFY po_items_filter FROM po_item_filter TRANSPORTING filter WHERE filter = abap_true. "#EC CI_STDSEQ

    APPEND LINES OF po_items_filter TO po_items.
    SORT po_items ASCENDING BY ebeln ebelp.

    CLEAR po_items_filter.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tc_po_plans IMPLEMENTATION.

  METHOD constructor.
    super->constructor( parent             = CAST zcl_tscreen( tscreen )
                        tc_name            = 'TC_9003_01'
                        data_source        = 'PO_PLANS'
                        data_wa            = 'PO_PLAN'
                        hide_empty_fields  = abap_true
                        selbar_name        = 'MANDT'
                        ref_structure_name = 'EKET' ).

    get_data( p_ebeln ).
  ENDMETHOD.

  METHOD get_data.

    SELECT * "#EC CI_ALL_FIELDS_NEEDED
      FROM eket
      INTO CORRESPONDING FIELDS OF TABLE po_plans
     WHERE ebeln = ebeln
     ORDER BY ebelp etenr.                                "#EC CI_SUBRC

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tc_po_histories IMPLEMENTATION.

  METHOD constructor.
    super->constructor( parent             = CAST zcl_tscreen( tscreen )
                        tc_name            = 'TC_9004_01'
                        data_source        = 'PO_HISTORIES'
                        data_wa            = 'PO_HISTORY'
                        hide_empty_fields  = abap_true
                        selbar_name        = 'MANDT'
                        ref_structure_name = 'EKBE' ).

    get_data( p_ebeln ).
  ENDMETHOD.

  METHOD get_data.

    SELECT * "#EC CI_ALL_FIELDS_NEEDED
      FROM ekbe
      INTO CORRESPONDING FIELDS OF TABLE po_histories
     WHERE ebeln = ebeln
     ORDER BY ebelp zekkn vgabe gjahr belnr buzei.        "#EC CI_SUBRC

  ENDMETHOD.

ENDCLASS.

##INCL_OK
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
        prog        LIKE sy-repid VALUE 'ZAESOP_TSCREEN_14',
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
