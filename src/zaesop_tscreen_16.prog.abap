*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_16
*&---------------------------------------------------------------------*
*&  zcl_treport 作为报表，zcl_tscreen_with_components 作为主屏幕和子屏幕，并使用 TABLE CONTROL 和 EDITOR 控件
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_16.

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
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_16' READ-ONLY.

    CLASS-DATA sub_screen TYPE sy-dynnr VALUE '9002'.
    CLASS-METHODS push_view.
    CLASS-METHODS get_parent_screen
      IMPORTING
                dynnr         TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING VALUE(parent) LIKE sy-dynnr.

    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_16_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

  PROTECTED SECTION.
    METHODS on_countdown_finished REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_16_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS poh REDEFINITION.
    METHODS exit REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_16_v9002 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_16_v9003 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_16_v9004 DEFINITION CREATE PUBLIC
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

    METHODS user_command_extend REDEFINITION.

    METHODS pai_tc_line REDEFINITION.
    METHODS pov REDEFINITION.

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
CLASS lcl_tscreen_16_v9000 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( refresh_interval = '5' ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'DIS_MODE'.
        IF get_display_mode( ) = zcl_tscreen=>display_mode_modify.
          set_display_mode( zcl_tscreen=>display_mode_show ).
          countdown_begin( 5 ).
        ELSE.
          set_display_mode( zcl_tscreen=>display_mode_modify ).
          countdown_stop( ).
        ENDIF.
    ENDCASE.
    CLEAR sy-ucomm.
  ENDMETHOD.

  METHOD on_countdown_finished.
    screen_util->execute_fcode( '/00' ).
    MESSAGE 'REFRESH SCREEN...' TYPE 'S'.
    timer->run( ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_16_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = lcl_prog=>get_parent_screen( sy-dynnr ) ).
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = p_ebeln.                               "#EC CI_SUBRC
    "此处为屏幕添加控件对象
    add_components( ).
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'SAVE'.
        CAST zif_text_editor( get_component( group = zif_tscreen_component=>c_component_editor id = 'LASTCHANGEDT' ) )->save_text( ).
      WHEN 'TC_9002_01_SHOW'.
        IF lcl_prog=>sub_screen = '9002'.
          lcl_prog=>sub_screen  = '9005'.
          CAST zcl_text_editor( get_component( group = zif_tscreen_component=>c_component_editor id = 'LASTCHANGEDT' ) )->change_visibility( zif_tscreen_component=>c_component_invisible ).
        ELSE.
          lcl_prog=>sub_screen  = '9002'.
          CAST zcl_text_editor( get_component( group = zif_tscreen_component=>c_component_editor id = 'LASTCHANGEDT' ) )->change_visibility( zif_tscreen_component=>c_component_visible ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD exit.

    "回写数据
    ekko-lastchangedatetime = CAST zif_text_editor( get_component( group = zif_tscreen_component=>c_component_editor id = 'LASTCHANGEDT' ) )->get_text( ).
    MESSAGE CONV string( ekko-lastchangedatetime ) TYPE 'I'.

    super->exit( ).

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

  METHOD add_components.

    TRY.
        NEW zcl_text_editor( parent         = CAST zcl_tscreen( me )
                             id             = 'LASTCHANGEDT'
                             container_name = 'CON_LAST_CHANGE'
                             content        = CONV string( ekko-lastchangedatetime )
                             length_content = strlen( CONV string( ekko-lastchangedatetime ) ) - 1
                          )->set_status_text( 'LASTCHANGEDATETIME'
                          )->set_toolbar_mode( 0 ).

      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      CATCH zcx_tscreen INTO DATA(lx_tscreen).
        MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_16_v9002 IMPLEMENTATION.

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
CLASS lcl_tscreen_16_v9003 IMPLEMENTATION.

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
CLASS lcl_tscreen_16_v9004 IMPLEMENTATION.

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

  METHOD user_command_extend .

    CASE ucomm.
      WHEN 'MAKTX'.

        ASSIGN po_items[ get_current_line( ) ]-maktx TO FIELD-SYMBOL(<maktx>).
        CHECK sy-subrc = 0.

        TRY.
            CALL FUNCTION 'ZFM_GBC_TEXT_EDITOR'
              EXPORTING
                io_editor   = zcl_text_editor_tool=>get_instance( )->set_status_text( title = '物料描述' text = 'PO_ITEMS[ GET_CURRENT_LINE( ) ]-MAKTX'
                                                                  )->set_statusbar_mode( 1
                                                                  )->set_toolbar_mode( 1 )
                iv_readonly = parent->display_mode
              CHANGING
                text        = <maktx>.

          CATCH cx_uuid_error INTO DATA(lx_uuid_error).
            MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
          CATCH zcx_tscreen INTO DATA(lx_tscreen).
            MESSAGE lx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

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

  METHOD before_filter_table_writeback .
    ##ITAB_KEY_IN_SELECT
    SELECT *
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

    SELECT *                                  "#EC CI_ALL_FIELDS_NEEDED
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

    SELECT *                                  "#EC CI_ALL_FIELDS_NEEDED
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
           END OF c_item_tabs.
*&SPWIZARD: DATA FOR TABSTRIP 'ITEM_TABS'
CONTROLS:  item_tabs TYPE TABSTRIP.
DATA: BEGIN OF g_item_tabs,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZAESOP_TSCREEN_16',
        pressed_tab LIKE sy-ucomm VALUE c_item_tabs-tab1,
      END OF g_item_tabs.

*&SPWIZARD: OUTPUT MODULE FOR TS 'ITEM_TABS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE item_tabs_active_tab_set OUTPUT.
  item_tabs-activetab = g_item_tabs-pressed_tab.
  CASE g_item_tabs-pressed_tab.
    WHEN c_item_tabs-tab1.
      g_item_tabs-subscreen = '9003'.
    WHEN c_item_tabs-tab2.
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
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
