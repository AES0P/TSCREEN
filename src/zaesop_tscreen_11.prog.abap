*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_11
*&---------------------------------------------------------------------*
*&  zcl_treport 作为报表，zcl_tscreen_with_components 作为自建屏幕，示例怎么个性化 TABLE CONTROL 控件
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_11.

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

    CLASS-DATA view_prog(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_11' READ-ONLY.

    CLASS-METHODS push_view.

    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_11_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_data
      IMPORTING
        ebeln TYPE ekko-ebeln.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS pov REDEFINITION.

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
      WHEN '9000'.
        TRY.
            NEW lcl_tc_po_items( view ).
          CATCH cx_uuid_error INTO DATA(lx_uuid_error).
            MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
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
CLASS lcl_tscreen_11_v9000 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    get_data( p_ebeln ).
  ENDMETHOD.

  METHOD get_data.

    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = ebeln.                                 "#EC CI_SUBRC

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
         ORDER BY ebeln.                  "#EC CI_NOWHERE  #EC CI_SUBRC

        ekko-ebeln = f4_event( key_field = 'EBELN' value_tab = lt_ekko ).
        CHECK ekko-ebeln IS NOT INITIAL.

        get_data( ekko-ebeln ).
        CAST lcl_tc_po_items( get_component( group = 'TC' id = 'TC_9000_01' ) )->get_data( ekko-ebeln ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tc_po_items IMPLEMENTATION.

  METHOD constructor.
    super->constructor( parent             = CAST zcl_tscreen( tscreen )
                        tc_name            = 'TC_9000_01'
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
      WHEN 'TEST'.
        MESSAGE '增强现有功能，或响应自定义功能，重写USER_COMMAND_EXTEND方法即可！' TYPE 'I'.
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
