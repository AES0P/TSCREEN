*&---------------------------------------------------------------------*
*& Report ZAESOP_TEST_TALV_TSCREEN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_test_talv_tscreen.

*&---------------------------------------------------------------------*
*&    TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS vrm.

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

CONTROLS tc_9001_01 TYPE TABLEVIEW USING SCREEN '9001'.

*&---------------------------------------------------------------------*
*&　　　　PARAMETERS
*&---------------------------------------------------------------------*
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln." NO-DISPLAY.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-DATA view_cls_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_view_prefix(24) VALUE 'LCL_TSCREEN_03' READ-ONLY.
    CLASS-DATA ebeln TYPE ekko-ebeln.
    CLASS-METHODS push_view.

    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.

ENDCLASS.

CLASS lcl_tscreen_03_v9000 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen FINAL.

  PUBLIC SECTION.

    METHODS pbo REDEFINITION.

  PRIVATE SECTION.

    DATA talv TYPE REF TO zcl_talv_parent.

ENDCLASS.

CLASS lcl_tscreen_03_v9001 DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_tscreen_with_components FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.

    METHODS fill_listbox.

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

    CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid ).

    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        DATA(class_name) = lcl_prog=>view_cls_prefix.
      WHEN OTHERS.
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
    CALL SCREEN 9000.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_03_v9000 IMPLEMENTATION.

  METHOD pbo.

    IF talv IS NOT BOUND.
      "工厂模式生成TALV并直接展示
      talv = zcl_talv_factory=>get_talv( VALUE #( type               = 'TALV_CUS'
                                                  ddic_type          = 'EKKO'
                                                  dynnr              = '9000'
                                                  container_position = '01'
                                                  container          = NEW cl_gui_custom_container( container_name = 'CON1' ) ) ) .
      talv->display( ).
    ELSE.
      talv->refresh( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
##CALLED
FORM f9000_01_handle_on_pbo USING talv TYPE REF TO zcl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE ##NEEDED .

  talv->save_fields( 'EBELN BUKRS BSTYP BSART LOEKZ AEDAT ERNAM' ).
  talv->refresh( ).

ENDFORM.
##CALLED
FORM f9000_01_handle_retrieve USING ddic_type TYPE tabname ##NEEDED
                           CHANGING alv_table TYPE STANDARD TABLE.

  SELECT *
    FROM ekko
    INTO CORRESPONDING FIELDS OF TABLE alv_table
      UP TO '500' ROWS
   WHERE ebeln IN s_ebeln
   ORDER BY ebeln.

ENDFORM.
##CALLED
FORM f9000_01_handle_double_click USING po_talv TYPE REF TO zcl_talv_parent ##NEEDED
                                        ps_row TYPE lvc_s_row
                                        ps_column TYPE lvc_s_col ##NEEDED
                               CHANGING ct_alv_table TYPE STANDARD TABLE.

  TYPES tty_ekko TYPE STANDARD TABLE OF ekko.
  FIELD-SYMBOLS <ekko> TYPE tty_ekko.
  ASSIGN ct_alv_table TO <ekko>.

  lcl_prog=>ebeln = <ekko>[ ps_row-index ]-ebeln.

  CALL SCREEN 9001.

ENDFORM.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_03_v9001 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
      FROM ekko
      INTO CORRESPONDING FIELDS OF ekko
     WHERE ebeln = lcl_prog=>ebeln.                       "#EC CI_SUBRC

    "此处为屏幕添加控件对象
    add_components( ).
    display_mode = zcl_tscreen=>display_mode_modify.

    "下拉框处理
    fill_listbox( ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD fill_listbox.

    "凭证类型
    SELECT *
      FROM t161t
      BYPASSING BUFFER
      INTO TABLE @DATA(lt_t161t)
      UP TO '50' ROWS
     WHERE spras = @sy-langu
     ORDER BY bsart, bstyp.
    IF sy-subrc = 0.

      DATA vrm_values TYPE vrm_values.
      LOOP AT lt_t161t ASSIGNING FIELD-SYMBOL(<ls_t161t>).
        APPEND VALUE vrm_value( key = <ls_t161t>-bsart text = <ls_t161t>-batxt ) TO vrm_values.
      ENDLOOP.

      screen_util->set_listbox( vrm_id = 'EKKO-BSART' vrm_values = vrm_values ).

    ENDIF.

    "公司代码
    "凭证类型
    SELECT bukrs, butxt
      FROM t001
      INTO TABLE @DATA(lt_t001)
      UP TO '50' ROWS
     ORDER BY bukrs.
    IF sy-subrc = 0.

      CLEAR vrm_values.
      LOOP AT lt_t001 ASSIGNING FIELD-SYMBOL(<ls_t001>).
        APPEND VALUE vrm_value( key = <ls_t001>-bukrs text = <ls_t001>-butxt ) TO vrm_values.
      ENDLOOP.

      screen_util->set_listbox( vrm_id = 'EKKO-BUKRS' vrm_values = vrm_values ).

    ENDIF.

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
CLASS lcl_tc_po_items IMPLEMENTATION.

  METHOD constructor.
    super->constructor( parent             = CAST zcl_tscreen( tscreen )
                        tc_name            = 'TC_9001_01'
                        data_source        = 'PO_ITEMS'
                        data_wa            = 'PO_ITEM'
                        ref_structure_name = 'ZSEKPO' ).

    get_data( lcl_prog=>ebeln ).
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
