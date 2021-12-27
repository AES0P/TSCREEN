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
  SELECTION-SCREEN:
  BEGIN OF LINE,
  PUSHBUTTON 1(5)  p_bt1 USER-COMMAND comm1," 查询
  PUSHBUTTON 7(5)  p_bt2 USER-COMMAND comm2," 置空
  END OF LINE.
SELECTION-SCREEN END OF SCREEN 9900.

*&---------------------------------------------------------------------*
*&　　　　DATA define
*&---------------------------------------------------------------------*
DATA ok_code TYPE sy-ucomm.

DATA: po_items TYPE STANDARD TABLE OF zsekpo,
      po_item  LIKE LINE OF po_items.

CONTROLS tc_9001_01 TYPE TABLEVIEW USING SCREEN '9001'.

"标题 for 多语言
DATA: BEGIN OF title,
        condition TYPE string,
        result    TYPE string,
        po_items  TYPE string,
      END OF title.
DATA: BEGIN OF title_ekpo,
        ebeln TYPE string,
        ebelp TYPE string,
        matnr TYPE string,
        maktx TYPE string,
        menge TYPE string,
        meins TYPE string,
        bstme TYPE string,
        mwskz TYPE string,
        retpo TYPE string,
        charg TYPE string,
        elikz TYPE string,
      END OF title_ekpo.
*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-DATA sub_screen TYPE sy-dynnr VALUE '9900'.
    CLASS-DATA view_prog_prefix(24) VALUE 'LCL_PROG' READ-ONLY.
    CLASS-DATA view_prefix(24) VALUE 'LCL_TSCREEN_17' READ-ONLY.
    CLASS-METHODS push_view.

    METHODS initialize REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS show REDEFINITION.
    METHODS exit REDEFINITION.

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
    METHODS pai REDEFINITION.

ENDCLASS.

CLASS lcl_tc_po_items DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_table_control FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        tscreen TYPE REF TO zif_tscreen
      RAISING
        cx_uuid_error
        zcx_tscreen.

    METHODS get_data.

    METHODS user_command_extend REDEFINITION.

    METHODS pai_tc_line REDEFINITION.
    METHODS pbo_tc_line REDEFINITION.
    METHODS poh REDEFINITION.
    METHODS pov REDEFINITION.

    METHODS is_line_insert REDEFINITION.
    METHODS is_deletion_confirmed REDEFINITION.

ENDCLASS.
"搜索帮助回调
CLASS lcl_f4_callback_handler DEFINITION CREATE PUBLIC FINAL.

  PUBLIC SECTION.
    INTERFACES if_f4callback_value_request.

    METHODS constructor
      IMPORTING
        method TYPE string.

  PRIVATE SECTION.
    DATA method TYPE string.
ENDCLASS.
*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    DATA view TYPE REF TO zif_tscreen.
    CASE sy-dynnr.
      WHEN '1000'."选择屏幕编号
        CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid ).
        DATA(class_name) = lcl_prog=>view_prog_prefix.
      WHEN OTHERS.
        CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid dynnr_super = '9000' ).
        class_name = lcl_prog=>view_prefix && '_V' && sy-dynnr.
    ENDCASE.

    CREATE OBJECT view TYPE (class_name).

  ENDMETHOD.

  METHOD initialize.

    p_bt1 = '查询'(001).
    p_bt2 = '置空'(002).

    title-condition = '查询条件'(t01).
    title-result    = '查询结果'(t02).
    title-po_items  = '采购订单行项目'(t03).

    set_title( EXPORTING tabname = 'ZSEKPO' CHANGING title = title_ekpo ).

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

  METHOD exit.
    super->exit( ).
    CAST zcl_tlog( tlog )->display_in_slg1( )->commit( )->free( ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_tscreen_17_v9000 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    set_display_mode( zcl_tscreen=>display_mode_modify ).
    ##NO_TEXT    tlog->add_log( '9000 Started' ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'EXPAND'.
        IF lcl_prog=>sub_screen = '9900'.
          lcl_prog=>sub_screen = '9002'.
        ELSE.
          lcl_prog=>sub_screen = '9900'.
        ENDIF.
      WHEN 'EXECUTE'.
        TRY.
            DATA(v9001) = CAST lcl_tscreen_17_v9001( get_sub_screen( '9001' ) )."拿到子屏幕对象
            CAST lcl_tc_po_items( v9001->get_component( group = zif_tscreen_component=>c_component_tc id = 'TC_9001_01' ) )->get_data( )."访问子屏幕控件的方法
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
    ##NO_TEXT    tlog->add_log( '9001 Started' ).
    capture = zcl_tscreen_with_components=>capture_from_inner_to_outer."事件冒泡
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
  ENDMETHOD.

  METHOD add_components.

    TRY.
        NEW lcl_tc_po_items( me ).
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
CLASS lcl_tscreen_17_v9900 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( dynnr_super = '9000' ).
    ##NO_TEXT    tlog->add_log( '9900 Started' ).
  ENDMETHOD.

  ##NEEDED
  METHOD pbo.
    LOOP AT SCREEN.
      screen-input  = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'COMM1'.
        tlog->add_log( type = 'W' content = 'COMM1' ).
        TRY.
            DATA(v9001) = CAST lcl_tscreen_17_v9001( zcl_tscreen_stack=>get_instance( )->current( dynnr_super = '9000' dynnr = '9001' ) )."拿到子屏幕对象
            CAST lcl_tc_po_items( v9001->get_component( group = zif_tscreen_component=>c_component_tc id = 'TC_9001_01' ) )->get_data( )."访问子屏幕控件的方法
          CATCH zcx_tscreen INTO DATA(gx_tscreen) ##NEEDED.
            MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
        ENDTRY.
      WHEN 'COMM2'.
        tlog->add_log( type = 'A' content = 'COMM2' ).
        CLEAR p_ebeln.
        CLEAR s_bukrs[].
      WHEN OTHERS.
    ENDCASE.
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

    ##NO_TEXT    parent->tlog->add_log( 'TC_9001_01 initialized' ).
    get_data( ).
  ENDMETHOD.

  METHOD get_data.
    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT ekpo~ebeln
           ekpo~ebelp
           ekpo~matnr
           makt~maktx
           ekpo~menge
           ekpo~mwskz
           ekpo~meins
           ekpo~retpo
           ekpo~elikz
      FROM ekpo
     INNER JOIN ekko
        ON ekko~ebeln = ekpo~ebeln
      LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
       AND makt~spras = sy-langu
      INTO CORRESPONDING FIELDS OF TABLE po_items
     WHERE ekpo~ebeln  = p_ebeln
       AND ekko~bukrs IN s_bukrs
     ORDER BY ebelp.                                      "#EC CI_SUBRC
    IF sy-subrc <> 0.
      ##NO_TEXT      parent->tlog->add_log( type = 'W' content = 'No data found' ).
    ELSE.
      ##NO_TEXT     parent->tlog->add_log( type = 'I' content = 'success' ).
    ENDIF.
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

    IF po_item-mwskz IS INITIAL.
      po_item-mwskz = 'J3'.
    ENDIF.

    "如果使用了自动带出功能，这里需要判断一下，自动带出时执行修改逻辑，但不修改自动带出的字段值
    IF sy-ucomm = zcl_tscreen_util=>c_fcode_tc_pov_bring_out.
      CLEAR sy-ucomm.
      MODIFY po_items FROM po_item TRANSPORTING mwskz WHERE ebeln = po_item-ebeln AND ebelp = po_item-ebelp. "#EC CI_STDSEQ
    ELSE.

      SELECT SINGLE maktx
        FROM makt
        INTO po_item-maktx
       WHERE matnr = po_item-matnr
         AND spras = sy-langu.                            "#EC CI_SUBRC

      super->pai_tc_line( ).
    ENDIF.

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

    ASSIGN po_items[ get_current_line( ) ] TO FIELD-SYMBOL(<po_item>).

    CASE parent->cursor_filed.

*&---------------------------------------------------------------------*
*&        F4_MATNR
*&---------------------------------------------------------------------*
      WHEN 'PO_ITEM-MATNR'.

        SELECT mara~matnr, makt~maktx                   "#EC CI_NOORDER
          FROM mara
          LEFT JOIN makt
            ON makt~matnr = mara~matnr
           AND makt~spras = @sy-langu
          INTO TABLE @DATA(lt_mara)
            UP TO '100' ROWS
         ORDER BY mara~matnr.
        IF sy-subrc = 0.
*        DATA(matnr) = parent->f4_event( key_field = 'MATNR' value_tab = lt_mara callback_form = 'FRM_F4_CALLBACK_ITEM_MATNR' )."和下边效果是一样的，但下边的方式更优
          DATA(matnr) = parent->f4_event( key_field = 'MATNR' value_tab = lt_mara callback_handler = NEW lcl_f4_callback_handler( CONV string( parent->cursor_filed ) ) ).
          CHECK matnr IS NOT INITIAL.
          <po_item>-matnr = matnr.
          TRY.
              parent->bring_out( EXPORTING source = 'MAKTX' CHANGING target = <po_item>-maktx ).
            CATCH zcx_tscreen INTO DATA(gx_tscreen) ##NEEDED.
              MESSAGE gx_tscreen->get_text( ) TYPE 'S' DISPLAY LIKE 'A'.
          ENDTRY.
        ENDIF.
      WHEN 'PO_ITEM-MWSKZ'.

        SELECT DISTINCT t007s~kalsm, t007s~mwskz, t007s~text1
          FROM t007a
         INNER JOIN t007s
            ON t007s~kalsm = t007a~kalsm
           AND t007s~mwskz = t007a~mwskz
           AND t007s~spras = @sy-langu
          BYPASSING BUFFER
          INTO TABLE @DATA(lt_mwskz)
*            UP TO '100' ROWS
          ORDER BY t007s~kalsm, t007s~mwskz.
        IF sy-subrc = 0.
          DATA(mwskz) = parent->f4_event( key_field = 'MWSKZ' value_tab = lt_mwskz callback_handler = NEW lcl_f4_callback_handler( CONV string( parent->cursor_filed ) ) ).
          CHECK mwskz IS NOT INITIAL.
          <po_item>-mwskz = mwskz.
        ENDIF.

    ENDCASE.

    super->pov( ).

  ENDMETHOD.

  METHOD is_line_insert .

    is_line_insert = super->is_line_insert( )."添加空行

    DATA(index) = lines( po_items ).

    "根据最后一行的行号递增
    po_items[ index ]-ebeln = p_ebeln.
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

"回调处理
**&---------------------------------------------------------------------*
**&　　　　FRM_F4_ITEM_MATNR_CALLBACK
**&---------------------------------------------------------------------*
*FORM frm_f4_callback_item_matnr TABLES record_tab  STRUCTURE seahlpres
*                              CHANGING shlp_top    TYPE shlp_descr
*                                       callcontrol TYPE ddshf4ctrl.
*
*  APPEND VALUE #(  shlpfield = 'F0001'
*                   sign      = 'I'
*                   option    = 'EQ'
*                   low       = '1*'
*                   high      = '' ) TO shlp_top-selopt.
*
*  callcontrol-multisel = abap_true."允许选择多条数据，并且返回一个按选择排序的值表
*
*ENDFORM.

CLASS lcl_f4_callback_handler IMPLEMENTATION.

  METHOD constructor.
    me->method = method.
  ENDMETHOD.

  METHOD if_f4callback_value_request~f4_call_callback.
    CASE to_upper( method ).
      WHEN 'PO_ITEM-MATNR'.
        APPEND VALUE #(  shlpfield = 'F0001'
                          sign      = 'I'
                          option    = 'EQ'
                          low       = '1*'
                          high      = '' ) TO cs_shlp-selopt."设定默认筛选条件值

        cs_callcontrol-multisel = abap_true."允许选择多条数据，并且返回一个按选择排序的值表
      WHEN 'PO_ITEM-MWSKZ'.
        MESSAGE '请按需自行添加回调处理' TYPE 'I'.
*        cs_callcontrol-multisel = abap_true."允许选择多条数据，并且返回一个按选择排序的值表
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
