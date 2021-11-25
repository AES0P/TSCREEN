*&---------------------------------------------------------------------*
*& Report ZAESOP_TEST_TALV_TSCREEN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_test_talv_tscreen.

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

  PROTECTED SECTION.
    METHODS add_components REDEFINITION.

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

    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
    SELECT * ##TOO_MANY_ITAB_FIELDS
      FROM ekpo
      LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
       AND makt~spras = sy-langu
      INTO CORRESPONDING FIELDS OF TABLE po_items
     WHERE ebeln = lcl_prog=>ebeln
     ORDER BY ebelp.                                      "#EC CI_SUBRC
    "此处为屏幕添加控件对象
    add_components( ).
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

  METHOD add_components.

    TRY.
        NEW zcl_table_control( parent             = CAST zcl_tscreen( me )
                               tc_name            = 'TC_9001_01'
                               data_source        = 'PO_ITEMS'
                               data_wa            = 'PO_ITEM'
                               ref_structure_name = 'ZSEKPO' ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        MESSAGE lx_uuid_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
