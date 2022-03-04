*MIT License
*
*Copyright (c) 2021 AES0P
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
CLASS zcl_tscreen DEFINITION
  PUBLIC
  INHERITING FROM zcl_tscreen_root
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zif_tscreen_component .

  PUBLIC SECTION.

    TYPES ty_attr_setting TYPE ztdynpro_attr .
    TYPES:
      tty_attr_setting TYPE STANDARD TABLE OF ty_attr_setting WITH DEFAULT KEY .

    CONSTANTS display_mode_show TYPE abap_bool VALUE abap_true ##NO_TEXT.
    CONSTANTS display_mode_modify TYPE abap_bool VALUE abap_false ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !program             TYPE syrepid DEFAULT sy-cprog
        !dynnr               TYPE sy-dynnr DEFAULT sy-dynnr
        !dynnr_super         TYPE sy-dynnr OPTIONAL
        !dynpro_type         TYPE scrhtyp DEFAULT zcl_tscreen=>dynpro_type_normal
        !display_mode        TYPE abap_bool DEFAULT zcl_tscreen=>display_mode_show
        !pfstatus            TYPE sypfkey OPTIONAL
        !pfstatus_repid      TYPE syrepid OPTIONAL
        !excluding_fcode     TYPE tty_fcode OPTIONAL
        !titlebar            TYPE gui_title OPTIONAL
        !titlebar_repid      TYPE syrepid OPTIONAL
        !titlebar_var1       TYPE string OPTIONAL
        !titlebar_var2       TYPE string OPTIONAL
        !titlebar_var3       TYPE string OPTIONAL
        !titlebar_var4       TYPE string OPTIONAL
        !titlebar_var5       TYPE string OPTIONAL
        !read_dynpro_setting TYPE abap_bool DEFAULT abap_true
        !refresh_interval    TYPE i DEFAULT 0 .
    METHODS get_display_mode
      RETURNING
        VALUE(display_mode) TYPE syucomm .
    METHODS set_display_mode
      IMPORTING
        !display_mode TYPE abap_bool .
    METHODS change_display_mode .
    METHODS f4_event
      IMPORTING
        VALUE(key_field)     TYPE dfies-fieldname
        !value_tab           TYPE STANDARD TABLE
        VALUE(callback_form) TYPE sy-xform OPTIONAL
        !callback_handler    TYPE REF TO if_f4callback_value_request OPTIONAL
      EXPORTING
        !return_table        TYPE zcl_tscreen_util=>f4_return_tab
      RETURNING
        VALUE(value)         TYPE shvalue_d .
    METHODS bring_out
      IMPORTING
        !source TYPE any
      CHANGING
        !target TYPE any
      RAISING
        zcx_tscreen .
    METHODS get_screen_util
      RETURNING
        VALUE(screen_util) TYPE REF TO zcl_tscreen_util .
    METHODS countdown_begin
      IMPORTING
        !refresh_interval TYPE i .
    METHODS countdown_stop .

    METHODS set_pfstatus
        REDEFINITION .
    METHODS zif_tscreen~exit
        REDEFINITION .
    METHODS zif_tscreen~handle_event
        REDEFINITION .
    METHODS zif_tscreen~pai
        REDEFINITION .
    METHODS zif_tscreen~poh
        REDEFINITION .
    METHODS zif_tscreen~pov
        REDEFINITION .
  PROTECTED SECTION.

    DATA display_mode TYPE abap_bool .
    DATA filedname TYPE feld-name .
    DATA cursor_filed TYPE feld-name .
    DATA cursor_filed_value TYPE dynfieldvalue .
    DATA dynpro_attr_setting TYPE tty_attr_setting .
    DATA screen_util TYPE REF TO zcl_tscreen_util .
    DATA timer TYPE REF TO cl_gui_timer .

    METHODS get_dynpro_setting .
    METHODS remove_child_screen
      RAISING
        zcx_tscreen .
    METHODS get_super_screen
      RETURNING
        VALUE(parent) TYPE REF TO zif_tscreen
      RAISING
        zcx_tscreen .
    METHODS get_sub_screen_iterator
      RETURNING
        VALUE(sub_screen_iterator) TYPE REF TO cl_object_collection_iterator .
    METHODS get_sub_screen
      IMPORTING
        !dynnr            TYPE sy-dynnr
      RETURNING
        VALUE(sub_screen) TYPE REF TO zif_tscreen
      RAISING
        zcx_tscreen .
    METHODS change_screen_editable
      RAISING
        zcx_tscreen .
    METHODS set_element_attr_by_setting
      IMPORTING
        VALUE(object) TYPE any OPTIONAL .
    METHODS get_current_info .
    METHODS on_countdown_finished
        FOR EVENT finished OF cl_gui_timer .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TSCREEN IMPLEMENTATION.


  METHOD constructor.

    DATA this_dynpro_type TYPE scrhtyp.
    IF dynnr_super IS NOT INITIAL.
      this_dynpro_type = zcl_tscreen=>dynpro_type_subscreen.
    ELSE.
      this_dynpro_type = dynpro_type.
    ENDIF.

    super->constructor( program         = program
                        dynnr           = dynnr
                        dynnr_super     = dynnr_super
                        dynpro_type     = this_dynpro_type
                        pfstatus        = pfstatus
                        pfstatus_repid  = pfstatus_repid
                        excluding_fcode = excluding_fcode
                        titlebar        = titlebar
                        titlebar_repid  = titlebar_repid
                        titlebar_var1   = titlebar_var1
                        titlebar_var2   = titlebar_var2
                        titlebar_var3   = titlebar_var3
                        titlebar_var4   = titlebar_var4
                        titlebar_var5   = titlebar_var5  ).

    screen_util = zcl_tscreen_util=>get_instance( program = program ).

    set_display_mode( display_mode ).

    IF read_dynpro_setting = abap_true.
      get_dynpro_setting( ).
    ENDIF.

    countdown_begin( refresh_interval ).

  ENDMETHOD.


  METHOD get_display_mode.
    display_mode = me->display_mode.
  ENDMETHOD.


  METHOD get_screen_util.
    screen_util = me->screen_util.
    screen_util->set_dynnr( dynnr ).
  ENDMETHOD.


  METHOD set_display_mode.

    CASE display_mode.
      WHEN display_mode_modify OR display_mode_show.
        me->display_mode = display_mode.
      WHEN OTHERS.
        me->display_mode = display_mode_show.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_tscreen~handle_event.

    get_current_info( ).

    CASE event.
      WHEN 'PBO'.
        initialize_pbo_by_dynnr( dynnr ).
        change_screen_editable( ).
        set_element_attr_by_setting( ).
        pbo( ).
      WHEN 'PAI'.
        IF if_log_record_pai = abap_true.
          tlog->add_log( type = 'I' content = 'PAI:' && sy-ucomm ).
        ENDIF.
        pai( ).
      WHEN 'POH'.
        poh( ).
      WHEN 'POV'.
        pov( ).
        screen_util->execute_fcode( '/00').
      WHEN 'EXIT'.
        exit( ).
      WHEN OTHERS.
*        MESSAGE e000(ztscreen) INTO zcx_tscreen=>error.
*        zcx_tscreen=>raise_text(  ).
    ENDCASE.

    IF timer IS BOUND.
      timer->run( ).
    ENDIF.

  ENDMETHOD.


  METHOD change_screen_editable.

    IF dynpro_type = zif_tscreen=>dynpro_type_subscreen.
      me->display_mode = CAST zcl_tscreen( get_super_screen( ) )->display_mode.
    ENDIF.

    LOOP AT SCREEN.
      IF me->display_mode = display_mode_show.
        CHECK NOT screen-name CS '_TABS_'."TAB页签不应该设为不可点击
        screen-input  = 0.
      ELSE.
        CHECK NOT screen-name CS '-OPTI_PUSH'."选择屏幕的筛选条件图标不应该设为可编辑
        screen-input  = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.


  METHOD f4_event.

    CHECK value_tab IS NOT INITIAL.
    CLEAR return_table.

    value = screen_util->f4_event(
                           EXPORTING
                              key_field        = key_field
                              value_tab        = value_tab
                              display          = display_mode
                              callback_form    = to_upper( callback_form )
                              callback_handler = callback_handler
                           IMPORTING
                             return_table      = return_table ).

  ENDMETHOD.


  METHOD get_current_info.

    screen_util->set_dynnr( dynnr ).

    cursor_filed       = screen_util->get_field_name_by_cursor( ).
    cursor_filed_value = screen_util->get_structure_field_value( cursor_filed ).

    filedname = screen_util->get_no_prefix_column_name( cursor_filed ).

  ENDMETHOD.


  METHOD set_element_attr_by_setting.

    CHECK dynpro_attr_setting IS NOT INITIAL.

    FIELD-SYMBOLS <setting> LIKE LINE OF dynpro_attr_setting.

    LOOP AT SCREEN.
      "优先以具体模式为准
      ##WARN_OK
      READ TABLE dynpro_attr_setting ASSIGNING <setting> WITH KEY tc_name = object
                                                                  name    = screen-name
                                                                  zmode   = display_mode
                                                                  BINARY SEARCH.
      IF sy-subrc <> 0.
        "没有设置具体模式，再使用通用模式
        ##WARN_OK
        READ TABLE dynpro_attr_setting ASSIGNING <setting> WITH KEY tc_name = object
                                                                    name    = screen-name
                                                                    zmode   = '*'
                                                                    BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF sy-subrc = 0.

        "check group
        IF <setting>-group_name IS NOT INITIAL.
          CHECK <setting>-group_name = screen-group1
             OR <setting>-group_name = screen-group2
             OR <setting>-group_name = screen-group3
             OR <setting>-group_name = screen-group4.
        ENDIF.

        CASE abap_true.
          WHEN <setting>-xoblg."标志: 必需输入字段 ?

            screen-active = screen-input = screen-required = 1.

          WHEN <setting>-xoptn."指示符: 可选字段 ?

            screen-active   = screen-input = 1.
            screen-required = 0.

          WHEN <setting>-xnodi."标识: 字段隐藏

            screen-invisible = <setting>-xnodi.
            screen-active    = screen-input = screen-required = 0.

          WHEN <setting>-xdisp."标志: 字段仅可显示 ?

            screen-active   = 1.
            screen-input    = 0.
            screen-required = 0.

        ENDCASE.

        MODIFY SCREEN.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_dynpro_setting.

    SELECT *                                            "#EC CI_SEL_DEL
     FROM ztdynpro_attr
     INTO CORRESPONDING FIELDS OF TABLE dynpro_attr_setting
    WHERE cprog = program
      AND dynnr = dynnr.                                  "#EC CI_SUBRC

    SORT dynpro_attr_setting ASCENDING BY tc_name name zmode group_name.

    "控制，如果同一屏幕元素，既配置了含组的数据，又配置了不含组的数据，以不含组的为准
    DELETE ADJACENT DUPLICATES FROM dynpro_attr_setting COMPARING tc_name name zmode.

    SORT dynpro_attr_setting ASCENDING BY tc_name name zmode group_name.

  ENDMETHOD.


  METHOD zif_tscreen~exit.

    remove_child_screen( ).

    CLEAR: display_mode,
           filedname,
           cursor_filed,
           cursor_filed_value,
           dynpro_attr_setting.

    countdown_stop( ).

    super->exit( ).

  ENDMETHOD.


  METHOD bring_out.

    CHECK get_display_mode( ) = zcl_tscreen=>display_mode_modify.
    IF screen_util->bring_out_data IS INITIAL.
      zcx_tscreen=>raise_text( 'BRING OUT DATA IS INITIAL' ).
    ENDIF.

    FIELD-SYMBOLS <source> TYPE any.
    ASSIGN screen_util->bring_out_data->* TO <source>.

    FIELD-SYMBOLS <target> TYPE any.
    ASSIGN COMPONENT to_upper( source ) OF STRUCTURE <source> TO <target>.
    IF sy-subrc = 0.
      target = <target>.
    ELSE.
      zcx_tscreen=>raise_text( 'NO FIELD FOUND' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_super_screen.

    CHECK dynnr_super IS NOT INITIAL AND dynpro_type = zif_tscreen=>dynpro_type_subscreen.

    DATA tscreen TYPE zcl_tscreen_stack=>ty_view.
    READ TABLE zcl_tscreen_stack=>get_instance( )->tscreens INTO tscreen WITH KEY dynnr = dynnr_super. "#EC CI_STDSEQ
    CHECK sy-subrc = 0.
    parent = tscreen-tscreen.

  ENDMETHOD.


  METHOD remove_child_screen.

    CHECK dynpro_type <> zif_tscreen=>dynpro_type_subscreen.

    DATA stack TYPE REF TO zcl_tscreen_stack.
    stack = zcl_tscreen_stack=>get_instance( ).

    DATA tabix TYPE sy-tabix.
    FIELD-SYMBOLS <tscreen> TYPE zcl_tscreen_stack=>ty_view.
    LOOP AT stack->tscreens ASSIGNING <tscreen> WHERE tscreen->dynnr_super = dynnr. "#EC CI_STDSEQ
      tabix = sy-tabix.
      <tscreen>-tscreen->handle_event( 'EXIT' ).
      DELETE stack->tscreens INDEX tabix.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_tscreen~pai ##NEEDED.
*CALL METHOD SUPER->ZIF_TSCREEN~PAI
*    .
  ENDMETHOD.


  METHOD zif_tscreen~poh ##NEEDED.
*CALL METHOD SUPER->ZIF_TSCREEN~POH
*    .
  ENDMETHOD.


  METHOD zif_tscreen~pov ##NEEDED.
*CALL METHOD SUPER->ZIF_TSCREEN~POV
*    .
  ENDMETHOD.


  METHOD set_pfstatus.

    FIELD-SYMBOLS <attr_setting> LIKE LINE OF dynpro_attr_setting.
    LOOP AT dynpro_attr_setting ASSIGNING <attr_setting> WHERE group_name = ''
                                                           AND tc_name    = 'PF-STATUS'
                                                           AND xnodi      = abap_true
                                                           AND zmode      = display_mode. "#EC CI_STDSEQ

      APPEND <attr_setting>-name TO excluding_fcode.

    ENDLOOP.

    super->set_pfstatus( pfstatus        = pfstatus
                         pfstatus_repid  = pfstatus_repid
                         excluding_fcode = excluding_fcode ).

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD countdown_begin.
    CHECK refresh_interval > 0.
    IF timer IS NOT BOUND.
      CREATE OBJECT timer.
      SET HANDLER on_countdown_finished FOR timer.
    ENDIF.
    timer->interval = refresh_interval.
    timer->run( ).
  ENDMETHOD.


  METHOD countdown_stop.
    CHECK timer IS BOUND.
    timer->cancel( ).
    timer->free( ).
    FREE timer.
  ENDMETHOD.


  METHOD on_countdown_finished.
    ##NO_TEXT    MESSAGE 'Plz redefine method ON_COUNTDOWN_FINISHED for the child which inheriting from ZCL_TSCREEN' TYPE 'S'.
    timer->run( )."循环倒计时事件
  ENDMETHOD.


  METHOD get_sub_screen_iterator.

    CHECK dynnr_super IS INITIAL AND dynpro_type = zif_tscreen=>dynpro_type_normal.

    DATA sub_screen_collection TYPE REF TO  cl_object_collection.
    CREATE OBJECT sub_screen_collection.

    FIELD-SYMBOLS <tscreen> TYPE zcl_tscreen_stack=>ty_view.
    LOOP AT zcl_tscreen_stack=>get_instance( )->tscreens ASSIGNING <tscreen> WHERE dynnr_super = me->dynnr. "#EC CI_STDSEQ "#EC CI_LOOP_INTO_WA
      sub_screen_collection->add( <tscreen>-tscreen ).
    ENDLOOP.

    sub_screen_iterator = sub_screen_collection->get_iterator( ).

  ENDMETHOD.


  METHOD get_sub_screen.

    DATA iterator TYPE REF TO cl_object_collection_iterator.
    iterator = get_sub_screen_iterator( ).

    WHILE iterator->has_next( ).

      sub_screen = CAST zif_tscreen( iterator->get_next( ) ).

      IF sub_screen->dynnr = dynnr.
        RETURN.
      ELSE.
        FREE sub_screen.
      ENDIF.

    ENDWHILE.

    ##NO_TEXT    zcx_tscreen=>raise_text( 'No view found' ).

  ENDMETHOD.


  METHOD change_display_mode.
    IF get_display_mode( ) = zcl_tscreen=>display_mode_modify.
      set_display_mode( zcl_tscreen=>display_mode_show ).
    ELSE.
      set_display_mode( zcl_tscreen=>display_mode_modify ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
