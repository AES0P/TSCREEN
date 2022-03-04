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
CLASS zcl_text_editor DEFINITION
  PUBLIC
  INHERITING FROM zcl_tcomponent
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_text_editor .

    ALIASES c_component_editor
      FOR zif_tscreen_component~c_component_editor .
    ALIASES c_component_tc
      FOR zif_tscreen_component~c_component_tc .
    ALIASES text
      FOR zif_text_editor~text .
    ALIASES title
      FOR zif_text_editor~title .
    ALIASES get_text
      FOR zif_text_editor~get_text .
    ALIASES initialize_by_tscreen
      FOR zif_tscreen_component~initialize_by_tscreen .
    ALIASES save_text
      FOR zif_text_editor~save_text .
    ALIASES set_statusbar_mode
      FOR zif_text_editor~set_statusbar_mode .
    ALIASES set_status_text
      FOR zif_text_editor~set_status_text .
    ALIASES set_text
      FOR zif_text_editor~set_text .
    ALIASES set_toolbar_mode
      FOR zif_text_editor~set_toolbar_mode .

    METHODS free .
    METHODS constructor
      IMPORTING
        !parent                     TYPE REF TO zcl_tscreen OPTIONAL
        !id                         TYPE string OPTIONAL
        !container_name             TYPE c OPTIONAL
        !content                    TYPE string
        !length_content             TYPE i OPTIONAL
        VALUE(style)                TYPE i OPTIONAL
        !wordwrap_mode              TYPE i OPTIONAL
        !length_line                TYPE i OPTIONAL
        !wordwrap_to_linebreak_mode TYPE i OPTIONAL
        !filedrop_mode              TYPE i OPTIONAL
        VALUE(lifetime)             TYPE i OPTIONAL
        VALUE(name)                 TYPE string OPTIONAL
      RAISING
        cx_uuid_error
        zcx_tscreen .
    METHODS get_editor
      RETURNING
        VALUE(editor) TYPE REF TO cl_gui_textedit .

    METHODS zif_tscreen_component~change_editable
        REDEFINITION .
    METHODS zif_tscreen_component~change_visibility
        REDEFINITION .
    METHODS zif_tscreen_component~initialize_by_tscreen
        REDEFINITION .
    METHODS zif_tscreen_component~set_component_attr_by_setting
        REDEFINITION .
  PROTECTED SECTION.

    DATA container TYPE REF TO cl_gui_container .
    DATA editor TYPE REF TO cl_gui_textedit .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TEXT_EDITOR IMPLEMENTATION.


  METHOD constructor.

    super->constructor( id ).

    IF container_name IS NOT INITIAL.

      "创建容器
      CREATE OBJECT container TYPE cl_gui_custom_container
        EXPORTING
          container_name              = container_name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.
      IF sy-subrc NE 0.
        ##NO_TEXT        zcx_tscreen=>raise_text( 'Can not create container' && container_name && ' for editor.' ).
      ENDIF.

    ELSE.

      container = cl_gui_custom_container=>screen0.

    ENDIF.

    CREATE OBJECT editor
      EXPORTING
        max_number_chars           = length_content
        style                      = style
        wordwrap_mode              = wordwrap_mode
        wordwrap_position          = length_line
        wordwrap_to_linebreak_mode = wordwrap_to_linebreak_mode
        filedrop_mode              = filedrop_mode
        parent                     = container
        lifetime                   = lifetime
        name                       = name
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      DATA msg TYPE string.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO msg.
      zcx_tscreen=>raise_text( msg ).
    ENDIF.

    set_text( content ).

    initialize_by_tscreen( parent ).

*    "由控件自身负责处理配置表控制的实现
*    call_attr_method_by_parent = abap_false.

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD free.

    editor->free( ).
*    container->free( )."非customer的container不要free，只要解除引用即可

    FREE editor.
    FREE container.

  ENDMETHOD.


  METHOD zif_text_editor~get_text.
    editor->get_textstream( IMPORTING text = me->text ).
    text = me->text.
  ENDMETHOD.


  METHOD zif_text_editor~set_statusbar_mode.
    me->editor->set_statusbar_mode( mode ).
    editor = me.
  ENDMETHOD.


  METHOD zif_text_editor~set_status_text.
    me->title = title.
    me->editor->set_status_text( text ).
    editor = me.
  ENDMETHOD.


  METHOD zif_text_editor~set_text.
    me->text = text.
    me->editor->set_textstream( me->text ).
    editor = me.
  ENDMETHOD.


  METHOD zif_text_editor~set_toolbar_mode.
    me->editor->set_toolbar_mode( mode ).
    editor = me.
  ENDMETHOD.


  METHOD zif_tscreen_component~change_editable.

    CHECK visibility = zif_tscreen_component=>c_component_visible."控件显示时，编辑性才有意义

    me->display_mode = display_mode.

    CASE display_mode.
      WHEN zcl_tscreen=>display_mode_show.
        editor->set_readonly_mode( 1 ).
      WHEN OTHERS.
        editor->set_readonly_mode( 0 ).
    ENDCASE.

    component = get_component( ).

  ENDMETHOD.


  METHOD zif_tscreen_component~change_visibility.

    "控件的显隐性可不取决于屏幕的显隐性
    IF visibility IS SUPPLIED.
      me->visibility = visibility.
    ENDIF.

    IF me->visibility = zif_tscreen_component=>c_component_visible.
      editor->set_visible( zif_tscreen_component=>c_component_visible ).
    ELSE.
      editor->set_visible( zif_tscreen_component=>c_component_invisible ).
    ENDIF.

    component = get_component( ).

  ENDMETHOD.


  METHOD zif_tscreen_component~set_component_attr_by_setting.

    CHECK parent->dynpro_attr_setting IS NOT INITIAL.

    FIELD-SYMBOLS <setting> LIKE LINE OF parent->dynpro_attr_setting.

    ##WARN_OK
    READ TABLE parent->dynpro_attr_setting ASSIGNING <setting> WITH KEY tc_name = id
                                                                        zmode   = parent->display_mode
                                                                        BINARY SEARCH.
    IF sy-subrc <> 0.
      "没有设置具体模式，再使用通用模式
      ##WARN_OK
      READ TABLE parent->dynpro_attr_setting ASSIGNING <setting> WITH KEY tc_name = id
                                                                          zmode   = '*'
                                                                          BINARY SEARCH.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.

      change_visibility( zif_tscreen_component=>c_component_visible ).

      CASE abap_true.
        WHEN <setting>-xoblg."标志: 必需输入字段 ?


        WHEN <setting>-xoptn."指示符: 可选字段 ?

          change_editable( zcl_tscreen=>display_mode_modify ).

        WHEN <setting>-xnodi."标识: 字段隐藏

          change_visibility( zif_tscreen_component=>c_component_invisible ).

        WHEN <setting>-xdisp."标志: 字段仅可显示 ?

          change_editable( zcl_tscreen=>display_mode_show ).

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD get_editor.
    editor = me->editor.
  ENDMETHOD.


  METHOD zif_text_editor~save_text.
    get_text( ).
    MESSAGE 'SAVED.' TYPE 'S'.
    editor = me.
  ENDMETHOD.


  METHOD zif_tscreen_component~initialize_by_tscreen.

    TRY.
        super->zif_tscreen_component~initialize_by_tscreen( tscreen ).
        CAST zcl_tscreen_with_components( tscreen )->add_component( group = zif_tscreen_component=>c_component_editor component = me ).
        change_editable( tscreen->display_mode ).
      CATCH zcx_tscreen ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
