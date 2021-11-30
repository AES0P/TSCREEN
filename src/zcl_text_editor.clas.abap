class ZCL_TEXT_EDITOR definition
  public
  inheriting from ZCL_TCOMPONENT
  create public .

public section.

  interfaces ZIF_TEXT_EDITOR .

  aliases C_COMPONENT_EDITOR
    for ZIF_TSCREEN_COMPONENT~C_COMPONENT_EDITOR .
  aliases C_COMPONENT_TC
    for ZIF_TSCREEN_COMPONENT~C_COMPONENT_TC .
  aliases TEXT
    for ZIF_TEXT_EDITOR~TEXT .
  aliases TITLE
    for ZIF_TEXT_EDITOR~TITLE .
  aliases GET_TEXT
    for ZIF_TEXT_EDITOR~GET_TEXT .
  aliases INITIALIZE_BY_TSCREEN
    for ZIF_TSCREEN_COMPONENT~INITIALIZE_BY_TSCREEN .
  aliases SAVE_TEXT
    for ZIF_TEXT_EDITOR~SAVE_TEXT .
  aliases SET_STATUSBAR_MODE
    for ZIF_TEXT_EDITOR~SET_STATUSBAR_MODE .
  aliases SET_STATUS_TEXT
    for ZIF_TEXT_EDITOR~SET_STATUS_TEXT .
  aliases SET_TEXT
    for ZIF_TEXT_EDITOR~SET_TEXT .
  aliases SET_TOOLBAR_MODE
    for ZIF_TEXT_EDITOR~SET_TOOLBAR_MODE .

  methods FREE .
  methods CONSTRUCTOR
    importing
      !PARENT type ref to ZCL_TSCREEN optional
      !ID type STRING optional
      !CONTAINER_NAME type C optional
      !CONTENT type STRING
      !LENGTH_CONTENT type I optional
      value(STYLE) type I optional
      !WORDWRAP_MODE type I optional
      !LENGTH_LINE type I optional
      !WORDWRAP_TO_LINEBREAK_MODE type I optional
      !FILEDROP_MODE type I optional
      value(LIFETIME) type I optional
      value(NAME) type STRING optional
    raising
      CX_UUID_ERROR
      ZCX_TSCREEN .
  methods GET_EDITOR
    returning
      value(EDITOR) type ref to CL_GUI_TEXTEDIT .

  methods ZIF_TSCREEN_COMPONENT~CHANGE_EDITABLE
    redefinition .
  methods ZIF_TSCREEN_COMPONENT~CHANGE_VISIBILITY
    redefinition .
  methods ZIF_TSCREEN_COMPONENT~INITIALIZE_BY_TSCREEN
    redefinition .
  methods ZIF_TSCREEN_COMPONENT~SET_COMPONENT_ATTR_BY_SETTING
    redefinition .
  PROTECTED SECTION.

    DATA container TYPE REF TO cl_gui_container .
    DATA editor TYPE REF TO cl_gui_textedit .
    DATA parent TYPE REF TO zcl_tscreen .
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
        zcx_tscreen=>raise_text( 'Can not create container' && container_name && ' for editor.' ).
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

    CHECK tscreen IS BOUND.

    me->parent  = tscreen.
    CAST zcl_tscreen_with_components( tscreen )->add_component( group = zif_tscreen_component=>c_component_editor component = me ).

    change_editable( tscreen->display_mode ).

  ENDMETHOD.
ENDCLASS.
