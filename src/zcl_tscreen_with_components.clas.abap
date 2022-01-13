class ZCL_TSCREEN_WITH_COMPONENTS definition
  public
  inheriting from ZCL_TSCREEN
  abstract
  create public .

public section.

  types:
    BEGIN OF ty_component,
        group      TYPE string,
        counts     TYPE i,
        components TYPE REF TO cl_object_collection,
      END OF ty_component .
  types:
    tty_component TYPE STANDARD TABLE OF ty_component WITH DEFAULT KEY .

  constants CAPTURE_FROM_OUTER_TO_INNER type I value 1 ##NO_TEXT.
  constants CAPTURE_FROM_INNER_TO_OUTER type I value 2 ##NO_TEXT.

  methods ADD_COMPONENT
    importing
      !GROUP type STRING
      !COMPONENT type ref to ZIF_TSCREEN_COMPONENT
    raising
      ZCX_TSCREEN .
  methods GET_COMPONENT
    importing
      !GROUP type STRING
      !ID type STRING
    returning
      value(COMPONENT) type ref to ZIF_TSCREEN_COMPONENT
    raising
      ZCX_TSCREEN .
  methods DEL_COMPONENT
    importing
      !GROUP type STRING
      !ID type STRING
    returning
      value(DELETED) type ABAP_BOOL
    raising
      ZCX_TSCREEN .
  methods GET_COMPONENTS_ITERATOR
    importing
      !GROUP type STRING
    returning
      value(ITERATOR) type ref to CL_OBJECT_COLLECTION_ITERATOR .
  methods STOP_PROPAGATION
    importing
      !STOP type ABAP_BOOL default ABAP_TRUE .

  methods ZIF_TSCREEN~EXIT
    redefinition .
  methods ZIF_TSCREEN~HANDLE_EVENT
    redefinition .
protected section.

  data CAPTURE type I value ZCL_TSCREEN_WITH_COMPONENTS=>CAPTURE_FROM_OUTER_TO_INNER ##NO_TEXT.
  data PROPAGATION_STOP type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data COMPONENTS type TTY_COMPONENT .

  methods ADD_COMPONENTS
  abstract .
  methods CALL_COMPONENTS_METHOD
    importing
      !METHOD type SEOCPDKEY-CPDNAME .

  methods CHANGE_SCREEN_EDITABLE
    redefinition .
  methods SET_ELEMENT_ATTR_BY_SETTING
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_TSCREEN_WITH_COMPONENTS IMPLEMENTATION.


  METHOD add_component.

    "没有当前控件所属的控件组，就新增一组控件
    IF NOT line_exists( components[ group = group ] ).   "#EC CI_STDSEQ
      FIELD-SYMBOLS <component> TYPE ty_component.
      APPEND INITIAL LINE TO components ASSIGNING <component>.
      <component>-group      = group.
      <component>-components = NEW cl_object_collection( ).
      SORT components BY group.
    ENDIF.

    "不允许重复添加控件
    IF get_component( group = group id = component->id ) IS BOUND.
      zcx_tscreen=>raise_text( 'COMPONENT ALREADY ADDED,DO NOT ADD AGAIN.' ).
    ENDIF.

    "向所属控件组添加控件
    READ TABLE components ASSIGNING <component> WITH KEY group = group BINARY SEARCH.
    ASSERT sy-subrc = 0.

    ADD 1 TO <component>-counts.
    <component>-components->add( component ).

  ENDMETHOD.


  METHOD change_screen_editable.

    super->change_screen_editable( ).

    FIELD-SYMBOLS <component> TYPE ty_component.
    LOOP AT components ASSIGNING <component>.

      DATA iterator TYPE REF TO cl_object_collection_iterator.
      iterator = <component>-components->get_iterator( ).

      WHILE iterator->has_next( ).                       "#EC CI_NESTED
        CAST zif_tscreen_component( iterator->get_next( ) )->change_visibility( )->change_editable( display_mode ).
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD del_component.
    components[ group = group ]-components->remove( get_component( group = group id = id ) ). "#EC CI_STDSEQ
  ENDMETHOD.


  METHOD get_component.

    DATA iterator TYPE REF TO cl_object_collection_iterator.
    iterator = get_components_iterator( group ).

    WHILE iterator->has_next( ).

      component = CAST zif_tscreen_component( iterator->get_next( ) ).

      IF component->id = id.
        RETURN.
      ELSE.
        FREE component.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD call_components_method.

    FIELD-SYMBOLS <component> TYPE ty_component.
    LOOP AT components ASSIGNING <component>.

      DATA iterator TYPE REF TO cl_object_collection_iterator.
      iterator = get_components_iterator( <component>-group ).

      DATA component TYPE REF TO object.
      WHILE iterator->has_next( ).                       "#EC CI_NESTED
        component = iterator->get_next( ).
        TRY.
            CALL METHOD component->(method).
          CATCH cx_sy_dyn_call_error INTO DATA(error).
            error->get_text( ).
*            RETURN.
        ENDTRY.
      ENDWHILE.

      FREE iterator.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_components_iterator.

    FIELD-SYMBOLS <component> TYPE ty_component.
    READ TABLE components ASSIGNING <component> WITH KEY group = group BINARY SEARCH.
    ASSERT sy-subrc = 0.

    iterator = <component>-components->get_iterator( ).

  ENDMETHOD.


  METHOD set_element_attr_by_setting.

    super->set_element_attr_by_setting( ).

    FIELD-SYMBOLS <component> TYPE ty_component.
    LOOP AT components ASSIGNING <component>.

      DATA iterator TYPE REF TO cl_object_collection_iterator.
      iterator = <component>-components->get_iterator( ).

      WHILE iterator->has_next( ).                       "#EC CI_NESTED
        DATA component TYPE REF TO zif_tscreen_component.
        component ?= iterator->get_next( ).
        "有的控件的PBO事件可以在父屏幕的PBO里一起处理，有的不行，比如table control
        "如果控件非要和父屏幕的PBO一起处理，则
        "1、控件本身新增PBO方法即可，无需修改其它地方代码，框架会按事件规则（冒泡或捕获）自动调用父类和控件的PBO事件
        "2、控件的call_attr_method_by_parent 属性设为 abap_true
        CHECK component->call_attr_method_by_parent = abap_true.
        component->set_component_attr_by_setting( ).
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_tscreen~exit.

    call_components_method( 'FREE' ).
    CLEAR: components.

    super->exit( ).

  ENDMETHOD.


  METHOD zif_tscreen~handle_event.

    "如果主屏幕和控件具有相同的事件名，则两者的事件都会执行
    "如果控件具有主屏幕没有的事件，则只执行控件的事件，反之亦然
    CASE capture.
      WHEN zcl_tscreen_with_components=>capture_from_outer_to_inner."捕获
        "父屏幕执行完事件后，再执行控件的事件
        super->handle_event( event ).
        call_components_method( event ).
      WHEN zcl_tscreen_with_components=>capture_from_inner_to_outer."冒泡
        "控件事件执行完后，再执行父亲屏幕的事件
        get_current_info( ).
        stop_propagation( abap_false ).
        call_components_method( event ).
        CHECK propagation_stop = abap_false."如果阻止冒泡则不执行父屏幕事件
        super->handle_event( event ).
    ENDCASE.

  ENDMETHOD.


  METHOD stop_propagation.
    propagation_stop = stop.
  ENDMETHOD.
ENDCLASS.
