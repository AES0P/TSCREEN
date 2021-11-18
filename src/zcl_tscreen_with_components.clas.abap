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

  methods ADD_COMPONENT
    importing
      !GROUP type STRING
      !COMPONENT type ref to ZIF_TSCREEN_COMPONENT .
  methods GET_COMPONENT
    importing
      !GROUP type STRING
      !ID type STRING
    returning
      value(COMPONENT) type ref to ZIF_TSCREEN_COMPONENT .
  methods DEL_COMPONENT
    importing
      !GROUP type STRING
      !ID type STRING
    returning
      value(DELETED) type ABAP_BOOL .
  methods GET_COMPONENTS_ITERATOR
    importing
      !GROUP type STRING
    returning
      value(ITERATOR) type ref to CL_OBJECT_COLLECTION_ITERATOR .

  methods ZIF_TSCREEN~EXIT
    redefinition .
  methods ZIF_TSCREEN~HANDLE_EVENT
    redefinition .
protected section.

  data COMPONENTS type TTY_COMPONENT .

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

    IF NOT line_exists( components[ group = group ] )."#EC CI_STDSEQ
      FIELD-SYMBOLS <component> TYPE ty_component.
      APPEND INITIAL LINE TO components ASSIGNING <component>.
      <component>-group      = group.
      <component>-components = NEW cl_object_collection( ).
      SORT components BY group.
    ENDIF.

    CHECK get_component( group = group id = component->id ) IS NOT BOUND.

    READ TABLE components ASSIGNING <component> WITH KEY group = group BINARY SEARCH.
    ASSERT sy-subrc = 0.

    <component>-counts += 1.
    <component>-components->add( component ).

  ENDMETHOD.


  METHOD change_screen_editable.

    super->change_screen_editable( ).

    FIELD-SYMBOLS <component> TYPE ty_component.
    LOOP AT components ASSIGNING <component>.

      DATA iterator TYPE REF TO cl_object_collection_iterator.
      iterator = <component>-components->get_iterator( ).

      WHILE iterator->has_next( )."#EC CI_NESTED
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
      WHILE iterator->has_next( )."#EC CI_NESTED
        component = iterator->get_next( ).
        TRY.
            CALL METHOD component->(method).
          CATCH cx_sy_dyn_call_error INTO DATA(error).
            error->get_text( ).
            RETURN.
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

      WHILE iterator->has_next( )."#EC CI_NESTED
        DATA component TYPE REF TO zif_tscreen_component.
        component ?= iterator->get_next( ).
        "有的控件的PBO事件可以在父屏幕的PBO里一起处理，有的不行，比如table control
        "如果控件非要和父屏幕的PBO一起处理，则
        "1、控件本身新增PBO方法即可，无需修改其它地方代码，框架会在调用完父屏幕的PBO后自动调用控件的PBO
        "2、控件的call_attr_method_by_parent 属性设为 abap_true
        CHECK component->call_attr_method_by_parent = abap_true.
        component->set_component_attr_by_setting( ).
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_tscreen~exit.

    CLEAR: components.

    super->exit( ).

  ENDMETHOD.


  METHOD zif_tscreen~handle_event.

    super->handle_event( event ).

    "主屏幕执行完事件后，会再执行控件的事件
    "如果主屏幕和控件具有相同的事件名，则两者的事件都会执行
    "如果控件具有和主屏幕不同的事件，则只执行控件的事件，反之亦然
    call_components_method( event ).

  ENDMETHOD.
ENDCLASS.