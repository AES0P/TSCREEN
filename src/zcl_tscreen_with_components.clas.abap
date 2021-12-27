CLASS zcl_tscreen_with_components DEFINITION
  PUBLIC
  INHERITING FROM zcl_tscreen
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_component,
        group      TYPE string,
        counts     TYPE i,
        components TYPE REF TO cl_object_collection,
      END OF ty_component .
    TYPES:
      tty_component TYPE STANDARD TABLE OF ty_component WITH DEFAULT KEY .

    CONSTANTS capture_from_outer_to_inner TYPE i VALUE 1 ##NO_TEXT.
    CONSTANTS capture_from_inner_to_outer TYPE i VALUE 2 ##NO_TEXT.

    METHODS add_component
      IMPORTING
        !group     TYPE string
        !component TYPE REF TO zif_tscreen_component
      RAISING
        zcx_tscreen .
    METHODS get_component
      IMPORTING
        !group           TYPE string
        !id              TYPE string
      RETURNING
        VALUE(component) TYPE REF TO zif_tscreen_component
      RAISING
        zcx_tscreen .
    METHODS del_component
      IMPORTING
        !group         TYPE string
        !id            TYPE string
      RETURNING
        VALUE(deleted) TYPE abap_bool
      RAISING
        zcx_tscreen .
    METHODS get_components_iterator
      IMPORTING
        !group          TYPE string
      RETURNING
        VALUE(iterator) TYPE REF TO cl_object_collection_iterator .

    METHODS zif_tscreen~exit
        REDEFINITION .
    METHODS zif_tscreen~handle_event
        REDEFINITION .
  PROTECTED SECTION.

    DATA capture TYPE i VALUE zcl_tscreen_with_components=>capture_from_outer_to_inner ##NO_TEXT.
    DATA components TYPE tty_component .

    METHODS add_components
        ABSTRACT .
    METHODS call_components_method
      IMPORTING
        !method TYPE seocpdkey-cpdname .

    METHODS change_screen_editable
        REDEFINITION .
    METHODS set_element_attr_by_setting
        REDEFINITION .
  PRIVATE SECTION.
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
        "1、控件本身新增PBO方法即可，无需修改其它地方代码，框架会在调用完父屏幕的PBO后自动调用控件的PBO
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
        "主屏幕执行完事件后，再执行控件的事件
        super->handle_event( event ).
        call_components_method( event ).
      WHEN zcl_tscreen_with_components=>capture_from_inner_to_outer."冒泡
        "控件事件执行完后，再执行主屏幕的事件
        get_current_info( ).
        call_components_method( event ).
        super->handle_event( event ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
