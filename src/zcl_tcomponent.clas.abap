class ZCL_TCOMPONENT definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_TSCREEN_COMPONENT
      abstract methods CHANGE_EDITABLE
                       CHANGE_VISIBILITY
                       SET_COMPONENT_ATTR_BY_SETTING
      data values CALL_ATTR_METHOD_BY_PARENT = ABAP_TRUE
                  VISIBILITY = ZIF_TSCREEN_COMPONENT=>C_COMPONENT_VISIBLE .

  aliases CALL_ATTR_METHOD_BY_PARENT
    for ZIF_TSCREEN_COMPONENT~CALL_ATTR_METHOD_BY_PARENT .
  aliases CHANGE_EDITABLE
    for ZIF_TSCREEN_COMPONENT~CHANGE_EDITABLE .
  aliases CHANGE_VISIBILITY
    for ZIF_TSCREEN_COMPONENT~CHANGE_VISIBILITY .
  aliases C_COMPONENT_DISPLAY
    for ZIF_TSCREEN_COMPONENT~C_COMPONENT_DISPLAY .
  aliases C_COMPONENT_EDITABLE
    for ZIF_TSCREEN_COMPONENT~C_COMPONENT_EDITABLE .
  aliases C_COMPONENT_INVISIBLE
    for ZIF_TSCREEN_COMPONENT~C_COMPONENT_INVISIBLE .
  aliases C_COMPONENT_VISIBLE
    for ZIF_TSCREEN_COMPONENT~C_COMPONENT_VISIBLE .
  aliases DISPLAY_MODE
    for ZIF_TSCREEN_COMPONENT~DISPLAY_MODE .
  aliases GET_COMPONENT
    for ZIF_TSCREEN_COMPONENT~GET_COMPONENT .
  aliases ID
    for ZIF_TSCREEN_COMPONENT~ID .
  aliases SET_COMPONENT_ATTR_BY_SETTING
    for ZIF_TSCREEN_COMPONENT~SET_COMPONENT_ATTR_BY_SETTING .
  aliases VISIBILITY
    for ZIF_TSCREEN_COMPONENT~VISIBILITY .

  methods CONSTRUCTOR
    importing
      !ID type STRING optional
    raising
      CX_UUID_ERROR .
protected section.

  data PARENT type ref to ZCL_TSCREEN .
  data TLOG type ref to ZIF_TLOG .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TCOMPONENT IMPLEMENTATION.


  METHOD constructor.
    IF id IS NOT INITIAL.
      me->id = id.
    ELSE.
      me->id = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_tscreen_component~get_component.
    component = me.
  ENDMETHOD.


  METHOD zif_tscreen_component~initialize_by_tscreen.

    IF tscreen IS NOT BOUND.
      zcx_tscreen=>raise_text( 'Parent is not bound' ).
    ENDIF.

    me->parent  = tscreen.
    tlog        = tscreen->tlog.

  ENDMETHOD.
ENDCLASS.
