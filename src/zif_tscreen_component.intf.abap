interface ZIF_TSCREEN_COMPONENT
  public .


  data ID type STRING read-only .
  data DISPLAY_MODE type ABAP_BOOL read-only .
  data VISIBILITY type ABAP_BOOL read-only .
  data CALL_ATTR_METHOD_BY_PARENT type ABAP_BOOL read-only .
  constants C_COMPONENT_VISIBLE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  constants C_COMPONENT_INVISIBLE type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants C_COMPONENT_EDITABLE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  constants C_COMPONENT_DISPLAY type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.

  methods GET_COMPONENT
    returning
      value(COMPONENT) type ref to ZIF_TSCREEN_COMPONENT .
  methods CHANGE_VISIBILITY
    importing
      !VISIBILITY type ABAP_BOOL optional
    returning
      value(COMPONENT) type ref to ZIF_TSCREEN_COMPONENT .
  methods CHANGE_EDITABLE
    importing
      !DISPLAY_MODE type ABAP_BOOL
    returning
      value(COMPONENT) type ref to ZIF_TSCREEN_COMPONENT .
  methods SET_COMPONENT_ATTR_BY_SETTING .
endinterface.
