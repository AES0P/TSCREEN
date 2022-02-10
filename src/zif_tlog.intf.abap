interface ZIF_TLOG
  public .


  methods CLEAR
    returning
      value(INSTANCE) type ref to ZIF_TLOG .
  methods ADD_LOG
    importing
      !TYPE type SY-MSGTY default 'S'
      !CONTENT type STRING
      !DYNNR type SY-DYNNR default SY-DYNNR
      !UCOMM type SY-UCOMM default SY-UCOMM
    returning
      value(INSTANCE) type ref to ZIF_TLOG .
  methods SAVE_LOG
    importing
      !GUID type GUID
      !SYMBOL type ICON_D optional
      !IF_COMMIT type ABAP_BOOL default ABAP_TRUE
    returning
      value(INSTANCE) type ref to ZIF_TLOG .
  methods SHOW_LOG
    importing
      !STYLE type C default '1'
    returning
      value(INSTANCE) type ref to ZIF_TLOG .
  methods COMMIT
    returning
      value(INSTANCE) type ref to ZIF_TLOG .
endinterface.
