interface ZIF_TRANSPORTING_REQUEST
  public .


  types TY_E071 type E071 .
  types:
    tty_e071 TYPE STANDARD TABLE OF ty_e071 WITH DEFAULT KEY .
  types TY_E071K type E071K .
  types:
    tty_e071k TYPE STANDARD TABLE OF ty_e071k WITH DEFAULT KEY .

  methods ASSOCIATE
    importing
      !ORDER_TYPE type TRFUNCTION default 'K'
      !TASK_TYPE type TRFUNCTION default 'S'
      !CATEGORY type E070-KORRDEV default 'SYST'
    returning
      value(TR_TOOL) type ref to ZIF_TRANSPORTING_REQUEST .
  methods ENTRYS
    importing
      !TABLE type TABLE
      value(DDIC_TYPE) type TROBJ_NAME optional "#EC CI_VALPAR
    returning
      value(TR_TOOL) type ref to ZIF_TRANSPORTING_REQUEST .
  methods ENTRY
    importing
      value(OBJNAME) type TROBJ_NAME "#EC CI_VALPAR
      value(OBJFUNC) type OBJFUNC default 'K'
      value(TABKEY) type TROBJ_NAME "#EC CI_VALPAR
    returning
      value(TR_TOOL) type ref to ZIF_TRANSPORTING_REQUEST .
  methods COMMIT
    importing
      !SHOW_ERROR type ABAP_BOOL default ABAP_TRUE
    returning
      value(IS_COMMITED) type ABAP_BOOL .
endinterface.
