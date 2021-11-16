interface ZIF_TSCREEN_STACK
  public .


  methods SIZE
    returning
      value(SIZE) type I .
  methods IS_EMPTY
    returning
      value(IS_EMPTY) type ABAP_BOOL .
  methods PUSH
    importing
      !TSCREEN type ref to ZIF_TSCREEN .
  methods POP
    raising
      ZCX_TSCREEN .
  methods TOP
    returning
      value(TSCREEN) type ref to ZIF_TSCREEN
    raising
      ZCX_TSCREEN .
endinterface.
