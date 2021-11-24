interface ZIF_TEXT_EDITOR
  public .


  data TEXT type STRING read-only .
  data TITLE type STRING read-only .

  methods SET_STATUS_TEXT
    importing
      !TEXT type C
      !TITLE type C optional
    returning
      value(EDITOR) type ref to ZIF_TEXT_EDITOR .
  methods SET_TOOLBAR_MODE
    importing
      !MODE type I
    returning
      value(EDITOR) type ref to ZIF_TEXT_EDITOR .
  methods SET_STATUSBAR_MODE
    importing
      !MODE type I
    returning
      value(EDITOR) type ref to ZIF_TEXT_EDITOR .
  methods SET_TEXT
    importing
      !TEXT type ANY
    returning
      value(EDITOR) type ref to ZIF_TEXT_EDITOR .
  methods SAVE_TEXT
    returning
      value(EDITOR) type ref to ZIF_TEXT_EDITOR .
  methods GET_TEXT
    returning
      value(TEXT) type STRING .
endinterface.
