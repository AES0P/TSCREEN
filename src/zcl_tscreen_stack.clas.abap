class ZCL_TSCREEN_STACK definition
  public
  create private

  global friends ZIF_TSCREEN .

public section.

  interfaces ZIF_TSCREEN_STACK .

  aliases IS_EMPTY
    for ZIF_TSCREEN_STACK~IS_EMPTY .
  aliases POP
    for ZIF_TSCREEN_STACK~POP .
  aliases PUSH
    for ZIF_TSCREEN_STACK~PUSH .
  aliases SIZE
    for ZIF_TSCREEN_STACK~SIZE .
  aliases TOP
    for ZIF_TSCREEN_STACK~TOP .

  types:
    BEGIN OF ty_view,
        program     TYPE sy-cprog, "主程序
        dynnr_super TYPE sy-dynnr, "父屏幕号
        dynnr       TYPE sy-dynnr, "屏幕号
        tscreen     TYPE REF TO zif_tscreen,
      END OF ty_view .
  types:
    tty_view TYPE STANDARD TABLE OF ty_view WITH KEY program dynnr .

  class-methods GET_INSTANCE
    returning
      value(INSTANCE) type ref to ZCL_TSCREEN_STACK .
  methods CLEAR
    raising
      ZCX_TSCREEN .
  methods IS_EXISTS
    importing
      !PROGRAM type SY-REPID
      value(DYNNR_SUPER) type SY-DYNNR optional
      !DYNNR type SY-DYNNR default SY-DYNNR
    returning
      value(IS_EXISTS) type ABAP_BOOL .
  methods IS_SUPER_EXISTS
    importing
      !PROGRAM type SY-REPID
      !DYNNR_SUPER type SY-DYNNR
    returning
      value(IS_EXISTS) type ABAP_BOOL .
  methods CURRENT
    importing
      !DYNNR_SUPER type SY-DYNNR optional
      !DYNNR type SY-DYNNR default SY-DYNNR
    returning
      value(TSCREEN) type ref to ZIF_TSCREEN
    raising
      ZCX_TSCREEN .
  PROTECTED SECTION.
private section.

  class-data STACK type ref to ZCL_TSCREEN_STACK .
  data TSCREENS type TTY_VIEW .
ENDCLASS.



CLASS ZCL_TSCREEN_STACK IMPLEMENTATION.


  METHOD clear.

    FIELD-SYMBOLS <view> LIKE LINE OF tscreens.
    LOOP AT tscreens ASSIGNING <view>.
      <view>-tscreen->exit( ).
    ENDLOOP.

    FREE tscreens.

    IF stack IS BOUND.
      FREE: stack.
    ENDIF.

  ENDMETHOD.


  METHOD current.

    FIELD-SYMBOLS <view> LIKE LINE OF tscreens.
    READ TABLE tscreens ASSIGNING <view> WITH KEY dynnr_super = dynnr_super
                                                  dynnr       = dynnr."#EC CI_STDSEQ
    IF sy-subrc = 0.
      tscreen = <view>-tscreen.
    ELSE.

      DATA tscreens_copy TYPE tty_view.
      tscreens_copy = tscreens.
      DELETE tscreens_copy WHERE dynnr_super IS INITIAL OR dynnr <> dynnr."#EC CI_STDSEQ
      READ TABLE tscreens_copy ASSIGNING <view> INDEX lines( tscreens_copy ).
      IF sy-subrc = 0.
        tscreen = <view>-tscreen.
      ELSE.
        zcx_tscreen=>raise_text( 'No view found.' ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF stack IS NOT BOUND.
      CREATE OBJECT stack.
    ENDIF.

    instance = stack.

  ENDMETHOD.


  METHOD is_exists.

    IF dynnr_super = dynnr.
      CLEAR dynnr_super.
    ENDIF.

    READ TABLE tscreens TRANSPORTING NO FIELDS WITH KEY program     = program
                                                        dynnr_super = dynnr_super
                                                        dynnr       = dynnr. "#EC CI_STDSEQ
    CHECK sy-subrc = 0.
    is_exists = abap_true.

  ENDMETHOD.


  METHOD is_super_exists.

    READ TABLE tscreens TRANSPORTING NO FIELDS WITH KEY program = program
                                                        dynnr   = dynnr_super."#EC CI_STDSEQ
    CHECK sy-subrc = 0.
    is_exists = abap_true.

  ENDMETHOD.


  METHOD zif_tscreen_stack~is_empty.
    IF size( ) = 0.
      is_empty = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_tscreen_stack~pop.
    DELETE tscreens INDEX lines( tscreens )."栈顶出栈
    IF sy-subrc <> 0.
      zcx_tscreen=>raise_text( 'Empty stack.' ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_tscreen_stack~push.

    IF tscreen->dynpro_type = zif_tscreen=>dynpro_type_subscreen.
      "添加子屏幕时，如果父屏幕不存在，则不允许添加
      IF NOT is_super_exists( program = tscreen->program dynnr_super = tscreen->dynnr_super ).
        RETURN.
      ENDIF.
    ENDIF.

    "不允许重复添加
    CHECK NOT is_exists( program = tscreen->program dynnr = tscreen->dynnr dynnr_super = tscreen->dynnr_super ).

    FIELD-SYMBOLS <view> LIKE LINE OF tscreens.
    APPEND INITIAL LINE TO tscreens ASSIGNING <view>."插入栈顶

    <view>-program     = tscreen->program.
    <view>-dynnr_super = tscreen->dynnr_super.
    <view>-dynnr       = tscreen->dynnr.
    <view>-tscreen     = tscreen.

  ENDMETHOD.


  METHOD zif_tscreen_stack~size.
    size = lines( tscreens ).
  ENDMETHOD.


  METHOD zif_tscreen_stack~top.

    FIELD-SYMBOLS <view> LIKE LINE OF tscreens.
    READ TABLE tscreens ASSIGNING <view> INDEX lines( tscreens ).
    IF sy-subrc = 0.
      tscreen = <view>-tscreen.
    ELSE.
      zcx_tscreen=>raise_text( 'No view found.' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
