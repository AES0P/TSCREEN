CLASS zcl_tscreen_stack DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS zif_tscreen .

  PUBLIC SECTION.

    INTERFACES zif_tscreen_stack .

    ALIASES is_empty
      FOR zif_tscreen_stack~is_empty .
    ALIASES pop
      FOR zif_tscreen_stack~pop .
    ALIASES push
      FOR zif_tscreen_stack~push .
    ALIASES size
      FOR zif_tscreen_stack~size .
    ALIASES top
      FOR zif_tscreen_stack~top .

    TYPES:
      BEGIN OF ty_view,
        program     TYPE sy-cprog, "主程序
        dynnr_super TYPE sy-dynnr, "父屏幕号
        dynnr       TYPE sy-dynnr, "屏幕号
        tscreen     TYPE REF TO zif_tscreen,
      END OF ty_view .
    TYPES:
      tty_view TYPE STANDARD TABLE OF ty_view WITH KEY program dynnr .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(instance) TYPE REF TO zcl_tscreen_stack .
    METHODS clear
      RAISING
        zcx_tscreen .
    METHODS is_exists
      IMPORTING
        !program           TYPE sy-repid
        VALUE(dynnr_super) TYPE sy-dynnr OPTIONAL
        !dynnr             TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING
        VALUE(is_exists)   TYPE abap_bool .
    METHODS is_super_exists
      IMPORTING
        !program         TYPE sy-repid
        !dynnr_super     TYPE sy-dynnr
      RETURNING
        VALUE(is_exists) TYPE abap_bool .
    METHODS current
      IMPORTING
        !dynnr_super   TYPE sy-dynnr OPTIONAL
        !dynnr         TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING
        VALUE(tscreen) TYPE REF TO zif_tscreen
      RAISING
        zcx_tscreen .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA stack TYPE REF TO zcl_tscreen_stack .
    DATA tscreens TYPE tty_view .
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
                                                  dynnr       = dynnr. "#EC CI_STDSEQ
    IF sy-subrc = 0.
      tscreen = <view>-tscreen.
    ELSE.

      DATA tscreens_copy TYPE tty_view.
      tscreens_copy = tscreens.
      DELETE tscreens_copy WHERE dynnr_super IS INITIAL OR dynnr <> dynnr. "#EC CI_STDSEQ
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
                                                        dynnr   = dynnr_super. "#EC CI_STDSEQ
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
