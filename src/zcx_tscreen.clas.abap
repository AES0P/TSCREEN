class ZCX_TSCREEN definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .

  class-data ERROR type STRING .
  data SYST_AT_RAISE type SYST .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ERROR type STRING optional
      !SYST_AT_RAISE type SYST optional .
  class-methods RAISE
    raising
      ZCX_TSCREEN .
  class-methods RAISE_TEXT
    importing
      !TEXT type STRING
    raising
      ZCX_TSCREEN .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_TSCREEN IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->ERROR = ERROR .
me->SYST_AT_RAISE = SYST_AT_RAISE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD if_message~get_longtext.

    IF   me->error         IS NOT INITIAL
      OR me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this as longtext as well
*--------------------------------------------------------------------*
      result = me->get_text( ).
    ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
      result = super->if_message~get_longtext( preserve_newlines = preserve_newlines ).
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_text.

    IF me->error IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this
*--------------------------------------------------------------------*
      result = me->error .
    ELSEIF me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied by syst create messagetext now
*--------------------------------------------------------------------*
      MESSAGE ID syst_at_raise-msgid TYPE syst_at_raise-msgty NUMBER syst_at_raise-msgno
           WITH  syst_at_raise-msgv1 syst_at_raise-msgv2 syst_at_raise-msgv3 syst_at_raise-msgv4
           INTO  result.
    ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
      CALL METHOD super->if_message~get_text
        RECEIVING
          result = result.
    ENDIF.
  ENDMETHOD.


  METHOD RAISE.
    RAISE EXCEPTION TYPE zcx_tscreen
      EXPORTING
        error = error.
  ENDMETHOD.


  METHOD raise_text.
    zcl_tlog=>get_instance( object         = zcl_tscreen_root=>log_object
                            subobject      = zcl_tscreen_root=>log_sub_object
                            identity       = CONV balnrext( sy-cprog )
                            level          = zcl_tscreen_root=>if_log_level
                            del_not_before = abap_true )->add_log( type = 'A' content = text ).
    RAISE EXCEPTION TYPE zcx_tscreen
      EXPORTING
        error = text.
  ENDMETHOD.
ENDCLASS.
