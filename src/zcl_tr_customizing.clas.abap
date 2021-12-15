class ZCL_TR_CUSTOMIZING definition
  public
  inheriting from ZCL_TRANSPORTING_REQUEST
  final
  create private .

public section.

  class-methods GET_INSTANCE
    returning
      value(TOOL) type ref to ZIF_TRANSPORTING_REQUEST .

  methods ZIF_TRANSPORTING_REQUEST~ASSOCIATE
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA instance TYPE REF TO zcl_tr_customizing .
ENDCLASS.



CLASS ZCL_TR_CUSTOMIZING IMPLEMENTATION.


  METHOD get_instance.

    IF instance IS NOT BOUND.
      CREATE OBJECT instance.
    ENDIF.

    tool = instance.

  ENDMETHOD.


  METHOD zif_transporting_request~associate.
    tr_tool = super->associate(
                       order_type    = 'W'
                       task_type     = 'Q'
                       category      = 'CUST' ).
  ENDMETHOD.
ENDCLASS.
