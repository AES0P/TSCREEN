CLASS zcl_tr_customizing DEFINITION
  PUBLIC
  INHERITING FROM zcl_transporting_request
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(tool) TYPE REF TO zif_transporting_request .

    METHODS zif_transporting_request~associate
        REDEFINITION .
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
