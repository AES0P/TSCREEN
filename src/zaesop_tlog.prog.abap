*&---------------------------------------------------------------------*
*& Report ZAESOP_TLOG
*&---------------------------------------------------------------------*
*&  执行前请前往SLG0配置日志对象 ZGBC 及其子对象 ZGBC001
*&---------------------------------------------------------------------*
REPORT zaesop_tlog.

*&---------------------------------------------------------------------*
*&　　　　SELECTION-SCREEN
*&---------------------------------------------------------------------*
PARAMETERS: p_object TYPE balobj_d  DEFAULT 'ZGBC',
            p_subobj TYPE balsubobj DEFAULT 'ZGBC001',
            p_id     TYPE balnrext  DEFAULT 'ZGBCR_LOG_TEST'.

PARAMETERS p_level  TYPE ballevel      DEFAULT '1'.
PARAMETERS p_date   TYPE recadatefrom  DEFAULT sy-datum.
PARAMETERS p_delnot TYPE recabool      DEFAULT abap_true.
PARAMETERS p_commit TYPE c AS CHECKBOX DEFAULT 'X'.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_gbc_cs_delete DEFINITION CREATE PUBLIC FINAL.

  PUBLIC SECTION.
    CLASS-METHODS main.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_gbc_cs_delete=>main( ).

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_gbc_cs_delete IMPLEMENTATION.

  METHOD main.

    DATA(log) = zcl_tlog=>get_instance( object = p_object subobject = p_subobj identity = p_id level = p_level del_date = p_date del_not_before = p_delnot ).

    "记录string
    log->success( 'success success success success success success success success success success success success success success success success success success success success success success success ' ).
    log->info( 'info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info info ' ).
    log->warning( 'warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning warning ' ).
    log->error( 'error error error error error error error error error error error error error error error error error error error error error error error error error error error error error error error error error error ' ).
    log->abort( 'abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort abort ' ).
    log->add_from_string( type = 'A' content = 'add_from_string add_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_stringadd_from_string' ).

    "记录debug
    log->debug( '123' ).

    "记录系统信息
    MESSAGE a001(00) WITH '111111111111111111111111111111111111111111111'
                          '222222222222222222222222222222222222222222222'
                          '333333333333333333333333333333333333333333333'
                          '44444444444444444444444444444444444444444445555'
                     INTO DATA(dummy).
    log->add_from_symsg( ).

    "记录异常信息
    log->add_from_exception( NEW cx_sy_range_out_of_bounds( ) ).

    "记录BAPI返回表
    DATA lt_bapiret2 TYPE STANDARD TABLE OF bapiret2.
    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = '123'
      TABLES
        return        = lt_bapiret2.

    log->add_from_bapi( it_bapiret = lt_bapiret2 ).

    "记录BAPI返回结构
    log->add_from_bapi( is_bapiret = lt_bapiret2[ 1 ] ).

    "使用标准SLG1展示本次记录
    log->display_in_slg1( abap_true ).

    "使用SALV展示本次记录数据
    log->display_as_alv_popup( ).

    IF p_commit = abap_true.
      log->commit( ).
    ENDIF.

    "用完一定要free
    log->free( ).

  ENDMETHOD.

ENDCLASS.
