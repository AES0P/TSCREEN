class ZCL_TLOG definition
  public
  final
  create private .

public section.

  types:
    BEGIN OF ty_mapping_rule,
        field TYPE fieldname,
        mean  TYPE fieldname,
      END OF ty_mapping_rule .
  types:
    BEGIN OF ty_return,
        type    TYPE char01,
        message TYPE char255,
      END OF ty_return .
  types:
    tty_mapping_rule TYPE STANDARD TABLE OF ty_mapping_rule WITH EMPTY KEY .

  data AUTO_COMMIT type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.

  class-methods GET_INSTANCE
    importing
      !OBJECT type BALOBJ_D
      !SUBOBJECT type BALSUBOBJ
      !IDENTITY type BALNREXT
      !LEVEL type BALLEVEL default '9'
      !DEL_DATE type RECADATEFROM optional
      !DEL_NOT_BEFORE type RECABOOL optional
      !AUTO_COMMIT type ABAP_BOOL optional
    returning
      value(LOG_INSTANCE) type ref to ZCL_TLOG .
  methods SUCCESS
    importing
      !CONTENT type STRING
    returning
      value(LOG) type ref to ZCL_TLOG .
  methods INFO
    importing
      !CONTENT type STRING
    returning
      value(LOG) type ref to ZCL_TLOG .
  methods WARNING
    importing
      !CONTENT type STRING
    returning
      value(LOG) type ref to ZCL_TLOG .
  methods ERROR
    importing
      !CONTENT type STRING
    returning
      value(LOG) type ref to ZCL_TLOG .
  methods ABORT
    importing
      !CONTENT type STRING
    returning
      value(LOG) type ref to ZCL_TLOG .
  methods DEBUG
    importing
      value(UNAME) type SY-UNAME optional
    returning
      value(LOG) type ref to ZCL_TLOG .
  methods ADD_FROM_STRING
    importing
      value(CONTENT) type STRING
      value(TYPE) type SYMSGTY .
  methods ADD_FROM_SYMSG
    importing
      !LEVEL type BALLEVEL default '9'
    returning
      value(MESSAGE) type RECAMSG .
  methods ADD_FROM_EXCEPTION
    importing
      !IO_EXCEPTION type ref to CX_ROOT
      !IF_CUMULATE type ABAP_BOOL optional
      !ID_DETLEVEL type BALLEVEL default '9'
      !ID_PROBCLASS type BALPROBCL optional
      !ID_TABNAME type TABNAME optional
      !ID_FIELDNAME type FIELDNAME optional
      !ID_VALUE type ANY optional
      !ID_INDEX type NUMERIC optional
      !ID_INTRENO type RECAINTRENO optional
      !ID_CONTEXT type ANY optional .
  methods ADD_FROM_BAPI
    importing
      !IT_BAPIRET type BAPIRETTAB optional
      !IS_BAPIRET type BAPIRET2 optional
      !IF_CUMULATE type ABAP_BOOL optional
    exporting
      !EF_ADD_ERROR type ABAP_BOOL
      !EF_ADD_WARNING type ABAP_BOOL .
  methods COMMIT
    exceptions
      ERROR .
  methods DISPLAY_IN_SLG1
    importing
      !AMODAL type ABAP_BOOL optional .
  methods GET_MSG_LIST
    returning
      value(MSG_LIST) type RE_T_MSG .
  methods GET_SALV
    changing
      !TABLE type STANDARD TABLE
    returning
      value(SALV) type ref to CL_SALV_TABLE .
  methods DISPLAY_AS_ALV_POPUP
    importing
      !START_COLUMN type I default 5   ##NUMBER_OK
      !START_LINE type I default 5   ##NUMBER_OK
      !END_COLUMN type I default 120   ##NUMBER_OK
      !END_LINE type I default 25  ##NUMBER_OK.
  methods FREE .
  methods GET_COLLECTOR
    returning
      value(COLLECTOR) type ref to IF_RECA_MESSAGE_LIST .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA instance TYPE REF TO zcl_tlog .
    DATA level TYPE ballevel .
    DATA object TYPE balobj_d .
    DATA subobject TYPE balsubobj .
    DATA identity TYPE balnrext .
    DATA collector TYPE REF TO if_reca_message_list .

    METHODS constructor
      IMPORTING
        !object         TYPE balobj_d
        !subobject      TYPE balsubobj
        !identity       TYPE balnrext
        !level          TYPE ballevel DEFAULT '9'
        !del_date       TYPE recadatefrom DEFAULT reca0_date-min
        !del_not_before TYPE recabool OPTIONAL
        !auto_commit    TYPE abap_bool OPTIONAL .
    METHODS is_valid_level
      IMPORTING
        !level                TYPE recamsg-detlevel
      RETURNING
        VALUE(is_valid_level) TYPE abap_bool .
    METHODS get_level
      IMPORTING
        VALUE(type)  TYPE sy-msgty
      RETURNING
        VALUE(level) TYPE ballevel .
ENDCLASS.



CLASS ZCL_TLOG IMPLEMENTATION.


  METHOD abort.
    add_from_string( content = content type = 'A' ).
    log = me.
  ENDMETHOD.


  METHOD add_from_bapi.

    collector->add_from_bapi(
                 EXPORTING
                   it_bapiret     = it_bapiret
                   is_bapiret     = is_bapiret
                   if_cumulate    = if_cumulate
                 IMPORTING
                   ef_add_error   = ef_add_error
                   ef_add_warning = ef_add_warning ).

  ENDMETHOD.


  METHOD add_from_exception.

    collector->add_from_exception( io_exception = io_exception
                                   if_cumulate  = if_cumulate
                                   id_detlevel  = id_detlevel
                                   id_probclass = id_probclass
                                   id_tabname   = id_tabname
                                   id_fieldname = id_fieldname
                                   id_value     = id_value
                                   id_index     = id_index
                                   id_intreno   = id_intreno
                                   id_context   = id_context  ).

  ENDMETHOD.


  METHOD add_from_string.

    CHECK get_level( type ) >= me->level.

    CONCATENATE sy-uzeit ':' content INTO content.

    cl_reca_string_services=>raise_string_as_symsg(
      EXPORTING
        id_string = content
        id_msgty  = type
      EXCEPTIONS
        message   = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      add_from_symsg( level = get_level( type ) ).
    ENDIF.

    IF auto_commit = abap_true.
      commit( )."for system store
    ENDIF.

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD add_from_symsg.
    collector->add_symsg( EXPORTING if_cumulate = abap_true id_detlevel = level IMPORTING es_message = message ).
  ENDMETHOD.


  METHOD commit.

    "调用对象方法, 保存消息到数据库
    collector->store(
      EXPORTING
        if_in_update_task = abap_false
      EXCEPTIONS
        error             = 1
        OTHERS            = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING error.
    ENDIF.

    "提交到数据库
    cf_reca_storable=>commit( ).

  ENDMETHOD.


  METHOD constructor.

    me->object    = object.
    me->subobject = subobject.
    me->identity  = identity.

    IF is_valid_level( level ).
      me->level = level.
    ELSE.
      me->level = '9'.
    ENDIF.

    "创建日志对象 gi_msglist, 传入为日志类型、子类型、外部信息
    collector = cf_reca_message_list=>create(
                                        id_object       = me->object   "(SLG0)
                                        id_subobject    = me->subobject  " (SLG0)
                                        id_extnumber    = me->identity "external info
                                        id_deldate      = del_date
                                        if_delnotbefore = del_not_before ).

    me->auto_commit = auto_commit.

  ENDMETHOD.


  METHOD debug.

    IF uname IS NOT SUPPLIED.
      uname = sy-uname.
    ENDIF.

    ##NO_TEXT  add_from_string( content = 'Started debug mode by - '  && uname type = 'A' ).

    log = me.

    ##NO_BREAK  BREAK-POINT.

  ENDMETHOD.


  METHOD display_as_alv_popup.

    DATA msg_list TYPE re_t_msg.
    msg_list = get_msg_list( ).                         "#EC CI_CONV_OK

    DATA: salv TYPE REF TO cl_salv_table.
    salv = get_salv( CHANGING table = msg_list ).

    salv->set_screen_popup( start_column = start_column
                            start_line   = start_line
                            end_column   = end_column
                            end_line     = end_line ).

    salv->display( ).

  ENDMETHOD.


  METHOD display_in_slg1.

    DATA: profile TYPE bal_s_prof.

    IF sy-batch IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = profile
        i_t_log_handle       = VALUE bal_t_logh( ( collector->get_handle( ) ) )
        i_amodal             = amodal
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5. "#EC CI_SUBRC
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD error.
    add_from_string( content = content type = 'E' ).
    log = me.
  ENDMETHOD.


  METHOD free.

    ##NO_TEXT    info( sy-uname && ' leave this transaction.' ).

    IF collector IS BOUND.
      collector->free( ).
    ENDIF.

    CLEAR: level,
           object,
           subobject,
           identity,
           collector,
           instance.

  ENDMETHOD.


  METHOD get_collector.
    collector = me->collector.
  ENDMETHOD.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          object         = object
          subobject      = subobject
          identity       = identity
          level          = level
          del_date       = del_date
          del_not_before = del_not_before
          auto_commit    = auto_commit.
    ENDIF.
    log_instance = instance.
  ENDMETHOD.


  METHOD get_level.
    "当日志级别大于等于控制打印级时, 输出日志, 否则不输出, 以节约系统资源
    "日志级别 1 - 9, 1表示该日志最为概略, 9表示该日志最为细节
    TRANSLATE type TO UPPER CASE.
    CASE type.
      WHEN 'A'.
        level = '9'.
      WHEN 'E'.
        level = '8'.
      WHEN 'W'.
        level = '7'.
      WHEN 'I'.
        level = '6'.
      WHEN 'S'.
        level = '5'.
      WHEN OTHERS.
        level = '9'.
    ENDCASE.
  ENDMETHOD.


  METHOD get_msg_list.
    collector->get_list( IMPORTING et_list = msg_list ).
  ENDMETHOD.


  METHOD get_salv.


    TRY.
        cl_salv_table=>factory(
            IMPORTING
              r_salv_table   = salv
            CHANGING
              t_table        = table ).
      CATCH cx_salv_msg ##NO_HANDLER.
    ENDTRY.

    IF salv IS BOUND.

      salv->get_columns( )->set_optimize( abap_true ).
      salv->get_functions( )->set_all( ).

      TRY.
          salv->get_columns(:
             )->get_column( 'CONTEXT' )->set_visible( abap_false ),
             )->get_column( 'PARAMS'  )->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD info.
    add_from_string( content = content type = 'I' ).
    log = me.
  ENDMETHOD.


  METHOD is_valid_level.

    ##NEEDED    DATA: numc TYPE n LENGTH 1,
             type TYPE dd01v-datatype.

    CALL FUNCTION 'NUMERIC_CHECK'
      EXPORTING
        string_in  = level
      IMPORTING
        string_out = numc
        htype      = type.
    IF type = 'NUMC'.
      is_valid_level = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD success.
    add_from_string( content = content type = 'S' ).
    log = me.
  ENDMETHOD.


  METHOD warning.
    add_from_string( content = content type = 'W' ).
    log = me.
  ENDMETHOD.
ENDCLASS.
