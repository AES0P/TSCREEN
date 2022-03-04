*MIT License
*
*Copyright (c) 2021 AES0P
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
CLASS zcl_tlog DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_tlog .

    ALIASES add_log
      FOR zif_tlog~add_log .
    ALIASES clear
      FOR zif_tlog~clear .
    ALIASES save_log
      FOR zif_tlog~save_log .

    DATA auto_commit TYPE abap_bool VALUE abap_true ##NO_TEXT.

    CLASS-METHODS get_instance
      IMPORTING
        !object             TYPE balobj_d
        !subobject          TYPE balsubobj
        !identity           TYPE balnrext
        !level              TYPE ballevel DEFAULT '9'
        !del_date           TYPE recadatefrom OPTIONAL
        !del_not_before     TYPE recabool OPTIONAL
        !auto_commit        TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(log_instance) TYPE REF TO zcl_tlog .
    METHODS success
      IMPORTING
        !content   TYPE string
      RETURNING
        VALUE(log) TYPE REF TO zcl_tlog .
    METHODS info
      IMPORTING
        !content   TYPE string
      RETURNING
        VALUE(log) TYPE REF TO zcl_tlog .
    METHODS warning
      IMPORTING
        !content   TYPE string
      RETURNING
        VALUE(log) TYPE REF TO zcl_tlog .
    METHODS error
      IMPORTING
        !content   TYPE string
      RETURNING
        VALUE(log) TYPE REF TO zcl_tlog .
    METHODS abort
      IMPORTING
        !content   TYPE string
      RETURNING
        VALUE(log) TYPE REF TO zcl_tlog .
    METHODS debug
      IMPORTING
        VALUE(uname) TYPE sy-uname OPTIONAL
      RETURNING
        VALUE(log)   TYPE REF TO zcl_tlog .
    METHODS add_from_string
      IMPORTING
        VALUE(content) TYPE string
        VALUE(type)    TYPE symsgty .
    METHODS add_from_symsg
      IMPORTING
        !level         TYPE ballevel DEFAULT '9'
      RETURNING
        VALUE(message) TYPE recamsg .
    METHODS add_from_exception
      IMPORTING
        !io_exception TYPE REF TO cx_root
        !if_cumulate  TYPE abap_bool OPTIONAL
        !id_detlevel  TYPE ballevel DEFAULT '9'
        !id_probclass TYPE balprobcl OPTIONAL
        !id_tabname   TYPE tabname OPTIONAL
        !id_fieldname TYPE fieldname OPTIONAL
        !id_value     TYPE any OPTIONAL
        !id_index     TYPE numeric OPTIONAL
        !id_intreno   TYPE recaintreno OPTIONAL
        !id_context   TYPE any OPTIONAL .
    METHODS add_from_bapi
      IMPORTING
        !it_bapiret     TYPE bapirettab OPTIONAL
        !is_bapiret     TYPE bapiret2 OPTIONAL
        !if_cumulate    TYPE abap_bool OPTIONAL
      EXPORTING
        !ef_add_error   TYPE abap_bool
        !ef_add_warning TYPE abap_bool .
    METHODS commit
      RETURNING
        VALUE(tlog) TYPE REF TO zcl_tlog
      EXCEPTIONS
        error .
    METHODS display_in_slg1
      IMPORTING
        !amodal     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(tlog) TYPE REF TO zcl_tlog .
    METHODS get_msg_list
      RETURNING
        VALUE(msg_list) TYPE re_t_msg .
    METHODS get_salv
      CHANGING
        !table      TYPE STANDARD TABLE
      RETURNING
        VALUE(salv) TYPE REF TO cl_salv_table .
    METHODS display_as_alv_popup
      IMPORTING
        !start_column TYPE i DEFAULT 5   ##NUMBER_OK
        !start_line   TYPE i DEFAULT 5   ##NUMBER_OK
        !end_column   TYPE i DEFAULT 120   ##NUMBER_OK
        !end_line     TYPE i DEFAULT 25  ##NUMBER_OK.
    METHODS free .
    METHODS get_collector
      RETURNING
        VALUE(collector) TYPE REF TO if_reca_message_list .
    METHODS get_se91_message
      IMPORTING
        !id            TYPE sy-msgid
        !num           TYPE sy-msgno
        !var1          TYPE sy-msgv1
        !var2          TYPE sy-msgv2
        !var3          TYPE sy-msgv3
        !var4          TYPE sy-msgv4
      RETURNING
        VALUE(message) TYPE sy-lisel .
    CLASS-METHODS get_ip
      RETURNING
        VALUE(ip) TYPE string .
    METHODS get_level
      IMPORTING
        VALUE(type)  TYPE sy-msgty
      RETURNING
        VALUE(level) TYPE ballevel .
    METHODS set_level
      IMPORTING
        VALUE(level) TYPE ballevel .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_kv,
        key     TYPE string,
        value1  TYPE string,
        value2  TYPE string,
        value3  TYPE string,
        value4  TYPE string,
        value5  TYPE string,
        value6  TYPE string,
        value7  TYPE string,
        value8  TYPE string,
        value9  TYPE string,
        timstmp TYPE baltimstmp,
      END OF ty_kv .
    TYPES:
      tty_kv TYPE STANDARD TABLE OF ty_kv WITH DEFAULT KEY .

    CLASS-DATA instance TYPE REF TO zcl_tlog .
    DATA level TYPE ballevel .
    DATA object TYPE balobj_d .
    DATA subobject TYPE balsubobj .
    DATA identity TYPE balnrext .
    DATA collector TYPE REF TO if_reca_message_list .
    DATA dynpro_info TYPE tty_kv .

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
    METHODS get_symbol
      IMPORTING
        !msgty        TYPE sy-msgty
      RETURNING
        VALUE(symbol) TYPE icon_d .
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

    tlog = me.

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

    tlog = me.

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

    ##NEEDED
    DATA: numc TYPE n LENGTH 1,
          type TYPE dd01v-datatype.

    CALL FUNCTION 'NUMERIC_CHECK'
      EXPORTING
        string_in  = level
      IMPORTING
        string_out = numc
        htype      = type.
    IF type = 'NUMC'.
      IF level >= 1 AND level <= 9.
        is_valid_level = abap_true.
      ENDIF.
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


  METHOD get_se91_message.

    CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
      EXPORTING
        message_id        = id
        message_number    = num
        message_var1      = var1
        message_var2      = var2
        message_var3      = var3
        message_var4      = var4
      IMPORTING
        message_text      = message
      EXCEPTIONS
        message_not_found = 1
        OTHERS            = 2. "#EC CI_SUBRC
    IF sy-subrc <> 0.
*      me->warning( iv_msg_id && iv_msg_num && TEXT-e05 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_tlog~add_log.

    DATA kv TYPE ty_kv.
    kv-key    = lines( dynpro_info ) + 1.
    kv-value1 = dynnr.
    kv-value2 = ucomm.
    GET TIME STAMP FIELD kv-timstmp.
    APPEND kv TO dynpro_info.

    CASE to_upper( type ).
      WHEN 'S'.
        success( content ).
      WHEN 'I'.
        info( content ).
      WHEN 'W'.
        warning( content ).
      WHEN 'E'.
        error( content ).
      WHEN 'A'.
        abort( content ).
      WHEN 'D'.
        debug( ).
      WHEN OTHERS.
        success( content ).
    ENDCASE.

    instance = me.

    "SAP标准日志增加时，如果点击频率过快，会导致新增不了，这个时候无法保证自增表和SAP条目一致
    IF collector->count( ) <> lines( dynpro_info ).
      DELETE dynpro_info INDEX lines( dynpro_info ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_tlog~clear.
    CLEAR dynpro_info.
    collector->clear( ).
    instance = me.
  ENDMETHOD.


  METHOD zif_tlog~save_log.

    instance = me.
    CHECK if_commit = abap_true.

    DATA logs TYPE STANDARD TABLE OF ztscreen_log.
    MOVE-CORRESPONDING get_msg_list( ) TO logs.         "#EC CI_CONV_OK

    DATA lv_name_text TYPE ad_namtext.
    ##WARN_OK
    SELECT SINGLE adrp~name_text
      FROM usr21
     INNER JOIN adrp
        ON usr21~persnumber = adrp~persnumber
      INTO lv_name_text
     WHERE usr21~bname = sy-uname.                        "#EC CI_SUBRC

*    DATA is_webgui TYPE c.
*    CALL FUNCTION 'GUI_GET_DESKTOP_INFO' ##FM_OLDED
*      EXPORTING
*        type   = 5
*      CHANGING
*        return = is_webgui.
*    "后台和非SAP GUI都不记录IP
*    IF is_webgui IS NOT INITIAL AND sy-batch = abap_false.
*      DATA ip TYPE string.
*      ip = cl_gui_frontend_services=>get_ip_address( ).
*    ENDIF.
    DATA ip TYPE string.
    ip = zcl_tlog=>get_ip( ).

    DATA opcode(1) TYPE x VALUE 5.
    DATA terminal TYPE ztscreen_log-terminal.
    CALL 'ThUsrInfo' ID 'OPCODE'   FIELD opcode
                     ID 'TERMINAL' FIELD terminal.        "#EC CI_CCALL

    FIELD-SYMBOLS <log> LIKE LINE OF logs.
    FIELD-SYMBOLS <dynpro_info> LIKE LINE OF dynpro_info.
    LOOP AT logs ASSIGNING <log>.

      <log>-guid      = guid.
      <log>-line      = sy-tabix.
      IF symbol IS INITIAL.
        <log>-symbol  = get_symbol( <log>-msgty ).
      ELSE.
        <log>-symbol  = symbol.
      ENDIF.
      <log>-sid       = sy-sysid.
      <log>-cprog     = sy-cprog.
      ##WARN_OK
      READ TABLE dynpro_info ASSIGNING <dynpro_info> WITH KEY key = sy-tabix. "#EC CI_STDSEQ
      IF sy-subrc = 0.
        <log>-dynnr   = <dynpro_info>-value1.
        <log>-ucomm   = <dynpro_info>-value2.
      ENDIF.

      <log>-instance  = sy-host.
      <log>-crnam     = sy-uname.
      <log>-name_text = lv_name_text.
      CONVERT TIME STAMP <log>-time_stmp
               TIME ZONE sy-zonlo
               INTO DATE <log>-crdat
                    TIME <log>-crtim.
      <log>-monat     = <log>-crdat+4(2).
      <log>-ip        = ip.
      <log>-terminal  = terminal.
      <log>-tcode     = sy-tcode.

      <log>-message   = get_se91_message( id   = <log>-msgid
                                          num  = <log>-msgno
                                          var1 = <log>-msgv1
                                          var2 = <log>-msgv2
                                          var3 = <log>-msgv3
                                          var4 = <log>-msgv4 ).

      IF NOT <log>-message CS 'PAI'.
        CLEAR <log>-ucomm.
      ENDIF.

    ENDLOOP.

    INSERT ztscreen_log FROM TABLE logs.
    IF sy-subrc <> 0.
      MESSAGE 'ERROR OCCURED WHEN INSERT DATA TO DB' TYPE 'S' DISPLAY LIKE 'E'.
*      zcx_tscreen=>raise_text( 'ERROR OCCURED WHEN INSERT DATA TO DB' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_symbol.

    CASE msgty.
      WHEN 'S'.
        symbol = icon_okay.
      WHEN 'I'.
        symbol = icon_led_yellow.
      WHEN 'W'.
        symbol = icon_message_warning_small.
      WHEN 'E'.
        symbol = icon_incomplete.
      WHEN 'A'.
        symbol = icon_cancel.
      WHEN 'D'.
        symbol = icon_alert.
      WHEN OTHERS.
        symbol = icon_message_information_small.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_tlog~commit.
    commit( ).
    instance = me.
  ENDMETHOD.


  METHOD zif_tlog~show_log.
    CASE style.
      WHEN '2'.
        display_as_alv_popup( ).
      WHEN OTHERS.
        display_in_slg1( ).
    ENDCASE.
    instance = me.
  ENDMETHOD.


  METHOD set_level.

    CHECK is_valid_level( level ).
    me->level = level.

  ENDMETHOD.


  METHOD get_ip.

    DATA is_webgui TYPE c.
    CALL FUNCTION 'GUI_GET_DESKTOP_INFO' ##FM_OLDED
      EXPORTING
        type   = 5
      CHANGING
        return = is_webgui.
    "后台和非SAP GUI都不记录IP
    IF is_webgui IS NOT INITIAL AND sy-batch = abap_false.
      ip = cl_gui_frontend_services=>get_ip_address( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
