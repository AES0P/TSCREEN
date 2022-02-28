CLASS zcl_treport DEFINITION
  PUBLIC
  INHERITING FROM zcl_tscreen
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA guid TYPE guid READ-ONLY .
    DATA management_strategy TYPE ztscreen_manager READ-ONLY .
    DATA ip TYPE string READ-ONLY .
    CLASS-DATA zkey TYPE ztscreen_manager-zkey .

    METHODS constructor
      IMPORTING
        !program             TYPE syrepid DEFAULT sy-cprog
        !dynnr               TYPE sy-dynnr DEFAULT sy-dynnr
        !dynpro_type         TYPE scrhtyp DEFAULT zcl_tscreen=>dynpro_type_selscreen
        !display_mode        TYPE abap_bool DEFAULT zcl_tscreen=>display_mode_modify
        !pfstatus            TYPE sypfkey OPTIONAL
        !pfstatus_repid      TYPE syrepid OPTIONAL
        !excluding_fcode     TYPE tty_fcode OPTIONAL
        !titlebar            TYPE gui_title OPTIONAL
        !titlebar_repid      TYPE syrepid OPTIONAL
        !titlebar_var1       TYPE string OPTIONAL
        !titlebar_var2       TYPE string OPTIONAL
        !titlebar_var3       TYPE string OPTIONAL
        !titlebar_var4       TYPE string OPTIONAL
        !titlebar_var5       TYPE string OPTIONAL
        !read_dynpro_setting TYPE abap_bool DEFAULT abap_true .
    CLASS-METHODS event
      IMPORTING
        !event TYPE c .
    METHODS initialize .
    METHODS check_authority .
    METHODS execute .
    METHODS show .
    METHODS top_of_page .
    METHODS at_line_selection .
    METHODS at_line_ucomm .
    METHODS end_of_page .
    METHODS set_title
      IMPORTING
        !tabname TYPE tabname
        !langu   TYPE sy-langu DEFAULT sy-langu
      CHANGING
        !title   TYPE any .
    CLASS-METHODS is_screen_exists
      IMPORTING
        !program           TYPE sy-repid
        VALUE(dynnr_super) TYPE sy-dynnr OPTIONAL
        !dynnr             TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING
        VALUE(is_exists)   TYPE abap_bool .
    CLASS-METHODS get_screen
      IMPORTING
        !dynnr_super   TYPE sy-dynnr OPTIONAL
        !dynnr         TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING
        VALUE(tscreen) TYPE REF TO zif_tscreen
      RAISING
        zcx_tscreen .
    CLASS-METHODS get_treport
      RETURNING
        VALUE(treport) TYPE REF TO zcl_treport .

    METHODS zif_tscreen~exit
        REDEFINITION .
    METHODS zif_tscreen~handle_event
        REDEFINITION .
    METHODS zif_tscreen~pbo
        REDEFINITION .
  PROTECTED SECTION.

    METHODS get_strategy
      IMPORTING
        !program     TYPE syrepid DEFAULT sy-cprog
        VALUE(tcode) TYPE sy-tcode DEFAULT sy-tcode .
    METHODS check_strategy
        FINAL .
    METHODS check_strategy_active_status .
    METHODS check_strategy_block_list .
    METHODS check_strategy_deactive_date .
    METHODS check_strategy_retry_flag .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TREPORT IMPLEMENTATION.


  METHOD check_authority ##NEEDED.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( program             = program
                        dynnr               = dynnr
                        dynpro_type         = dynpro_type
                        display_mode        = display_mode
                        pfstatus            = pfstatus
                        pfstatus_repid      = pfstatus_repid
                        excluding_fcode     = excluding_fcode
                        titlebar            = titlebar
                        titlebar_repid      = titlebar_repid
                        titlebar_var1       = titlebar_var1
                        titlebar_var2       = titlebar_var2
                        titlebar_var3       = titlebar_var3
                        titlebar_var4       = titlebar_var4
                        titlebar_var5       = titlebar_var5
                        read_dynpro_setting = read_dynpro_setting ).

    ip = zcl_tlog=>get_ip( ).

    get_strategy( program = program tcode = sy-tcode ).

    check_strategy( ).

    TRY.
        guid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).

        IF management_strategy-zlevel IS NOT INITIAL.
          CAST zcl_tlog( tlog )->set_level( management_strategy-zlevel ).
        ENDIF.
        ##NO_TEXT
        tlog->add_log( 'Started' ).
      CATCH cx_uuid_error.
        MESSAGE 'GUID ERROR' TYPE 'A'.
    ENDTRY.

  ENDMETHOD.


  METHOD execute ##NEEDED.
  ENDMETHOD.


  METHOD initialize ##NEEDED.
  ENDMETHOD.


  METHOD show ##NEEDED.
  ENDMETHOD.


  METHOD zif_tscreen~exit.
    ##NO_TEXT
    tlog->add_log( 'Ended' )->save_log( guid = guid ).
    super->exit( ).
  ENDMETHOD.


  METHOD zif_tscreen~handle_event.

    get_current_info( ).

    CASE event.
      WHEN 'INIT'.
        initialize( ).
      WHEN 'AUTH'.
        check_authority( ).
      WHEN 'EXE'.
        execute( ).
      WHEN 'SHOW'.
        show( ).
      WHEN 'LINE_TOP'.
        top_of_page( ).
      WHEN 'LINE_SEL'.
        at_line_selection( ).
      WHEN 'LINE_UCOMM'.
        IF if_log_record_pai = abap_true.
          tlog->add_log( type = 'I' content = 'UCOMM:' && sy-ucomm ).
        ENDIF.
        at_line_ucomm( ).
      WHEN 'LINE_END'.
        end_of_page( ).
      WHEN OTHERS.
        super->handle_event( event ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_tscreen~pbo ##NEEDED.
*CALL METHOD SUPER->ZIF_TSCREEN~PBO
*    .
  ENDMETHOD.


  METHOD set_title.

    DATA components TYPE cl_abap_structdescr=>component_table.
    components = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( title ) )->get_components( ).

    SELECT fieldname,scrtext_s AS ddtext
      FROM dd03m
      INTO TABLE @DATA(dd03t)
     WHERE tabname    = @tabname
       AND ddlanguage = @langu
       AND fldstat    = 'A'.
    IF sy-subrc = 0.

      FIELD-SYMBOLS <component> LIKE LINE OF components.
      FIELD-SYMBOLS <field> TYPE any.
      LOOP AT components ASSIGNING <component>.

        ASSIGN COMPONENT <component>-name OF STRUCTURE title TO <field>.
        ASSERT sy-subrc = 0.

        TRY.
            ##WARN_OK
            <field> = dd03t[ fieldname = <component>-name ]-ddtext. "#EC CI_STDSEQ
          ##NO_HANDLER  CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_screen.
    tscreen = zcl_tscreen_stack=>get_instance( )->current( dynnr_super = dynnr_super dynnr = dynnr ).
  ENDMETHOD.


  METHOD event.

    TRY.
        get_screen( )->handle_event( event ).
      CATCH zcx_tscreen INTO DATA(gx_tscreen) ##NEEDED.
        MESSAGE gx_tscreen->get_text( ) TYPE 'A'.
    ENDTRY.

  ENDMETHOD.


  METHOD is_screen_exists.
    is_exists = zcl_tscreen_stack=>get_instance( )->is_exists( program = program dynnr_super = dynnr_super dynnr = dynnr ).
  ENDMETHOD.


  METHOD check_strategy.

    "是否启用
    check_strategy_active_status( ).

    "黑名单
    check_strategy_block_list( ).

    "停用时间
    check_strategy_deactive_date( ).

    "能否重调用
    check_strategy_retry_flag( ).

  ENDMETHOD.


  METHOD check_strategy_active_status.

    IF management_strategy-zactive = abap_false.
      "程序未启用！请在事务代码 ZTSCREEN00 中进行配置。
      MESSAGE e004(ztscreen).
    ENDIF.

  ENDMETHOD.


  METHOD check_strategy_block_list.

    DATA: block_user     TYPE string_table,
          block_terminal TYPE string_table.

    IF management_strategy-zswitch_block = abap_false.
      RETURN.
    ENDIF.

    SPLIT management_strategy-zblock_user     AT ',' INTO TABLE block_user.
    SPLIT management_strategy-zblock_terminal AT ',' INTO TABLE block_terminal.

    SORT: block_user ASCENDING,block_terminal ASCENDING.

    LOOP AT block_user ASSIGNING FIELD-SYMBOL(<block_user>).
      IF to_upper( <block_user> ) = to_upper( sy-uname ).
        "用户&1已被加入本程序黑名单，请联系管理员！
        MESSAGE e001(ztscreen) WITH sy-uname.
      ENDIF.
    ENDLOOP.

    LOOP AT block_terminal ASSIGNING FIELD-SYMBOL(<block_terminal>).
      IF <block_terminal> = ip.
        "终端&1已被加入本程序黑名单，请联系管理员！
        MESSAGE e002(ztscreen) WITH ip.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_strategy_deactive_date.

    IF NOT (  management_strategy-zbdate IS NOT INITIAL
       AND management_strategy-zsdate IS NOT INITIAL ).
      RETURN.
    ENDIF.

    DATA: beginning_ts TYPE timestamp,
          current_ts   TYPE timestamp,
          ending_ts    TYPE timestamp.

    DATA time_zone TYPE sy-zonlo VALUE 'UTC'.

    CONVERT DATE management_strategy-zbdate
            TIME management_strategy-zbtime
            INTO TIME STAMP beginning_ts
            TIME ZONE time_zone.

    CONVERT DATE sy-datum
            TIME sy-uzeit
            INTO TIME STAMP current_ts
            TIME ZONE time_zone.

    CONVERT DATE management_strategy-zsdate
            TIME management_strategy-zstime
            INTO TIME STAMP ending_ts
            TIME ZONE time_zone.

    IF current_ts >= beginning_ts AND current_ts <= ending_ts.

      "检查放行启用开关
      IF management_strategy-zswitch_extra = abap_true.

        DATA extra_list TYPE string_table.
        SPLIT management_strategy-zextra_list AT ',' INTO TABLE extra_list.

        LOOP AT extra_list ASSIGNING FIELD-SYMBOL(<extra>).
          IF <extra> = 'IP:' && zcl_tlog=>get_ip( )
               OR to_upper( <extra> ) = 'USER:' && sy-uname.
            RETURN.
          ENDIF.
        ENDLOOP.

      ENDIF.

      "程序在 &1-&2 至 &3-&4 期间停用，若需紧急使用，请联系管理员将你的IP地址或者用户名加入放行列表。
      MESSAGE e003(ztscreen) WITH management_strategy-zbdate management_strategy-zbtime
                                  management_strategy-zsdate management_strategy-zstime.
    ENDIF.

  ENDMETHOD.


  METHOD check_strategy_retry_flag.

    DATA retry_flag TYPE ztscreen_manager-zdebug.
    IMPORT zdebug = retry_flag FROM MEMORY ID 'ZDEBUG'.

    IF retry_flag = abap_true AND management_strategy-zdebug <> abap_true.
      "程序未启用重调功能！请在事务代码 ZTSCREEN00 中进行配置。
      MESSAGE e005(ztscreen).
    ENDIF.

  ENDMETHOD.


  METHOD get_strategy.

    IF NOT to_upper( tcode+0(1) ) CA 'YZ'.
      CLEAR tcode.
    ENDIF.

    IF management_strategy IS INITIAL.
      SELECT SINGLE *
        FROM ztscreen_manager
        INTO management_strategy
       WHERE cprog = program
         AND tcode = tcode
         AND zkey  = zkey.                                "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD get_treport.
    TRY.
        treport = CAST zcl_treport( get_screen( dynnr = '1000' ) ).
      CATCH zcx_tscreen.
        "未找到报表控制类ZCL_TREPORT，请检查你的代码！
        MESSAGE e008(ztscreen).
    ENDTRY.
  ENDMETHOD.


  METHOD at_line_ucomm ##NEEDED.
  ENDMETHOD.


  METHOD end_of_page ##NEEDED.
  ENDMETHOD.


  METHOD at_line_selection ##NEEDED.
  ENDMETHOD.


  METHOD top_of_page ##NEEDED.
  ENDMETHOD.
ENDCLASS.
