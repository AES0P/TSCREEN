*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_RUNNING_LOG
*&---------------------------------------------------------------------*
*&  此报表按不同维度，展示使用了TSCREEN框架的程序的运行概况
*&  仅作开发参考，具体维度分类逻辑请自行改造！！！
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_running_log.

*&---------------------------------------------------------------------*
*&　　　　SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_all    RADIOBUTTON GROUP grp1 USER-COMMAND select DEFAULT 'X',
              p_detail RADIOBUTTON GROUP grp1,
              p_prog   RADIOBUTTON GROUP grp1,
              p_day    RADIOBUTTON GROUP grp1,
              p_monat  RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK blk1.

TABLES ztscreen_log.
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_person FOR sy-uname DEFAULT sy-uname,
                  s_prog   FOR sy-cprog,
                  s_datum  FOR ztscreen_log-crdat,
                  s_monat  FOR ztscreen_log-monat.
SELECTION-SCREEN END OF BLOCK blk2.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-METHODS push_view.

    METHODS initialize REDEFINITION.
    METHODS pbo REDEFINITION.
    METHODS check_authority REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS execute REDEFINITION.
    METHODS show REDEFINITION.

  PROTECTED SECTION.
    METHODS on_countdown_finished REDEFINITION.

  PRIVATE SECTION.

    DATA data TYPE STANDARD TABLE OF ztscreen_log.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT zcl_tscreen_stack=>get_instance( )->is_exists( program = sy-repid ).

    DATA view TYPE REF TO zif_tscreen.
    CREATE OBJECT view TYPE lcl_prog.

  ENDMETHOD.

  METHOD initialize.

    DATA datum LIKE LINE OF s_datum.
    datum-option = 'EQ'.
    datum-sign   = 'I'.
    datum-low    = sy-datum - 0.
    datum-high   = sy-datum.
    APPEND datum TO s_datum[].

    DATA monat LIKE LINE OF s_monat.
    monat-option = 'EQ'.
    monat-sign   = 'I'.
    monat-low    = sy-datum+4(2).
    monat-high   = sy-datum+4(2).
    APPEND monat TO s_monat[].

    countdown_begin( '60' )."60S自动刷新

  ENDMETHOD.

  METHOD pbo.

    LOOP AT SCREEN.

      CASE abap_true.
        WHEN p_detail.
          IF screen-name CS 'S_PROG' OR screen-name CS 'S_DATUM' OR screen-name CS 'S_MONAT'.
            screen-active   = 0.
          ELSEIF screen-name CS 'S_PERSON'.
            screen-active   = 1.
          ENDIF.
        WHEN p_prog.
          IF screen-name CS 'S_PERSON' OR screen-name CS 'S_DATUM' OR screen-name CS 'S_MONAT'.
            screen-active   = 0.
          ELSEIF screen-name CS 'S_PROG'.
            screen-active   = 1.
          ENDIF.
        WHEN p_day.
          IF screen-name CS 'S_PERSON' OR screen-name CS 'S_PROG' OR screen-name CS 'S_MONAT'.
            screen-active   = 0.
          ELSEIF screen-name CS 'S_DATUM'.
            screen-active   = 1.
          ENDIF.
        WHEN p_monat.
          IF screen-name CS 'S_PROG' OR screen-name CS 'S_DATUM' OR screen-name CS 'S_PERSON'.
            screen-active   = 0.
          ELSEIF screen-name CS 'S_MONAT'.
            screen-active   = 1.
          ENDIF.
        WHEN OTHERS.
          screen-active   = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.

  ##NEEDED
  METHOD check_authority.

  ENDMETHOD.

  METHOD pai.
    CASE abap_true.
      WHEN p_detail.
        CLEAR: s_prog[],s_datum[],s_monat[].
      WHEN p_prog.
        CLEAR: s_person[],s_datum[],s_monat[].
      WHEN p_day.
        CLEAR: s_person[],s_prog[],s_monat[].
      WHEN p_monat.
        CLEAR: s_prog[],s_datum[],s_person[].
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD execute.

    SELECT *                                            "#EC CI_NOFIELD
      FROM ztscreen_log
      INTO CORRESPONDING FIELDS OF TABLE data
     WHERE crdat IN s_datum
       AND crnam IN s_person
       AND cprog IN s_prog
       AND monat IN s_monat
       AND cprog <> 'ZAESOP_TSCREEN_RUNNING_LOG'.
    IF sy-subrc <> 0.
      MESSAGE 'NO DATA FOUND' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO TRANSACTION 'ZTSCREEN02'.
    ELSE.
      MESSAGE CONV string( lines( data ) ) TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD show.

    DATA layout TYPE lvc_s_layo.
    layout-zebra      = 'X' .    " 斑马线
    layout-cwidth_opt = 'X' .    " 自动调整ALV列宽
    layout-sel_mode   = 'A' .    " ALV 控制: 选择方式

    DATA: lt_sort TYPE lvc_t_sort,
          ls_sort LIKE LINE OF lt_sort.

    CASE abap_true.
      WHEN p_all.
        CLEAR ls_sort.
        ls_sort-spos      = '01'.
        ls_sort-fieldname = 'GUID'.
        ls_sort-down      = abap_true.
        APPEND ls_sort TO lt_sort.
        CLEAR ls_sort.
        ls_sort-spos      = '02'.
        ls_sort-fieldname = 'CRNAM'.
        ls_sort-up        = abap_true.
        APPEND ls_sort TO lt_sort.
        CLEAR ls_sort.
        ls_sort-spos      = '03'.
        ls_sort-fieldname = 'CPROG'.
        ls_sort-up        = abap_true.
        APPEND ls_sort TO lt_sort.
        CLEAR ls_sort.
        ls_sort-spos      = '04'.
        ls_sort-fieldname = 'MONAT'.
        ls_sort-up        = abap_true.
        APPEND ls_sort TO lt_sort.
        CLEAR ls_sort.
        ls_sort-spos      = '05'.
        ls_sort-fieldname = 'CRDAT'.
        ls_sort-down      = abap_true.
        APPEND ls_sort TO lt_sort.
      WHEN p_detail.
        ls_sort-spos      = '01'.
        ls_sort-fieldname = 'CRNAM'.
        ls_sort-up        = abap_true.
        APPEND ls_sort TO lt_sort.
      WHEN p_prog.
        ls_sort-spos      = '01'.
        ls_sort-fieldname = 'CPROG'.
        ls_sort-up        = abap_true.
        APPEND ls_sort TO lt_sort.
      WHEN p_day.
        CLEAR lt_sort.
        ls_sort-spos      = '01'.
        ls_sort-fieldname = 'CRDAT'.
        ls_sort-up        = abap_true.
        APPEND ls_sort TO lt_sort.
      WHEN p_monat.
        CLEAR lt_sort.
        ls_sort-spos      = '01'.
        ls_sort-fieldname = 'MONAT'.
        ls_sort-up        = abap_true.
        APPEND ls_sort TO lt_sort.
      WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program = sy-repid
        is_layout_lvc      = layout
        i_structure_name   = 'ZTSCREEN_LOG'
        it_sort_lvc        = lt_sort
      TABLES
        t_outtab           = data
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2. "#EC CI_SUBRC
    ##NEEDED
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.

  METHOD on_countdown_finished.

    timer->run( )."循环倒计时事件

    DATA grid TYPE REF TO cl_gui_alv_grid.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = grid.
    CHECK grid IS BOUND.
    execute( ).
    grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).

  ENDMETHOD.

ENDCLASS.


##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
