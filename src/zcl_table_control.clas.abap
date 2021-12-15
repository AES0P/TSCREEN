CLASS zcl_table_control DEFINITION
  PUBLIC
  INHERITING FROM zcl_tcomponent
  CREATE PUBLIC

  GLOBAL FRIENDS zif_tscreen .

  PUBLIC SECTION.

    INTERFACES zif_table_control .

    ALIASES convert_all_lines_mark
      FOR zif_table_control~convert_all_lines_mark .
    ALIASES c_component_editor
      FOR zif_tscreen_component~c_component_editor .
    ALIASES c_component_tc
      FOR zif_tscreen_component~c_component_tc .
    ALIASES c_filter_off
      FOR zif_table_control~c_filter_off .
    ALIASES c_filter_on
      FOR zif_table_control~c_filter_on .
    ALIASES ddic_tabname
      FOR zif_table_control~ddic_tabname .
    ALIASES delete_line
      FOR zif_table_control~delete_line .
    ALIASES demark_all_columns
      FOR zif_table_control~demark_all_columns .
    ALIASES demark_all_lines
      FOR zif_table_control~demark_all_lines .
    ALIASES dynnr
      FOR zif_table_control~dynnr .
    ALIASES empty_fields
      FOR zif_table_control~empty_fields .
    ALIASES filter
      FOR zif_table_control~filter .
    ALIASES filter_column
      FOR zif_table_control~filter_column .
    ALIASES hide_empty_columns
      FOR zif_table_control~hide_empty_columns .
    ALIASES insert_line
      FOR zif_table_control~insert_line .
    ALIASES in_filter_mode
      FOR zif_table_control~in_filter_mode .
    ALIASES mark_all_columns
      FOR zif_table_control~mark_all_columns .
    ALIASES mark_all_lines
      FOR zif_table_control~mark_all_lines .
    ALIASES mass_column
      FOR zif_table_control~mass_column .
    ALIASES pai_tc_line
      FOR zif_table_control~pai_tc_line .
    ALIASES pbo_tc_line
      FOR zif_table_control~pbo_tc_line .
    ALIASES poh
      FOR zif_table_control~poh .
    ALIASES pov
      FOR zif_table_control~pov .
    ALIASES prefix
      FOR zif_table_control~prefix .
    ALIASES program
      FOR zif_table_control~program .
    ALIASES ref_structure_name
      FOR zif_table_control~ref_structure_name .
    ALIASES screen_lines
      FOR zif_table_control~screen_lines .
    ALIASES scroll
      FOR zif_table_control~scroll .
    ALIASES selbar
      FOR zif_table_control~selbar .
    ALIASES set_visibility
      FOR zif_table_control~set_visibility .
    ALIASES show_row_content
      FOR zif_table_control~show_row_content .
    ALIASES show_row_ddic_detail
      FOR zif_table_control~show_row_ddic_detail .
    ALIASES sort
      FOR zif_table_control~sort .
    ALIASES tc_name
      FOR zif_table_control~tc_name .
    ALIASES tty_dynpread
      FOR zif_table_control~tty_dynpread .
    ALIASES tty_empty_field
      FOR zif_table_control~tty_empty_field .
    ALIASES ty_empty_field
      FOR zif_table_control~ty_empty_field .
    ALIASES unfilter
      FOR zif_table_control~unfilter .
    ALIASES user_command
      FOR zif_table_control~user_command .
    ALIASES initialize_by_tscreen
      FOR zif_tscreen_component~initialize_by_tscreen .
    ALIASES transport_data
      FOR zif_table_control~transport_data .

    METHODS constructor
      IMPORTING
        !parent             TYPE REF TO zcl_tscreen
        !tc_name            TYPE string
        !ref_structure_name TYPE c
        !data_source        TYPE string
        !data_wa            TYPE string
        !ddic_tabname       TYPE c OPTIONAL
        !prefix             TYPE c DEFAULT `TC_`
        !selbar_name        TYPE c DEFAULT `SEL`
        !in_filter_mode     TYPE abap_bool DEFAULT abap_false
        !filter_column_name TYPE c DEFAULT `FILTER`
        !hide_empty_fields  TYPE abap_bool DEFAULT abap_false
      RAISING
        cx_uuid_error
        zcx_tscreen .
    METHODS set_visible
      RETURNING
        VALUE(table_control) TYPE REF TO zcl_table_control .
    METHODS set_invisible
      RETURNING
        VALUE(table_control) TYPE REF TO zcl_table_control .
    METHODS mark_lines
      IMPORTING
        !mode TYPE abap_bool .
    METHODS mark_columns
      IMPORTING
        !mode TYPE abap_bool .
    METHODS get_cell_value_by_index
      IMPORTING
        !fieldname   TYPE feld-name
        !index       TYPE int4
      RETURNING
        VALUE(value) TYPE dynfieldvalue .
    METHODS get_cell_value_by_cursor
      RETURNING
        VALUE(value) TYPE dynfieldvalue .
    METHODS get_selected_columns
      RETURNING
        VALUE(selected_columns) TYPE lvc_t_col .
    METHODS get_visible_columns
      RETURNING
        VALUE(columns) TYPE lvc_t_fcat .
    METHODS get_fixed_column
      RETURNING
        VALUE(fixed_lines) TYPE i .
    METHODS set_fixed_column
      IMPORTING
        !fixed_lines         TYPE i
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS get_current_line
      RETURNING
        VALUE(index) TYPE sy-index .
    METHODS set_cursor
      IMPORTING
        !field               TYPE c
        !index               TYPE i
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS set_top_line
      IMPORTING
        !index               TYPE i
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS set_sel_mode_row
      IMPORTING
        !mode                TYPE int1
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS set_sel_mode_column
      IMPORTING
        !mode                TYPE int1
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS get_column
      IMPORTING
        !column_name     TYPE c
      RETURNING
        VALUE(tc_column) TYPE scxtab_column .
    METHODS set_column
      IMPORTING
        !column_name         TYPE c
        !tc_column           TYPE scxtab_column
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS set_column_visible
      IMPORTING
        !column_name     TYPE c
      RETURNING
        VALUE(tc_column) TYPE scxtab_column .
    METHODS set_column_invisible
      IMPORTING
        !column_name     TYPE c
      RETURNING
        VALUE(tc_column) TYPE scxtab_column .
    METHODS set_column_visibility
      IMPORTING
        !column_name     TYPE c
        !visibility      TYPE abap_bool
      RETURNING
        VALUE(tc_column) TYPE scxtab_column .
    METHODS set_column_vislength
      IMPORTING
        !column_name     TYPE c
        !vislength       TYPE iconlength
      RETURNING
        VALUE(tc_column) TYPE scxtab_column .
    METHODS set_column_editable
      IMPORTING
        !column_name     TYPE c
      RETURNING
        VALUE(tc_column) TYPE scxtab_column .
    METHODS set_column_display
      IMPORTING
        !column_name     TYPE c
      RETURNING
        VALUE(tc_column) TYPE scxtab_column .
    METHODS set_cell_editable
      IMPORTING
        !field_name          TYPE c
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS set_cell_display
      IMPORTING
        !field_name          TYPE c
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS set_row_editable
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS set_row_display
      RETURNING
        VALUE(table_control) TYPE REF TO zif_table_control .
    METHODS is_line_insert
      RETURNING
        VALUE(is_line_insert) TYPE abap_bool .
    METHODS is_deletion_confirmed
      IMPORTING
        !index              LIKE sy-tabix
      RETURNING
        VALUE(is_confirmed) TYPE abap_bool .
    METHODS before_filter_table_writeback .
    METHODS after_filter_table_writeback .
    METHODS get_field_prefix
      RETURNING
        VALUE(prefix) TYPE tabname .
    METHODS user_command_extend
      IMPORTING
        !ucomm TYPE sy-ucomm .
    METHODS is_line_selected
      RETURNING
        VALUE(is_line_selected) TYPE abap_bool .
    METHODS is_ddic_tabname_valid
      EXPORTING
        !contflag                    TYPE contflag
      RETURNING
        VALUE(is_ddic_tabname_valid) TYPE abap_bool .

    METHODS zif_tscreen_component~change_editable
        REDEFINITION .
    METHODS zif_tscreen_component~change_visibility
        REDEFINITION .
    METHODS zif_tscreen_component~initialize_by_tscreen
        REDEFINITION .
    METHODS zif_tscreen_component~set_component_attr_by_setting
        REDEFINITION .
  PROTECTED SECTION.

    DATA parent TYPE REF TO zcl_tscreen .
    DATA tc TYPE REF TO scxtab_control .
    DATA data_name TYPE string .
    DATA data TYPE REF TO data .
    DATA data_wa_name TYPE string .
    DATA data_wa TYPE REF TO data .
    DATA ok_code TYPE REF TO sy-ucomm .
    DATA screen_util TYPE REF TO zcl_tscreen_util .

    METHODS bound .
    METHODS set_tc_screen_line
      IMPORTING
        !line            TYPE i
      RETURNING
        VALUE(component) TYPE REF TO zcl_table_control .
    METHODS get_data_size
      RETURNING
        VALUE(size) TYPE i .
  PRIVATE SECTION.

    ALIASES export_data
      FOR zif_table_control~export_data .
    ALIASES import_data
      FOR zif_table_control~import_data .
ENDCLASS.



CLASS ZCL_TABLE_CONTROL IMPLEMENTATION.


  METHOD set_row_editable.

    LOOP AT SCREEN.
      screen-input = '1'.
      MODIFY SCREEN.
    ENDLOOP.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_sel_mode_column.

    CHECK mode >= 0 AND mode <= 2.

    tc->*-col_sel_mode = mode.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_sel_mode_row.

    CHECK mode >= 0 AND mode <= 2.

    tc->*-line_sel_mode = mode.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_top_line.

    tc->*-top_line = index.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_visible.

    set_visibility( abap_false ).

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD user_command_extend ##NEEDED.
    "to be redefine
  ENDMETHOD.


  METHOD zif_table_control~convert_all_lines_mark.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    FIELD-SYMBOLS <line> TYPE any.
    LOOP AT <tab> ASSIGNING <line>.

      FIELD-SYMBOLS <field> TYPE any.
      ASSIGN COMPONENT selbar OF STRUCTURE <line> TO <field>.
      CHECK sy-subrc = 0.
      IF <field> = zcl_tscreen=>display_mode_show.
        <field> = zcl_tscreen=>display_mode_modify.
      ELSE.
        <field> = zcl_tscreen=>display_mode_show.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD zif_table_control~delete_line.

    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    FIELD-SYMBOLS: <line>  TYPE any,
                   <field> TYPE any.

    LOOP AT <tab> ASSIGNING <line>.

      DATA tabix TYPE sy-tabix.
      tabix = sy-tabix.

      ASSIGN COMPONENT selbar OF STRUCTURE <line> TO <field>.
      IF sy-subrc = 0 AND <field> = abap_true.

        CHECK is_deletion_confirmed( tabix ).
        DELETE <tab> INDEX tabix.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_table_control~demark_all_columns.
    mark_columns( abap_false ).
  ENDMETHOD.


  METHOD zif_table_control~demark_all_lines.
    mark_lines( abap_false ).
  ENDMETHOD.


  METHOD zif_table_control~filter.

    DATA selected_columns TYPE lvc_t_col.
    selected_columns = get_selected_columns( ).
    IF selected_columns IS INITIAL.
      MESSAGE '请先选择列' TYPE 'E'.
    ENDIF.

    DATA: filter_rows TYPE lvc_t_fidx,
          layout      TYPE lvc_s_layo.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    CALL FUNCTION 'LVC_FILTER'
      EXPORTING
        it_fieldcat      = get_visible_columns( )
        it_selected_cols = selected_columns
        is_layout        = layout
      IMPORTING
        et_filter_index  = filter_rows "被过滤的行的行号
      TABLES
        it_data          = <tab>
      CHANGING
        ct_filter        = filter_columns
      EXCEPTIONS
        no_change        = 1
        OTHERS           = 2.
    CASE sy-subrc.
      WHEN 1.
      WHEN 0.

        in_filter_mode = abap_true.

        FIELD-SYMBOLS: <line>  TYPE any,
                       <field> TYPE any.

        LOOP AT <tab> ASSIGNING <line>.

          DATA tabix TYPE sy-tabix.
          tabix = sy-tabix.

          "标记被筛行
          READ TABLE filter_rows FROM tabix TRANSPORTING NO FIELDS."with key
          IF sy-subrc = 0.
            ASSIGN COMPONENT filter_column OF STRUCTURE <line> TO <field>.
            IF sy-subrc = 0.
              <field> = abap_true.
            ENDIF.
          ENDIF.

        ENDLOOP.

    ENDCASE.

    demark_all_columns( ).

  ENDMETHOD.


  METHOD zif_table_control~hide_empty_columns.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.
    IF <tab> IS INITIAL."数据为空时，隐藏空列会把整个TC隐藏
      RETURN.
    ENDIF.

    DATA field_prefix TYPE string.
    field_prefix = get_field_prefix( ).

    IF empty_fields IS INITIAL.

      DATA: field TYPE lvc_s_fcat.
      LOOP AT get_visible_columns( ) INTO field.

        DATA: where(72) TYPE c.
        CONCATENATE field-fieldname
                    '<> SPACE AND'
                    field-fieldname
                    '<> ''0'''
               INTO where
       SEPARATED BY space.

        LOOP AT <tab> TRANSPORTING NO FIELDS WHERE (where). "#EC CI_NESTED
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          set_column_invisible( field_prefix && '-' && field-fieldname ).

          DATA: empty_field TYPE ty_empty_field.
          empty_field-field = field-fieldname.
          APPEND empty_field TO empty_fields.
        ENDIF.

        CLEAR where.

      ENDLOOP.

    ELSE.

      FIELD-SYMBOLS <empty_field> LIKE LINE OF empty_fields.
      LOOP AT empty_fields ASSIGNING <empty_field>.
        set_column_visible( field_prefix && '-' && <empty_field>-field ).
      ENDLOOP.
      IF sy-subrc = 0.
        FREE: empty_fields.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_table_control~insert_line.

    CHECK is_line_insert( ).

    "set the new top line
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    DATA lines TYPE i.
    lines = lines( <tab> ).

    IF lines >= screen_lines.
      IF get_current_line( ) <> lines.
        set_top_line( lines - screen_lines + 1 ).
      ELSE.
        set_top_line( lines ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_table_control~mark_all_columns.
    mark_columns( abap_true ).
  ENDMETHOD.


  METHOD zif_table_control~mark_all_lines.
    mark_lines( abap_true ).
  ENDMETHOD.


  METHOD zif_table_control~mass_column.

    CHECK data IS BOUND.

    DATA: tc_column TYPE scxtab_column.
    tc_column = get_column( screen_util->get_field_name_by_cursor( ) ).

    IF tc_column IS INITIAL OR tc_column-screen-input = 0."列不可编辑时不能批量复制
      RETURN.
    ENDIF.

    "取行号
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.
    IF copy_from_top = abap_true.
      FIELD-SYMBOLS: <line> TYPE any.
      READ TABLE <tab> ASSIGNING <line> INDEX 1.
    ELSE.
      READ TABLE <tab> ASSIGNING <line> INDEX get_current_line( ).
    ENDIF.

    "取列名
    DATA: column_name TYPE scxtab_column-screen-name.
    column_name = screen_util->get_no_prefix_column_name( tc_column-screen-name ).

    "取CELL值
    FIELD-SYMBOLS: <field_source> TYPE any.
    ASSIGN COMPONENT column_name OF STRUCTURE <line> TO <field_source>.
    CHECK sy-subrc = 0.

    "设定粘贴起始行
    DATA: beginning_line TYPE sy-index.
    IF paste_from_top = abap_true.
      beginning_line = 1.
    ELSE.
      beginning_line = get_current_line( ).
    ENDIF.

    LOOP AT <tab> ASSIGNING <line> FROM beginning_line.

*      "通过配置表配成不可编辑时，不能粘贴
*      READ TABLE parent->dynpro_attr_setting ASSIGNING <setting> WITH KEY tc_name = id
*                                                                          name    = ''
*                                                                          zmode   = display_mode
*                                                                          BINARY SEARCH.
      IF sy-subrc = 0.

      ENDIF.

      FIELD-SYMBOLS: <field> TYPE any.
      ASSIGN COMPONENT column_name OF STRUCTURE <line> TO <field>.
      IF sy-subrc = 0.
        <field> = <field_source>.
      ENDIF.

    ENDLOOP.

    screen_util->execute_fcode( '/00' ).

  ENDMETHOD.


  METHOD zif_table_control~pai_tc_line.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.

    ASSIGN data->*    TO <table>.
    ASSIGN data_wa->* TO <line>.

    MODIFY <table> FROM <line> INDEX tc->*-current_line.

  ENDMETHOD.


  METHOD zif_table_control~pbo_tc_line.
    screen_lines = sy-loopc.
    set_component_attr_by_setting( ).
  ENDMETHOD.


  METHOD zif_table_control~poh ##NEEDED.
    "to be redefine
  ENDMETHOD.


  METHOD zif_table_control~pov.
*    screen_util->execute_fcode( zcl_tscreen_util=>c_fcode_tc_pov_bring_out && ':' && parent->cursor_filed ).
    screen_util->execute_fcode( zcl_tscreen_util=>c_fcode_tc_pov_bring_out ).
  ENDMETHOD.


  METHOD zif_table_control~scroll.

    IF tc->*-lines = 0.

      DATA: tc_new_top_line TYPE i.
      tc_new_top_line = 1.

    ELSEIF ucomm IS NOT INITIAL.

      CALL FUNCTION 'SCROLLING_IN_TABLE'
        EXPORTING
          entry_act             = tc->*-top_line
          entry_from            = 1
          entry_to              = tc->*-lines
          last_page_full        = 'X'
          loops                 = screen_lines
          ok_code               = ucomm
          overlapping           = 'X'
        IMPORTING
          entry_new             = tc_new_top_line
        EXCEPTIONS
          no_entry_or_page_act  = 1
          no_entry_to           = 2
          no_ok_code_or_page_go = 3
          OTHERS                = 0. "#EC CI_SUBRC
      IF sy-subrc = 0.
      ENDIF.

      "set the new top line
      set_top_line( tc_new_top_line ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_table_control~set_visibility.

    tc->*-invisible = visibility.

    FIELD-SYMBOLS <tc_column> TYPE scxtab_column.
    LOOP AT tc->*-cols ASSIGNING <tc_column>.
      <tc_column>-screen-invisible = visibility.
    ENDLOOP.

    "和TC相关的控件，也要保持和TC一致的显隐性
    DATA mode TYPE i.
    IF visibility = abap_true.
      mode = 1.
    ELSE.
      mode = 0.
    ENDIF.
    LOOP AT SCREEN.

      CHECK screen-name <> tc_name &&'_HIDE'."隐藏按钮不受显隐性影响，否则隐藏了就回不去了

      CHECK screen-name CS tc_name."限制只让TC相关元素受影响
*      screen-active = mode.
      screen-invisible = mode.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_table_control~show_row_content.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    FIELD-SYMBOLS: <line> TYPE any.
    READ TABLE <tab> ASSIGNING <line> INDEX index.
    CHECK sy-subrc = 0 AND <line> IS NOT INITIAL.

    DATA: outdatas TYPE lvc_t_deta.
    DATA: fieldcatlog  TYPE lvc_s_fcat.
    LOOP AT get_visible_columns( ) INTO fieldcatlog.

      DATA outdata TYPE lvc_s_deta.
      CLEAR: outdata.

      FIELD-SYMBOLS: <field> TYPE any.
      ASSIGN COMPONENT fieldcatlog-fieldname OF STRUCTURE <line> TO <field>.
      IF sy-subrc = 0.
        outdata-columntext = fieldcatlog-coltext.
        IF outdata-columntext IS INITIAL.
          outdata-columntext = fieldcatlog-reptext.
        ENDIF.
        outdata-value = <field>.
        CONDENSE outdata-value.
        APPEND outdata TO outdatas.
      ENDIF.

    ENDLOOP.

    DATA: outtable     TYPE lvc_t_detm,
          outdata_line TYPE lvc_s_detm.

    CLEAR: outdata_line.
    outdata_line-blockindex = 1.
    outdata_line-detailtab = outdatas.
    APPEND outdata_line TO outtable.

    DATA: fieldcatlogs TYPE lvc_t_fcat.
    CLEAR: fieldcatlogs.
    CLEAR: fieldcatlog.
    fieldcatlog-fieldname = 'COLUMNTEXT'.
    fieldcatlog-coltext   = '列'.
    fieldcatlog-outputlen = '30'.
    APPEND fieldcatlog TO fieldcatlogs.

    CLEAR: fieldcatlog.
    fieldcatlog-fieldname = 'VALUE'.
    fieldcatlog-coltext   = '内容'.
    fieldcatlog-outputlen = '50'.
    APPEND fieldcatlog TO fieldcatlogs.

    DATA: layout TYPE lvc_s_layo.
    layout-no_toolbar = abap_true.

    CALL FUNCTION 'LVC_ITEM_DETAIL'
      EXPORTING
        i_title               = 'DETAIL'
        i_screen_start_column = 0
        i_screen_start_line   = 0
        i_screen_end_column   = 0
        i_screen_end_line     = 0
        it_fieldcatalog       = fieldcatlogs
        is_layout             = layout
      TABLES
        t_outtab              = outtable.

  ENDMETHOD.


  METHOD zif_table_control~show_row_ddic_detail.

    "此代码改编自 SE16N 细节查看按钮部分源码,适用任何列表展示功能使用，而不局限TC或ALV某一类

    DATA: details TYPE STANDARD TABLE OF se16n_selfields,
          detail  LIKE LINE OF details.

    DATA: field TYPE lvc_s_fcat.

    DATA: dd02v  TYPE dd02v.
    DATA: dd27pt TYPE STANDARD TABLE OF dd27p,
          dd27p  LIKE LINE OF dd27pt.

    DATA: tabname   TYPE ddobjname,
          fieldname TYPE dfies-lfieldname.

    DATA: rseumod  TYPE rseumod.
    DATA: view     TYPE dd25v-viewname.
    DATA: dfies    TYPE dfies.

    DATA: timestmp TYPE cest1-timestmp,
          date     LIKE sy-datum,
          time     LIKE sy-uzeit.

    FIELD-SYMBOLS: <f>        TYPE any,
                   <g>        TYPE any,
                   <tab_line> TYPE any.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.
    READ TABLE <tab> INDEX row ASSIGNING <tab_line>.
    CHECK sy-subrc = 0.

*.in case of views, some texts are not read by FIELDINFO_GET
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = ref_structure_name
        langu         = sy-langu
      IMPORTING
        dd02v_wa      = dd02v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND
       dd02v-tabclass = 'VIEW'.
      view = ref_structure_name.
      CALL FUNCTION 'DD_VIEW_GET'
        EXPORTING
          view_name      = view
          withtext       = 'X'
        TABLES
          dd27p_tab_a    = dd27pt
        EXCEPTIONS
          access_failure = 1
          OTHERS         = 2. "#EC CI_SUBRC
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    CLEAR details.
    LOOP AT get_visible_columns( ) INTO field.
      CLEAR detail.

      tabname   = field-ref_table.
      fieldname = field-ref_field.

      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname    = tabname
          lfieldname = fieldname
        IMPORTING
          dfies_wa   = dfies
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING dfies TO detail ##ENH_OK.
*........fixed type in table leads to missing text
        IF detail-scrtext_l IS INITIAL.
*......if the table is a view with tables with direct type input even
*......fieldtext could be empty
          IF dfies-fieldtext IS INITIAL.
            READ TABLE dd27pt INTO dd27p WITH KEY viewfield = dfies-fieldname. "#EC CI_STDSEQ
            IF sy-subrc = 0.
              detail-scrtext_l = dd27p-ddtext.
            ELSE.
              detail-scrtext_l = field-fieldname.
            ENDIF.
          ELSE.
            detail-scrtext_l = dfies-fieldtext.
          ENDIF.
        ENDIF.
      ELSE.
        detail-scrtext_l = field-fieldname.
      ENDIF.
      ASSIGN COMPONENT field-fieldname OF STRUCTURE <tab_line> TO <f>.
      detail-fieldname = field-fieldname.
      detail-tabname   = field-ref_table.
*    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< begin of 1424238
      IF dfies-datatype = 'CURR' AND dfies-tabname = dfies-reftable.
        ASSIGN COMPONENT dfies-reffield OF STRUCTURE <tab_line> TO <g>.
        WRITE <f> TO detail-low CURRENCY <g> LEFT-JUSTIFIED.
      ELSE.
*.......convert timestamp into nice format
        IF dfies-domname = 'RKE_TSTMP'.
          timestmp = <f>.
          CALL FUNCTION 'RKE_TIMESTAMP_CONVERT_OUTPUT'
            EXPORTING
              i_dayst    = sy-dayst
              i_timestmp = timestmp
              i_tzone    = sy-tzone
            IMPORTING
              e_date     = date
              e_time     = time.
*          concatenate date ',' time into detail-low
*             in CHARACTER MODE.
          WRITE date TO detail-low(10).
          WRITE time TO detail-low+12(8).
        ELSE.
          WRITE <f> TO detail-low LEFT-JUSTIFIED.
        ENDIF.
      ENDIF.
*    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< end of 1424238
*..consider user setting in terms of conversion exits
*..if tbconvert is set, show the value always converted
      SELECT SINGLE *                         "#EC CI_ALL_FIELDS_NEEDED
        FROM rseumod INTO rseumod WHERE uname = sy-uname. "#EC CI_SEL_NESTED
      IF sy-subrc = 0 AND rseumod-tbconvert = abap_true.
        detail-low_noconv = detail-low.
      ELSE.
*......unconverted line
        detail-low_noconv = <f>.
        SHIFT detail-low_noconv LEFT DELETING LEADING space.
      ENDIF.
      APPEND detail TO details.
    ENDLOOP.

    CALL FUNCTION 'TSWUSL_SHOW_DETAIL'
      TABLES
        it_selfields = details.


  ENDMETHOD.


  METHOD zif_table_control~sort.

    DATA: sortorder_tab TYPE abap_sortorder_tab,
          sortorder     LIKE LINE OF sortorder_tab.

    FIELD-SYMBOLS <selected_column> TYPE lvc_s_col.
    LOOP AT get_selected_columns( ) ASSIGNING <selected_column>.

      CLEAR: sortorder.

      sortorder-name = <selected_column>-fieldname.

      CASE ucomm.
        WHEN 'UP'.
          sortorder-descending = abap_false.
        WHEN 'DOWN'.
          sortorder-descending = abap_true.
      ENDCASE.
      sortorder-astext = abap_false.

      APPEND sortorder TO sortorder_tab.

    ENDLOOP.
    IF sy-subrc = 0.
      FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
      ASSIGN data->* TO <tab>.
      SORT <tab> STABLE BY (sortorder_tab).
    ENDIF.

    demark_all_columns( ).

  ENDMETHOD.


  METHOD zif_table_control~unfilter.

    FIELD-SYMBOLS: <line>  TYPE any,
                   <field> TYPE any.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      ASSIGN COMPONENT filter_column OF STRUCTURE <line> TO <field>.
      IF sy-subrc = 0.
        <field> = abap_false.
      ENDIF.

    ENDLOOP.

    demark_all_columns( ).

    in_filter_mode = abap_false.

  ENDMETHOD.


  METHOD zif_table_control~user_command.


    SEARCH ok_code->* FOR tc_name."功能码必须包含TC名字
    IF sy-subrc <> 0 AND ok_code->* <> 'PICK'."PICK代表点击事件
      RETURN.
    ENDIF.

    DATA ucomm TYPE sy-ucomm.
    IF ok_code->* = 'PICK'.
      ucomm = parent->cursor_filed.
      parent->cursor_filed_value = get_cell_value_by_cursor( ).
    ELSE.
      DATA: offset TYPE i.
      offset = strlen( tc_name ) + 1.
      ucomm = ok_code->*+offset.
    ENDIF.

    CASE ucomm.

      WHEN 'INSR'.

        insert_line( ).

      WHEN 'DELE'.

        delete_line( ).

      WHEN 'P--' OR                     "top of list
           'P-'  OR                     "previous page
           'P+'  OR                     "next page
           'P++'.                       "bottom of list

        scroll( ucomm ).

      WHEN 'MARK'.

        mark_all_lines( ).

      WHEN 'DMRK'.

        demark_all_lines( ).

      WHEN 'COVT'.

        convert_all_lines_mark( ).

      WHEN 'SORT_UP'.

        "sort tc data ASCENDING
        sort( 'UP' ).

      WHEN 'SORT_DOWN'.

        "sort tc data DESCENDING
        sort( 'DOWN' ).

      WHEN 'MASS'.

        mass_column( ).

      WHEN 'FILTER'.

        demark_all_lines( ).
        filter( ).
        before_filter_table_writeback( ).

      WHEN 'UNFILTER'.

        unfilter( ).
        after_filter_table_writeback( ).
*        demark_all_lines_of_tc( EXPORTING sel_name = sel_name CHANGING table = table ).

      WHEN 'DETAIL'.

        show_row_content( get_current_line( ) ).

      WHEN 'DDIC'.

        show_row_ddic_detail( get_current_line( ) ).

      WHEN 'EMPTY'.

        hide_empty_columns( ).

      WHEN 'HIDE'.

        IF visibility = zif_tscreen_component=>c_component_visible.
          visibility = zif_tscreen_component=>c_component_invisible.
        ELSE.
          visibility = zif_tscreen_component=>c_component_visible.
        ENDIF.

      WHEN 'IMPORT'.

        import_data( ).

      WHEN 'EXPORT'.

        export_data( ).

      WHEN 'TRANS'.

        transport_data( ).

      WHEN OTHERS.

*        user_command_extend( ucomm ).

    ENDCASE.

    "可以对现有功能增强，也可实现自开发功能
    user_command_extend( ucomm ).

    CLEAR ok_code->*.

  ENDMETHOD.


  METHOD zif_tscreen_component~change_editable.

    CHECK visibility = zif_tscreen_component=>c_component_visible."控件显示时，编辑性才有意义

    me->display_mode = display_mode."控件的可编辑性只取决于屏幕可编辑性,否则请重写此方法

    DATA mode TYPE i.
    CASE display_mode.
      WHEN zcl_tscreen=>display_mode_show.
        mode = 0.
      WHEN OTHERS.
        mode = 1.
    ENDCASE.

    "如果数据源为空，不能编辑
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.
    IF <tab> IS INITIAL.
      mode = 0.
    ENDIF.

    FIELD-SYMBOLS <tc_column> TYPE scxtab_column.
    LOOP AT tc->*-cols ASSIGNING <tc_column>.
      <tc_column>-screen-input = mode.
    ENDLOOP.

    "覆盖TC相关的功能按钮的可编辑性
    LOOP AT SCREEN.

      CHECK screen-name CS tc_name."限制只让TC相关元素受影响

      "过滤功能的按钮需要互斥显示，一个可点另一个就不可点
      IF screen-name = tc_name && '_FILTER'.
        IF in_filter_mode = zif_table_control=>c_filter_on.
          screen-input  = 0.
        ELSE.
          screen-input  = 1.
        ENDIF.
        MODIFY SCREEN.
      ELSEIF screen-name = tc_name && '_UNFILTER'.
        IF in_filter_mode = zif_table_control=>c_filter_on.
          screen-input  = 1.
        ELSE.
          screen-input  = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

    component = get_component( ).

  ENDMETHOD.


  METHOD zif_tscreen_component~change_visibility.

    "控件的显隐性可不取决于屏幕的显隐性
    IF visibility IS SUPPLIED.
      me->visibility = visibility.
    ENDIF.

    IF me->visibility = zif_tscreen_component=>c_component_visible.
      set_visible( ).
    ELSE.
      set_invisible( ).
    ENDIF.

    set_tc_screen_line( get_data_size( ) ).

    component = get_component( ).

  ENDMETHOD.


  METHOD zif_tscreen_component~set_component_attr_by_setting.

    CHECK parent->dynpro_attr_setting IS NOT INITIAL.

    FIELD-SYMBOLS <setting> LIKE LINE OF parent->dynpro_attr_setting.

    LOOP AT SCREEN.

      "优先以具体模式为准
      ##WARN_OK
      READ TABLE parent->dynpro_attr_setting ASSIGNING <setting> WITH KEY tc_name = id
                                                                          name    = screen-name
                                                                          zmode   = display_mode
                                                                          BINARY SEARCH.
      IF sy-subrc <> 0.
        "没有设置具体模式，再使用通用模式
        ##WARN_OK
        READ TABLE parent->dynpro_attr_setting ASSIGNING <setting> WITH KEY tc_name = id
                                                                            name    = screen-name
                                                                            zmode   = '*'
                                                                            BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF sy-subrc = 0.

        "check group
        IF <setting>-group_name IS NOT INITIAL.
          CHECK <setting>-group_name = screen-group1
             OR <setting>-group_name = screen-group2
             OR <setting>-group_name = screen-group3
             OR <setting>-group_name = screen-group4.
        ENDIF.

        set_column_visibility( column_name = screen-name visibility = abap_false ).

        CASE abap_true.
          WHEN <setting>-xoblg."标志: 必需输入字段 ?

            screen-active = screen-input = screen-required = 1.

          WHEN <setting>-xoptn."指示符: 可选字段 ?

            screen-active   = screen-input = 1.
            screen-required = 0.
            set_column_editable( screen-name ).

          WHEN <setting>-xnodi."标识: 字段隐藏

            screen-invisible = <setting>-xnodi.
            screen-active    = screen-input = screen-required = 0.

            set_column_visibility( column_name = screen-name visibility = <setting>-xnodi ).

          WHEN <setting>-xdisp."标志: 字段仅可显示 ?

            screen-active    = 1.
            screen-input     = 0.
            screen-required  = 0.
            screen-invisible = abap_false.
            set_column_display( screen-name ).

        ENDCASE.

        MODIFY SCREEN.

      ENDIF.

    ENDLOOP.

    set_sel_mode_column( 2 ).

  ENDMETHOD.


  METHOD set_row_display.

    LOOP AT SCREEN.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD after_filter_table_writeback  ##NEEDED.
    "to be redefine
  ENDMETHOD.


  METHOD before_filter_table_writeback  ##NEEDED.
    "to be redefine
  ENDMETHOD.


  METHOD bound.

    DATA var TYPE string.

    "tc
    var = to_upper( '(' && program && ')' && tc_name ).
    FIELD-SYMBOLS <tc> TYPE scxtab_control.
    ASSIGN (var) TO <tc>.
    ASSERT sy-subrc = 0.
    GET REFERENCE OF <tc> INTO tc.

    "data source
    var = to_upper( '(' && program && ')' && data_name ).
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    ASSIGN (var) TO <data>.
    ASSERT sy-subrc = 0.
    GET REFERENCE OF <data> INTO data.

    "data_wa
    var = to_upper( '(' && program && ')' && data_wa_name ).
    FIELD-SYMBOLS <data_wa> TYPE any.
    ASSIGN (var) TO <data_wa>.
    ASSERT sy-subrc = 0.
    GET REFERENCE OF <data_wa> INTO data_wa.

    "OK_CODE
    var = to_upper( '(' && program && ')' && 'OK_CODE' ).
    FIELD-SYMBOLS <ok_code> TYPE sy-ucomm.
    ASSIGN (var) TO <ok_code>.
    ASSERT sy-subrc = 0.
    GET REFERENCE OF <ok_code> INTO ok_code.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( tc_name )."使用TC名做控件ID

    initialize_by_tscreen( parent ).

    me->tc_name             = tc_name.
    me->prefix              = prefix.
    me->data_name           = data_source.
    me->data_wa_name        = data_wa.
    me->selbar              = selbar_name.
    me->in_filter_mode      = in_filter_mode.
    me->filter_column       = filter_column_name.
    me->ref_structure_name  = ref_structure_name.
    me->ddic_tabname        = ddic_tabname.

    bound( ).

    IF hide_empty_fields = abap_true.
      hide_empty_columns( ).
    ENDIF.

    "由控件自身负责处理配置表控制的实现
    call_attr_method_by_parent = abap_false.

  ENDMETHOD.


  METHOD get_cell_value_by_cursor.
    value = get_cell_value_by_index( fieldname = screen_util->get_field_name_by_cursor( )
                                     index     = screen_util->get_cursor_line( ) ).
  ENDMETHOD.


  METHOD get_cell_value_by_index.

    DATA: dynpfields TYPE tty_dynpread,
          dynpfield  LIKE LINE OF dynpfields.

    dynpfield-stepl     = index.
    dynpfield-fieldname = fieldname.
    APPEND dynpfield TO dynpfields.

    value = screen_util->get_dynpro_field_value( dynpfields ).

  ENDMETHOD.


  METHOD get_column.
    READ TABLE tc->*-cols INTO tc_column WITH KEY screen-name = column_name. "#EC CI_STDSEQ
  ENDMETHOD.


  METHOD get_current_line.

    GET CURSOR LINE index.
    index = index + tc->*-top_line - 1.

  ENDMETHOD.


  METHOD get_field_prefix.

    DATA: tc_column TYPE scxtab_column.
    READ TABLE tc->*-cols INTO tc_column INDEX 1.
    IF sy-subrc = 0.
      DATA: offset TYPE i.
      offset = screen_util->get_prefix_offset( tc_column-screen-name ).
      prefix = tc_column-screen-name+0(offset).
    ENDIF.

  ENDMETHOD.


  METHOD get_fixed_column.
    fixed_lines = tc->*-fixed_cols.
  ENDMETHOD.


  METHOD get_selected_columns.

    FIELD-SYMBOLS <tc_column> TYPE scxtab_column.
    LOOP AT tc->*-cols ASSIGNING <tc_column> WHERE selected = abap_true. "#EC CI_STDSEQ
      DATA: selected_column LIKE LINE OF selected_columns.
      selected_column-fieldname = screen_util->get_no_prefix_column_name( <tc_column>-screen-name ).
      APPEND selected_column TO selected_columns.
      CLEAR selected_column.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_visible_columns.

    DATA: tc_column TYPE scxtab_column.
    READ TABLE tc->*-cols INTO tc_column INDEX 1.

    DATA: offset TYPE i.
    offset = screen_util->get_prefix_offset( tc_column-screen-name ) + 1.

    FIELD-SYMBOLS <column>  LIKE LINE OF columns.
    columns = screen_util->get_columns_of_structure( ref_structure_name ).

    LOOP AT columns ASSIGNING <column>.

      DATA: tabix TYPE sy-tabix.
      tabix = sy-tabix.

      <column>-ref_field  = <column>-fieldname.

      "如果DDIC有，TC没有，最终以TC为准
      ##WARN_OK
      FIELD-SYMBOLS <tc_column> TYPE scxtab_column.
      READ TABLE tc->*-cols ASSIGNING <tc_column> WITH KEY screen-name = to_upper( tc_column-screen-name+0(offset) && <column>-fieldname ). "#EC CI_STDSEQ
      IF sy-subrc <> 0.
        DELETE columns INDEX tabix.
        CONTINUE.
      ELSE.
        <column>-col_pos = <tc_column>-index.
      ENDIF.

      "如果DDIC有，TC有，但TC列不可见，最终以TC为准
      ##WARN_OK
      READ TABLE tc->*-cols TRANSPORTING NO FIELDS WITH KEY screen-name = to_upper( tc_column-screen-name+0(offset) && <column>-fieldname ) invisible = abap_true. "#EC CI_STDSEQ
      IF sy-subrc = 0.
        DELETE columns INDEX tabix.
      ENDIF.

    ENDLOOP.

    "过滤字段不显示
    DELETE columns WHERE fieldname =  filter_column.     "#EC CI_STDSEQ

    "排序-影响批导
    SORT columns ASCENDING BY col_pos.

  ENDMETHOD.


  METHOD is_deletion_confirmed.
    is_confirmed = abap_true.
  ENDMETHOD.


  METHOD is_line_insert.
    "to be redefine
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    APPEND INITIAL LINE TO <tab>.
    is_line_insert = abap_true.
  ENDMETHOD.


  METHOD mark_columns.

    DATA: tc_column TYPE scxtab_column.
    tc_column-selected = mode.

    MODIFY tc->*-cols FROM tc_column TRANSPORTING selected WHERE selected <> mode. "#EC CI_STDSEQ

  ENDMETHOD.


  METHOD mark_lines.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    FIELD-SYMBOLS <line> TYPE any.
    LOOP AT <tab> ASSIGNING <line>.

      FIELD-SYMBOLS <field> TYPE any.
      ASSIGN COMPONENT selbar OF STRUCTURE <line> TO <field>.
      IF sy-subrc = 0.
        <field> = mode.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_cell_display.

    LOOP AT SCREEN.
      IF screen-name = field_name.
        screen-input = '0'.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.

    table_control ?= get_component( ).
  ENDMETHOD.


  METHOD set_cell_editable.

    LOOP AT SCREEN.
      IF screen-name = field_name.
        screen-input = '1'.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_column.

    FIELD-SYMBOLS <tc_column> TYPE scxtab_column.
    READ TABLE tc->*-cols ASSIGNING <tc_column> WITH KEY screen-name = column_name. "#EC CI_STDSEQ
    IF sy-subrc = 0.
      <tc_column> = tc_column.
    ENDIF.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_column_display.

    tc_column = get_column( column_name ).

    tc_column-screen-input = 0.
    set_column( column_name = column_name tc_column = tc_column ).

  ENDMETHOD.


  METHOD set_column_editable.

    tc_column = get_column( column_name ).

    tc_column-screen-input = 1.
    set_column( column_name = column_name tc_column = tc_column ).

  ENDMETHOD.


  METHOD set_column_invisible.
    set_column_visibility( column_name = column_name visibility = abap_true ).
  ENDMETHOD.


  METHOD set_column_visibility.

    tc_column = get_column( column_name ).

    tc_column-invisible = visibility.
    set_column( column_name = column_name tc_column = tc_column ).

  ENDMETHOD.


  METHOD set_column_visible.
    set_column_visibility( column_name = column_name visibility = abap_false ).
  ENDMETHOD.


  METHOD set_column_vislength.

    tc_column = get_column( column_name ).

    tc_column-vislength = vislength.
    set_column( column_name = column_name tc_column = tc_column ).

  ENDMETHOD.


  METHOD set_cursor.

    SET CURSOR FIELD field LINE index.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_fixed_column.

    tc->*-fixed_cols = fixed_lines.

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD set_invisible.

    set_visibility( abap_true ).

    table_control ?= get_component( ).

  ENDMETHOD.


  METHOD get_data_size.

    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    ASSIGN data->* TO <data>.
    ASSERT sy-subrc = 0.

    size = lines( <data> ).

  ENDMETHOD.


  METHOD set_tc_screen_line.

    tc->*-lines = line.

    component ?= get_component( ).

  ENDMETHOD.


  METHOD zif_tscreen_component~initialize_by_tscreen.

    me->parent  = tscreen.

    program     = tscreen->program.
    dynnr       = tscreen->dynnr.
    screen_util = tscreen->get_screen_util( ).

    CAST zcl_tscreen_with_components( tscreen )->add_component( group = zif_tscreen_component=>c_component_tc component = me ).

  ENDMETHOD.


  METHOD zif_table_control~import_data.

    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    ASSIGN data->* TO <data>.
    IF <data> IS NOT INITIAL.
      DATA answer TYPE c.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = '覆盖？'(001)
          text_question  = '现有数据不为空，是否覆盖已有数据？'(002)
        IMPORTING
          answer         = answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0 OR answer <> '1'.
        RETURN.
      ENDIF.
    ENDIF.

    DATA filename TYPE rlgrap-filename.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        static        = 'X'
        mask          = ',*.XLS,*.XLSX '
      CHANGING
        file_name     = filename
      EXCEPTIONS
        mask_too_long = 1
        OTHERS        = 2. "#EC CI_SUBRC
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    DATA tab TYPE REF TO data.
*&---根据fieldcat生成动态内表
    cl_alv_table_create=>create_dynamic_table(
     EXPORTING
       it_fieldcatalog           = get_visible_columns( )
     IMPORTING
       ep_table                  = tab
     EXCEPTIONS
       generate_subpool_dir_full = 1
       OTHERS                    = 2 ).
    IF sy-subrc <> 0.
      MESSAGE '当前没有可见行或转换失败，请重试'(004) TYPE 'W'.
    ENDIF.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN tab->* TO <tab>.

    DATA raw_data TYPE truxs_t_text_data.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = abap_true
        i_tab_raw_data       = raw_data
        i_filename           = filename
      TABLES
        i_tab_converted_data = <tab>
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE '数据转换失败，请检查EXCEL文件，确保格式与展示表一致'(003) TYPE 'W'.
    ELSE.
      MOVE-CORRESPONDING <tab> TO <data>.
    ENDIF.

  ENDMETHOD.


  METHOD is_line_selected.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN data->* TO <tab>.

    DATA condition TYPE char72.

    condition = selbar && ' = ABAP_TRUE'.

    LOOP AT <tab> TRANSPORTING NO FIELDS WHERE (condition).
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      is_line_selected = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_table_control~export_data.

    CALL FUNCTION 'XML_EXPORT_DIALOG'
      EXPORTING
        i_xml                      = NEW cl_salv_bs_ex_office2007(
                                         cl_salv_ex_util=>factory_result_data_table(
                                           r_data         = data
                                           t_fieldcatalog = get_visible_columns( ) ) )->transform( )
        i_default_extension        = 'XLSX'
        i_initial_directory        = ''
        i_default_file_name        = 'EXPORT.XLSX'
        i_mask                     = 'Excel(*.XLSX)|*XLSX'
        i_application              = ''
      EXCEPTIONS
        application_not_executable = 1
        OTHERS                     = 2. "#EC CI_SUBRC
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD is_ddic_tabname_valid.

    SELECT SINGLE contflag
     FROM dd02l
     INTO contflag
    WHERE tabname  = ddic_tabname
      AND as4local = 'A'
      AND tabclass = 'TRANSP'.
    IF sy-subrc <> 0.
      MESSAGE ddic_tabname && '数据表名不正确，请在实例化时检查传入的表名'(005) TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      is_ddic_tabname_valid = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_table_control~transport_data.

    IF ddic_tabname+0(1) <> 'Z'.
      MESSAGE '不允许操作非自建表' TYPE 'W'.
      RETURN.
    ENDIF.

    DATA contflag TYPE contflag.
    CHECK is_ddic_tabname_valid( IMPORTING contflag = contflag ).

    DATA tab TYPE REF TO data.
    CREATE DATA tab TYPE STANDARD TABLE OF (ddic_tabname).
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN tab->* TO <tab>.

    DATA line TYPE REF TO data.
    CREATE DATA line TYPE (ddic_tabname).
    FIELD-SYMBOLS <line> TYPE any.
    ASSIGN line->* TO <line>.

    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    ASSIGN data->* TO <data>.
    IF is_line_selected( )."选中则只传输选中的数据
      DATA condition TYPE char72.
      condition = selbar && ' = ABAP_TRUE'.
      FIELD-SYMBOLS <data_line> TYPE any.
      FIELD-SYMBOLS <mandt> TYPE sy-mandt.
      LOOP AT <data> ASSIGNING <data_line> WHERE (condition).
        MOVE-CORRESPONDING <data_line> TO <line>.
        ASSIGN COMPONENT 'MANDT' OF STRUCTURE <line> TO <mandt>.
        ASSERT sy-subrc = 0.
        <mandt> = sy-mandt.
        APPEND <line> TO <tab>.
      ENDLOOP.
    ELSE."未选中则不传输
      MESSAGE '未选择数据' TYPE 'W'.
      RETURN.
*      MOVE-CORRESPONDING <data> TO <tab>.
*      LOOP AT <tab> ASSIGNING <line>.
*        ASSIGN COMPONENT 'MANDT' OF STRUCTURE <line> TO <mandt>.
*        ASSERT sy-subrc = 0.
*        <mandt> = sy-mandt.
*      ENDLOOP.
    ENDIF.

    IF contflag = 'A'.
      NEW zcl_transporting_request( )->associate(  )->entrys( <tab> )->commit( ).
    ELSEIF contflag = 'C'.
      zcl_tr_customizing=>get_instance( )->associate( )->entrys( <tab> )->commit( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
