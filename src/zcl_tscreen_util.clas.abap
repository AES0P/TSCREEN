class ZCL_TSCREEN_UTIL definition
  public
  final
  create private

  global friends ZIF_TSCREEN .

public section.

  types:
    BEGIN OF ty_element,
        element TYPE screen-name.
    TYPES: END OF ty_element .
  types:
    tty_d021s TYPE STANDARD TABLE OF d021s WITH DEFAULT KEY .
  types:
    tty_dynpread TYPE STANDARD TABLE OF dynpread WITH DEFAULT KEY .
  types:
    tty_elements TYPE STANDARD TABLE OF ty_element WITH DEFAULT KEY .
  types:
    f4_return_tab TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY .

  constants C_FCODE_TC_POV_BRING_OUT type SY-UCOMM value 'TC_POV_BRING_OUT' ##NO_TEXT.

  class-methods GET_INSTANCE
    importing
      !PROGRAM type SY-REPID optional
    returning
      value(UTIL) type ref to ZCL_TSCREEN_UTIL .
  methods GET_DYNNR
    returning
      value(DYNNR) type SY-DYNNR .
  methods SET_DYNNR
    importing
      !DYNNR type SY-DYNNR default SY-DYNNR .
  methods EXECUTE_FCODE
    importing
      !FCODE type C default '%X%X%X' .
  methods F4_EVENT
    importing
      value(KEY_FIELD) type DFIES-FIELDNAME
      !VALUE_TAB type STANDARD TABLE
      value(CALLBACK_FORM) type SY-XFORM optional
      !CALLBACK_HANDLER type ref to IF_F4CALLBACK_VALUE_REQUEST optional
      value(DISPLAY) type ABAP_BOOL optional
    exporting
      !RETURN_TABLE type F4_RETURN_TAB
    returning
      value(VALUE) type SHVALUE_D .
  methods READ_SYSMSG
    importing
      value(SHOW) type ABAP_BOOL default ABAP_FALSE
    returning
      value(MSG) type BAPI_MSG .
  methods SET_LISTBOX
    importing
      value(VRM_ID) type VRM_ID
      value(VRM_VALUES) type VRM_VALUES .
  methods GET_DYNPRO_FIELD_VALUE
    importing
      value(DYNPRO_FIELDS) type TTY_DYNPREAD
    returning
      value(VALUE) type DYNFIELDVALUE .
  methods GET_ALL_ELEMENTS
    returning
      value(FIELD_LIST) type TTY_D021S .
  methods IS_VALID_STRING
    importing
      value(STRING) type ANY
    returning
      value(VALID) type ABAP_BOOL .
  methods GET_COLUMNS_OF_STRUCTURE
    importing
      value(STRUCTURE_NAME) type C
    returning
      value(COLUMNS) type LVC_T_FCAT .
  methods GET_STRUCTURE_FIELD_VALUE
    importing
      value(FIELD_NAME) type C
    returning
      value(VALUE) type DYNFIELDVALUE .
  methods GET_PREFIX_OFFSET
    importing
      value(SCREEN_NAME) type C
    returning
      value(OFFSET) type I .
  methods GET_NO_PREFIX_COLUMN_NAME
    importing
      value(SCREEN_NAME) type C
    returning
      value(FIELD_NAME) type FIELDNAME .
  methods CALL_TRANSACTION
    importing
      value(TCODE) type SY-TCODE
      value(VALUE) type DYNFIELDVALUE .
  methods GET_FIELD_NAME_BY_CURSOR
    returning
      value(FIELD_NAME) type FELD-NAME .
  methods GET_FIELD_VALUE_BY_CURSOR
    returning
      value(VALUE) type DYNFIELDVALUE .
  methods GET_CURSOR_LINE
    returning
      value(INDEX) type I .
  PROTECTED SECTION.
private section.

  class-data TSCREEN_UTIL type ref to ZCL_TSCREEN_UTIL .
  data PROGRAM type SYREPID .
  data DYNNR type SY-DYNNR .
  data BRING_OUT_DATA type ref to DATA  ##NEEDED.

  methods CONSTRUCTOR
    importing
      !PROGRAM type SY-REPID .
ENDCLASS.



CLASS ZCL_TSCREEN_UTIL IMPLEMENTATION.


  METHOD set_listbox.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = vrm_id
        values          = vrm_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2. "#EC CI_SUBRC
    IF sy-subrc <> 0 ##NEEDED.
    ENDIF.

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD set_dynnr.
    me->dynnr = dynnr.
  ENDMETHOD.


  METHOD read_sysmsg.

    CHECK sy-msgid IS NOT INITIAL.
    CHECK sy-msgty IS NOT INITIAL.
    CHECK sy-msgno IS NOT INITIAL.

    MESSAGE  ID sy-msgid
           TYPE sy-msgty
         NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           INTO msg.

    IF show = abap_true.
      MESSAGE msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_string.
    ##FS_ASSIGN_OK
    FIELD-SYMBOLS <result> TYPE STANDARD TABLE.

    FIND ALL OCCURRENCES OF REGEX '[^[:print:]]'
                               IN string
                          RESULTS <result>
                    IGNORING CASE.
    IF sy-subrc <> 0.
      valid = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_structure_field_value.

    CHECK field_name IS NOT INITIAL.

    DATA: dynpfields TYPE tty_dynpread,
          dynpfield  LIKE LINE OF dynpfields.

    CLEAR: dynpfields,
           dynpfield.

    dynpfield-fieldname = field_name.
    APPEND dynpfield TO dynpfields.

    value = get_dynpro_field_value( dynpfields ).

  ENDMETHOD.


  METHOD get_prefix_offset.

    DATA: name TYPE char40.
    DATA: fdpos TYPE sy-fdpos.

    name = screen_name.

    SEARCH name FOR '-'.
    IF sy-subrc = 0.

      fdpos = sy-fdpos.
      name+fdpos(1) = '/'.

      IF name CA '-'.
        offset = get_prefix_offset( name ).
      ELSE.
        offset = fdpos.
      ENDIF.

    ENDIF .

  ENDMETHOD.


  METHOD get_no_prefix_column_name.

    CHECK screen_name IS NOT INITIAL.

    DATA: offset TYPE i.
    offset = get_prefix_offset( screen_name ) + 1.

    field_name = screen_name+offset.

  ENDMETHOD.


  METHOD get_instance.

    IF tscreen_util IS NOT BOUND.
      CREATE OBJECT tscreen_util
        EXPORTING
          program = program.
    ENDIF.

    util = tscreen_util.

  ENDMETHOD.


  METHOD get_field_value_by_cursor.

    DATA: dynpfields TYPE tty_dynpread,
          dynpfield  LIKE LINE OF dynpfields.

    CLEAR: dynpfields,
           dynpfield.

    DATA fieldname TYPE fieldname.

    GET CURSOR FIELD fieldname.

    dynpfield-fieldname = fieldname.
    APPEND dynpfield TO dynpfields.

    value = get_dynpro_field_value( dynpfields ).

  ENDMETHOD.


  METHOD get_field_name_by_cursor.

    GET CURSOR FIELD field_name.

  ENDMETHOD.


  METHOD get_dynpro_field_value.

    DATA: dynpro_field LIKE LINE OF dynpro_fields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = program
        dynumb               = dynnr
        translate_to_upper   = abap_true
      TABLES
        dynpfields           = dynpro_fields "TABLES参数 值传递不会引发 入参不能修改 的异常，引用传递会
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0 .
*      read_sysmsg( abap_true ).
      read_sysmsg( ).
    ELSE.
      READ TABLE dynpro_fields INTO dynpro_field INDEX 1.
      value = dynpro_field-fieldvalue.
    ENDIF.

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD get_dynnr.
    dynnr = me->dynnr.
  ENDMETHOD.


  METHOD get_columns_of_structure.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_buffer_active  = 'X'
        i_structure_name = structure_name
      CHANGING
        ct_fieldcat      = columns
      EXCEPTIONS
        OTHERS           = 0.

  ENDMETHOD.


  METHOD get_all_elements.

    CALL FUNCTION 'RS_SCRP_GET_SCREEN_INFOS'
      EXPORTING
        dynnr                 = dynnr
        progname              = program
      TABLES
        fieldlist             = field_list
      EXCEPTIONS
        dynpro_does_not_exist = 1
        no_field_list         = 2
        cancelled             = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
      read_sysmsg( abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD f4_event.

    DATA: return_table_line LIKE LINE OF return_table.

    DATA: dynpfld_mapping_tab TYPE TABLE OF dselc.

    CLEAR: return_table,return_table_line,
           dynpfld_mapping_tab.

    CLEAR bring_out_data.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = key_field
        value_org        = 'S'
        dynpprog         = program
        dynpnr           = dynnr
        callback_program = program
        callback_method  = callback_handler
        display          = display
        callback_form    = callback_form  ""控制先输入筛选条件，后展示值集
      TABLES
        value_tab        = value_tab
        return_tab       = return_table
        dynpfld_mapping  = dynpfld_mapping_tab
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
*      read_sysmsg( abap_true ).
    ELSE.
      READ TABLE return_table INTO return_table_line INDEX 1.
      value = return_table_line-fieldval.

      CHECK value IS NOT INITIAL.
      DATA where TYPE string.
      CONCATENATE key_field '= ''' INTO where SEPARATED BY space." key_field = '
      CONCATENATE where value '''' INTO where."  key_field = 'value'

      LOOP AT value_tab REFERENCE INTO bring_out_data WHERE (where).
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        REPLACE '=' IN where WITH 'CS'.
        LOOP AT value_tab REFERENCE INTO bring_out_data WHERE (where).
          EXIT.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD execute_fcode.

    CHECK sy-batch = abap_false.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2. "#EC CI_SUBRC
    IF sy-subrc <> 0 ##NEEDED.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    me->program = program.
  ENDMETHOD.


  METHOD call_transaction.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'                     "930588
      EXPORTING
        tcode  = tcode
      EXCEPTIONS
        ok     = 0
        not_ok = 2
        OTHERS = 3.
    IF sy-subrc <> 0.
      MESSAGE e172(00) WITH tcode.
    ENDIF.

    CHECK value IS NOT INITIAL.

    CASE tcode.
      WHEN 'ME23N'.

        SET PARAMETER ID 'BES' FIELD value.
        CALL TRANSACTION tcode AND SKIP FIRST SCREEN.    "#EC CI_CALLTA

      WHEN 'MM03'.

        DATA ematn TYPE ekpo-ematn.
        ematn = value.

        CALL FUNCTION 'MMPUR_MATERIAL_DISPLAY'
          EXPORTING
            im_matnr      = ematn
*           im_werks      = lv_werks
*           im_lgort      = lv_lgort
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc NE 0.
          read_sysmsg( abap_true ).
        ENDIF.

      WHEN OTHERS.

        "这里没有处理的事务，就在子类自行重写补充

    ENDCASE.

  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD get_cursor_line.
    GET CURSOR LINE index.
  ENDMETHOD.
ENDCLASS.
