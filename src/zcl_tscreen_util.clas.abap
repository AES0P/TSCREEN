CLASS zcl_tscreen_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zif_tscreen .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_element,
        element TYPE screen-name.
    TYPES: END OF ty_element .
    TYPES:
      tty_d021s TYPE STANDARD TABLE OF d021s WITH EMPTY KEY .
    TYPES:
      tty_dynpread TYPE STANDARD TABLE OF dynpread WITH EMPTY KEY .
    TYPES:
      tty_elements TYPE STANDARD TABLE OF ty_element WITH EMPTY KEY .

    CLASS-METHODS get_instance
      IMPORTING
        !program    TYPE sy-repid OPTIONAL
      RETURNING
        VALUE(util) TYPE REF TO zcl_tscreen_util .
    METHODS get_dynnr
      RETURNING
        VALUE(dynnr) TYPE sy-dynnr .
    METHODS set_dynnr
      IMPORTING
        !dynnr TYPE sy-dynnr DEFAULT sy-dynnr .
    METHODS execute_fcode
      IMPORTING
        !fcode TYPE c DEFAULT '%X%X%X' .
    METHODS f4_event
      IMPORTING
        VALUE(key_field)     TYPE dfies-fieldname
        !value_tab           TYPE STANDARD TABLE
        VALUE(callback_form) TYPE sy-xform OPTIONAL
        VALUE(display)       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(value)         TYPE shvalue_d .
    METHODS read_sysmsg
      IMPORTING
        VALUE(show) TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(msg)  TYPE bapi_msg .
    METHODS set_listbox
      IMPORTING
        VALUE(vrm_id)     TYPE vrm_id
        VALUE(vrm_values) TYPE vrm_values .
    METHODS get_dynpro_field_value
      IMPORTING
        VALUE(dynpro_fields) TYPE tty_dynpread
      RETURNING
        VALUE(value)         TYPE dynfieldvalue .
    METHODS get_all_elements
      RETURNING
        VALUE(field_list) TYPE tty_d021s .
    METHODS is_valid_string
      IMPORTING
        VALUE(string) TYPE any
      RETURNING
        VALUE(valid)  TYPE abap_bool .
    METHODS get_columns_of_structure
      IMPORTING
        VALUE(structure_name) TYPE c
      RETURNING
        VALUE(columns)        TYPE lvc_t_fcat .
    METHODS get_structure_field_value
      IMPORTING
        VALUE(field_name) TYPE c
      RETURNING
        VALUE(value)      TYPE dynfieldvalue .
    METHODS get_prefix_offset
      IMPORTING
        VALUE(screen_name) TYPE c
      RETURNING
        VALUE(offset)      TYPE i .
    METHODS get_no_prefix_column_name
      IMPORTING
        VALUE(screen_name) TYPE c
      RETURNING
        VALUE(field_name)  TYPE fieldname .
    METHODS call_transaction
      IMPORTING
        VALUE(tcode) TYPE sy-tcode
        VALUE(value) TYPE dynfieldvalue .
    METHODS get_field_name_by_cursor
      RETURNING
        VALUE(field_name) TYPE feld-name .
    METHODS get_field_value_by_cursor
      RETURNING
        VALUE(value) TYPE dynfieldvalue .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA tscreen_util TYPE REF TO zcl_tscreen_util .
    DATA program TYPE syrepid .
    DATA dynnr TYPE sy-dynnr .
    DATA bring_out_data TYPE REF TO data ##NEEDED.

    METHODS constructor
      IMPORTING
        !program TYPE sy-repid .
ENDCLASS.



CLASS ZCL_TSCREEN_UTIL IMPLEMENTATION.


  METHOD set_listbox.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = vrm_id
        values          = vrm_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.

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
      read_sysmsg( abap_true ).
    ELSE.
      READ TABLE dynpro_fields INTO dynpro_field INDEX 1.
      value = dynpro_field-fieldvalue.
    ENDIF.

  ENDMETHOD."#EC CI_VALPAR


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

    DATA: return_tab      TYPE TABLE OF ddshretval,
          return_tab_line LIKE LINE OF return_tab.

    DATA: dynpfld_mapping_tab TYPE TABLE OF dselc.

    CLEAR: return_tab,return_tab_line,
           dynpfld_mapping_tab.

    CLEAR bring_out_data.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = key_field
        value_org        = 'S'
        dynpprog         = program
        dynpnr           = dynnr
        callback_program = program
        display          = display
        callback_form    = callback_form  ""控制先输入筛选条件，后展示值集
      TABLES
        value_tab        = value_tab
        return_tab       = return_tab
        dynpfld_mapping  = dynpfld_mapping_tab
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
*      read_sysmsg( abap_true ).
    ELSE.
      READ TABLE return_tab INTO return_tab_line INDEX 1.
      value = return_tab_line-fieldval.

      CHECK value IS NOT INITIAL.
      DATA where TYPE string.
      CONCATENATE key_field '= ''' INTO where SEPARATED BY space." key_field = '
      CONCATENATE where value '''' INTO where."  key_field = 'value'

      LOOP AT value_tab REFERENCE INTO bring_out_data WHERE (where).
        EXIT.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD execute_fcode.

    CHECK sy-batch = abap_false.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = fcode
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2."#EC CI_SUBRC
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
        CALL TRANSACTION tcode AND SKIP FIRST SCREEN. "#EC CI_CALLTA

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

  ENDMETHOD."#EC CI_VALPAR
ENDCLASS.
