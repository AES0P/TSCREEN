CLASS zcl_tcomponent_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_common,
        group TYPE string,
        id    TYPE string,
      END OF ty_common .
    TYPES:
      BEGIN OF ty_tc.
        INCLUDE TYPE ty_common.
    TYPES:
        ref_structure_name TYPE char255,
        data_source        TYPE string,
        data_wa            TYPE string,
        ddic_tabname       TYPE char255,
        prefix             TYPE char255,
        selbar_name        TYPE char255,
        in_filter_mode     TYPE abap_bool,
        filter_column_name TYPE char255,
        hide_empty_fields  TYPE abap_bool,
      END OF ty_tc .
    TYPES:
      BEGIN OF ty_editor.
        INCLUDE TYPE ty_common.
    TYPES:
        container_name             TYPE char255,
        content                    TYPE string,
        length_content             TYPE i,
        style                      TYPE i,
        wordwrap_mode              TYPE i,
        length_line                TYPE i,
        wordwrap_to_linebreak_mode TYPE i,
        filedrop_mode              TYPE i,
        lifetime                   TYPE i,
        name                       TYPE string,
      END OF ty_editor .

    CLASS-METHODS get_tcomponent
      IMPORTING
        !parent           TYPE REF TO zcl_tscreen
        !params           TYPE string
      RETURNING
        VALUE(tcomponent) TYPE REF TO zif_tscreen_component
      RAISING
        zcx_tscreen
        cx_uuid_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA common_attr TYPE ty_common .
    CLASS-DATA tc_attr TYPE ty_tc .
    CLASS-DATA editor_attr TYPE ty_editor .

    CLASS-METHODS translate_params
      IMPORTING
        !params TYPE string .
ENDCLASS.



CLASS ZCL_TCOMPONENT_FACTORY IMPLEMENTATION.


  METHOD get_tcomponent.

    translate_params( params ).

    CASE common_attr-group.
      WHEN zif_tscreen_component=>c_component_tc.

        tcomponent =  NEW zcl_table_control( parent             = parent
                                             tc_name            = common_attr-id
                                             ref_structure_name = tc_attr-ref_structure_name
                                             data_source        = tc_attr-data_source
                                             data_wa            = tc_attr-data_wa
                                             ddic_tabname       = tc_attr-ddic_tabname
                                             prefix             = tc_attr-prefix
                                             selbar_name        = tc_attr-selbar_name
                                             in_filter_mode     = tc_attr-in_filter_mode
                                             filter_column_name = tc_attr-filter_column_name
                                             hide_empty_fields  = tc_attr-hide_empty_fields ).

      WHEN zif_tscreen_component=>c_component_editor.

        tcomponent =  NEW zcl_text_editor( parent                     = parent
                                           id                         = common_attr-id
                                           container_name             = editor_attr-container_name
                                           content                    = editor_attr-content
                                           length_content             = editor_attr-length_content
                                           style                      = editor_attr-style
                                           wordwrap_mode              = editor_attr-wordwrap_mode
                                           length_line                = editor_attr-length_line
                                           wordwrap_to_linebreak_mode = editor_attr-wordwrap_to_linebreak_mode
                                           filedrop_mode              = editor_attr-filedrop_mode
                                           lifetime                   = editor_attr-lifetime
                                           name                       = editor_attr-name ).

      WHEN OTHERS.
        zcx_tscreen=>raise_text( 'No component found' ).
    ENDCASE.

  ENDMETHOD.


  METHOD translate_params.

    /ui2/cl_json=>deserialize( EXPORTING json = params
                                CHANGING data = common_attr ).

    IF common_attr IS INITIAL.
      ASSERT 1 = 2.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = params
                                CHANGING data = tc_attr ).

    IF tc_attr-prefix IS INITIAL.
      tc_attr-prefix = `TC_`.
    ENDIF.
    IF tc_attr-selbar_name IS INITIAL.
      tc_attr-selbar_name = `SEL`.
    ENDIF.
    IF tc_attr-filter_column_name IS INITIAL.
      tc_attr-filter_column_name = `FILTER`.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = params
                                CHANGING data = editor_attr ).


  ENDMETHOD.
ENDCLASS.
