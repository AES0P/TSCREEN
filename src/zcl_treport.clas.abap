CLASS zcl_treport DEFINITION
  PUBLIC
  INHERITING FROM zcl_tscreen
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA guid TYPE guid READ-ONLY .

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
    METHODS initialize .
    METHODS check_authority .
    METHODS execute .
    METHODS show .
    METHODS set_title
      IMPORTING
        !tabname TYPE tabname
        !langu   TYPE sy-langu DEFAULT sy-langu
      CHANGING
        !title   TYPE any .

    METHODS zif_tscreen~exit
        REDEFINITION .
    METHODS zif_tscreen~handle_event
        REDEFINITION .
    METHODS zif_tscreen~pbo
        REDEFINITION .
  PROTECTED SECTION.
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
    TRY.
        guid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
        ##NO_TEXT        tlog->add_log( 'Started' ).
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
    ##NO_TEXT    tlog->add_log( 'Ended' )->save_log( guid = guid ).
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
            <field> = dd03t[ fieldname = <component>-name ]-ddtext. "#EC CI_STDSEQ
          ##NO_HANDLER  CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
