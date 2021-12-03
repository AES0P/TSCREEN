class ZCL_TREPORT definition
  public
  inheriting from ZCL_TSCREEN
  abstract
  create public .

public section.

  class-data GUID type GUID read-only .

  methods CONSTRUCTOR
    importing
      !PROGRAM type SYREPID default SY-CPROG
      !DYNNR type SY-DYNNR default SY-DYNNR
      !DYNPRO_TYPE type SCRHTYP default ZCL_TSCREEN=>DYNPRO_TYPE_SELSCREEN
      !DISPLAY_MODE type ABAP_BOOL default ZCL_TSCREEN=>DISPLAY_MODE_MODIFY
      !PFSTATUS type SYPFKEY optional
      !PFSTATUS_REPID type SYREPID optional
      !EXCLUDING_FCODE type TTY_FCODE optional
      !TITLEBAR type GUI_TITLE optional
      !TITLEBAR_REPID type SYREPID optional
      !TITLEBAR_VAR1 type STRING optional
      !TITLEBAR_VAR2 type STRING optional
      !TITLEBAR_VAR3 type STRING optional
      !TITLEBAR_VAR4 type STRING optional
      !TITLEBAR_VAR5 type STRING optional
      !READ_DYNPRO_SETTING type ABAP_BOOL default ABAP_TRUE .
  methods INITIALIZE .
  methods CHECK_AUTHORITY .
  methods EXECUTE .
  methods SHOW .

  methods ZIF_TSCREEN~EXIT
    redefinition .
  methods ZIF_TSCREEN~HANDLE_EVENT
    redefinition .
  methods ZIF_TSCREEN~PBO
    redefinition .
protected section.
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
      WHEN OTHERS.
        super->handle_event( event ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_tscreen~pbo ##NEEDED.
*CALL METHOD SUPER->ZIF_TSCREEN~PBO
*    .
  ENDMETHOD.
ENDCLASS.
