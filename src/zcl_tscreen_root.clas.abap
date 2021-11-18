class ZCL_TSCREEN_ROOT definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_TSCREEN
      abstract methods HANDLE_EVENT
                       PAI
                       PBO
                       POH
                       POV .

  aliases DYNNR
    for ZIF_TSCREEN~DYNNR .
  aliases DYNNR_SUPER
    for ZIF_TSCREEN~DYNNR_SUPER .
  aliases DYNPRO_TYPE
    for ZIF_TSCREEN~DYNPRO_TYPE .
  aliases DYNPRO_TYPE_DIALOG
    for ZIF_TSCREEN~DYNPRO_TYPE_DIALOG .
  aliases DYNPRO_TYPE_NORMAL
    for ZIF_TSCREEN~DYNPRO_TYPE_NORMAL .
  aliases DYNPRO_TYPE_SELSCREEN
    for ZIF_TSCREEN~DYNPRO_TYPE_SELSCREEN .
  aliases DYNPRO_TYPE_SUBSCREEN
    for ZIF_TSCREEN~DYNPRO_TYPE_SUBSCREEN .
  aliases EXIT
    for ZIF_TSCREEN~EXIT .
  aliases HANDLE_EVENT
    for ZIF_TSCREEN~HANDLE_EVENT .
  aliases IS_INITIALIZED
    for ZIF_TSCREEN~IS_INITIALIZED .
  aliases PAI
    for ZIF_TSCREEN~PAI .
  aliases PBO
    for ZIF_TSCREEN~PBO .
  aliases POH
    for ZIF_TSCREEN~POH .
  aliases POV
    for ZIF_TSCREEN~POV .
  aliases PROGRAM
    for ZIF_TSCREEN~PROGRAM .

  types TY_FCODE type FCODE .
  types:
    tty_fcode TYPE STANDARD TABLE OF ty_fcode WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !PROGRAM type SYREPID
      !DYNNR type SY-DYNNR
      !DYNNR_SUPER type SY-DYNNR optional
      !DYNPRO_TYPE type SCRHTYP optional
      !PFSTATUS type SYPFKEY optional
      !PFSTATUS_REPID type SYREPID optional
      !EXCLUDING_FCODE type TTY_FCODE optional
      !TITLEBAR type GUI_TITLE optional
      !TITLEBAR_REPID type SYREPID optional
      !TITLEBAR_VAR1 type STRING optional
      !TITLEBAR_VAR2 type STRING optional
      !TITLEBAR_VAR3 type STRING optional
      !TITLEBAR_VAR4 type STRING optional
      !TITLEBAR_VAR5 type STRING optional .
  methods INITIALIZE_PBO_BY_DYNNR
    importing
      value(DYNNR) type SY-DYNNR default SY-DYNNR
    returning
      value(TSCREEN) type ref to ZCL_TSCREEN_ROOT .
  methods INITIALIZE_PBO .
  methods SET_PFSTATUS
    importing
      !PFSTATUS type SYPFKEY
      !PFSTATUS_REPID type SYREPID optional
      value(EXCLUDING_FCODE) type TTY_FCODE optional .
  methods SET_TITLEBAR
    importing
      !TITLEBAR type GUI_TITLE
      !TITLEBAR_REPID type SYREPID optional
      !TITLEBAR_VAR1 type STRING optional
      !TITLEBAR_VAR2 type STRING optional
      !TITLEBAR_VAR3 type STRING optional
      !TITLEBAR_VAR4 type STRING optional
      !TITLEBAR_VAR5 type STRING optional .
  PROTECTED SECTION.

    DATA pfstatus TYPE pfstatus .
    DATA pfstatus_repid TYPE syrepid .
    DATA excluding_fcode TYPE tty_fcode .
    DATA titlebar TYPE gui_title .
    DATA titlebar_repid TYPE syrepid .
    DATA titlebar_var1 TYPE string .
    DATA titlebar_var2 TYPE string .
    DATA titlebar_var3 TYPE string .
    DATA titlebar_var4 TYPE string .
    DATA titlebar_var5 TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TSCREEN_ROOT IMPLEMENTATION.


  METHOD constructor.

    me->program     = program.
    me->dynnr       = dynnr.
    me->dynnr_super = dynnr_super.
    me->dynpro_type = dynpro_type.

    set_pfstatus( pfstatus        = pfstatus
                  pfstatus_repid  = pfstatus_repid
                  excluding_fcode = excluding_fcode ).

    set_titlebar( titlebar       = titlebar
                  titlebar_repid = titlebar_repid
                  titlebar_var1  = titlebar_var1
                  titlebar_var2  = titlebar_var2
                  titlebar_var3  = titlebar_var3
                  titlebar_var4  = titlebar_var4
                  titlebar_var5  = titlebar_var5 ).

    zcl_tscreen_stack=>get_instance( )->push( me ).

  ENDMETHOD.


  METHOD initialize_pbo.

    IF pfstatus_repid IS NOT INITIAL.
      SET PF-STATUS pfstatus OF PROGRAM pfstatus_repid EXCLUDING excluding_fcode.
    ELSE.
      SET PF-STATUS pfstatus OF PROGRAM program EXCLUDING excluding_fcode.
    ENDIF.

    IF titlebar_repid IS NOT INITIAL.
      SET TITLEBAR titlebar OF PROGRAM titlebar_repid WITH titlebar_var1
                                                           titlebar_var2
                                                           titlebar_var3
                                                           titlebar_var4
                                                           titlebar_var5.
    ELSE.
      SET TITLEBAR titlebar OF PROGRAM program WITH titlebar_var1
                                                    titlebar_var2
                                                    titlebar_var3
                                                    titlebar_var4
                                                    titlebar_var5.
    ENDIF.

    is_initialized = abap_true.

  ENDMETHOD.


  METHOD initialize_pbo_by_dynnr.

    CHECK dynpro_type = zcl_tscreen_root=>dynpro_type_normal.

    DATA sypfkey TYPE sypfkey.
    sypfkey = dynnr.

    set_pfstatus( sypfkey ).
    set_titlebar( sypfkey ).
    initialize_pbo( ).

    tscreen = me.

  ENDMETHOD.


  METHOD set_pfstatus.
    me->pfstatus        = pfstatus.
    me->pfstatus_repid  = pfstatus_repid.
    me->excluding_fcode = excluding_fcode.
  ENDMETHOD. "#EC CI_VALPAR


  METHOD set_titlebar.

    "标题占位符最多支持9个，一般不会超过5个
    me->titlebar       = titlebar.
    me->titlebar_repid = titlebar_repid.

    me->titlebar_var1 = titlebar_var1.
    me->titlebar_var2 = titlebar_var2.
    me->titlebar_var3 = titlebar_var3.
    me->titlebar_var4 = titlebar_var4.
    me->titlebar_var5 = titlebar_var5.

  ENDMETHOD.


  METHOD zif_tscreen~exit.

    IF dynpro_type <> zif_tscreen=>dynpro_type_subscreen.
      zcl_tscreen_stack=>get_instance( )->pop( ).
    ENDIF.

    CLEAR: program,
           dynnr,
           dynpro_type,
           is_initialized.

    CLEAR: pfstatus,
           pfstatus_repid,
           excluding_fcode,
           titlebar,
           titlebar_repid,
           titlebar_var1,
           titlebar_var2,
           titlebar_var3,
           titlebar_var4,
           titlebar_var5.

  ENDMETHOD.
ENDCLASS.
