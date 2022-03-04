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
CLASS zcl_tscreen_root DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcx_tscreen .

  PUBLIC SECTION.

    INTERFACES zif_tscreen
      ABSTRACT METHODS handle_event
      pai
      pbo
      poh
      pov .

    ALIASES dynnr
      FOR zif_tscreen~dynnr .
    ALIASES dynnr_super
      FOR zif_tscreen~dynnr_super .
    ALIASES dynpro_type
      FOR zif_tscreen~dynpro_type .
    ALIASES dynpro_type_dialog
      FOR zif_tscreen~dynpro_type_dialog .
    ALIASES dynpro_type_normal
      FOR zif_tscreen~dynpro_type_normal .
    ALIASES dynpro_type_selscreen
      FOR zif_tscreen~dynpro_type_selscreen .
    ALIASES dynpro_type_subscreen
      FOR zif_tscreen~dynpro_type_subscreen .
    ALIASES is_initialized
      FOR zif_tscreen~is_initialized .
    ALIASES program
      FOR zif_tscreen~program .
    ALIASES exit
      FOR zif_tscreen~exit .
    ALIASES handle_event
      FOR zif_tscreen~handle_event .
    ALIASES pai
      FOR zif_tscreen~pai .
    ALIASES pbo
      FOR zif_tscreen~pbo .
    ALIASES poh
      FOR zif_tscreen~poh .
    ALIASES pov
      FOR zif_tscreen~pov .

    TYPES ty_fcode TYPE fcode .
    TYPES:
      tty_fcode TYPE STANDARD TABLE OF ty_fcode WITH DEFAULT KEY .

    CONSTANTS log_object TYPE balobj_d VALUE 'ZGBC' ##NO_TEXT.
    CONSTANTS log_sub_object TYPE balsubobj VALUE 'ZGBC001' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !program         TYPE syrepid
        !dynnr           TYPE sy-dynnr
        !dynnr_super     TYPE sy-dynnr OPTIONAL
        !dynpro_type     TYPE scrhtyp OPTIONAL
        !pfstatus        TYPE sypfkey OPTIONAL
        !pfstatus_repid  TYPE syrepid OPTIONAL
        !excluding_fcode TYPE tty_fcode OPTIONAL
        !titlebar        TYPE gui_title OPTIONAL
        !titlebar_repid  TYPE syrepid OPTIONAL
        !titlebar_var1   TYPE string OPTIONAL
        !titlebar_var2   TYPE string OPTIONAL
        !titlebar_var3   TYPE string OPTIONAL
        !titlebar_var4   TYPE string OPTIONAL
        !titlebar_var5   TYPE string OPTIONAL .
    METHODS initialize_pbo_by_dynnr
      IMPORTING
        VALUE(dynnr)   TYPE sy-dynnr DEFAULT sy-dynnr
      RETURNING
        VALUE(tscreen) TYPE REF TO zcl_tscreen_root .
    METHODS initialize_pbo .
    METHODS set_pfstatus
      IMPORTING
        !pfstatus              TYPE sypfkey
        !pfstatus_repid        TYPE syrepid OPTIONAL
        VALUE(excluding_fcode) TYPE tty_fcode OPTIONAL .
    METHODS set_titlebar
      IMPORTING
        !titlebar       TYPE gui_title
        !titlebar_repid TYPE syrepid OPTIONAL
        !titlebar_var1  TYPE string OPTIONAL
        !titlebar_var2  TYPE string OPTIONAL
        !titlebar_var3  TYPE string OPTIONAL
        !titlebar_var4  TYPE string OPTIONAL
        !titlebar_var5  TYPE string OPTIONAL .
  PROTECTED SECTION.

    CLASS-DATA if_log_record_pai TYPE abap_bool VALUE abap_true ##NO_TEXT.
    CLASS-DATA if_log_level TYPE ballevel VALUE '1' ##NO_TEXT.
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
    DATA tlog TYPE REF TO zif_tlog .
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

    tlog = zcl_tlog=>get_instance( object         = zcl_tscreen_root=>log_object
                                   subobject      = zcl_tscreen_root=>log_sub_object
                                   identity       = CONV balnrext( program )
                                   level          = zcl_tscreen_root=>if_log_level
                                   del_not_before = abap_true ).

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
  ENDMETHOD.                                             "#EC CI_VALPAR


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
