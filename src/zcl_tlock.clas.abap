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
CLASS zcl_tlock DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_tlock .

    ALIASES auto_release
      FOR zif_tlock~auto_release .
    ALIASES has_lock
      FOR zif_tlock~has_lock .
    ALIASES lock
      FOR zif_tlock~lock .
    ALIASES unlock
      FOR zif_tlock~unlock .

    METHODS constructor
      IMPORTING
        !guid TYPE guid .
    METHODS get_condition
      RETURNING
        VALUE(condition) TYPE string .
    METHODS set_condition
      IMPORTING
        !condition TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA guid TYPE guid .
    DATA condition TYPE string .
    DATA release TYPE abap_bool .
    DATA release_time TYPE int1 .
ENDCLASS.



CLASS ZCL_TLOCK IMPLEMENTATION.


  METHOD constructor.
    me->guid = guid.
    condition = 'GUID = GUID'.
  ENDMETHOD.


  METHOD get_condition.
    condition = me->condition.
  ENDMETHOD.


  METHOD set_condition.
    me->condition = condition.
  ENDMETHOD.


  METHOD zif_tlock~auto_release.
    release          = abap_true.
    me->release_time = min.
    tlock = me.
  ENDMETHOD.


  METHOD zif_tlock~has_lock.
    ##WARN_OK
    SELECT SINGLE *
      FROM ztscreen_lock
      INTO lock_info
     WHERE (condition)."#EC CI_NOORDER  "#EC CI_DYNWHERE
    IF sy-subrc = 0.
      has_lock = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_tlock~lock.

    DATA lock TYPE ztscreen_lock.

    ##ENH_OK
    MOVE-CORRESPONDING ylzd TO lock.

    lock-guid         = guid.
    lock-repid        = sy-cprog.
    lock-auto_release = release.
    lock-release_time = release_time.

    lock-erdat = sy-datum.
    lock-ertim = sy-uzeit.
    lock-ernam = sy-uname.
    lock-aedat = lock-erdat.
    lock-aetim = lock-ertim.
    lock-aenam = lock-ernam.

    INSERT ztscreen_lock FROM lock.
    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_tlock~unlock.
    DELETE FROM ztscreen_lock WHERE guid = guid.
    COMMIT WORK.
  ENDMETHOD.


  METHOD zif_tlock~update_lock.

    DATA lock TYPE ztscreen_lock.
    IF has_lock( IMPORTING lock_info = lock ).
      lock-aedat = sy-datum.
      lock-aetim = sy-uzeit.
      lock-aenam = sy-uname.
      UPDATE ztscreen_lock FROM lock.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
