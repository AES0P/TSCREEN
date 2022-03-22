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
*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_RELEASE_LOCK
*&---------------------------------------------------------------------*
*&  自动释放锁JOB程序
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_release_lock.

TABLES: ztscreen_lock.

*&---------------------------------------------------------------------*
*&　　　　SELECT-OPTIONS
*&---------------------------------------------------------------------*
SELECT-OPTIONS: s_guid  FOR ztscreen_lock-guid,
                s_repid FOR ztscreen_lock-repid.

SELECTION-SCREEN SKIP 1.

*&---------------------------------------------------------------------*
*&　　　　PARAMETERS
*&---------------------------------------------------------------------*
PARAMETERS: p_rel TYPE ztscreen_lock-auto_release DEFAULT abap_true.
PARAMETERS: p_force TYPE xfeld DEFAULT abap_false.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-METHODS push_view.

    METHODS constructor.

    METHODS pai REDEFINITION.
    METHODS execute REDEFINITION.
    METHODS show REDEFINITION.

  PROTECTED SECTION.

    METHODS is_outtime
      IMPORTING
        lock           TYPE ztscreen_lock
      RETURNING
        VALUE(outtime) TYPE int4.

    METHODS write_success
      IMPORTING
        lock TYPE ztscreen_lock.

  PRIVATE SECTION.

    DATA locks TYPE STANDARD TABLE OF ztscreen_lock.
    DATA release_list TYPE STANDARD TABLE OF ztscreen_lock.

ENDCLASS.

*&---------------------------------------------------------------------*
*&　　　　CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_prog IMPLEMENTATION.

  METHOD push_view.

    CHECK NOT is_screen_exists( program = sy-repid ).

    DATA view TYPE REF TO zif_tscreen.
    CREATE OBJECT view TYPE lcl_prog.

  ENDMETHOD.

  METHOD constructor.
    release_time = 5."可在构造函数中修改预设释放时间
    super->constructor( ).
  ENDMETHOD.

  METHOD pai.
    IF p_rel = abap_false AND s_guid[] IS INITIAL.
      tlog->add_log( type = 'E' content = 'Must input GUID' ).
      MESSAGE 'Must input GUID' TYPE 'E'.
    ENDIF.
    IF p_force = abap_true AND lines( s_guid[] ) <> 1.
      tlog->add_log( type = 'E' content = 'Only one entry can be unlock in the same time' ).
      MESSAGE 'Only one entry can be unlock in the same time' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD execute.

    SELECT *
      FROM ztscreen_lock
      INTO CORRESPONDING FIELDS OF TABLE locks
     WHERE        guid IN s_guid
       AND       repid IN s_repid
       AND auto_release = p_rel.

  ENDMETHOD.

  METHOD show.

    IF locks IS INITIAL.
      MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
      tlog->add_log( type = 'E' content = 'No data found' ).
      RETURN.
    ENDIF.

    LOOP AT locks ASSIGNING FIELD-SYMBOL(<lock>).

      DATA(outtime) = is_outtime( <lock> ).
      IF outtime >= 0.
        write_success( <lock> ).
      ELSE.
        IF p_force = abap_true.
          write_success( <lock> ).
        ELSE.
          outtime = abs( outtime ).
          WRITE: / <lock>-guid,<lock>-repid,<lock>-zylzd1,<lock>-zylzd2,<lock>-zylzd3, '未到时,还需等待' COLOR 6 , outtime , '(S)'."'强制清除' HOTSPOT ON.
          tlog->add_log( type = 'E' content = <lock>-guid && <lock>-repid && <lock>-zylzd1 && <lock>-zylzd2 && <lock>-zylzd3 && '未到时,还需等待' && outtime && '(S)' ).
        ENDIF.
      ENDIF.

    ENDLOOP.
    CLEAR locks.

    IF release_list IS NOT INITIAL.
      DELETE ztscreen_lock FROM TABLE release_list.
      COMMIT WORK.
      CLEAR release_list.
    ENDIF.


  ENDMETHOD.

  METHOD write_success.
    WRITE: / lock-guid,lock-repid,lock-zylzd1,lock-zylzd2,lock-zylzd3, '已清除' COLOR 1.
    tlog->add_log( lock-guid && lock-repid && lock-zylzd1 && lock-zylzd2 && lock-zylzd3 && '已清除' ).
    APPEND lock TO release_list.
  ENDMETHOD.

  METHOD is_outtime.

    DATA: lastchange_ts TYPE timestamp,
          current_ts    TYPE timestamp.

    DATA time_zone TYPE sy-zonlo VALUE 'UTC'.

    CONVERT DATE lock-aedat
            TIME lock-aetim
            INTO TIME STAMP lastchange_ts
            TIME ZONE time_zone.

    CONVERT DATE sy-datum
            TIME sy-uzeit
            INTO TIME STAMP current_ts
            TIME ZONE time_zone.

    "当前时间-上次更改时间-预设时长
    outtime = current_ts - lastchange_ts - lock-release_time * '60'.

  ENDMETHOD.

ENDCLASS.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
