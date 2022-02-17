*&---------------------------------------------------------------------*
*& Report ZAESOP_TSCREEN_02
*&---------------------------------------------------------------------*
*&  将报表程序执行事件等效为选择屏幕的事件流，通过处理每一个事件流方法，响应每一个报表事件
*&---------------------------------------------------------------------*
REPORT zaesop_tscreen_02_1 NO STANDARD PAGE HEADING
                           LINE-SIZE 80 LINE-COUNT 7.

PARAMETERS p_carrid TYPE sflight-carrid.

*&---------------------------------------------------------------------*
*&　　　　CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_prog DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_treport FINAL.

  PUBLIC SECTION.

    CLASS-METHODS push_view.

    METHODS execute REDEFINITION.

    "注意：下列事件不是在1000屏幕中，但是可以写在LCL_PROG类中
    METHODS top_of_page REDEFINITION.
    METHODS at_line_selection REDEFINITION.
    METHODS at_line_ucomm REDEFINITION.
    METHODS end_of_page REDEFINITION.

    TYPES: BEGIN OF sflight_tab_type,
             carrid TYPE sflight-carrid,
             connid TYPE sflight-connid,
             fldate TYPE sflight-fldate,
           END OF sflight_tab_type.

  PRIVATE SECTION.

    DATA: sflight_tab TYPE TABLE OF sflight_tab_type,
          sflight_wa  LIKE LINE  OF sflight_tab.

    DATA lines TYPE i.

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

  METHOD top_of_page.
    WRITE: / sflight_wa-carrid, sflight_wa-connid.
    ULINE.
  ENDMETHOD.

  METHOD execute.

    SET PF-STATUS sy-dynnr.

    SELECT carrid, connid, fldate
      FROM sflight
     WHERE carrid = @p_carrid
     ORDER BY carrid, connid
      INTO CORRESPONDING FIELDS OF TABLE @sflight_tab.    "#EC CI_SUBRC

    LOOP AT sflight_tab INTO sflight_wa.           "#EC CI_LOOP_INTO_WA
      AT NEW connid.
        SELECT COUNT( DISTINCT fldate )
               FROM sflight
               WHERE carrid = @sflight_wa-carrid AND
                     connid = @sflight_wa-connid
               INTO @lines.          "#EC CI_SROFC_NESTED "#EC CI_SUBRC
        lines += 3.
        NEW-PAGE LINE-COUNT lines.
      ENDAT.
      IF sflight_wa-fldate - sy-datum >= 0.
        WRITE: / sflight_wa-fldate COLOR = 5 HOTSPOT ON.
      ELSE.
        WRITE: / sflight_wa-fldate COLOR = 2.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD at_line_selection.
    MESSAGE 'Fldate in the future'(001) TYPE 'S'.
  ENDMETHOD.

  METHOD at_line_ucomm.
    MESSAGE sy-ucomm TYPE 'S'.
  ENDMETHOD.

  METHOD end_of_page.
    ULINE.
  ENDMETHOD.

ENDCLASS.

##INCL_OK
INCLUDE zaesop_tscreen_event_inc."通用EVENT include
