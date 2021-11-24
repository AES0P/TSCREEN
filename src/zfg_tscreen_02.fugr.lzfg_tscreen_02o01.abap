*----------------------------------------------------------------------*
***INCLUDE LZFG_TSCREEN_02O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  ##NEEDED
  DATA exclude_fcode TYPE STANDARD TABLE OF fcode.
  IF is_readonly = abap_true
    AND CAST zif_tscreen_component( editor )->display_mode <> zcl_tscreen=>display_mode_show.
    APPEND 'DIS_MODE' TO exclude_fcode.
    APPEND 'SAVE' TO exclude_fcode.
    CAST zif_tscreen_component( editor )->change_editable( zcl_tscreen=>display_mode_show ).
  ELSE.
    CLEAR exclude_fcode.
  ENDIF.

  SET PF-STATUS sy-dynnr EXCLUDING exclude_fcode.
  SET TITLEBAR sy-dynnr WITH editor->title.

ENDMODULE.
