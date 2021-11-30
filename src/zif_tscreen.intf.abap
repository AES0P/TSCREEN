INTERFACE zif_tscreen
  PUBLIC .


  CONSTANTS dynpro_type_normal TYPE scrhtypn VALUE '' ##NO_TEXT.
  CONSTANTS dynpro_type_selscreen TYPE scrhtypn VALUE 'S' ##NO_TEXT.
  CONSTANTS dynpro_type_subscreen TYPE scrhtypi VALUE 'I' ##NO_TEXT.
  CONSTANTS dynpro_type_dialog TYPE scrhtypm VALUE 'M' ##NO_TEXT.
  DATA program TYPE syrepid READ-ONLY .
  DATA dynnr TYPE sy-dynnr READ-ONLY .
  DATA dynnr_super TYPE sy-dynnr READ-ONLY .
  DATA dynpro_type TYPE scrhtyp READ-ONLY .
  DATA is_initialized TYPE abap_bool READ-ONLY .

  METHODS handle_event
    IMPORTING
      !event TYPE c
    RAISING
      zcx_tscreen .
  METHODS pbo .
  METHODS pai
    IMPORTING
      !ucomm TYPE sy-ucomm DEFAULT sy-ucomm .
  METHODS poh .
  METHODS pov .
  METHODS exit
    RAISING
      zcx_tscreen .
ENDINTERFACE.
