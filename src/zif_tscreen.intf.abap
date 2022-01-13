interface ZIF_TSCREEN
  public .


  constants DYNPRO_TYPE_NORMAL type SCRHTYPN value '' ##NO_TEXT.
  constants DYNPRO_TYPE_SELSCREEN type SCRHTYPN value 'S' ##NO_TEXT.
  constants DYNPRO_TYPE_SUBSCREEN type SCRHTYPI value 'I' ##NO_TEXT.
  constants DYNPRO_TYPE_DIALOG type SCRHTYPM value 'M' ##NO_TEXT.
  data PROGRAM type SYREPID read-only .
  data DYNNR type SY-DYNNR read-only .
  data DYNNR_SUPER type SY-DYNNR read-only .
  data DYNPRO_TYPE type SCRHTYP read-only .
  data IS_INITIALIZED type ABAP_BOOL read-only .

  methods HANDLE_EVENT
    importing
      !EVENT type C
    raising
      ZCX_TSCREEN .
  methods PBO .
  methods PAI
    importing
      !UCOMM type SY-UCOMM default SY-UCOMM .
  methods POH .
  methods POV .
  methods EXIT
    raising
      ZCX_TSCREEN .
endinterface.
