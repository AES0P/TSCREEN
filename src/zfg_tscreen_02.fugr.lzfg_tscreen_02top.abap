FUNCTION-POOL zfg_tscreen_02.               "MESSAGE-ID ..

* INCLUDE LZFG_TSCREEN_02D...                " Local class definition

DATA ok_code LIKE sy-ucomm.

##NEEDED DATA is_readonly TYPE abap_bool.
##NEEDED DATA editor TYPE REF TO zif_text_editor.

##NEEDED FIELD-SYMBOLS <text> TYPE any.
