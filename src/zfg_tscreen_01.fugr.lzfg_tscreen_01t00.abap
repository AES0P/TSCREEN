*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTDYNPRO_ATTR...................................*
DATA:  BEGIN OF STATUS_ZTDYNPRO_ATTR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTDYNPRO_ATTR                 .
CONTROLS: TCTRL_ZTDYNPRO_ATTR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTDYNPRO_ATTR                 .
TABLES: ZTDYNPRO_ATTR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
