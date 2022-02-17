*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2022.02.14 at 15:16:23
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTSCREEN_MANAGER................................*
DATA:  BEGIN OF STATUS_ZTSCREEN_MANAGER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTSCREEN_MANAGER              .
CONTROLS: TCTRL_ZTSCREEN_MANAGER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTSCREEN_MANAGER              .
TABLES: ZTSCREEN_MANAGER               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
