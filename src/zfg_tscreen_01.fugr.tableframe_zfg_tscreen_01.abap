*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_TSCREEN_01
*   generation date: 2021.11.16 at 14:42:00
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_TSCREEN_01     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
