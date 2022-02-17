*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_TSCREEN_00
*   generation date: 2022.02.14 at 15:16:23
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_TSCREEN_00     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
