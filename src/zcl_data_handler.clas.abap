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
CLASS zcl_data_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_data_handler .

    ALIASES export_data
      FOR zif_data_handler~export_data .
    ALIASES import_data
      FOR zif_data_handler~import_data .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DATA_HANDLER IMPLEMENTATION.


  METHOD zif_data_handler~export_data.

    IF zcl_treport=>get_treport( )->management_strategy-zexport = abap_false.
      "程序未启用数据导出功能，请联系管理员！
      MESSAGE s006(ztscreen) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'XML_EXPORT_DIALOG'
      EXPORTING
        i_xml                      = NEW cl_salv_bs_ex_office2007(
                                         cl_salv_ex_util=>factory_result_data_table(
                                           r_data         = data
                                           t_fieldcatalog = fieldcatalog ) )->transform( )
        i_default_extension        = 'XLSX'
        i_initial_directory        = ''
        i_default_file_name        = 'EXPORT.XLSX'
        i_mask                     = 'Excel(*.XLSX)|*XLSX'
        i_application              = ''
      EXCEPTIONS
        application_not_executable = 1
        OTHERS                     = 2. "#EC CI_SUBRC
    IF sy-subrc <> 0 ##NEEDED.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD zif_data_handler~import_data.

    IF zcl_treport=>get_treport( )->management_strategy-zimport = abap_false.
      "程序未启用数据导入功能，请联系管理员！
      MESSAGE s007(ztscreen) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    ASSIGN data->* TO <data>.
    ASSERT sy-subrc = 0.

    IF <data> IS NOT INITIAL.
      DATA answer TYPE c.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = '覆盖？'(001)
          text_question  = '现有数据不为空，是否覆盖已有数据？'(002)
        IMPORTING
          answer         = answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0 OR answer <> '1'.
        RETURN.
      ENDIF.
    ENDIF.

    DATA filename TYPE rlgrap-filename.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        static        = 'X'
        mask          = ',*.XLS,*.XLSX '
      CHANGING
        file_name     = filename
      EXCEPTIONS
        mask_too_long = 1
        OTHERS        = 2. "#EC CI_SUBRC
    IF sy-subrc <> 0 ##NEEDED.
* Implement suitable error handling here
    ENDIF.

    DATA tab TYPE REF TO data.
*&---根据fieldcat生成动态内表
    cl_alv_table_create=>create_dynamic_table(
      EXPORTING
        it_fieldcatalog           = fieldcatalog
      IMPORTING
        ep_table                  = tab
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2 ).
    IF sy-subrc <> 0.
      MESSAGE '当前没有可见行或转换失败，请重试'(003) TYPE 'W'.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN tab->* TO <tab>.
    ASSERT sy-subrc = 0.

    DATA raw_data TYPE truxs_t_text_data.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = abap_true
        i_tab_raw_data       = raw_data
        i_filename           = filename
      TABLES
        i_tab_converted_data = <tab>
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE '数据转换失败，请检查EXCEL文件，确保格式与展示表一致'(004) TYPE 'W'.
    ELSE.
      MOVE-CORRESPONDING <tab> TO <data>.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
