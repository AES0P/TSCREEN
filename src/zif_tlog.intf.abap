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
INTERFACE zif_tlog
  PUBLIC .


  METHODS clear
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS add_log
    IMPORTING
      !type           TYPE sy-msgty DEFAULT 'S'
      !content        TYPE string
      !dynnr          TYPE sy-dynnr DEFAULT sy-dynnr
      !ucomm          TYPE sy-ucomm DEFAULT sy-ucomm
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS save_log
    IMPORTING
      !guid           TYPE guid
      !symbol         TYPE icon_d OPTIONAL
      !if_commit      TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS show_log
    IMPORTING
      !style          TYPE c DEFAULT '1'
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
  METHODS commit
    RETURNING
      VALUE(instance) TYPE REF TO zif_tlog .
ENDINTERFACE.
