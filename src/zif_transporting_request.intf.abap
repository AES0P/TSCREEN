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
INTERFACE zif_transporting_request
  PUBLIC .


  TYPES ty_e071 TYPE e071 .
  TYPES:
    tty_e071 TYPE STANDARD TABLE OF ty_e071 WITH DEFAULT KEY .
  TYPES ty_e071k TYPE e071k .
  TYPES:
    tty_e071k TYPE STANDARD TABLE OF ty_e071k WITH DEFAULT KEY .

  METHODS associate
    IMPORTING
      !order_type    TYPE trfunction DEFAULT 'K'
      !task_type     TYPE trfunction DEFAULT 'S'
      !category      TYPE e070-korrdev DEFAULT 'SYST'
    RETURNING
      VALUE(tr_tool) TYPE REF TO zif_transporting_request .
  METHODS entrys
    IMPORTING
      !table           TYPE table
      VALUE(ddic_type) TYPE trobj_name OPTIONAL          "#EC CI_VALPAR
    RETURNING
      VALUE(tr_tool)   TYPE REF TO zif_transporting_request .
  METHODS entry
    IMPORTING
      VALUE(objname) TYPE trobj_name                     "#EC CI_VALPAR
      VALUE(objfunc) TYPE objfunc DEFAULT 'K'
      VALUE(tabkey)  TYPE trobj_name                     "#EC CI_VALPAR
    RETURNING
      VALUE(tr_tool) TYPE REF TO zif_transporting_request .
  METHODS commit
    IMPORTING
      !show_error        TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(is_commited) TYPE abap_bool .
ENDINTERFACE.
