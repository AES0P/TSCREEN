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
INTERFACE zif_tlock
  PUBLIC .


  METHODS has_lock
    EXPORTING
      !lock_info      TYPE ztscreen_lock
    RETURNING
      VALUE(has_lock) TYPE abap_bool .
  METHODS auto_release
    IMPORTING
      !min         TYPE int1
    RETURNING
      VALUE(tlock) TYPE REF TO zif_tlock .
  METHODS lock
    IMPORTING
      !ylzd TYPE zsbc_00002 OPTIONAL .
  METHODS update_lock .
  METHODS unlock .
ENDINTERFACE.
