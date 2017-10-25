
*&---------------------------------------------------------------------*
*& Report ZTEST_ZCL_PDF
*&---------------------------------------------------------------------*
*& Copyright (c) 2017 Bjørn Espen Raadim
*&
*& Permission is hereby granted, free of charge, to any person obtaining a copy
*& of this software and associated documentation files (the "Software"), to deal
*& in the Software without restriction, including without limitation the rights
*& to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*& copies of the Software, and to permit persons to whom the Software is
*& furnished to do so, subject to the following conditions:
*&
*& The above copyright notice and this permission notice shall be included in all
*& copies or substantial portions of the Software.
*&
*& THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*& IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*& FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*& AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*& LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*& OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*& SOFTWARE.
*&---------------------------------------------------------------------*

REPORT  ZTEST_ZCL_PDF.

DATA : gt_data TYPE TABLE OF x255.
DATA : wa_line TYPE x255.

DATA: lo_docking_container TYPE REF TO cl_gui_docking_container.
DATA: lo_html              TYPE REF TO cl_gui_html_viewer.
DATA: lv_url               TYPE char255.
DATA: ok_code(20)          TYPE c.
DATA: lo_pdf               TYPE REF TO zcl_pdf.




*PARAMETERS fname TYPE string.

* Read PDF File
* PERFORM read_file.

PERFORM create_pdf.


* Display to screen

PERFORM display_pdf.


*&---------------------------------------------------------------------*
*&      Form  create_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_pdf.
  DATA lv_text      TYPE string.
  DATA lv_content   TYPE xstring.
  DATA lv_len       TYPE i.
  data lv_font_size TYPE i.

  create object lo_pdf.

  lv_text = 'Hello world! : - ( )'' " \|/?´`^¨§$'.

  lo_pdf->rect( iv_x = 10 iv_y = 10 iv_width = 80 iv_heigth = 200  iv_style = 'S' ).

lv_font_size = lo_pdf->get_max_font_size_for_width( iv_text = lv_text iv_max_width = 80 ).
 lo_pdf->set_font_size( lv_font_size ).

  lo_pdf->text_box( iv_text = lv_text iv_x = 10 iv_y = 10 iv_width = 80 iv_height = 200 ).
  lo_pdf->add_page( ).

  lv_content = lo_pdf->output( ).

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_content
    IMPORTING
      output_length = lv_len
    TABLES
      binary_tab    = gt_data.



ENDFORM.                    "create_pdf


*&---------------------------------------------------------------------*
*&      Form  read_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_file.
*
*  OPEN DATASET fname FOR INPUT IN BINARY MODE.
*  IF sy-subrc = 0.
*    DO.
*      READ DATASET fname INTO wa_line.
*      IF sy-subrc = 0.
*        APPEND wa_line TO gt_data.
*      ELSE.
*        APPEND wa_line TO gt_data.
*        EXIT.
*      ENDIF.
*    ENDDO.
*  ENDIF.

ENDFORM.                    "read_file


*&---------------------------------------------------------------------*
*&      Form  display_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_pdf.
  IF NOT gt_data IS INITIAL.
    CALL SCREEN 100.
  ELSE.
    WRITE :/ 'No data to display'.
  ENDIF.
ENDFORM.                    "display_pdf

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  CLEAR ok_code.
  IF NOT ( lo_docking_container IS INITIAL ).
    CALL METHOD lo_docking_container->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
  ENDIF.

  IF NOT ( lo_html IS INITIAL ).
    CALL METHOD lo_html->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
  ENDIF.

  CREATE OBJECT lo_docking_container
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      extension = 5000.

  CREATE OBJECT lo_html
    EXPORTING
      parent = lo_docking_container.
*
* Load the HTML
  lo_html->load_data(
    EXPORTING
      type         = `application`
      subtype      = `pdf`
    IMPORTING
      assigned_url         = lv_url
    CHANGING
      data_table           = gt_data
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4 ).

* Show it

  lo_html->show_url( url = lv_url  in_place = 'X' ).

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN '&F03' OR 'EXIT' OR 'CANC'.
      IF NOT ( lo_docking_container IS INITIAL ).
        CALL METHOD lo_docking_container->free
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3.
      ENDIF.

      IF NOT ( lo_html IS INITIAL ).
        CALL METHOD lo_html->free
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3.
      ENDIF.

      leave program.
  ENDCASE.



ENDMODULE.                 " USER_COMMAND_0100  INPUT
