class ZCL_PDF definition
  public
  final
  create public .

public section.

*&---------------------------------------------------------------------*
*& Class ZCL_PDF
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


  types:
*"* public components of class ZCL_PDF
*"* do not include other source files here!!!
    typ_text_lines type table of string with default key .
  types:
    BEGIN OF typ_smpl_kerning_pairs_x_line,
             char(1)   TYPE c,
             kerning   TYPE i,
           END OF typ_smpl_kerning_pairs_x_line .
  types:
    typ_smpl_kerning_pairs_x_tab TYPE HASHED TABLE OF typ_smpl_kerning_pairs_x_line WITH UNIQUE KEY char .
  types:
    BEGIN OF typ_simple_font_metric_line,
        char(1)               TYPE c,
        wx                    TYPE i,
        kerning_pairs         TYPE typ_smpl_kerning_pairs_x_tab,
      END OF typ_simple_font_metric_line .
  types:
    typ_simple_font_metric_tab type hashed table of typ_simple_font_metric_line with unique key char .
  types:
    BEGIN OF typ_simple_fm_data_line,
      post_script_name        TYPE string,
      simple_fm_data_gzip     TYPE xstring,
      simple_fm_data          TYPE zcl_pdf=>typ_simple_font_metric_tab,
    end of typ_simple_fm_data_line .
  types:
    typ_simple_fm_data_tab type hashed table of typ_simple_fm_data_line with unique key post_script_name .
  types:
    BEGIN OF typ_page_format.
    TYPES name   TYPE string.
    TYPES width  TYPE p LENGTH 7 DECIMALS 2.
    TYPES height TYPE p LENGTH 7 DECIMALS 2.
    TYPES END OF typ_page_format .
  types:
    typ_page_format_tab type standard table of typ_page_format with default key .

  constants CONST_FONT_HELVETICA type STRING value 'helvetica'. "#EC NOTEXT
  constants CONST_FONT_TIMES type STRING value 'times'. "#EC NOTEXT
  constants CONST_FONT_COURIER type STRING value 'courier'. "#EC NOTEXT
  constants CONST_FONT_STYLE_NORMAL type STRING value 'normal'. "#EC NOTEXT
  constants CONST_FONT_STYLE_BOLD type STRING value 'bold'. "#EC NOTEXT
  constants CONST_FONT_STYLE_ITALIC type STRING value 'italic'. "#EC NOTEXT
  constants CONST_FONT_STYLE_BOLD_ITALIC type STRING value 'bolditalic'. "#EC NOTEXT
  constants CONST_HOR_TEXT_ALIGN_LEFT type I value 1. "#EC NOTEXT
  constants CONST_HOR_TEXT_ALIGN_RIGHT type I value 2. "#EC NOTEXT
  constants CONST_HOR_TEXT_ALIGN_MIDDLE type I value 3. "#EC NOTEXT

  methods SHRINK_FONT_TO_FIT_BOX
    importing
      !IV_TEXT type STRING
      !IV_MAX_WIDTH type I
      !IV_MAX_HEIGHT type I
    returning
      value(RV_FONT_SIZE) type I .
  class ZCL_PDF definition load .
  methods MEASURE_LINES_HEIGHT
    importing
      !IT_LINES type ZCL_PDF=>TYP_TEXT_LINES
    returning
      value(RV_LEN) type I .
  methods GET_PAGE_HEIGHT
    returning
      value(RV_VALUE) type I .
  methods GET_MAX_FONT_SIZE_FOR_WIDTH
    importing
      !IV_TEXT type STRING
      !IV_MAX_WIDTH type I
    returning
      value(RV_SIZE) type I .
  methods SPLIT_PARAGRAPH_TO_LINES
    importing
      !IV_STRING type STRING
      !IV_MAXLEN type I
    returning
      value(RT_LINES) type TYP_TEXT_LINES .
  methods GET_CURRENT_LINE_SPACE
    returning
      value(RV_VAL) type I .
  methods SET_FONT_SIZE
    importing
      !IV_SIZE type I .
  methods SET_FONT
    importing
      !IV_FONT_NAME type STRING
      !IV_FONT_STYLE type STRING .
  methods TEXT_BOX
    importing
      !IV_TEXT type STRING
      !IV_X type I
      !IV_Y type I
      !IV_WIDTH type I
      !IV_HEIGHT type I
      !IV_HOR_ALIGN type I default CONST_HOR_TEXT_ALIGN_LEFT
    returning
      value(RV_END_Y) type I .
  methods RECT
    importing
      !IV_X type I
      !IV_Y type I
      !IV_WIDTH type I
      !IV_HEIGTH type I
      !IV_STYLE type STRING .
  methods WRITE
    importing
      !IV_STR type STRING .
  methods TEXT
    importing
      !IV_TEXT type STRING
      !IV_X type ANY
      !IV_Y type ANY
    returning
      value(RV_Y) type I .
  methods OUTPUT
    returning
      value(RV_XSTR) type XSTRING .
  methods GET_FONT_SIZE
    returning
      value(RV_FONT_SIZE) type I .
  methods ADD_PAGE .
  methods GET_FONT
    importing
      !IV_FONT_NAME type STRING optional
      !IV_FONT_STYLE type STRING optional
    returning
      value(RV_FONT_ID) type STRING .
  methods CONSTRUCTOR
    importing
      !IV_ORIENTATION type STRING optional
      !IV_UNIT type STRING optional
      !IV_FORMAT type STRING optional .
protected section.

  types:
*"* protected components of class /IONO/CL_PDF
*"* do not include other source files here!!!
    typ_char_widths_tab type standard table of i with default key .
  types:
    typ_page  TYPE STANDARD TABLE OF xstring WITH DEFAULT KEY .
  types:
    typ_pages TYPE STANDARD TABLE OF typ_page WITH DEFAULT KEY .
  types:
    BEGIN OF typ_doc_properties,
    title    TYPE string,
    subject  TYPE string,
    author   TYPE string,
    keywords TYPE string,
    creator  TYPE string,
    END OF typ_doc_properties .
  types:
    typ_offsets_tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY .
  types:
    typ_content TYPE STANDARD TABLE OF xstring WITH DEFAULT KEY .
  types:
    BEGIN OF typ_font,
    id               TYPE string,
    object_number    TYPE i,
    post_script_name TYPE string,
    font_name        TYPE string,
    font_style       TYPE string,
    encoding         TYPE string,
    medata           TYPE REF TO data,
    END OF typ_font .
  types:
    BEGIN OF typ_font_style_map,
     font_style   TYPE string,
     font_id      TYPE string,
   END OF typ_font_style_map .
  types:
    typ_font_style_map_tab TYPE HASHED TABLE OF typ_font_style_map WITH UNIQUE KEY font_style .
  types:
    BEGIN OF typ_font_name_map,
      font_name     TYPE string,
      font_styles   TYPE typ_font_style_map_tab,
    END OF typ_font_name_map .
  types:
    typ_font_name_map_tab TYPE HASHED TABLE OF typ_font_name_map WITH UNIQUE KEY font_name .
  types:
    typ_font_tab TYPE SORTED TABLE OF typ_font WITH UNIQUE KEY id .
  types:
    typ_decimal TYPE p LENGTH 16 DECIMALS 7 .
  types:
    BEGIN OF typ_text_flags,
     source_encoding TYPE string,
     output_encoding TYPE string,
     auto_encode     TYPE abap_bool,
     no_bom          TYPE abap_bool,
     END OF typ_text_flags .
  types:
    BEGIN OF TYP_ENCODING_MAP,
          encoding     type string,
          sap_codepage type abap_encoding,
          encoder      type ref to cl_abap_conv_out_ce,
         END OF TYP_ENCODING_MAP .
  types:
    typ_encoding_map_tab type hashed table of typ_encoding_map with unique key encoding .

  data GT_SIMPLE_FONT_METRIC_DATA type TYP_SIMPLE_FM_DATA_TAB .
  data GV_ACTIVE_FONT_SIZE type I value 16. "#EC NOTEXT .  .  .  . " .
  data GV_ORIENTATION type STRING .
  data GV_UNIT type STRING value 'mm'. "#EC NOTEXT .  .  .  . " .
  data GV_ACTIVE_FONT_ID type STRING .
  data GV_VERSION type STRING value '20130127'. "#EC NOTEXT .  .  .  . " .
  data GV_PDF_VERSION type STRING value '1.3'. "#EC NOTEXT .  .  .  . " .
  data GT_PAGE_FORMATS type TYP_PAGE_FORMAT_TAB .
  data GV_TEXT_COLOR type STRING value '0 g'. "#EC NOTEXT .  .  .  . " .
  data GV_DRAW_COLOR type STRING value '0 G'. "#EC NOTEXT .  .  .  . " .
  data GV_PAGE type I .
  data GV_CONTENT_LENGTH type I value 0. "#EC NOTEXT .  .  .  . " .
  data GV_SCALE_FACTOR type TYP_DECIMAL .
  data GT_PAGES type TYP_PAGES .
  data GV_DOC_PROPERTIES type TYP_DOC_PROPERTIES .
  data GV_LINE_CAP_ID type I value 0. "#EC NOTEXT .  .  .  . " .
  data GV_LINE_JOIN_ID type I value 0. "#EC NOTEXT .  .  .  . " .
  data GV_OBJECT_NUMBER type I value 2. "#EC NOTEXT .  .  .  . " .
  type-pools ABAP .
  data GV_OUT_TO_PAGES type ABAP_BOOL value ABAP_FALSE. "#EC NOTEXT .  .  .  . " .
  data GT_OFFSETS type TYP_OFFSETS_TAB .
  data GT_CONTENT type TYP_CONTENT .
  data GT_FONTS type TYP_FONT_TAB .
  data GT_FONT_MAP type TYP_FONT_NAME_MAP_TAB .
  data GV_LINE_WIDTH type TYP_DECIMAL value '0.200025'. "#EC NOTEXT .  .  .  . " .
  data GT_ENCODING_MAP type TYP_ENCODING_MAP_TAB .
  data GV_ENCODING type STRING value 'WinAnsiEncoding'. "#EC NOTEXT .  .  .  . " .
  data GV_PAGE_WIDTH type TYP_DECIMAL .
  data GV_PAGE_HEIGHT type TYP_DECIMAL .
  data GV_ACTIVE_PAGE_FORMAT type TYP_PAGE_FORMAT .

  class-events EVT_ADD_PAGE
    exporting
      value(IV_PAGE_NO) type I .
  class-events EVT_POST_PUT_RESOURCES .
  class-events EVT_PUT_CATALOG .
  class-events EVT_PUT_RESOURCES .
  class-events EVT_PUT_XOBJECT_DICT .
  class-events EVT_FONT_ADDED
    exporting
      value(IS_FONT) type TYP_FONT .

  methods ESCAPE_TEXT
    importing
      !IV_TEXT type STRING
    returning
      value(RV_TEXT) type STRING .
  methods GET_STRING_WIDTH
    importing
      !IV_STR type STRING
    returning
      value(RV_LEN) type I .
  methods SPLIT_LONG_WORD
    importing
      !IV_WORD type STRING
      !IT_CHAR_WIDTHS type TYP_CHAR_WIDTHS_TAB
      !IV_CUR_WIDTH type I
      !IV_MAXLEN type I
    returning
      value(RT_LINES) type TYP_TEXT_LINES .
  methods GET_WORD_LENGTH
    importing
      !IT_CHAR_WIDTHS type TYP_CHAR_WIDTHS_TAB
    returning
      value(RV_LENGTH) type I .
  methods F2
    importing
      !IV_VAL type ANY
    returning
      value(RV_RET) type STRING .
  methods GET_CHAR_WIDTHS
    importing
      !IV_WORD type STRING
    returning
      value(RV_WIDTH_TAB) type TYP_CHAR_WIDTHS_TAB .
  methods GET_FONT_METRIC_DATA
    returning
      value(RV_DATA) type STRING .
  methods GET_STYLE
    importing
      !IV_STYLE type STRING
    returning
      value(RV_STYLE) type STRING .
  methods INITIALIZE_FONT_METRIC .
  methods PUT_PAGES .
  methods GET_SIMPLE_FONT_METRIC
    returning
      value(RR_SIMPLE_FM) type ref to DATA .
  methods GET_SAP_ENCODER
    importing
      !IV_ENCODING type STRING
    returning
      value(RV_SAP_ENCODING) type TYP_ENCODING_MAP .
  methods INITIALIZE_ENCODING_MAP .
  methods BUILD_DOCUMENT
    returning
      value(RV_XSTR) type XSTRING .
  methods _ADD_PAGE .
  methods BEGIN_PAGE .
  methods PDF_ESCAPE
    importing
      !IV_STR type STRING
    returning
      value(RV_STR) type STRING .
  methods PUT_CATALOG .
  methods PUT_INFO .
  methods PUT_RESOURCES .
  methods PUT_TRAILER .
  methods ADD_TO_FONT_DICTIONARY
    importing
      !IV_FONT_ID type STRING
      !IV_FONT_NAME type STRING
      !IV_FONT_STYLE type STRING .
  methods GET_FONT_FROM_DICTIONARY
    importing
      !IV_FONT_NAME type STRING
      !IV_FONT_STYLE type STRING
    returning
      value(RV_FONT_ID) type STRING .
  methods PUT_FONT
    importing
      !IV_FONT type TYP_FONT .
  methods PUT_FONTS .
  methods PUT_RESOURCE_DICTIONARY .
  methods PUT_XOBJECT_DICT .
  methods ADD_FONT
    importing
      !IV_POST_SCRIPT_NAME type STRING
      !IV_FONT_NAME type STRING
      !IV_FONT_STYLE type STRING
      !IV_ENCODING type STRING .
  methods ADD_FONTS .
  methods NEW_OBJECT
    returning
      value(RV_OBJ_NO) type I .
  methods PUT_STREAM
    importing
      !IV_XSTR type XSTRING .
  methods OUT
    importing
      !IV_XSTR type XSTRING
      !IV_NEW_LINE type ABAP_BOOL default ABAP_TRUE .
  methods INITIALIZE_PAGE_FORMATS .
private section.

*"* private components of class /IONO/CL_PDF
*"* do not include other source files here!!!
  methods XS
    importing
      !IV_STR type STRING
      !IV_ENCODING type STRING default 'WinAnsiEncoding'
    returning
      value(RV_XSTR) type XSTRING .
  methods TO_8BIT_STREAM
    importing
      !IV_TEXT type STRING .
  methods JOIN_STRING_TAB
    importing
      !LT_STR_TAB type STANDARD TABLE
      !LV_DELIMETER type ANY
    returning
      value(RV_STR) type STRING .
  class-methods STRING_PAD_LEFT
    importing
      !IV_STR type STRING
      !IV_PAD_CHAR type C
      !IV_LEN type I
    returning
      value(RV_RET) type STRING .
  methods TO_STRING
    importing
      !IV_VALUE type ANY
    returning
      value(RV_STR) type STRING .
ENDCLASS.



CLASS ZCL_PDF IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->ADD_FONT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_POST_SCRIPT_NAME            TYPE        STRING
* | [--->] IV_FONT_NAME                   TYPE        STRING
* | [--->] IV_FONT_STYLE                  TYPE        STRING
* | [--->] IV_ENCODING                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_font.
  DATA lv_len  TYPE i.
  DATA ls_font TYPE typ_font.

  lv_len = lines( me->gt_fonts ) + 1.

  ls_font-id = |F{ me->to_string( lv_len ) }|. "me->string_pad_left( str = lv_len pad_char = '0' len = 10 )
  ls_font-post_script_name = iv_post_script_name.
  ls_font-font_name        = iv_font_name.
  ls_font-font_style       = iv_font_style.
  ls_font-encoding         = iv_encoding.


  INSERT ls_font INTO table me->gt_fonts.

  me->add_to_font_dictionary( iv_font_id = ls_font-id iv_font_name = ls_font-font_name iv_font_style = ls_font-font_style ).

  RAISE EVENT evt_font_added EXPORTING is_font = ls_font.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->ADD_FONTS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_fonts.




  FIELD-SYMBOLS <font> TYPE typ_font.

  me->add_font( iv_post_script_name = 'Helvetica'
                iv_font_name        = const_font_helvetica
                iv_font_style       = const_font_style_normal
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Helvetica-Bold'
                iv_font_name        = const_font_helvetica
                iv_font_style       = const_font_style_bold
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Helvetica-Oblique'
                iv_font_name        = const_font_helvetica
                iv_font_style       = const_font_style_italic
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Helvetica-BoldOblique'
                iv_font_name        = const_font_helvetica
                iv_font_style       = const_font_style_bold_italic
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Courier'
                iv_font_name        = const_font_courier
                iv_font_style       = const_font_style_normal
                iv_encoding         = me->gv_encoding ).


  me->add_font( iv_post_script_name = 'Courier-Bold'
                iv_font_name        = const_font_courier
                iv_font_style       = const_font_style_bold
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Courier-Oblique'
                iv_font_name        = const_font_courier
                iv_font_style       = const_font_style_italic
                iv_encoding         = me->gv_encoding ).


  me->add_font( iv_post_script_name = 'Courier-BoldOblique'
                iv_font_name        = const_font_courier
                iv_font_style       = const_font_style_bold_italic
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Times-Roman'
                iv_font_name        = const_font_times
                iv_font_style       = const_font_style_normal
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Times-Bold'
                iv_font_name        = const_font_times
                iv_font_style       = const_font_style_bold
                iv_encoding         = me->gv_encoding ).


  me->add_font( iv_post_script_name = 'Times-Italic'
                iv_font_name        = const_font_times
                iv_font_style       = const_font_style_italic
                iv_encoding         = me->gv_encoding ).

  me->add_font( iv_post_script_name = 'Times-BoldItalic'
                iv_font_name        = const_font_times
                iv_font_style       = const_font_style_bold_italic
                iv_encoding         = me->gv_encoding ).


  DATA lt_str  TYPE table of string.
  DATA lv_font_name TYPE string.
  DATA lv_font_style TYPE string.

  LOOP AT me->gt_fonts ASSIGNING <font>.
    SPLIT <font>-post_script_name AT '-' INTO TABLE lt_str.

    IF lines( lt_str ) > 1.
      READ TABLE lt_str INDEX 1 INTO lv_font_name.
      READ TABLE lt_str INDEX 2 INTO lv_font_style .
    ELSE.
      READ TABLE lt_str INDEX 1 INTO lv_font_name.
      CLEAR lv_font_style.
    ENDIF.

    me->add_to_font_dictionary( iv_font_id = <font>-id iv_font_name = lv_font_name iv_font_style = lv_font_style ).
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->ADD_PAGE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method ADD_PAGE.
  me->_add_page( ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->ADD_TO_FONT_DICTIONARY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FONT_ID                     TYPE        STRING
* | [--->] IV_FONT_NAME                   TYPE        STRING
* | [--->] IV_FONT_STYLE                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_to_font_dictionary.

  DATA ls_font_style_map TYPE typ_font_style_map.
  DATA ls_font_name_map  TYPE typ_font_name_map.

  FIELD-SYMBOLS <font_name_map> TYPE typ_font_name_map.
  FIELD-SYMBOLS <font_style_map> TYPE typ_font_style_map.

  READ TABLE me->gt_font_map WITH TABLE KEY font_name = iv_font_name ASSIGNING <font_name_map>.
  IF sy-subrc = 0.
    READ TABLE <font_name_map>-font_styles WITH TABLE KEY font_style = iv_font_style ASSIGNING <font_style_map>.
  ENDIF.

  IF <font_name_map> IS NOT ASSIGNED.
    ls_font_name_map-font_name = iv_font_name.
    INSERT ls_font_name_map INTO TABLE me->gt_font_map.
    READ TABLE me->gt_font_map WITH TABLE KEY font_name = iv_font_name assigning <font_name_map>.
  ENDIF.

  IF <font_style_map> IS NOT ASSIGNED.
    ls_font_style_map-font_style = iv_font_style.
    INSERT ls_font_style_map INTO TABLE <font_name_map>-font_styles.
    READ TABLE <font_name_map>-font_styles WITH TABLE KEY font_style = iv_font_style assigning <font_style_map>.
  ENDIF.

  <font_style_map>-font_id = iv_font_id.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->BEGIN_PAGE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method BEGIN_PAGE.
  field-symbols <page> type typ_page.

  me->gv_page = me->gv_page + 1.
  gv_out_to_pages = abap_true.

  APPEND initial line to me->gt_pages assigning <page>.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->BUILD_DOCUMENT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_XSTR                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD build_document.
  DATA lv_cross_ref_offset TYPE i.
  DATA lv_xstr  TYPE xstring.
  DATA lv_xn    TYPE xstring.
  DATA lv_str   TYPE string.

  FIELD-SYMBOLS <offset> TYPE i.
  FIELD-SYMBOLS <xstr>   TYPE xstring.

  me->gv_out_to_pages = abap_false.
  CLEAR me->gt_content[].
  CLEAR me->gt_offsets[].

* Create two empty lines
  APPEND INITIAL LINE TO me->gt_offsets ASSIGNING <offset>.
  APPEND INITIAL LINE TO me->gt_offsets ASSIGNING <offset>.
  APPEND INITIAL LINE TO me->gt_offsets ASSIGNING <offset>.

  me->out( xs( |%PDF-{ me->gv_pdf_version }| ) ).

  me->put_pages( ).

  me->put_resources( ).

  me->new_object( ).

  me->out( xs( |<<| ) ).
  me->put_info( ).
  me->out( xs( |>>| ) ).
  me->out( xs( |endobj| ) ).

  me->new_object( ).
  me->out( xs( |<<| ) ).
  me->put_catalog( ).
  me->out( xs( |>>| ) ).
  me->out( xs( |endobj| ) ).

  lv_cross_ref_offset = me->gv_content_length.
  me->out( xs( |xref| ) ).
  me->out( xs( |0 { me->gv_object_number + 1 }| ) ).
  me->out( xs( |0000000000 65535 f | ) ).
  DELETE me->gt_offsets INDEX 1. "Delete the first line

  LOOP AT me->gt_offsets ASSIGNING <offset>.
    me->out( xs( |{ string_pad_left( iv_str = me->to_string( <offset> ) iv_pad_char = '0' iv_len = 10 ) } 00000 n | ) ).
  ENDLOOP.

  me->out( xs( |trailer| ) ).
  me->out( xs( |<<| ) ).
  me->put_trailer( ).
  me->out( xs( |>>| ) ).
  me->out( xs( |startxref| ) ).
  me->out( xs( me->to_string( lv_cross_ref_offset ) ) ).
  me->out( xs( |%%EOF| ) ).

  me->gv_out_to_pages = abap_true.

  lv_str = cl_abap_char_utilities=>newline.
  lv_xn = xs( lv_str ).

  LOOP AT me->gt_content ASSIGNING <xstr>.
    AT LAST.
      CLEAR lv_xn.
    ENDAT.
    CONCATENATE rv_xstr <xstr> lv_xn INTO rv_xstr IN BYTE MODE.
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ORIENTATION                 TYPE        STRING(optional)
* | [--->] IV_UNIT                        TYPE        STRING(optional)
* | [--->] IV_FORMAT                      TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.
  DATA lv_dec TYPE p LENGTH 10 DECIMALS 2.
  DATA lv_format TYPE string.

  FIELD-SYMBOLS <page_format> TYPE typ_page_format.
  field-symbols <i> type i.

* Create two initial lines for offsets..
  APPEND INITIAL LINE TO me->gt_offsets assigning <i>.
  APPEND INITIAL LINE TO me->gt_offsets assigning <i>.

  IF iv_orientation IS NOT SUPPLIED.
    me->gv_orientation = 'p'.
  else.
    me->gv_orientation = iv_orientation.
  ENDIF.

  IF iv_unit IS NOT SUPPLIED.
    me->gv_unit = 'mm'.
  else.
    me->gv_unit = iv_unit.
  ENDIF.

  IF iv_format IS NOT SUPPLIED.
    lv_format = 'a4'.
  ELSE.
    lv_format = iv_format.
  ENDIF.

  me->initialize_page_formats( ).
  me->initialize_encoding_map( ).
  me->initialize_font_metric( ).



  CASE me->gv_unit.
    WHEN 'pt'.
      me->gv_scale_factor = 1.
    WHEN 'mm'.
      lv_dec = '25.4'.
      me->gv_scale_factor = 72 / lv_dec.
    WHEN 'cm'.
      lv_dec = '2.54'.
      me->gv_scale_factor = 72 / lv_dec.
    WHEN 'in'.
      me->gv_scale_factor = 72.
    WHEN OTHERS.
*     @todo: throw exception!
  ENDCASE.

  TRANSLATE lv_format TO LOWER CASE.

  READ TABLE me->gt_page_formats WITH KEY name = lv_format ASSIGNING <page_format>.
  IF sy-subrc <> 0.
*   @todo: throw invalid_format_exception.
  ELSE.
    me->gv_active_page_format = <page_format>.
  ENDIF.

  me->gv_page_height = <page_format>-height / me->gv_scale_factor.
  me->gv_page_width  = <page_format>-width / me->gv_scale_factor.


  CASE me->gv_orientation.
    WHEN 'p' OR 'portrait'.
      me->gv_orientation = 'p'.
      IF me->gv_page_width > me->gv_page_height.
        lv_dec = me->gv_page_width.
        me->gv_page_width = me->gv_page_height.
        me->gv_page_height = lv_dec.
      ENDIF.
    WHEN 'l' OR 'landscape'.
      me->gv_orientation = 'l'.
      IF me->gv_page_height > me->gv_page_width.
        lv_dec = me->gv_page_width.
        me->gv_page_width = me->gv_page_height.
        me->gv_page_height = lv_dec.
      ENDIF.
    WHEN OTHERS.
*   @todo: throw exception
  ENDCASE.

  me->add_fonts( ).
  me->gv_active_font_id = 'F1'.
  me->_add_page( ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->ESCAPE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD escape_text.

  rv_text = iv_text.

  REPLACE ALL OCCURRENCES OF '\' IN rv_text WITH '\\'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_text WITH '\t'.
  REPLACE ALL OCCURRENCES OF '(' IN rv_text WITH '\('.
  REPLACE ALL OCCURRENCES OF ')' IN rv_text WITH '\)'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN rv_text WITH '\b'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN rv_text WITH '\f'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv_text WITH '\n'.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->F2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        ANY
* | [<-()] RV_RET                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD f2.
  DATA lv_dec TYPE p LENGTH 16 DECIMALS 2.
  lv_dec = iv_val.
  rv_ret = lv_dec.
  IF rv_ret CA '-'.
    REPLACE ALL OCCURRENCES OF '-' IN rv_ret WITH ''.
    rv_ret = |-{ rv_ret }|.
  ENDIF.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_CHAR_WIDTHS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WORD                        TYPE        STRING
* | [<-()] RV_WIDTH_TAB                   TYPE        TYP_CHAR_WIDTHS_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_CHAR_WIDTHS.

  DATA lv_char   TYPE c.
  DATA lv_index  TYPE i.
  DATA lv_chars  TYPE i.
  DATA lv_length TYPE i.
  DATA lr_data   TYPE REF TO data.
  data lv_width  type i.

  FIELD-SYMBOLS <font>         TYPE typ_font.
  FIELD-SYMBOLS <lt_simple_fm> TYPE typ_simple_font_metric_tab.
  FIELD-SYMBOLS <ls_fm_cur>    TYPE typ_simple_font_metric_line.
  FIELD-SYMBOLS <ls_fm_prev>   TYPE typ_simple_font_metric_line.
  FIELD-SYMBOLS <ls_kerning>   TYPE typ_smpl_kerning_pairs_x_line.
  field-symbols <prev_width>   type i.
  field-symbols <cur_width>   type i.

  lr_data = me->get_simple_font_metric( ).
  ASSIGN lr_data->* TO <lt_simple_fm>.

  READ TABLE me->gt_fonts WITH KEY id = me->gv_active_font_id ASSIGNING <font>.

  lv_chars = strlen( iv_word ).
  lv_index = 0.

  DO lv_chars TIMES.
    lv_char = iv_word+lv_index(1).

    APPEND INITIAL LINE TO rv_width_tab assigning <cur_width>.

*   Get char metric data for current char
    READ TABLE <lt_simple_fm> WITH TABLE KEY char = lv_char ASSIGNING <ls_fm_cur>.

    IF sy-subrc = 0.
      <cur_width> = <ls_fm_cur>-wx * me->gv_active_font_size.

*     Adjust for kerning
      IF <ls_fm_prev> IS ASSIGNED.
        READ TABLE <ls_fm_prev>-kerning_pairs WITH TABLE KEY char = lv_char ASSIGNING <ls_kerning>.
        IF sy-subrc = 0. "Kernnig for curren char exists
         <prev_width> = <prev_width> + ( <ls_kerning>-kerning * me->gv_active_font_size ). "Adjust previous char length for kerning.
        ENDIF.
      ENDIF.

*     Store previous because of kerning
      GET REFERENCE OF <ls_fm_cur> INTO lr_data.
      ASSIGN lr_data->* TO <ls_fm_prev>.
      GET REFERENCE OF <cur_width> into lr_data.
      ASSIGN lr_data->* TO <prev_width>.
    else. " char not found in char metric table
*     TODO: Exception? Default length?   Add 1000?
    ENDIF.

    lv_index = lv_index + 1.
  ENDDO.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->GET_CURRENT_LINE_SPACE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_VAL                         TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_CURRENT_LINE_SPACE.
  rv_val = ( me->gv_active_font_size / me->gv_scale_factor ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->GET_FONT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FONT_NAME                   TYPE        STRING(optional)
* | [--->] IV_FONT_STYLE                  TYPE        STRING(optional)
* | [<-()] RV_FONT_ID                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_font.
  DATA lv_font_name  TYPE string.
  DATA lv_font_style TYPE string.

  FIELD-SYMBOLS <font> TYPE typ_font.

  IF iv_font_name IS NOT SUPPLIED OR iv_font_style IS NOT SUPPLIED.
    READ TABLE me->gt_fonts WITH TABLE KEY id = me->gv_active_font_id ASSIGNING <font>.
    IF sy-subrc = 0.
      lv_font_name  = <font>-font_name.
      lv_font_style = <font>-font_style.
    ENDIF.
  ELSE.
    IF iv_font_name IS SUPPLIED.
      lv_font_name = iv_font_name.
    ENDIF.
    IF iv_font_style IS SUPPLIED.
      lv_font_style = iv_font_style.
    ENDIF.
  ENDIF.


  rv_font_id = me->get_font_from_dictionary( iv_font_name = lv_font_name iv_font_style = lv_font_style ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_FONT_FROM_DICTIONARY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FONT_NAME                   TYPE        STRING
* | [--->] IV_FONT_STYLE                  TYPE        STRING
* | [<-()] RV_FONT_ID                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_FONT_FROM_DICTIONARY.

  DATA ls_font_style_map TYPE typ_font_style_map.
  DATA ls_font_name_map  TYPE typ_font_name_map.

  FIELD-SYMBOLS <font_name_map> TYPE typ_font_name_map.
  FIELD-SYMBOLS <font_style_map> TYPE typ_font_style_map.

  READ TABLE me->gt_font_map WITH TABLE KEY font_name = iv_font_name ASSIGNING <font_name_map>.
  IF sy-subrc = 0.
    READ TABLE <font_name_map>-font_styles WITH TABLE KEY font_style = iv_font_style ASSIGNING <font_style_map>.
  ENDIF.

  if sy-subrc = 0.
    rv_font_id = <font_style_map>-font_id.
  endif.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_FONT_METRIC_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_FONT_METRIC_DATA.

*  Simple font metric data - gzipped and base64 encoded. This allows us to store data without tables

 rv_data =
|7LvXDrPcmiZ4zlW06hR1kcG0du0WOZgcDSclcrDJmatvvr2nNN09o1G1NIe/LNsrgWGt932C| &
|vfy3/352v/+yF/PSDP2//Qvyr/C//Jeiz4a86at/+5dtLf/r61/++9//liznf0vSZPzfRj7H| &
|9st/ezr/7V/qdR3/GwQdx/GvSzL+azZ00J8DnhH/8s/D9+S3Fcvf/+aYpvf3vzVr0f39b5bp| &
|ev/uco5ief9uMLrwd27Y5qaY/wb9P3r+5iq6pQn/Lur/zjMe8+9SrFh/7/ObyH6OKOrlGbwE| &
|J3N9bON4qcO/juC+E1zhuPdB1Jwu67Fkdw26iI6K4hTZ4IxJvkFcSX05pFBSmYD0FnacW/Bu| &
|fsOQAlkfyJIxCcz3HiVvqMthBMZkYto9Eo3mAtk9H8fY7zqD+6i90cl4FX3+xVH1yFUdKF7Q| &
|lCdhnf2I9bZpP0fCFMK267zjhKFgiIVXCgWxzlZT484wZE1mY9/lsdijDyxvzarcVjDlXTIh| &
|F/CpvymKpKZKb0jR/sodfW5w/NyUt78TaKde5I11KFT47DeXchTPXvvhSsFMUfQCZj+ov8Dy| &
|h+YQROAtELTaUupGKPUJGyXbRBcJUvfRB2u9Q1n0zdgO97hwhOI1Pqj3MEbqLtToOjkuCtOY| &
|mdd2d3kveojuEYBEyaqDR3IckX5cHJxMk0FwTNKLHG/jcR7TzRHlWbqu5CEddxoZjVF3MJyq| &
|Rq3gaR4/tQPFUYpNecCp4FRHLy1KsyRoxWVC5fT5NGN5HihKRfxibbp5mgeIX5RE8VQrL5+n| &
|K6n7sx7E4MCi2OioKF2MCag3+Ez197E/bdKcWou1GN39XtJfyy9Sz3dDREdGDR7IJK02murh| &
|HS58zxMVQsEaJ/PyHi1Cphe0ewDnccxtI2Ky1mN64hemYkvW7W00UTrYUiplyGe1Oz7l+Cwn| &
|XCqR9BAPCzJlkXlmhkTEzLfeU0WtoAdcFjowr5CBAwR3XyImfg6ZAOOPUTZO9YNaV39F+Qkm| &
|IE3io1s4MN2iYj7zWfdSPWJ7ezRTpNOnUSlXBiS0HzlZtUiPIJGMYzjKw2+tESQXq5gvPFUt| &
|+2OPS3g568uCb5jxcsHuYRv7Whtb2KDjvV7tbnLQsETAm9VIjWQnrq6UiiSsQPFZ4jYzfcMX| &
|deGqV2loq1JwSkuydMs6IwjZWGvhZUM39ccXGDisK/vrhZ/DBNTTpfjq+/EMV2iwoD+6YRs6| &
|5o3yyXsGLSqyWaObGsg2AxKUvtmbm7lGwa2QXeRPWGkmBybflF9/mFMAi9Y5/dc336QrdO7V| &
|ZF90euIT1m6vjb7Ke3uT7Xvp+UsSOXetJApLYTYVgt+PZ1k1rCSz4r2uZaweEgD5e3MvAhVw| &
|+MDvgXtimXHXFxe6ijxrkto0efOm+riwvzkMW9KuRIXpYe32S7mOaoiGcqSjYzb8C6o7YPIy| &
|XtbS71MOIQGH6epUKkPgb1W4A8ENPJiLBndClfLbJrxhELCvK89S522dtuMt/Eg/5ZSZwpkC| &
|zYGDCfnOPSctolBud0q/6xi60QojqkkYdG3jiWpXVxtLejsSIroid4qr1TLcYbD3ITq7Eos5| &
|43aH0q7A6b9/4pzINzgSOjzb9o7Kkts3ZwUnnTcnTr8UciI4XrQQnuGroy/7a7OiZaJORYsw| &
|CKd8GPjj08kvB7j2UigsgSlySN10eVKADQY5MibN57Sx3ZQpCRtW93TB/4rZShr26TmGQ6F2| &
|acVZ8L3O13KVFMXH1gL0IFj/rJfwN+j/DdT/N6SH/v436J9s8f/NGf+VHX75X8TxF3H8RRx/| &
|EcdfxPEXcfwfEYeZ/pppK/7ij7/44y/++Is//uKPv/jjP80ff3HHX9zxF3f8xR1/ccdf3PGf| &
|5Q65+O3F2mTJf5Y1qBihfvQnKLKydL9jWuCLedDneYpTKo1rKHzfL1pN9GbMhC/TDK5LzwuT| &
|nIb9KyqUl0L8GpdoWmGnaYG5/fQ9R33IeYse6MP898UueLlnL6Iv74zP0BiHuvdpshAK7QhY| &
|3BC1FRCRnFCyQvnNgxcBgRxNgygExMuOB8GGlRJlQe2+bZBVQJ+8XMDPMaTb1hvIRUUUUUe6| &
|lQYgXhbygwWv14OI0GZhuoWQNxETfH0QCyCfK/XSBXm+JzwzuckEqdSAojEzo094U7yMtFHx| &
|pO/hYPeGpRumIS20EnwBpWunQWBDl6u0Lmf52lbAfI69ddLqy98NfdcSlT9e65X5y/L3D1a2| &
|8wSGO72dBUYtEPR6gYSeBzCx9s6LAomSWDIaDCuO7ulWAK6LOJdjz5SF/2YmgzJFIjEEXG6w| &
|t/8u8lz25qWwptCz7Nf88tL9W1iwheh77aEY5Egp2g7N9lHdCwKggZCt8hHDy/NmpwsJ+pDs| &
|nVEmEqofzTyoTA5Z7wtZyFuZmxcYUJiVGusk59So7fsmuKdzk41+24RMAVSKga0AC99Da5zn| &
|KmGWan4C+opZfd90u6emJXljOAFfdgA9kIzVurdmO9TS02E9Vw72GKmSgd+p6AKcHmkJG/ii| &
|EkM+rDp8JlLkZVB902vxjGlplT+P7qDmJU3aaVtTu1QKdkPnh44SXe9QihuN7iL5eB2BA03T| &
|RfuiqbYhSRYz8m40+z64LVaK2Nf4rK9eT56reIPEJpd8sjYQPh4+uvfhK2xAml9DOvBACFs/| &
|H2CD8xXvpQ/J35PQltEXmS7O87kqr0cLyj1jl2DPO2cBBknHBHGvrGdHRyTl594sqMzaxIGN| &
|Ja5rq3OAEKBKLd9GkNuUimKc0VYk2SzVMRewt+4Ceafn53kSw4f6Xuo8MbzfH3Y78aC6mikz| &
|+I6Ci0JDaBwN3B/6J5xQdD6ThPdIqmrEKtwg1pQg2qR5hlfyZ4G6isNumfWGEl08ObDtee6h| &
|bgqzadPz9blrgevwBghkxhPy/jNvTGdXPia4E94EPMtnXs9GXAtOw1SxZUJnAdeh8j5nuLfO| &
|H1M/VLzvoe0udbIAsyKL+g04oCiydknx3Du5QcGDpguMTwsia3UerCdPxCahvjWeGqiTOtSt| &
|UbbYWmRz6qkMRfArQemvorwNT7JwoJbwWOmdFO4tvOZfM5bVAilEUrRIKA0+wSVmvRNa0VKi| &
|Zu9BdpGLIP/DFses5FHs6KQ2902x38tHqAB6bULkA0V9eweF9NURF1Wf85xzlYhJdJXocyJ+| &
|SbZkIxhMyqC6Q4V8Jc/mk6/2APP6ACM9LikUvd2A34ZcI9LZJsVWgSSexiV93YL41r1VWA6U| &
|2iT7RmJpUjwXPNC10SNsEPbhVNCWs1LuY1Gh8Mm4npp94GM+DG2oDHgoDawRA6/SthVlCpJ8| &
|BvNzcz3hF9jXHYbKFJLfC66lsaRNHh0ZbW2ONtiH2qairTgQaAGgKvepaFGQlGQ2oTZ7W7Wp| &
|+6Bree6fVd0tVp56N+f2Uvdr0Xrq0iQ477qpBYcckf11EuD1MbyEdFNAY36Bo/S2y2eOtH07| &
|u7FyUf9Saa0nZBLHSeKmHL/JQQJv2WaOiP8xnTQkE+ZCtqG1LT+5udkRUHBqAP3jnsIpDhtJ| &
|BU4E1sfgK2s6Kf570zgtJCumWbRG88lJjac8EGtxzG3JzTYCjy6vwZxJVN1u8buqMQB7Ln4e| &
|RASJi0pm1M1lTX6NQKoMoZ6VxlGSQxUU2BAS6RFpzciG1blCLrPWjeN+2Yh31bZrv4ddD6AL| &
|IHixJbXCbNOkwuInF78zSKJqcNPRRzJun5jaOAzVx6Uwxe+1mzipNl/QZdwv3ARSm8Afl9Lr| &
|D/oiYQ6olNOR7JUzHe60GdsVqxbpM04LcloN8TT63ZgXgv5btXW5GkLb1rTqzaAK1alCIHrV| &
|ctsV7xgeGL0VgKtrDnnn2jX0ZK5//bU0EGZ8bUKaZhx2kTR7gvLYlChpgx9X88djFQRRkldm| &
|R6iYi4cUz92WOr4QtAKGtTMrbN7Lsi0M/TLbUKD0yir5mp9QUpZ92fxw1mcV3hIou9rLrDrK| &
|hvkKJcV4CmOmtka0YqqGXKUCiHOs+gii22eogxYWcpr1t4X0C1X8MEGvEamHzf64EqdgpvCW| &
|HclVYPNr1fp/1OMpcLriaWx0IPUbl0aU4N1oC/mLAm3y5klm6qN6xdPnzzuJsPr7sUubLdrV| &
|50FYfNaW3mYQyvzNwfLO+B2k8HjsSSDIoOUzF2S/Yj9L3F3K0OnM6HAW8ioVFQeHnPiKeClW| &
|2aiwYpb1WyggwRsOH7zhwSwbjp+R8vO43NEB2OO1fRAkwjXGsPgD1yjIsgu+PfzcQe3QEtqa| &
|yERqInxRwLzHAIHYBjoaXkesvZ8MWLeT9FIWgW4kAxDGJ83e9c5xRXc3QuakCL9qo9/XioNq| &
|wzG7W7Copvgy5GDAXWUbbduPX64yCIHXJkUx+rafZm6TVRUwCqA19PhP4rzj/RKicOrYliRl| &
|sdFAqSLfht0Q6BjaXSeJH9jxZpZ3ZG4VDP37FjL0OtpiSYJEmR6BDLCPbwgLoeq6Wmz4SHmS| &
|8g4txkjOQVPGWYbXKH6NZ68GS7MZjyPNFTfKUVdA8e3XxV72VUIwz/bbRmKAeIO5JHWylndC| &
|H3J2O7IGcTT1Q+tIdTDOJogDL6Ky864+NVMj1creE1P97MXnbn/MFcEd2MNW6IjHgc6T03os| &
|lOkohPMtkLZQKxME8uePx9hTmkbaXBDjavTcw+zQHDgu4Cg/qhDLZyt5AEmtD4ZG5CCHEIDJ| &
|2HjOCkiGFXArqu3+KA23hwvn3WoM3iqEJ76+5FYeb0pQ1q5LK6VBhJWp+UFtRKZk3ECd8I3h| &
|VP8FOGYNKl8lZyWEvjNSnF9QpMmm55c0uojwUJgYloqqVuvpwaW+24isVqsUr0JnDJboSolE| &
|1bl14OHnFxh+XnfQGXSSqVFTpYenFvvRMNc6FIP4eeP6MnM0lrbP9i2viIAXvbg7X6EXX0re| &
|ZXuaLZ0hROIUCVUASLF9SnZtIoWwZ1wfvo5pv1XJc7ofU/Wnag/vRxBMITs5dfSVQYkTWeUJ| &
|qmRsdb7PSVaJVCGcogcIJIBzxKljGEfiYmF6jOOCaHzNVIinMIrQ/ugLfxxX9Ktw7eIcgSpN| &
|XZTDaKkefT+7pgdTjsEZCs66nsMCMmNKHK8QUBmvoNBxnkBClbM0tpv02EsxHETm2W/uNP4w| &
|69+Gr2D5FegElaaf3kLQJqRzf2k4DhLfECCdcGN/yBHDX+bX5SDEpJUab5KPV507mvuRXDnC| &
|4KSVwR4c3lAKh3riu6q02LT/lL8cs34J5qkbE1ANJuwJCif4P2XRYIUrlQslpwAiIzjX3+81| &
|Z0BwXTj9nGoIgVN6z1eUf/mo6cD0B9s/1+vAYar6Zb4HEJ5NGKnN3D0mTnEXtlTD6AiFZBQG| &
|3y9qXAODdKjsxAqsuKg1zwly1qO3jXX7zMhpSk1UXmtrb5DmDFzokBdr/N0gjfTRhEbHoGzE| &
|O3YVSjDh4Tw+Oe7I2czH2LCCBw0ZeGLlXZ4WZS5/6Q1tXt/HFEkfmTiA2mhl9GumaGCA4jXG| &
|OO+2DLLSce3QgZrFDydgbvV9xY/eOPCrsCHk57wbqJWSH9U+2SAuUv14ItfLcACVYiNGegk6| &
|CkOPtRM67h55lJyqHpZt8Iusnm872e91jpkcWyZXZEs+eA3v90s6NlAS71bpEEqyckoD0Ad9| &
|wBkTh1do/gKWh8IopSfsctJ3N8NFiJEtT9FWvYL9sWEe7nnzS65huB4xXjrPOP/yAkWV9Urv| &
|B7ChGH576evzDJ8ONPk5RFxOhWEMRvcsHYohefXy/UDk5JO5HLDUbauFWkfwXji+5BJkcIp8| &
|/s704x2ADnu5ivN2dTNNIfQ7/aLwj7qkpYQ+nipHcra8y5xo9m+9GlECpp8qsJ83PGMSk2Xl| &
|AF6g7C2X2UtFAI4xj29AFSu9ZS8r+6CMPb9hqRtAFB49L1cUOczXwRUSm0C/RFoMeGU/oYau| &
|76WqTxlFv1ot2fjTVwCTvvkqn5HW5cmtzY79F2V6TRd2SeA+uRByMtc/fMJIX66pbFb5VYKh| &
|vIeaSbSeSZiRrfjFhCAMls4K2KGe6n92Jsi8zkZkYguZHx6M2RlrySN9BI2qyzF6Zu4Q9pPh| &
|UVhoQyFlg7qIxfh8lUwuOKm94Dx0YkAwmbeuBSgnM7++a8u5G5c3VqDpWnRojcGQ/v1YiMjd| &
|cYKy9+pbauM8A/GShZBpw3msW/0gzoWJnEjAZtxJG9B6anBn2gZ3SIbN8vOjc2H7sMNV59Yg| &
|j3524BSKUVSoLetvuy2PcdBsPjqcWiIZTlAw/ZlCIGeMln0LqcQ9UsRm/nRKAiwPbI23nAEz| &
|QuRsFVP/ozg4C/6JLJt3WJZk4k78jz6QipFPigHYYIl4wDs/rz8qupEZ4vvgGSRY9aVw1tO0| &
|icx9yrhd3HwVRgEoyMtxCIrlHJV7hQNKeS242edro3tglWiulZ62tAJbR/RTYzxLV1tudapg| &
|QzLJU3d+fJ9kH7iB3stSnaNskoS1HMMkmyhu1Sxpai5CyeAAmOyVCP1wsVQA4iT94Zthr0Aa| &
|vy7mUEwkaj4gIeKwI6ksOusuhC4bhEabcigcHF0fcA8o0AiQNLWXC/hC4BT7fgch/szF/Ssg| &
|4W8tb2GLl/tnakKbi7WIpaK90RPlZqzUeDn3AyA8epdCBDsdU6DU0SgHfx1A6ecCayYs5S2N| &
|0v5zsBCJRGRtZsO0ekUm8NK/FnqXBcdwVIs5dkjU41ttzLUHFehjpYlFu9y4x0DS3cQYfHin| &
|/IJ6mV9mQNC39HkoM96IrGpZry4wLD/BPESZEuqvLJLLN+qA8Delj/KXEN2czsW17PULwPES| &
|Kgpd/sgX9Ojr4AxApEppAocrAikZRq4YEGOSTeOfSe3VImWCMvmdG0V83Mmj/G0/NzMnBNqL| &
|MyB0j8f8uf7LJ6xfa64zNUZhcE6fDwM5MyOcw8CPG5rshW+FMwR6rz3f6SCekiy9itDiaH6g| &
|INdwevkGvHrHdjhLtyA5QTFtCy5LtiJLCKi7z100jYHCrld6FgwkYe+fRK1iGJUijMPpJEtY| &
|XiXgx3/kINGPMuDr0wLuyBbEkEo6tMAfX9Ue3cf1Ht56VOzunTt8PYGYvP1ZfmybBv4ihIk/| &
|L0Gom1N4t7nb+2b/ezM3gEnxE5mbx756zMm39Y7eKPl1hhemNKugk1aUW6BmXQ/jo62g/6QQ| &
|MUA4KexHEgyhor8mNWgWIdwQnwFQK5roqak3ZWi3lTBqB7dJLCXyBIIZsZJpbuLzzzSFjT6s| &
|ZR+jxYwOO9q5Kr4i1258dPMiR5v8hAcHzNZEny3aU8YxKMobV3G0lohIEe1RSIzL6xsTDwze| &
|bBM1TvLL4xSEYqZn8WwJPzBqc1CEKzMKxt4gAdTMNG+FzPRl7E3vV84p8AyHxvxRvt34EX7S| &
|12rd57Yfa/J/11tXOsdQ8WU/a7Q1wyWC8hooAmoOdqleYPdiY/aRKnKfE0RFaQYH/jQq6/ry| &
|hUHfCYuhwBBpdltau6fcSKu1ZBwbCd7sykGbQWjerAEYDus6wsQEqtz0ijbnBkw+dur7Ze3P| &
|NscK4ys80boCu1yP3WEIwRmOp8ATu3NRvv7i1NabDc99pCwPEF9W0TyGTcaNLXdnzQ9nvfx6| &
|p+suXQToGDi/FZhyVcIjmKU8DT0k9cpWKH+Be0eot7Shc0CptOhgDAzKNxDKVBV59xx++c8F| &
|m6pxfGcdKrP7Vc/yo8GhFF86+zKJuPw2S7SsIv2aP9MyHXnS7bLZoKRf2JwEqMtMJ3EfxJIk| &
|9PXXpLNCEkl/24sQC8Vt8vSepTJybw21/fYsQZTH4TzvtIoaufOCU7h485ewo0lAAe+5dMZ1| &
|lPYgqGvtcFGcKojxEx2vVHlT8hwTwSdq9/tqtnP+9L82r8Jvmzch7BWNCVeFa6JN4Room7iA| &
|gapySobL6onT6x4fhD4SV/pWYyT57E8hnHpU+pZMnnLiamE7Kv9oz5cfLglVv60K5FrTKCGr| &
|DwLvbbXf25WRaFAle9Nv0SlMyqi8Aml1Dr5h+9V177jGEHKPK5teaQTUPaZ93MldVRWigO2W| &
|8n7bWPwHaNTBeUxWzGTO58Mxw4tM0DQSQQ+uK8fmL8OVKmtWb7hJuF74Vv1cqAqju9gajN3R| &
|vJOIUfwxYTlh+AF1bSQ4MyTNmeBHVbOdR8AIfr9pRlKv3Wb9CBXX+Ttpj3vbH4s5Pek/aTGn| &
|/KRAfFisX97s9LQnzcgAiRRZby1MUNtk1G2y04lUzSd9hkfxKB9XLuCbRalHIdEwGedEcvqw| &
|Sy5hT5GXqVuqPwlmce6Mmse2Bww5gRcVWb6k4ud99QbRs/epTYhp1nuDSwa1sHLvYDp7I3Le| &
|cnv/c1D9/SqVe4uC04qPz4332/MSWz2wW0aN0weMul1KSj5iJ2hiZVrASq9Qvr2AEt3bgXuj| &
|bKlPPFEGHFxpmSxt7y2GCa1farszNDGJBdpMIJsge92tnXUKGQd1DitMQsSUnec+ocLZioLW| &
|SqaTJscoDsu4rM8KTEvCjcKoEWvdXWB0UyTu9OzsGhBn4Lsrx9maR8tYJpMiiotsFFyaHVpO| &
|e2OVkbxD5QS3N8VfhKRUapo/wKAQpMCKm0eAxy/Bp8E5E0tgou7vSCp3LnInwZeterlTaGnp| &
|y9VdkUmfpv4Mncmuh69YOyroYwRnGKFNM6hROlw9RbGQPEpSiMcJcLU+Ox+09A9kDMZxchW3| &
|kyrFCbibwEM22+fpUMTXa/q1F/N6KywbaOW3iuQmfD5UbY+hHny27UYn0ICytuMH2JrLfrX4| &
|manvMX3ZvNLYHgnK8Cz5nht671UjHrh9xKrr6cseMuNUQTeuXyqbxkxZ+dF3YrALiGhOeeI3| &
|cSTXhsFRHkLJfnzrO0Lb/dOfsJXa9ivPD1gjP8IbuZiGG7JJ2YWf/QCl6puO4Ab81/lTB2Bn| &
|MafxUPzk++ZuZcomla0apUdfTGs38KvUjQlvpMb4UfDWDJyzhXTtBNunxcjDGYMtF6IOlOZx| &
|NX1Ahq64v7Ci32SVJJG3uNL62zdp8U7nL+TP+4yZZ7NAz3I9Gv4a0+mTmaCsCrIffDX3hi+b| &
|Dw7BIZuhAuwfLOYWOUw4o+TOx54NJ7AVZ/qUkdE+ATucESF8knVCoug7rML7uFSfMT3uDf/5| &
|njQIe6Xv00hdP5MK+IYp/+lYdVNNMFQch8ib8tM78J8fsn0GP5AajaVJQ1WD8e0X/rSP689/| &
|xOPJpf1CxGHDfjBssV96Ai43uUhfRrqknxwvoicxVpatY4dY/cK4kfxcN5Wr47tsP9aKRYPH| &
|I1vMyWJ5yCfqNE7D0e7TsazGHj7QXbzMLJHiNUxVdbrZcIzNLy+HsBhGyDw2CmJWYg+ljRRe| &
|CcQDlfhJYBxHI+H0cMChdxb1a5kGowoTwOrteh78QGSZsImmG4hMYvMqy8Qs1UpfxrQr9Svq| &
|BqNhgsRwxIE1aRH5JyQSu87FjNYyna2fJWN5JBBV//RJWLkdkBtKPGMpSNJ/M9ZUWZwbCs0j| &
|sChcuRpnqS9h0xGJts2hMb+DHechZxJKEF6YOZcbCmBhg4okfGZWkJCRlgWSz++5MpP06W4/| &
|53LWG31Fb23PirmcwBHFqG4dwYfD8hVE+7Rck0f1eCuB3g/VPW3nHOXnW+edAVaV1anCMovg| &
|N8zZj/wrumYI0oxMnrSAeIOZxVdMruYv0n8RBLL7tFB4awWfBu90gLZpPCjG6EZqaMyrAgtq| &
|aAYPyncEOnhodz3Z2UdNyrqO3qw7wg8NhJY/O6LXHbN/rMXvEi65Smc3QmDxEqNW+9/SiLIs| &
|vhUvna6JJwwdXfZR6sJx7W0dIi5jgckuMkKD/P66V5Wt4tBkNG8+fow3xlaRNWcClN6yyTkn| &
|iIkPBKLjg2+3BDaRYK/xDbk3stCe5be09r+Uw8OnNcvcUoLpj4iLm2KWy/EHZIjSVgYDfag3| &
|Hc/p57aSBLHHeV2zbJOve08ibEavPfn8+XYCGoX8V8Cw+TREt/OMf8BJWdcSFjsXBRDQ+5xe| &
|+rtm2SW9F/EgsEkWJndR20vLPy5yXeqlba8JoQNsC3IPdHJsDoSnTD8cEgS4EBvXD7mGLQDa| &
|Ijn6VMFmUVLQpst74xfC1v0u/e5DE0Mq086HkxF60SSGWOWrQslN2HN51/wElTmQK/I003CW| &
|oNcd2PdfF6K0wBpJmlKHoaKnI8f1Bht4uxgXAytwIjjh3LnBLhsPqFD1rY35J67n/rVrKeHd| &
|c0Wkj7jHZKDDccHPrgAcjPtX1O6V+9AHymEUW2OzjAXGJ+Vvm4KCnr8fkYa//JuNo4JQfAft| &
|+J3qwOMYYRbt9OXhzZfpX8gjE1/nr6a+CeTh1C0GAz1uvwM5d+SxwFpJ+kbS/+QymaBgax5/| &
|wJeveTCXmqTOrETMZkag7QDaYCUoI7hYHzODeAnOc1/tOeqjLCiGfSfAdpZGPE4g20KvOiP1| &
|gNh9nsCGYCE+id4Szrwi6PhtSb4GHCtSwnVvN04xM9J+abMKZyD3RObRuHc+/3mhVEgI04H4| &
|6q17h8PTVM4r1oXpGnlhOsZucsZkmMRAQfBkWIz9FIO/ckz/lzJaznMMFtM7V+MUu23hk4Yz| &
|OBJH3+t2JJCngfJg7yhZ1b9hVNKBgR/yBxAdEY2CQLc4SRwJRzIH5osIvhFRPdNOtymcVXV+| &
|FeVhr4vjlF6vqnq8H/+kuMvFwvrQdr+wAhSGq39+vhYZlb/2iFfQOzXo+RIHq3YMovHf60Zw| &
|sLQf/kYii351a6B/wzn4+RnmuCtVy3uFRiL5nnKgIMdXcRZ1ZHBobzmIQSfovCqvVpi/gZfb| &
|45Y43Tef5OlHzMqn88PEj6t3owbVxZRyMy9oEMGFdnpfGvh0YF/iXhIgdx+iaWvOWWr91m1o| &
|r/yFeSZ8/yziDu1Kf6wDa3yhSG3u1z1YkdyODt0oSO9ijiWyt58AJuYQSmb3idxf95fwczAd| &
|tEJ4RYUcW1mVFGcX8FS9zvPwMPK20TiX+EKk6udgKbDiXUNr1482Md7PAghdYqK43f0MafnT| &
|wAnwwMBvfifdKIG4z834BrQq0TNpP3swIYNb/q/69dOLk5IHrZpp5NuzBA9UFC0kFveodgwU| &
|Jpn90EprlLVWMzGUBMlUnVJ9BCAm+Gb3PeCv075zj/XX7/qeElaqm18sBGYlxj4CAnURZSND| &
|ezM1VGXc2IoMRwLvq03ApcL7GhUL9emetMvv0LyzamNC5wc7dBd7ovKuFC+/rJi/NmsXrwUg| &
|IWmlXewHdiMTt6/ERUV4OV9U+yfnLGbMRXoneEJv4sHNc0NFzOAKnaX4jh85aP3AIOZjl826| &
|amsaeNwUUrXjXpwvuICpj0D/7m2Mo+VxyiO+Bz3zVWy81m2xTQXyx8Et9ZArQlyjZSflpm8m| &
|5pWZTA26gAD1hxlYLfOZWhounxGC4Le+uzumc0Z9zaiCHh16uv771WOcn1vOo1GhUE1UB6Tq| &
|LvrJzMsR1Jqg2d8dAo1f/IL2MYWc48Y/rltIMXzQKI9hE6OtycyaveOrsmLXK84oNSN/VuCm| &
|UC6TXh1+82wd0lmavatyZBhA1On2LriqfrrGqT77MqqJud6VXcNbhBOEnPu2MoGtuW/prhDv| &
|Kr3M91PnmsRHEPmkF2ieLIWcnmADSLI1oZb+JdHvfjv8x56COXIfY9HPFv5VI/qzrI0r9lMQ| &
|qcJb+Om+M7j24YS1yn8izkJxNzyf4Fs5VwI4BonRhBjc+vc58EjZsOf2vktn4DzqXik305ZT| &
|Lj8BtfzVTumvst3TkLClXsuJhvV5dDyI9ZYmpg8vwGfQ1Px9E/JkwPcjP+MFIfs7vONvHgTg| &
|Y5h2c2tIyQvnt/qnGM4/23LvfXvEu2q9twabc60AyYnHvsCLJLXKt0OVzrwiL0XhrXGU3lCX| &
|+PhlCF7o1PJv5H8qw/HbAtfeJB8E3ceY7B40JYN/lIH/qBi1RqLpj38zK1GfRnyJG2Vqpr2C| &
|hI4Zrrhr6BXAVhF65zfof33mhU5+ph8BXdEYE+Nxu+FHa0w3rt5oB9L5ZHKVgDAiS2Dau60H| &
|nvUz591Wg9iN+Js5A5m3JTnqiovBK5Fn7Qisq0FXROfx9SbzSPfKl02f8DOWezqMbozePPec| &
|JPVQLhhwOiyoL1qMqBfD5Sp0Zrn+SnrKYa/sFw/5zV0AXtDo7quJcgDhZUlOoxS2Uao+Z0nE| &
|Ix6CgYaMOcwBFtcnppyX1lPNXHBGX9zHQo2PUCnBqzIIaozHQsy8xKtL82sABE043nl15jhP| &
|pNlMw2v9dffeNtsINidhtQFCgAyWWT1ypEZp9uluQZp4vN7yyl/3YqV3j0sQtPQ8BRyDkENH| &
|9m//P29m/D/6D+6fHY1f+hbzB46bSf8SUhy57joGqBrbQ8M3jvG96FbavE5TGYM1aP7xbAHX| &
|sIy6Lv5PUuwlneOTZWQVOEJOX5InKn7v6ZqLSHuxn4b19LSCdti76XWK0RhJew0fG5kaqXJF| &
|+5vC+rRFz5Km+3ZDaNBiePaAMmDpZM9zFqyU0r3KYZm+zwPXiNSoD/q1wGl8l5nZgs9ct0I7| &
|BJR4EDTZvPblyEyKal8yCsWZBToKnqOAJ8C5hVGvcdG1GqWS8sNDCPupXj96AdkKWueHkgyK| &
|f+wGlW/kq8CorXINin5BFlZSBIrFxp3RNBVTBHB75p0h5ge6e6zOoHXbnsQFKfys833DrM/v| &
|tUEdRVPpNkMQ1Kv96y43qW/PuRs9iFif9na/UAWr6BdAiH4KJoO9vEx2fgQJggVmh2deX0Cp| &
|a9K9saqlZS7bmEL4zvEp03mTD60MBIPQK+shAnpvXUwWfJ9aQKA461tjVh1ZpyV/LcsbSVc6| &
|aHY6lKBPwt4Zsc2vYAuNGiLNiOvz7rU56apRhF2xYZ9FEEWkq7VlNggwIQPlPb0JFH00Odlq| &
|P/m16rvy6xD++wjFSuzpGv7njsfiIs7NnIYfbL4S1txR3dG0XlRLcq5P0aMBuaXDNV4fYie2| &
|7vZeu5cuNLwtj0+mbfdrPP7qR6EZfeVOSXMgRCoHpx+fvtKguA1hCPpiM20WD7SGImC9SunP| &
|fmP/3NDN/rO90URXSUxRil3TvNalO03rDtkPLK1CZ1/jmSB+WUFFOeh09PYlL2RFGQxbbQIH| &
|ElxcUcodxX3nmXUBF8oPtwND8EiAdhoLsnTo00JVf2D1omoV4zQmUPuPqa6Oy3bvfi+IPjXV| &
|uMNaE1BHEjWK0jM2eRcecdqk7Fo3IFFp47KuSyFdKiwd2il+yjJfqKg2htN5V31t1wEc8cMn| &
|5dTXnZ4CseLAdQUSLjN8c9HHJmGXHJKMw+CYreBv3cY8f3gpggNFtTXajdyW6uc5vlCMDqr3| &
|52pSNfcy9viRGIvVQAilMmgwXfkLxMXQSIcgtnx81sDPs2HaC8uZefUk78YPKNl7XA04ls9t| &
|lx9B25NR44tf5g2fDBE+4wcQTgn3wYvqqoae8N7eiEHZk6WtvhBIfdrLRq2tJemHbNn3zLz5| &
|lv/SUOtKHtg6D/9aFon9Y8vHlP0T7SXhPaTMxJGKzU3Si4qmRrCrMFJOKc+Pz+iJGSODH/On| &
|o2XqmQgL1RbMZCLLDjBk6pXfr07rhn+2R7J4rOxOBLfj6xjBQO7shuYYfnEZZvTOQVU5br+H| &
|UfqaOe+7g42byg9EEFKKert9VXcxycYuJS5Z2cBA2+kvVOoj1Jb2qxR37sl9S5XjkmZhyEqj| &
|uVp6abP2aIKdBlqU5Cq2VLlD8VY5NmRX5s8XsOWvv38pYG1flfkylsDwU5WoIsp/icou3hy/| &
|aSrEMn5OWsXEKT7z8oViYpWvQvsSRigIdGiPLCZifpxJIVNa1wds3JWyR2j7DElAvOv/o97V| &
|obQMtf3VCdu8gw6z5hcng/AVklWoi2dVZ5LAD1JixIkTpW0ptsVx5wAl2AI/afUpyDQ+P2mn| &
|wcbJL+grIYyJeT2mg+q+jvZQ/homA+O9AvNzibVpeVK4ONeDUAzmXvrqSM0XGF2YJvXXq3yt| &
|0blhweqXNdwHfsIKOoMt/WO3hHh8czVGF6nZx+B2xxs96vQI/85vhyX96hlDmWCjAwGP6oXb| &
|QvCS7Vmqpzxneg89KoLy0Ud9EMykX9/HwQfd031rHq+kzFuMvwxmPk7jVKDKoUPZzVcR0j+A| &
|/sG5bAiyURErxXVtja/fDPnuJVYIxLT5586fZiwFUWBtvJEcs9mQmFHiSI4kZ2SftVV+n1VI| &
|hEYAkE3sob2NNXZwFV/llS8jKI5oi3l42oThngdFB+YcDZ7sL74ppKUitjWc2CdMGUq6nvZD| &
|UzA3RHihA44WIE3Ay3wtTygncrK3fbGkbuujRqhHlQa8/kcWU2s32I8AC/k49ahiXdrlCqar| &
|lFQk3zqNxSosXwEUPT7h4p5qer00+e1MBvN9QgJypHaoj8qAYr50/qOOJr/BeTWGZfM1O00r| &
|c1D15R+BItq4/5mBYBPFmhke0HdwIZG5BsMnMSCEyWIae/A/gju9KrZhx5do7pxdtu1gTmys| &
|IpGpRGpPxp432xfUYu4nBL51pGq7x0g2K+DMVumfk+NDvEHeU5+XjWBdC6JEPzs7N2V+S6v6| &
|Sq76l+SmapMTjGfu1iBfFObVA3eA6TFAg49n4VCUpwn36rR4f+YLr7jzoOGW3ENWyL2ROcKt| &
|ynW50fnRRs2h/74Fs0eGYJvhP4tD+kYLTIz9Ozo4WnPlnbDfyiJAsgnbcho3hZN9T0jETWlU| &
|37D9JmN1Z3iuuhs/U/XEXXdUSu7o7CBgdWIgJGA6Un0endIhWDx2P2vkjBGtdafbY6F+R4+H| &
|FUeP8I7h6XuwH1cFzTJZKaErVwgml08N8yvUmtP9HA6ouSRYeT/4fGJ+ZZMoHmcZyZPTW5UX| &
|Mt3ua9pFpLvfot64VzmtZiz8vqfl0spmju85fXQ3X1U1Ej5SGuCr/VKT+Z8NT1whksjW5iTY| &
|fwY1RLtdwY5r3LMwdilMgsldmh/58uyz4rtW/sh3n9SUh6sO3wYUjI1A6TutshxEsHRdHcTF| &
|c0Zx9/GPcy9hpdNBa0tapPDeET4fIjMd3udBYzehn+q4/D9f2XItuVPbkU1qUh2SM/Mn4kNb| &
|9LrV8QROPXjV2H3T5wSzKhTfzBihGLLhncLkE9XHngTsgrPK+uc3CSkOtVdOzpC3YY6KcNUj| &
|Bz7EAYFq4OKe9GI6RHhw8imHuGOBs3z8ziAd9Gkb+rrVZk5BhKuGpy7igNy57g0XxLhSnPwn| &
|PqIptc392DKM6g3e9/78yMjYFSLQh+Dc4OaGn8j23hSWhMxZNS5awBvrubOi9g/S/lQmOjyV| &
|kpMP0/zhH4Yeg4j7eCEa1EbEPS2/5xbDqnscY/fn94qE8+x5dd6Kzy3swxXL5CccZjsU8MS5| &
|Yfyjw3w6/uylZFomahmXYznWfjm2J6iCNHyZ94NnuSoZHKvUTCSghNb+Xmu6+545X7r0yF8U| &
|6A7obVaSerXj+GOi76Q+tvndOqPmwwoDe9xrIDm7f8aIzpOK72H/NOpXNeQnROgHsjznNcFI| &
|+qRghQBlWIfJPthDElmCNJNfkzY8h347c+TVtvUMaqgaXivwIUiIcWCL/CXPCRrEerj0C5cm| &
|ijLTjAX0JDHAkg+gqMchipeS/+gqTgzYRIxR4TdWIM1E7+/7Scxd5vu68g422GvWiY7IWcPI| &
|bpKhTD+LJHjBkEkqC2hXwCFvO/bjva5+XMtuLi+1Ic7PwSSI0s3WEpKcrHMcCbSLA9owbi4f| &
|/dkieYXxDtpZpAY376GEPgegHbtecBY6H7NO8jyNoGgL+Sgr2TRbTpZAjtClqaQ9YzBrkO3M| &
|OVRFDCBe8XoBWhb1MXIpoMHumccAEC8e5tx5MN6Duh+s/u0RSDsprfKgjwr2Iwg7btFWjpvf| &
|td2M0955hbH24gaRRZgsG/bVLd3mvJyLMaBANLTsS8wCm222tsIlm2zg5IU5U4XLx45eF23r| &
|dgVKQqqu2twIEavZsFO1vkV4ETK7YBGWev513zhgPdSGXVkFWvtC+yRiCnd1ksRUz+Ykg6VQ| &
|7GSCwY7G5Eg57oVGCO1hDfIKJcpz/bEDl4uwPu1n1RaASnH0B/YiL+19EH4QlIRRNsV3ZfUQ| &
|J/Iek13unKv5FBze/px9hGJGlvElVc2rMLhOhuemcj6/NM1PGqg9d9qsVdyM5jqdaF0792Sj| &
|ZQ3Q64y+S/Dn/xM6T4GiQbo0tqbZqC3LUSa9skKPvn9MERMUnW7nqVwCpys0jdZHFKjYgu9k| &
|lJyfDdM2WplQ5qOQ4Djq6o9ePr51Pgkv+UBHuFBWSqa5ktKgUEBLRwZdyNAkDYgvn8ZSRHVY| &
|D8mtrBMCWYdpNHWMBo3Z9sitoqMK98LOKvnAWD9oRZwm0CPXzjfdGD/8caSPQw5/j5kEUvCS| &
|M6mrqX09Kh+7nRRKoY+XnyjG0Uj4uBJt0hQ3YaDPD77GRYaRqvi+V09iSaZ+hG1HIG0Ow7qK| &
|XEBw6d3JfWUH1Osc9/i1DkYLslIYcnplu/lc/4G+VSrjISeKrGZYVHlPul073Z3BvcPolk7h| &
|ij8+jUwAcwrl+kaeHFq+0wmvuqlP1hatfrdwLpsOQ8JqnrRwCXNTYZRU6sAos91VeVgbDCsm| &
|Aszd8Pg07jYKrKsfmrCUtY/9CApr4EKIMpqoKXQt3QdU5VTqWDu6VQal3w0sNa250fTOFKn3| &
|D5+EHqTkd2DtYZ/PH4Be56EJ320QnE0XOY71SnFVkg9jb8L1wC4sl8k8udMcLlevgcf3EpcB| &
|cz/xduEPgbNtx8t39FVyxqaA9EsIA2dF2UdvmBqpTV6Y4tLbr9deuxOSEj/t7GLQ/nAzCO4g| &
|I0SlbUJvi01jwlgHWCnjf2zPy6ifCJDgYvwQfKzJHZM4lA4H4vMhG5wWhxTBoO9ia1lg0Ei8| &
|dXJqW+BNmwaHXfS3XFkJyijCJdN5wSvyAgvAib/hDZnGWYE4KFOpty07NB8GZLaGc8x2Jq0g| &
|bSZtGrLQK+7DJ19/wc4FjzJHP/NvJ1vd+tHE2ofVCOjvBAs2EIO2d7yOPSq+QIQqmSam9mkb| &
|QTLeg4aMrFCGAnkM+Ix836fdmAT93tAFkrC2LHUTxTKIIBaA0HH03q313KUiBZ0iUe++PXfZ| &
|pHUcW17JCa2gAuXN88RJEINT02DJ8VysSO4vsjefhKH3CpehH1UCzjsLoU9yzyZr5BwPuyEy| &
|qUm+J4OVOivkjyMFHomV1Iy7J7XuIqyWb0zXyIKACN7b+4l9+s6C3omrAZDJYv68MrKIT/N0| &
|Gvoboph5QILEMtPZMcF78ZlNz8Tg6fXhKnBod/IHvxDkOMoRxzkCYoR+G/bCOQTYOmRevdGe| &
|dvVk/F3J3VfYS0JBO4SYbvyOYJ/Tf5e+35rjXlJ9+l0nigi2796uHdTou+KW7yIBUSyWgGC/| &
|r9yXa0p/nciUv9SELei7l0fKIYOyZByhcxsUdodybfrtRHJNpRgox/YGqVBcI9aFO/RSXaty| &
|jAEiXzjEd0EtYA3C+I8yT1+lsTKftQHThKlAL2Ua/FrmTDS7kGsIwUauL3+jv/fIdc74dROh| &
|+72B/7nyj/LiIDE8+ZOj3N1deO6Ho3jmVN806hmTnIgsk6fhIp0sttlFUyxHH+TK/2DvT5Zd| &
|BbZgQXDOrzCgFc2gBoAA0fftjL4H0Qr4+uLc+7LyvapJZtkrsxrsbXufEwohJIJYa7kjJ5y1| &
|2vgIgCXPBD75njzBJ/opzplvPG++ZLZZk/Kncu8twauycTj87MVDDrHPE+yB5s47b2d5NDx0| &
|iJvGkpV79wBkXvaFRi2yqtd/jON7rPabsi3r+Nh4T1V1vDYC/sVKb2LGx/GXmYy5mpsWW2l2| &
|a3JmZbMOx/X8h+HYgJSpu10Hk+K9IyzyEJ9yDdm+W6566Ap74yB8z2Dr/Y6uz0dXes2wWt6z| &
|YoBjsHaEZNDZGKdFl5wZ+gHGpZXN/oMQVAV6cRjaZSJrayo9fb5tJrI4fjswTHBakjEDv2Yy| &
|NMfjne/fRbtXOf2SIuWPt+bjgsMAJ9yx5ncjNagcCoPUhXCtSu8T+G4rHjNzK8HLwaRr3IX+| &
|pL9HT5h0kR3Bpqr7uEAPBX3b4f1Te6loDeDbCpQ1ZjmfzndiwyCe0cr9k+yiTtejlZhEqNyK| &
|6b88qSvSzj1b7wXzPRYUmfY+gft4Tpc4e7ttqrYA+vPDyV5y9aasmYNNrmNMYcFG/jhWqfhh| &
|A3mgHPWj+6BELypEJTSWWJmQ9CNQ96sY7RaJxTE4dFluANVRAkn02xncwch2sYR63VrwGafh| .
 rv_data = |{ rv_data }| &
|tMKcf7uqvyj9wxvRFk5n/ix5FQk42fOJGHnqNqSQUHRxpHbtbgsUg7R2OIWIuKl0t5VW+Kzn| &
|QmmL8qxbkT1EJ1ejdgO/v35gG5X8FfqvXkV2sXbsVPjgyudip+LskAksAFaRZZWT2Cafrwjb| &
|E1ilurNpWMKcOkYwpy9JzlR0DzloXCp/DuPUwESn51+VPcyPZWsupniFlz8wwwOc/bBATfo8| &
|ECc3n509G1m51ZVsyf2OgKBHqfk1aIH4rNANYAp2ONF9Htinm0XJmcGLV8QXhLj1RI4Q8D9z| &
|SPAH8sHAVChtcdIpbkJd2ckkCKywVVgpC29Nu+GCx85cisbrbJ8EnVFep+tYaS5MFAO+JRck| &
|ESBiWt4h2HjNC9zAnICEggvIPNy6cDHxsxO614B23Jd6+KXsPtjj5p3p8s6TcYO48Tl3CJIe| &
|kFvNnr8eMxUBtjE/xJ166yFFkpqCfR4cL9UW5qwW61ea87PScybtCQPCvjupqt7279mOEh74| &
|cYx0fQJJN1GwWyG+jS6TLdlCmudEOBvhLSpCDF2Yx4xM5JUpjApJJnnf1b84NDAfhmo8hUWP| &
|JdAJBoWumeECbLjbOhdRiGlcPV/5z5NFIz/12QZbNVy8B9MVyu/15MoDN2hsR7+df3rjgz2I| &
|0KyQcMzkNnE4Q/+2wHIpk3m+DFZJtYzTKu0BgRx/xXhkiZKB4RKGUpPxztqWcnjbyVBI+FCt| &
|G33u32yoZdvCFs8W+Q6xogGcnblzUpm7eaUGncVMWXc+qPFD+KrVijyeFa7LiLBlJK89c9dJ| &
|w2QXLcYiX2IsIlVsnVxTnITa1iMJeCvEjljba9kdTdRBl43z46ioetl2B/7c92ERUScqIBIu| &
|N5T3ziraKwgupW+ilJjvu1BrMUqSikv4O4Di5+bjy0ZFyEZTk/jpSJYaczVKz37vJ9I0KJnK| &
|LWO5jy0z4KReznycfMJBh2/6ti7TR2ih/fYrzsFAb0rrVwm8vpbeQkxy9zuZDcgcU06522Tp| &
|m36uhwDxFFK7UbCabTFKVcuI09BBkiBAPRe1US/xT6W5gNaPtUVwDa/t1Zdz+Svyv7aR/vci| &
|rh9WpIMT3XkahmWppNshzuK7z9d+P2dC+nej+7ciAKJxp1kf/fWD52ge/LLJXZO+euBxhCxc| &
|cs0oT3c+rBOoneZtsMqkskQOfM4zIUK+0Zv0a9rS6KqwcwYwwXtTxHgGM7yP53wg4HLNpTB/| &
|HLQUF2r4ROjR++SLkltXVXMqoJdJ1KeviEdJUV45XInxUaSzHi8nQLoBJo3YvZXjfN9PXkCG| &
|TZcgeT3o2pZT+k2Q+1rKHK41TrB3sAch/FPmfHeyQZ9TPWcuqg1ZvrbezkAacD1PCME+wnLV| &
|00O3EzBW7/gAmnjSdNTHDdZSgnxPMwL1m3J7+wIP02mzh+0qQ9b7xKze7Y684HkHyhHLORSF| &
|HAjyPND7YEJD/zTiqy8IetpGYuaoyDwVcwklH82NbVybIh2DVPvshG+pyzbn6UJbt7LYMOCX| &
|atWfO6sG6Ejj6CkeHQht16kfQi3SORTtHEs3qC3WymImWUrnsViGpVCsZPgDc/LlFWqelbh9| &
|3STAKWX7RDL286CfymBUmabjLEAfYxjNNUi2pE9Qt47s9xfzjPiQpFVOogXuEjGzIwe1H06K| &
|2Gxk+40eA7ZQmzEr1K8HhfwuldMOJ7n6bo6JIfuGSvC/tLcuUdBSzS3z+1UgcBTN9ctRaYr+| &
|dAqfRAB6SEtq/DiR3MGzIY412RbSkJKdTEV1Rk8FPRJ8L/N1CEjvIk9XE3oC8urz46UoNQYE| &
|5jNbj5eBcL8AobMGWrdLCIVlM4ev93fpE/CoOwLv7Dym4WUqJ1B9wOfkMBTkfJ3Znjtn5ude| &
|aXnlyYCarEHUynqHDmD91UOqCc6xKby2C53dFwvGxfShCiEdvOt7mCI6mr2ULalh0Ta2vl4G| &
|DlV5TxJJvFYEecfxkMLjqp4AvPA/ay05CgzNNL3UBMOHJaV6GleCCCfq4qEZDByrX0u6DATJ| &
|p5wiEbKqr0NEEbYAc5radiUNyWZogHnqH3LWNPwpJgTv8HL7tS3zgt3g6aMQhX+hEsxboySp| &
|RYkxXbW9II+LD+TcPkcJMpw8tz/l4rCR+gH29ibfVCQVB8VGfs4meMEk6YF9zof5RoxE2uzD| &
|nmOXsQWY41jO+LnWIli3zEsky3D24JHIKdSDMXcFYO3DNE+ZNYVfaYAw6+XL8CqoH0lB/MV3| &
|Fbm3bqGUpoq0pbUSuY2vZ4cVO9tSnfB3iVWGueHvtwvppgII1A7I4ZUFGUaIvKLituw/dVOH| &
|2MNwLw5e1Nhk59dod/Jlz8RpPoDjskcxMkG5SpOX3YNmfl/fESEyQPJcwVzuCcrvmFWN4uKS| &
|aCbR1Ee/X63hsjW5T66qsXGo4faVyKgy4m66NNtL2oOwrOO2F2+rbr3pZQOMYbg2stqU05Po| &
|3dnjq0xtfe7P7V07DdN6SHgVFSi7QvBlbtmPPYbpx+TNR+rev3uMSK7pSRmJB78eUvEaXYoM| &
|zjnAIETuOF0+I6ygs/lEFMtO6PnfPbY+TupSbBKyz9QCTv73K7WiF07PLvpPnP8kpmgyBtA4| &
|vim/oUYieGxS46jOuPL6aGGfHNLrIucmvPdpwN3X+PRlHFEWROOh8ce4pl2m91F2Vc7S9RuU| &
|u3+x+bpbdBiG6SXXRlTwhPg6H5xGtmJO6ho6peNH5w7ddGIuw0HWUylH9Uza6Zwp/HUSGfcS| &
|QSdF47kT0HrOUzF+nbVTITh3Oyo5Q/JwHQVOE+0UJd6txNQxrFB5zaygLI4e+tKsckeps5Jv| &
|3Xhu4t/v+COcSQFmOtY6H3x9oJYkP3buBs5IIFNjIKgJ/1B7TMilKU44THo+VvKlJuudIMEL| &
|hh5M8v2YnWJf/fqNlaH4AqPi/78ejBJqZkvi1PKTvJx/msurz77p/9JG82mJqRXzhLR5O8XF| &
|z3SSUV/lkEAWWJ3bDZ5McgivuPleh5DF7rC5V2GSFJaq1T0MDuWnrBGgJ9bsmHiXIaxIJlGv| &
|9mY6p/GqnMnzyn/EDHAR7235+rbj2vd2HnoNwsZ3QHKjnMexwbHfcCIc7ho5PaHhTnt7Lt7k| &
|K25N1C0X/3tHeqhID1jJF/ME6PV2BhwdyR47aNFBrtunsRU/KKMAnfoBIp2oeTcEDUdOe789| &
|kzj0x3DJ1/HV0drWmjDhS42/yP7KFmBZE3dsatV+DuYoPz4rp4rbzuK0PZ8FZDSNGCFC4kSR| &
|XSYFfHvLFyafH6rFw7eMRVlA11huQHUIQSrwW2C1BlPo/ft//P9EBfR/c1HMPzHQnxjoTwz0| &
|Jwb6EwP9iYH+xEB/YqA/MdCfGOhPDPQnBvoTA/2Jgf7EQH9ioD8x0J8Y6E8M9CcG+hMD/YmB| &
|/sRAf2KgPzHQnxjoTwz0Jwb6EwP9iYH+xEB/YqA/MdCfGOhPDPT/j2Kg/y+EQH8+h38+h38+| &
|h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+| &
|h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+h38+| &
|h38+h/+7fA6da0in/v+qknF8MJxb67YgxSUbrJ34j6TSOsp1mqx5iczF9TxN3fnlpIZLyijr| &
|Wu/Ls9/8t8vVIL04SXq/HGZm704FZC4810MzwF9IcHUiZxESJhCxhE8ODMLjKF8YStQVUhbI| &
|FruYrhLKNtqYr3aggCJhgOkfD0wdZCLCBEBshMx7BEOih3arzqEdUgcfYTlcASoh37jo262o| &
|CTX3vdkZ6COFPRjuw6eEDvvIiL3+FMGCB7slXV+A2Krc28IwC46XRe3oBzXiob0F1pq86+vj| &
|v+7Eg2f6fKEJfswk6O9mji5PMs6l+1XeegmCH7VulZGmbuA2HpDV8l+8srRCbys7Ul8PIe2d| &
|VpAsB5NYh7JdozyW+35qgS/kZ2uB3prL83uIHQjRmlQQfJdVuVQAfO+Skp/9BA3ttOIHVVqW| &
|5NYQn5b9XCH7GeNvHVa3q2nXJwpxjUO5qB0E9q0c/Kisyo9a0qj3I1KwWyD4dGqycGLcnXp0| &
|FR+aZYflZpWM3Oe1HZLWvEFG3tLffspdYolmeQnttuLv16HCUUigedBREplkkPECVK7lZSFF| &
|VG6mMGqF+qv/jbnpjftrzE76ixHq/QlXMTSysZP3G7RWUj+9QVHKM4+bkSmgdiRyeXdYCCh/| &
|W/XhEEddm53XIwZpD8VmcC/6RrtvGuce+LNKtaM5MB+wardshCzLkMZNK6ojOvWCpdMaTtbp| &
|ahKgOesIZRgXd+IgS2AvUIZv2c8WZk/etgjMOrQsnYxURg8NXf+6pkd/mfFyWSR8gef6IW2U| &
|AWukCWVyBl6/48dDsD3rlEzo+UkJN7p+/AQqGIK9P/liWZENG1FZJ9v7Rw5OHyWp6hhpOQgp| &
|DDL0mPxKM3GidiKBOwUbH87BPGz0IDYbGUzybOD0a8VLBGlQ7jUqITpkSCv4EQ118WdiMVGC| &
|WscyIRCfTucX8Au9drf2AVY6luHrwUDQEN/Kp5KqxaiC9/UwzvdSisdLG7CaueeSRd0JqZR+| &
|7U6Ek3GFAg/4GfsCfnIQfumGXtHAVVpNfPAwR7y8W3yziuYx/VH30FsTHrJ3umPJqkk6eh+m| &
|YK1wfphKKrz3lw4GP/iptQ7BykNibUtkLMDknZYnlC/zW/xi+wS76wFC5t3hn+jgz6eAr0vE| &
|Th8KjEKtlYxKZOSHr1+V0Jf2w0U5xylca+V5riIqIGJlfWEkAb4q6d2iPWN4k7sd4g2dZzQK| &
|BF+FX1xYxgpRKCvTZ675cb/bsT7l7Q8I/aZ9iEumzLsuLgKq5fAFfd6+G7vrkY/O1/ALXmH8| &
|1uukzVmNHDjJb53wlzKXj7wtv12z/iOd3B5l1G+lPzJCz51/v60AAqadwQTzMqp9nXbp80q2| &
|T2Zr/CYw9ofE5E6w1kVJWnW2+I8w9EK+vSdhRba3bLUU0Qqf3L7zXvi9fyEGeCXDsBZtU1w5| &
|v/hsKqSLk38PnvmoYJXk3AH1HjTikHhM+v+eSuA2Q7H+33W7ffWvfwtcFv9Z4DJ+G1rQom2X| &
|WdX/vMAlWvzYWO55Tvrw+HCOktBI7Htbf3EtpGp8WsynBiLtHMDX/X2YsXGl/2OBS/X9zhkU| &
|QkHaPRB5+TZhftxtRCGsnyJlvo/rFoajuUQZmOcFa+BkkW1HDSjkk4i2HSR97fPeaBilUOzz| &
|fuFnGpEthrVwj6XLdpHb54bHFlOf/F6CmV2OA29+aOR+HSAVbSvWnjAJrEO+kzvx71aDlTAV| &
|+0IJgsyTV4szd0l+NgSq8+f1u/lhYZJWwbLcd4zc4Ivk3w8ayvdjvn0QLVHygIHePlCyb6Oj| &
|tbHXl1ZH6NCgfd+PgkgWDAPn4cCenVI7iYRqhtEP4SshaiycMwv6PQKhb9HvPgQlFq8B5BjI| &
|eIih4vt1HjC6osbx40I6wMhELjFRJn7hlIIUb7fgr7HzHF9X7v4UB94cOyeAyAFet9oKxI4v| &
|OAB+IBx8t3MJVph5kMf4HigoxCFDs28+TbbfvzWpls4AKdhBT0Ju2Te5uOm6/fZ1I9FiR90X| &
|0hihAF47AO7gtR0yDSlMWslHn9O3faSD2JI1ryU5EosFxJPj+xOY8OdO6u7670KXPg/t0ZFZ| &
|Ln3w3J2eI79RwCQ/w/KqFmiUz+MwoNIvfR31XtS/y0i7i6HLu0xPTSoRtmnoMPo+n8jahDhA| &
|hPPd51rOu+nLHPrUhmBgXfRS2s4IRo4mfI/h6aOIQ9/FNuREmjeGPt9lvaJYZG0bquFoqvI6| &
|SgciGZBvenHykTrSlV+NT7pTgOvEORzFebawyI870mPPSmFr97L1H6LNb+RWs9RezyiCE2mt| &
|mai/0MxzmK2LfrvnsOlaskD6VlPzSUaue+48/E+Kru1auETy6JiwchUbM/qb9MGrap/QFE+R| &
|GRmMUyTe0QfvdjJNn/lAybY+m7F5vjWUD2oWiMzkIU59mD5V8b+LLrblN8jIcJzRFxFgnxA3| &
|/cB6MNjvqNnTVqgj7FfR2Wmz+PdV4flZUSp9PS/VD+B08xXvgxQUwbTQ4R5hnTF9ABi3CTsu| &
|HPu3XiiMi8HAC35DmX9qa8YzsGg+CP4QYJEXcEb6VStcd+AESC9KetupZYz/9NiBKOCtRTGS| &
|NdcqnMoBwwkPnRY4v2Iqn364p1Lx9vyM0+RMLJ9Mjs04JaTsandSKVASHx3UiuK2n2rwXgvO| &
|szrNOHGZfYIxddt/iwaMEDsKVvWfbpt9EwRNOvPEcxMzTYz0ATnPedinfAKDxW7PzDdxCc7G| &
|dYWUVCjuGwwn3eJfpyLhwu2Ib6KWn8MxgwB+f6FfTIRjV1V0DzPrDqa2pei7xZwI4LWOuDeN| &
|5L0zic+glKDfuju5YPV2lg+98M5FnkkOt8Y9swz0YKjNBL15WWG7mf1arL9NqStVTf7OCgK2| &
|Nbf6C7atsgEHstWfsE4w3qESWz/QyJ1Pj9Oq6adL6qu94dlnHihB1MpsH25CmweiafmnGb80| &
|YxY38MxyhM3TckvXAKQ1rgH1nAiJKOaR7GPgH8T/EtsWcbbCITDbzJvN1UG12vMSPoPp3DzB| &
|Q5KGKJsdNYAvrqz19SyxU3xe+OL4R7x6m2l8ZqiDpHOswZj4Tm5nVuZsg3llr4eITtEsec7T| &
|r/BKI/Cshr+CX1cD/3Y09M/OVMPnufcDVTphEvlz+BriGbXWyla2WP9r6A1tbt9A+knB9eMj| &
|l2907sFCycmXFYH8LAQClr5bvww46ZzMrUgjwc8GUuuotTV1urQLnIY0k99/hefQhtXjbH2B| &
|khIcoYDODt/AIEgbTJs+4Em1AJKBFD131/clweClNeBngVZQJhaqBTlBCnYak71sanj+o0Rt| &
|UT9QldH1n1RJUcpYLdem9to7J2NrXg28HX+eQuPM7tw1l3SPGxnWycJfzPO1xht2P3kXbLhD| &
|wcKsn34iaFzs+6XMbzp4JpWJ15+FfCLElGbsDfzTwLIRj6G/MhbmsfRRPNOnF4wj76Aa8Ozf| &
|LSAW+rzIfPG2UaHTE/n/uQPErXlCi1iroTWnPWzupUnA4XLsM5Fdb0iRX+VW7JgrloNCY86+| &
|h0xm1RBy55KmZtdAZP1rJTNYFA3HeTDdgpJTqJcKv3WNKDWlBPr4RUiH6zGnyC8ig7EXv/Cf| &
|nL2YReR4ObAYq1YujuseJin96pix/23HBlZWKQ3HdoiHaBLNi6/awgF0Z1U2wlGR038b8xyJ| &
|/q38MYaarLXZQePbNqrINx5Xiab3NY+j/0JbRBhWwlGJkZ/m1eXD7l06LwLrb8rSJ7U8L8JC| &
|MjvhIKGpxTTeWk08YdLk+/Q12twYoAHHII2MxWsbv62M6l9NnLS5DdpcmVCJ0ycaQHNFahT2| &
|1V2o3gjs15ZsRbpkT9wGnV7fudi6CfFLPUMpHMkNjcpqNAqvq6FcBGuOdtW4nugWyhBldcBP| &
|2dh869+PnUtUdBfHguTkjqi1ToC20W4O7hZHx/DbRdnFcTiL6aKyJx/h9fYUT2fNT6j8k2TH| &
|dlwBrcKaPMvN/MnVbt1VzJxpfMbL9b/VJvFIesoWs5iRxEg4SRcSMwn/1vy0iSyon7Mc8xHR| &
|FFbnWrOlD8DXxzOxM2jj7YhO/y/Adz7wsZlZyqjbZygUpegrnzAPzupU0+/SF/iZAxXOfBrJ| &
|Z9ZmX/8/HgNwcq+IdDPCE7N7bTOf/BP8HO5bbSlrP7HdRWrw89iMrJTk4xVQHaSfZydx5tlT| &
|Xv/soUY+IJLcOF5bD70geNUOOPJdVZvcOZChJJXuYMPV2ZPVP9n3E7zI6f66gZso+faUCnuQ| &
|+f9Uo5+X8OL0f6RyXwFUVpwpxqjFJunUotVhOUEoI7ixeKIPlDKl7DOczwHxOiw1Rr3/yP+x| &
|iCOuluVBFdZis2rvGSNpyQAIi3Yqs6lLnV1FX1uW/buElRBFwTGSJ+XaQwx/Akx1/64X6hbL| &
|4GzRknC+w7D5NpPmHT30NBrmMxg8QPnY24tWq1NSuMye+pltRs+Sw2fS2XnHYNOMRytze4Kl| &
|HQfj7CL3bMf+rKTm7Syb+nr52o36lMLY/QEwFSnJvEpctOCcNcwgM8hZp64fLuerTgaHb1vi| &
|7TS/rKy7PgY3IDOtNRwKX9ttjt8eFevMPYMIUq0UCCLkX+WruzycfDir+boeyxH2ltljk7iO| &
|ondI6L7Cs6xwGTeoP8HNpXsx8rbAXwZG6c8n5dQhH+GYB3juc8ibzaBoj6GV1Gg6O73fodUI| &
|zLu1KgQWq7xyzIBc7Ic8Sf/tk1iL+aHOScTWC+ZUzzQ0c0YSIH4KtjjkPKH6+oqobqvF1oXf| &
|4kNmFdtkpcQt6rjuo/eey/XnttNp42p2um2l2izcSbQn1PrqZydrawGL7hiSp+oi9/x8BgwX| &
|+JbtnFRsP691pbqoiUt6EyQtCg9dkLWaTx7MIG0z5ClJAZ1tQjX2EUVJhLKA6UXCEAmnnWiw| &
|JH1iC66rUBcS8cuJjRU+eU5kTSuWPI7uWOqSOGMWSImJeU98OuCfdCnT0/GOO4sHaE/ef+IV| &
|phHF8nnd6lKaRLAzmc/sRgcDftd++uT++PsE/n/7dMblhCretvaIaCL4OJs0NSFjqwJwoY0n| &
|CS+xEae6ublDEk6xImrbnoxJ5LwmWQ+/9X51io3zL1W+rDxOr1/DuZLFW07MVQ3jSp5ocTEz| &
|ARdvS7zoNI3ls3KncBKrVd79NrU1gp9UBq2irpWIGOlafMcYHUcmAh5nSs4/NfVKkLh8EGvL| &
|CGPT5Aco6OQLgVwqJP8j6rhJrs3EW1c2TWezaAqnReSN3S78nP9SM93N2qiVVjAundVIAg/p| &
|Hkl19zHRTZcauKEwfiWVqVU0Gd39GhhMDmHjxtKK+maniVARfvIDun/gaYF30hEXEvzZ5f1Y| &
|psNzeMz5zbv8r2+VgcZgWUpJ8sI3Tg0sl52CClJEpiWmvwMIs6MBv0CvLSmRmCSW8u7PkvcG| &
|23uZNhngGaLXMcZRSUucuAPir05ST5ukvYGxPHXAmcUuOtraZ4NvU3i6KXPL76nxTwpp0wtW| &
|CzSIhuFkPY5brrGiyHwTVrcsXJoHegZ5GFOWZLuwIcjrLS/3hgaHVGFZXsBuZJbq9Upt4t/k| &
|dR/iYhtirQ+ltFgBOiOE93nqwGAtxYNAWsDOX8F725ovpchv272+Y1f8MsottzBFSYbk5pOD| &
|UloWfl0cVFBkf7hMCqA1jovIXiztVbvZl3zv+JsQAC1SSUFqK3PyY7bMvi37yb43J2IOv+0d| &
|SDaRuWc2Dsanjq4YQ35L5VfAgcbsWctzDyFZajrLu6NkwhngjZuzWJiTeu5TMa0nOQyXWzVB| &
|PE8+JRCEnsT4n76kSY2nErcZx9XljKI0/HKzvTOQOclN38RqennggXuo73c7LeByCi19JdlC| &
|bPn5c185E40OETg4Jf+0dxLm9Iwlnl5vGDHpcf5Q0fxrISJWzjve73kNAxWN5CGD+0ThoF+t| &
|ecDJsfnQj56lgL/WHeO1PVVF8KnfbzHaV+HmbzHaVv7Wwda8NU8zaQsdFHImBeipTthoEIuA| &
|uiDSlqn5tZ31g2JIHSp40ek3nXlolGKEV+9YvkM0mzeL+xNyKPiklQndiaFxIUp25fYWAch7| &
|pYUYby+7y9KfYSXJGyHupoRhNm5mAkTHhvfp10NU/UUBQbSywWJfiixZYIjUdJcmAza5dnML| &
|iQx4IQcrnJNwPxF3pYJ1k4jftvnhqg6lPSiGIYjN1AMihQm03PRRJKFvQcBvaG4orJCGoDYl| &
|GMc6ooFS4PW8wJ7C+nbTvcQpLFsp+D5M5DxEfHkqxbFR+H1SWLvl+CCkZVrfB+qlO/H8VX5c| &
|U2HbWvaD8e4n0y4v212KXHwlSEDZXG42vRz7G0qDauV4T13mfY4x7YchvmHbU+Z900IuGGN6| &
|Xg8rjn7/WZ49UyOk413g1t6y4DjRAwD+2VLEF647WR9RbXIFHOuWrBxJvMT31vAgbmOhbKXO| &
|reK/V2WSqbOm+Hn+bb4UkfwB32HCJlbSKvbIlGSUpvtlPbBywl+KcqgJZndJX1EBobd5UkEh| &
|lit9K4Kod24+N05bnwtUql8Vsvi9Adg1TCNH/OQh0OeIH7ZUJRQe5I9Ou2dcW81WqaPMuaFB| &
|0exHq4RFxv6Bk0Vi7uCPKLBXu+vYGtO2QkpAKrSD0Th3lPedVO8BYpvxqlzJJcU9ATOoKu9a| &
|rBkXuFG0+IpezjtatZewBkKs/ut3scvSlplYhfYAvlz7loNO/Tqot/XvxFo9K7H2Xo2fDq2g| &
|4DpWgqveptQR4DolWFUUtp6JOQX9JQkRZA7vx0mty9MD9vL47YSpHb7td4jKGpHYxjxXYi1X| &
|GkWde/T+z2J5HD9UJz8/5dsWmYtXBLIWk/7Zxk5aiaEsL+RUFQIiHWHpmrKsMi51iO+xNK8+| &
|Lt3zxFzzh6i5SZ3YYHd/UYRDgqh5kGxt9eCPhRV3BBs60xCQOMI85WIKAIXbHUpWyA/6RTs6| &
|Z5lI02viyxCh2TpGI1LhXWfM483HcRA8s/HhFpedoX787hzcGkAvfvZOSr63/AAw/16TFfdu| &
|b7jmCy6K368o0oQmXEd0iVYdqhZ5iNt/iJZfMtW2IyJ7yROn8w/KfmhjxZPm9ELiSyIBH3FN| &
|pd1CnzXjPVEKVFUNwabXmZVg7wup/GLqrtHTi11hC41S+VB+0REeaDTd73zMBtCIaCdJHQ/q| &
|gSGrbi3exmi/Ni07BGjGqpbP3/UPLEl/qEyOpzmY4WP8I1LMmOeeO93z/1hC8oZV23iYHq/Q| &
|GMJFwEy9jv2lhM28+LQH+ze6QIzCj8bwzX9cSfyIpzx1zUgjbHylHR5iYPdBYY/Nqa+h6C0u| &
|celzdtwEu2GATDU/LC1mlqZdYlTbDp4YZCZZ6i0Ldv/rWdI0eON/HOST9XXgu4egbDdKyk/W| &
|0suT1XTjRDmFEgyAvdToP4MNNoXUqdgTjv+53ZTw8NEaM4KXl9mqwBDlKZ1Nut0Wm5n7t3RW| &
|p5jH/9m+4d4KAal1fdZq9XYuNYNCjWfnUtuDjBYpnnIx8PjA/zvCP9LaGPIl4ZXLLM/j0VrM| &
|NvKT5GNRbLL4CpM/YyZyz8ihCjy71mAP+uRKlyQZHRvP3qnZ33eESg+1+VpSaw3sYiRWj1OO| &
|s+gNkY2v26JZptFJXTSjojwA9svTUutcNm6LdNMsAaeTtlh3LRr3G+ip0rgxrDH+sGKRHuLS| &
|ifsgOJGTx23sTBbHx0eYDsEVIy4oA/8WIYEm1TreLn663rEE0Ph99lYEne0FH6xfkXS0XwMk| &
|kayRPjEJUx5+zsWii2J+kjP/q8Wb9E/83mBgain26OjUJWeEhJCFSJWlvOmW5uydVZlditfX| &
|vMCiNDc02yjTYUXNrMLihglW8UN477Pq6+87LzgvAcn84Il4XJXki/CpvS/RWlkz7D4wZ6+q| &
|2n/OnjmqkbzFwwNdA/9JPSrDPeG074HtvVfH0d+CTjoXyyCAg7xpw5HmdD4c53wK7fQ62h92| &
|KGxVhxjhE/qdXc1RvGLebzf/+acN+H6sDTWe1lU56SY1ubWch/u3AA9C4oJCmFItWB+2psIB| &
|WZXK9/eRvV6sg15oy9iyUv/GK6lYe4Yw9L5/1WJxzZGHcB67r0azyVWrPGj7HTWVwiCDYDv9| &
|z65Zz561KMl+Nu9/scxllT7Pjcy+9Vlko2feKVWjzxa5cArDsT1cVW5d8+gyMO7HAq6fI02c| &
|FpKa8hDmzLJcapckGQ15hsfSqrocfzqrgFdv6qFnuzSxeqgTH0WMf/rbDx37ARoGkX/3gvoA| &
|r+KQ7VXd5o8ViYFo/7LBAYnqHew+/j797OjHwCqcNE595uj7QKfk4N//x5O3rUwx1NeL/Y06| &
|1rx1CQh4uutUxkA1qcKTZjXDpJivvWTo6xXIKKFhi8CDckwgF9TOJ2J2q4BDAbqSbIJ0Veri| &
|g5lCCRzRigEYIe1Nqt3e3aK933L5DV6OjMINhfhzk51hYGVXiHpcV3Yz4rnD/LL517xpUvts| &
|G37TMH+/lEEjFSxkAIQR3Iec5donZP7ndgN7n49yjV95Pnq5zPHp9zvLyrmI15YuVb0/xXim| &
|1HhLkELIiP1haxJA6t/ATI4V7Z+iqj8gv+HIrfdcNA22YW32NPaZfEAXmZSX4ILPJXltyVUq| &
|uBTar2ClKuy8YN/2qHUUAKJZwRG51gOUR/rp8MpedFKifz5YF5xzCTKg3yL0Hu0Ppc9HAm4G| &
|Tk3Ra4DGCVMOYZcP3DAGvIviGIg/JwTGCLfcpabDU718WX4BFZwKf6XDDAE/399YTo+w7vMS| &
|KUvlk8+le35pr+hP/bRvFMHjrcbf4NkBVx5iX/sq0mPdkysrFC+4vZi+pw7tWhK20OiWdL24| &
|RF+k9zjVG27OsXzQZj1N7iHmO7xe7d56PSUrBjxCdzh0vhqQTWcJRLNEPQRWRzbJKZ/MsLzR| &
|F45hDgj9x3vqMIoAfatxH4XYDuHSGarPuIB7TNIOLAM3S1gwKU8nOfRorjJb6VxfT9wmkVj1| &
|lAzR1xbHiKzHMiKrsYDI+8Yg316Tnvr6lRfJqA2l/LpJtiUxgIjQ8/ZazNG8OmtE18pX//5W| &
|RGTMzGwZw//SHuuTkN4LVeg6PaFQA5ricVyXUGxDfPA04E9Xqvod2Yi3HFRb59CpcqGTGpBK| &
|8InP0cgr6jDzVabnQHVPYq/b8FLhNJW9rLzvvtCWsze0cDh/B+DBzCVkGvFQVYW3TNdZ/cjA| &
|RAqVf2QBhS+fpCYSu9sgmi6V6l/9rshU/e6x+PmliQeFfvK9XFy95W/AUD1EOYhpMeY4iY/t| &
|8tUduUriXUPsB3cHFHWX5Z8Mt5A5OoVPYxvQKkr7wsSIvFbfG3mSyga+Fy4fAKpFpMUuWqjy| &
|E/UX8x5HixFaUx1T6cM/3Z1AaOfGEv/Knn6vtth7b2nbSITVbjR/vX/Ek0WVuPYcCQZUfubr| &
|LpBhlZmet4vPBw8F8pPj1MloFO4TtkWteYfS6WIk23yy4pfELajMEQejKCIYduLw7+7lbwdj| &
|wORFMiPOHqMwKPtPq3vu736AMRY5U9gTqxJWvG/OvwUfxTTCs4kxelh0ftuCi5fgC1rz1jwK| &
|DHl/+xpAnZricROr8ELJIrS6Ic7/t/akNCrIi7necYGx7xTBWn5RdHjb6w4zf1ESSPIR6UYO| &
|g9HefTD86kISB1a/zT9viMI1Qcypsw0uQzhv+CYmW/cMs3xVMXHA6Nx+X5/B1q/PfcfnhNLi| &
|OezXhxXq5hB/kpcvTe0D7adr96Nz3de5h3lTISqR5zV2bNpob7Zu9gFH6+1NkVs3Fxj8+/kW| &
|w5y8FLHLW0Zcj08+Ta+nNQ8RK+AzjDtZUsTJPiJJvYdsQ9OguC34cIf3Pswk4hwI2+eSP19G| &
|a8Rq8e9rSjg60DuLEpAkznTWOdqC7i0gYxX7bXnL63aQbmKu0YnSrN9moguL7rO94+2nDfjV| &
|/jO2GOvepjNbiztX/4q6OsCHtS/Bi2XFuX7wKHA7geQZqE+v9XdA3mnFtL4btdarlhlWfskb| &
|mfEgY3pBU4QP3Vosxg2KB9OAxqdQcHv+zh8PIpy4buUXQPp7WLfSL3xiMh3evv2J3vMVXCJd| &
|R9XgYaswC2q6s34O7aiFafvO3Q/24rHLIKsIFWZlfd8PN1hFAQVqHv7/KCb/ad/n58kTJVKJ| &
|svfvcmCzUJVxnIhusd6/zVwW+f2/tYEv8ex5xsvsc9tx0Xf6S+w70mf67hVwu7gpLzjr4zC/| &
|Zy2LI7Hy7hEKVv4FeSzoj4yPOTtH1RD+jtQL+NxNZ5Wd2bKiB/0YuzchZpHU63M+WMQEWe3Z| &
|6HNxnK18d2muSsY79PTb+UhEMXzumAz78Vqc24TNMYH/PKC4lfnCSaOw4CnCh90ZE9UZvNLS| &
|G+RggdueSfMtBTL3BGSjNx4t22FG9HRNb68dy3IBiSVXvsC50y49BPkrVORfeeAVfE8iGdRn| &
|1NtKCTo0GgxI9u0zdvm0Ybmq9OTV+edc8vE7o+HCEGYLlW+/2g4eELZ4oV/wVX6bF1QeaBhC| &
|w4faPiTd3wik1MaPyqCShoL0dYJZW0A/7X+TMvP/1ONIW9I32f91VQ726ntMyKO8PGPexRtb| &
|NC3KaV2mbjs0k4RD9K5kZCxR0a1AvxeL/3lV97NEOeK5ZgjORvr5XavxEZAqkL+hVJCA3gZW| &
|Pl5H7YOasGGDHuaf7OD8YD6o29WzRAnie7p+eBR6WLqXZZImJlAkhnzoGiYKw0CBtUBb9CZO| &
|GuTyGiSTHOIKkcxyA1swOjXTdJsQHYO6l0a+DDAyS7AJfbp521hcgDGFpcuBEfrnbcC5CLjf| &
|H76WmAQZny0/cUIzJTAzvBD9t8lzqkr0Pp7ZdhdYehwjFmIq0RY/aB2Wo6fu8hDzDqHJ4CAf| &
|8AOIXzjdS/XA2AzcwQdqoRCJvQuoMI2tIF8ZNJDhHWotBEEHe0NxXmL8rd8SC1GnDqnLeMYM| &
|iRFh8FkY4LNq42dxZA+K73+sdwah6FRB/BQyYWDHjVXJgdnoX5Xjt7p9oIdhnNQs+gsGTdtN| &
|4gZSolfmru3AQUDLu9FWPYnkO8+n+4qj2bTJ6joxlHyXc8nETTYky2d5VVR2zAITo/GQ0J/e| &
|g0GfvLUsO/1fj93imow7YJZB9G/5SXG+/MsjO3MsNGiEkponlMYAXyf7wfby+OfJ+hqwNpDz| &
|F3LQtATlFEh8iQL6pyEaWz8GDRcgegNfKWkDw21DDRD7uD3kidKN5M3IESf3QG3wa8ltjVGK| &
|zdRQDR5puLeoWzqSBPoZAUMQnEaIRici0Kb5wzywKGZXFIlifUP5++laRTJp3/kRbyO9J5qx| &
|oxgE10t0E0+dJNUvGVQFNRcIXQ6J37d50dBIBkS66WqYVWCO/Llua0oqKH5OAGkbIOxgp1EF| &
|r4clj6zcv35NDnOuJNK5hg+fpZBl9G4W/oA/pCeHjgYD85Vv1td/SSrRWWZ2HTosgBuIcT4q| &
|Wp9bKHvBRHErEXk7W5nRr4baDtNE7lGs0eMNP53QW39aMmFfFDi7CGvQ9SMI4A/Eia9glSYV| &
|GmmhrwO2iPKGXA2EGukuVq5Jik+hfEB2sOB4k2e4vu6wuQtuec0POH8DpEXRsUf8u7WtSpQx| &
|/OAtQ/zni3iXQPD++XD4E3oQvj6AzDw/M23lXF3ukZpfsmcIRs18MalSsNnBMqDx3/VTnZV/| &
|z+18xpPQmWZG8UE2quxLkqayeroZs8UxpJrfDw2cs7l7GbvaDclRzM6XNxa17uxDg0EA/bfw| &
|69jQ2XeEqYAreVtedfbqmVwQ2qjdP5/rvytmLUvO6x/aXhsqeVKFbkbiKatGs8aW+pNsYu4q| &
|AG9H0ghdnyfEshNThHkdFswx1kjHgrYQbUe3y0JZJeivnbfbVc/XI9O5m/M9I55RRi8kp5ZQ| &
|C9+zGmBLrRQtCuWiX9Bz1uZ/7qYfj+TlOI/gnTbqN7pYZJI6PRzY6rnzKPTPSNh/vW+XZqvm| &
|swhj7B5T0dII0BQYeiXVyyCenQ4kKInFh3ujP/RwzT5t/eGIk/lDg8fqzgw0YMK3sQUzG1k7| &
|+mi6MYW8aUNxFfXJ0gLdWNufuOBrj1DOZ6T6X+tn0XMW46Kwzozky4n9d7sSfj5Fp8GtKiLT| &
|91MsHUx33nFZ9NyM61I6b7CLA8mEK9z5fff+mliYn0q9O6HZFJpRNIew89TVgFd6mp9NSbAV| &
|lnpwmiJQiNI7VOOb3b+vUcC9EvXvpmSALn+Vmo/dJmenp5G4SXh8acGT9rOVombge2FUyU8A| &
|WqFZJv3LZR0Q2QyJ05BW6tR7kFrL5CIlRNfin71xcsLODbe/QdpchvFkQUWH5zx9ZRUJbQc1| &
|JoLzOfpsdafzjjYRm55KrdMj+KbHK//hJxHY1tbkKEDYyB7NKvVUF5JlWs6UKGE8B03/q/zD| &
|4yaZRTHZ6WylG2SP758P/3UkRn66nN+FIrIz2YrXMHCC1wXQB5HOJ0JsnR8WPxDTgXhQ79Im| &
|jPXgRJ/p+SKS65kuikXw9TzUH/54Iww7Dc/wZK2vuZ7fF4vE8IlHAdY4H0t3Eu8ejW2jVVqm| &
|LoaQDhbtjouq7NKi6lkr6xLfZql0zVMlO0tOmjQya8nSysz9YypNDW6EuQJ89lnSMieE3yvu| &
|F/x2Xxn7z7oXnAYJFn3mv6vDntRJlJsspLX9HBDpaEbNUpZFtTazvxfya6EeooaAcGO0jZX0| &
|wqIH06gR/Z+LnE/S6FRMe6MfF2puuEfM2ng9FFQu/JLKgjsv4iLv4Fgcw1armJmK20AIgZnx| &
|C+3fqkFh/DaimLiDSttpC6HkiZ18/NYUj1vOziZar7UWZLAZq6+XNGyZFhk2cWWGfisolJm1| &
|XwBEE1J9Yw67K1Kavl8iajUfKX5fITOCnc1l2V2VD+XQjc7hwskn9KWodqEXtthmDZr02TN5| &
|dePVi1jmAUdvCE1V++j0tn54UAQ/KpSOHyW+kaxR37eNq+/aqkjjyvBQan4dMfpGlZd5zxnY| &
|D51WC2Ei8XJthBSAHHZ/Q7n0eHflTyHe82IswpoUVDciFHubt4qxUaP6j6JE9y4+2G748ybr| &
|G/7qJqd6Hybmvg69VTMHAsyv8Y/Omehnlv/rdOMAtJUW/vrDl/73/boUxq7lCClo7N0vkGaT| &
|0c/qydM4I3EbyHDMUuLvgfu1WQVUq4QNgm+B/5QBZhu4xc9jHlI5C5lo99z4fqO8MYQTkizo| &
|v1tYx07mcdlhg6R36zWSxrf0s3fOYxx5Aty810OD4oKe/cDBtM2KIWJPyh2GFxd6m2cr8y47| &
|6tNy+P2b/FuNJ240DrElh+iHOpi/6xOCeC/pDgKw2mYRNDOBji7BP5vTT80umorJIp20qizf| &
|gjfJ2ihSMRWRVLyd6xYn7c3EPvOBUCrB1lmbw5tqOiMLoHuxf6cSg/I/2XYOozci/k3vGsvJ| &
|CqW+dYdTOJ9Z0lZxR4rpIb8TO/gdtRIuhXdfqHVuej/Hv9kn6QL3nGp8zgW/mhs4YVKelNSd| &
|YeQG0dd1kcv9uu0ssD2JQUesP5PatioZ4yTcWRiPs0bnx/T2/aseIsHIQPPiquXUdVeG20ap| &
|sYcx1k/9TMRqtLOfo9dGJbCUsIhNIPGn0RCHpd3cMUmw855lg2c33u2jqnpXkAt4DSXrQmyr| &
|v+710n3VGnhOd9HnbSm5rHCkYVnb/HUN0dSsrNUJw9m/3uGao1aygH1Lz05pOXLxMtxFABeb| &
|Skgug21UD/7n7VvPksdn75dPfCdclH4PTq1lLBbaKpEmaVvicKJfE2Ki7NdL9c23OYbjfk+U| &
|auP/k7X3WHIeZrYE93wVLejdkt6Tojc7em9FJz39sL5/bvftjplFR3RFlAoEKIoAEifPUYGZ| &
|AKzFjX1A438Fhzka21mnixN8h4hjSuwDrK77+hQEqRefIlTvpK50ENOIQjmKoZ3aUcvxLkFk| &
|HtD/wsOsUZyTfuQ7Y/9iivJN3cLJq8Yr/WWTr92OsVjZw5DxRUPClTE1JmZlVbG0QKhsuc5N| &
|iaoSfpIB0PInjZG3SyJimH7ejKB+eqj76DL0DOBjrPXf1Z3n6jLGKh1btSK/NY/53/XHSbcP| &
|M6exLvP3euS8AwxLpiebk3O90k2nu5qWqjZCTCHU99GLS0FSKZfhTuyS+B6O4vbOy3DPtZnD| &
|9eLFrdc1N7aGXcpfHRDjTho/DkP005/fHganRxzJpdQYvkh4Mj9IdGeMktoZepr3YtTc8m5U| &
|y/olv061PS1POJX/GEPNpLsJOHN5IqMjrAIsNeby+LTGZHun7m+pYb+GY3sKKehbomC6Of1o| &
|2frGUAUS7cXFozqu9/n9XlS1kOZJrECWuY9o4Rb69aaLokbd75ea2rcw9BTdFmbOvbp8Py8H| &
|p/exYoqWyin5xvVNi1Ut5uH+s+4b6Eo/P4ALoIzQMnS/0Dcf1HDs0KM+9/BLXLQUB5dPIwF9| &
|WsjroHt9LW7SopHfUZPcrg7HrUOESb7Ge/syJChIMgqU2ZDVJZ6iPQ/vq1E7jKvtN+rNAvat| &
|bXx3vq3CmU4TvDBGkG01m4wRpU1ENu8sPn1aBuNg1EQTDKEMoFuYMvcuGd7HbuF7ihNrVlN/| &
|1DJ3bBtJq4K0JWvrZ17+aLNXe8y7Rbpvy+wVczd/D/bQdY3h7Ht9I8D+SA/mCsZIFTFBeSGB| .
 rv_data = |{ rv_data }| &
|R4HR6H8pwSipGttkJ/7uuq2IiqKqNHpunuiHLZPa34CLiWZ3X8s10h15/LIPcNaantGQoD9K| &
|QQ/DNyNjm4NyJVYaRVYc1Y1yXVN0N7Mel/N2Bntb3rPF8dhvtylDvmXfJmPlW5dMqgAYqx6P| &
|D6iLDxNfTpQbSlrs95GYU2pfFmnYvluze1tYca3ADEcaT/drZ6mL11MxMDxqWZ/Ni5i21Hpg| &
|1bqLr+ef7cxikYw1dCXD6HFS5DK5GD4wUjj9NRkPYR4Ylg0TQztqhbWUmSV+491KOdsSNFWd| &
|HHKjAKY8gE2ytL+I10dChbk6aL9r+1xsdnSziQbVuwj8ZBNesTu2D5QoxgJlptsEVsmy0TY2| &
|mMGyjyDcpEBhvYPiwMA9RpHfKwg3hSb0H1nRPRZGFLfLqHHuk6zTPzqupTcsFYf+RuqMkkHL| &
|7Mi6+gVfFAG7aqtMICNZiuj17s1RSPVhvQz3t80jihic/B99MvSXLCKIgNFu+qKrLyMffvBf| &
|RRpR+G8r+V+XgPcv0gvPDH7PxRxr569LPWY54km4sA4iDNuC6CyTBL0drsVK+7wQPy4pVKgP| &
|cF32p4se3Qlh9NwReJaNc+Ac9rsrIHO+755iW3KdpLIirjz7Sb/Me6R2dk/j1P+64rvAn/ez| &
|KDPzUXaRJpD24aPVapzDVV2FHhpEUnB6+hqAqD6VlJi2NhThvkSCBuFLgb5ZnEfZ3X/K8IVe| &
|dfbUXaceBjKzEs/K/aqvWymNr4C64hv2rOjFII+s3mg3MZpjiM4VslyZxOk1dEowMftjpm96| &
|nzaL9cjTCn/rR3+zLGI65G8i8Ie8T19Tx7tosqpt2j99yQNLHPQdFsPNpy75Kvx9G8yEfQ9J| &
|tXr89AVfZvjQYSncWz/HV+i+u+Lo62Dm/neOVY/a/l2SuAtda5qBwZMiU2GIlneZdYggU3kK| &
|wsynBpwstui3hM0mrLgwYgB2ncugyjgwgWMujAQxdmJLUbuvPEI5eSoC/fEaGuTmdl+oy7Uo| &
|7JE2pjjxuyvMfAEyHz4Xa5zMns3cfdhWeXQQ+69OgDndjttCQf+iRrWKoOwA23IvXFhqrwgb| &
|7jL90S244SvYESlW8F+2I3teH+7zUT413wgBjLiJn34TTnT8NpoE9ZdFKVSholkUD2ybnjnF| &
|qv7G3rnmchzK9fW8O+/XN5HTth3SGnLWPbbncH6UzBZCauQVHcjaGtTYu//n5cmOnFXv8aCA| &
|TmBqZLNz1aQ41/lge3XmD9m2YN+3TfSeYRyELJj++JEY5mHN2jF+dZzkXR5+QIxg5fr1j+N+| &
|NwXQ6rj9cstBfGwotG5YCussWCqKXOdV4ayAkqKTicswPgst+0x538MZ1WvH9Ega3d2fjto3| &
|HJdTTkUD0I6Hm3fUEsA3jU3nidLsLrmV8Sa3cvCXJhmQxWbsbciziwsgz3lY1fyq/u2xOgZN| &
|hVRfu1lmhbPEBrBiIHHqcMW1VJ1g5IyfmZzQ+20e5Me5liOlEQEK989Gv8HfNL8tVCGJUJTP| &
|QocJTbkrQXuL7uOoNH0C/lHLaYuFVJIj3/E1RR2QtPG6fXqEdVGZK764a9kXzFKsj+6by8P6| &
|i60XOaZA6GuHg4xawSnhz5MNDPYqsH3ZgJBvy48WqIln0mTO5xjE8o/8zceYO+eiMy6+5GCT| &
|jXG1Pfr4Azx3Zi6QyzxMxtXKvqHZD+BOKyytx2h49uopngSvSR0si+6G5qy14XNlN2I0gQ3V| &
|LhB4ZVmr0EoGhRNC51kyUjKw7M3fytIWDAW4rQJ1iaJ4/iM5+rZVHzvUpOGhVw+/d0bfllqf| &
|S7KmKjGXSJ/+cRoWXKuCycQnewS//UaEv2947VwRAEGQOfb5uWrb/qs4zU8d25qiSALXcmzD| &
|1B25mjrFXgzioyW0ZKozWwJFdAHiiTgxjwhVjaZMvIm/R5duhp02BHzrKLhF0HogM9mdQXWr| &
|FS82KMwmZuEYLazPnb1CpLK7xAzha7pDzlzExc9fd+ur+qE5mAD+7QL3BT3U1wkZSPmF5Yib| &
|ifWXztFqTdYIWtUXUgael/HzCDTTnu21fa6jrmdhOlzOC8KY/sqHRzZfoJLqT7Nft20Pti06| &
|yk6rOjE5VUj7P+Rqv+aOgYbM4EkPqzhhhxmnbu64qhs3LmymjYtKKuOiBIq0awDcc99ldcuv| &
|urrWyPAptBET34LEmWdFO0AbeQXpuA/MO9B5j7Q6rIZfio9kK6PhLsSjvlcaA6PjHMQ/4LiN| &
|j3f9+iJp9zCZdV1ZRvu+KhuCwOGnP7M0uKTsi5yZjrXdlM3Vl5zPBarWLTBWDnZ/EjW5aIfm| &
|NwCv+66h4a1b6ldveuJ4EcE4JFLYDEsSXodGWBGWyxXzWPbvzdRhxCTyekEvT6EONnWwXNSU| &
|j8Wzi9wAVzGf5xrx9WXpWhbisJW4I7Tv+va/ln9pZdJBvLoGSlLGKhl+NsccZ31OSZcuXblb| &
|MACes5HqQa10e9/fexHMiVjUTb/dL+J0Ab3W4Y/ye7jH8GJVmgMMVNJGqnaYPp2kk5HwYVdQ| &
|V6nlulkJmHGd5TXyfkEnkTjgmFlZZof2D71Aeq6UUUrLJKMn8kbGMXgv0CR+en1asjyoUdL9| &
|FvQnKzw55Hk8rQBl8vTaM6Bvd3ykd952zUEyEDgpSSlHptEEZAaav7XTKwKLtAnmD1AcqIUv| &
|/oIXo+E+oMJ+1frRqS0JoO+kHD4UFSzk626H19LDK298+5T8GeebUtoux3yqJFTiUe/7Hee7| &
|FujEljA3O9O9Oyc7NV87tsK46QDXSejmSOJYEKKZ/wtpuKF2CZHpnkCzpB0KavZWkdgJvHqb| &
|P7GIMDqkZwkbm7w55+wUI5fbpHKDrxEB/A7NMMMc0/EZCTb5KIPhpdjfyyUaXXg/A+3frF/7| &
|9zuyeQX+JKQCGzYRPxJXgPYqGZffmiODuVQpEEn/8+Cv/ExuFkKDduZbieuPt4hbWU9C+pM8| &
|cngyxOGHHNbjj/PA1xPltbkbvDLong8uegNG3+Q6BKU+2R03nRc3pL93aOSj9QOV/Hew/ZUf| &
|mLgykemC9RWt0viCJZDUE4/7yPcIxwgSg0HiZPQH6LFWT7LUhKgTbSP/pBLno/3l9x2h45tl| &
|GARq8TMGYHg67njpRXV933vmbs3YRJQXlZ+sNN6w8an8dQEehx5kVL4PlpEU36+PQagBj1Vw| &
|YvKZFNFpBriWTZo53FpDBUprirjWGIn+9ZHjv44/tIppjsgCbhSjcwmNmFgrSP+46MzNZl6A| &
|lr4UnD75e9Yx8UZzPq4+/feNsjO/HtzLzILtUuHRVRLq28qji1qWA2pSZVgFSh8glHjP3gK3| &
|VCAhNm2po3L6a1V/d+RTrfnG1caIwdru2gmyheN/ttWB9WoCIiAA/pZWGnKEpiB5xy9qZOAm| &
|vQv2x4N5bF3DWcoGdTIjjNYO9gmVsKg176YgdoZx8CBh5F0aicm7MeYUAaS5si0oD5bQKWLO| &
|8vIVDJ+eVMAykLZN/mak3PMP43+VYsDbL6P9yJH7JcxnCuPRQnbtDF/tB7L1pdoAbUPB9H4v| &
|YAXaMrpiUAV/s9G0XmswSTT0Ikm3ON/olAuljOr7+6EqfAbFkuXpSu9LFpNxcH1/B0alICBe| &
|tROLpPi1Q3Z9h8NUypCGHpvnzJ8JHuAPatHoBkJoFWpCpR2rUzcNMWuss/Eq6DFqr4lM95+v| &
|YJ/lJHKNMjuKklzx+GoY8vG3LXHgBW6feRUepPWrYJT+7bgLlnuoBlq/nYxDXBttDE83Y/Y9| &
|zG/JVB6uwfCYI32Yya7fWCLd83O7Cl/y3vbwZA+zJUpCyO6t2DCHdfYBfRXdeY0JnhF4qULa| &
|xSnbc2zX5mEyKXCElzceO9MomdYW/CM2jqtZmtzXwZxP29Q5q9+qLlLvBwH01dqrZh/Ve/sG| &
|sqSg3//SYT5J+miXDfkCJYohHTyQfv1B+hzu0nn4yIOhJ28rLlstu4mHF3j3yLtudtO2h3jf| &
|kd87Bu9EHbEfKMsmHx+0zEWgA/hvB9ae9WqIfzrl5M0987lIGYmlSyNkuNecmMzF+1/K6Cu6| &
|fwwuBaanF46ZKQDMqIkCW0uG3HU7Jcl15H6K57AoECr2PUyMhJOcDfCHL2PNy63Nk2jeUhQ4| &
|fxnRqlH5cMEEvVv5/nvW4v1zJbT01/R98Q2fvWbn7gJnVKBn6ThS18kPKUntvE3f93/ag15c| &
|42e0Lava69+BP8z/i5vdrpMwgMlV5s34q8xkHA6vipKUe9uFNa9E99HH+PX7NJ9bbaLywOgL| &
|O9ZfSHTrtVZdufIwKJD3cbbUvkfvVwR01bnPMZpkj/620s5hommJ988risi6P8XWAbMPOqHk| &
|EIFjh//yMrpfICnRDkr+ZNBEfyxY0hH1f3XTw//phof/FoakNaZbGA1ne79bSVAUGxu9hdlr| &
|vPj8WuZRBpfpLMdvesDIbr2Y8aWrV7w4kyyeV9QaMKSYTMiZdF5VH4J2hHG7EEsHgWxgRqLm| &
|+Svq3zt6g9jIv204pKMgQM9fgH6pkWd/+K8yXw9R+7xlcsEB5PWs5uLz+rxAe/qYJH2/CPKV| &
|7DdGluXrbezDOeJ3HHovPD+kqvlqII170WO9upWC4PQrzuONbtDXIBygd4zJG+8q4ttvlYNN| &
|/QAtY1avZ7JOqjR0FiP3EP9WCAlvMIz+cvB4k90He6NVJiJg8fYUkAaD86fQJkAudXZU2ftw| &
|aTquyqKgb/xFlq/K+hQHSafgjlJbZHQTSP5eC2igv5owfsUXDMDRmUh8uSLbavwBqV8icGO1| &
|x2OlulTMX05sKnjuRz2x3d2vgPVmkGON05nHk/OYF2I3VBRzqhyjjHmFcdYQDR1+feL9y53g| &
|BwIygVUOCL3BnIRBioAfY369RyzHGkFH4KQWkHeIeOX9u7MYZzCU/gspalEBotxg5uUfmb8f| &
|PypaHkVjAFSSrgHW9AvjaKqzLLMCL9ItW4eXJrQREjmEe3m+LOicTkOTfsqhk36o3OWkkCKl| &
|Cxwit4fb3IFHB4DxE9yXNr6JyVcolFrc5heBYc5UYkFvx0OFZ4MgGXDTl4cDbNzyzGovg38R| &
|cY3Zp6HhkXLdR0GNlmJb4KWW2KckvKx5rU5M+B8w0msWhl9ICvHNsV1Utn2Unsh0xRqJjV+O| &
|43os+bPXZBZGJZetG591zUVrKQGUr2m0jq/0Qmb51KK/dK93naOEWKjZFxpeSjtm2fNZ8+Og| &
|Km8x3mw8hDMttMVDKgeefES1fVxfJQokGFjfQT/K9erxV6F+CEnMdfnCvENItuDwuEbqoGdZ| &
|9BzRYPGEEQSL1dc+Cr88y5ke5JrfnKu7pykHW49AwEDcPBmDp1+uumOGQNAiV0VnZxGKNTXj| &
|eIkwJXwl2bsZicFKhfuZIpgRhhfOcVDRSv+lxG9W8vf7BJx5sh522sLec/j8pd22Gbrg0zem| &
|g/Fns1U4O747xcl4lm7zO/ExXjhYy6gVF9dQj2I0U3Arn/ZfAtA0xSxHQvtuP6bLcg1j93Wg| &
|N9m0NiPmEwI35W3w8Pa4qrHrYXgcGx2O1lyPPxRLHJ4oPfviAm2WBAIC7y9Cg5LZUfbnYTRb| &
|d7Xcz42pvzRIEy3Vj44vK7gRJmx7uxxmKKfHPcjju5k6wxybunGaEqo66mt/hkDpu2DN6OFb| &
|SsB9WPE3P6fvsRRCredMYRbj2Rs4T2xB0g251WnjPuMYTpTumqk5SLVJ8eJzgYOKIIaAZpqd| &
|VmTfrCO/G7psDhxTQHtYgo4yhHsuWUv/Hf1UBzsWTbvzvhZag736IYj++KjncRGHhVN/0ksW| &
|MYCXw01Y5VVjeTJwedaYsFR9KLYq6Gb2VQ4HGxHweKBVKmDLupiUGDdmkVDs02V5bn+1nL2X| &
|uv+28oUC704MVofHQ6dL5A1LzEiSQeW34m3FUdxXFtBqlflry9ODpZd1qVDi5rejuxjthjvq| &
|/Cij2mYt4oIrCDzcs+ZFJLrn2lsHxp2Dedtc07qlctAaxVl7jJu9+UBSZMVwBZazBOIdJOI4| &
|IxXp5E4d+OahLdcIhgD0RpS44fbv69F26XM+cwdiUjxSamuhpl2xRm0aAeW0ljBndz4e9rzq| &
|jdxkYhRcY5042zVviuE8bTEgpCvrJ8thCe8E0uk5WJeXkMqpeHNrxUg1FLiE07mpOzvS37NF| &
|KfcUeki85nZ8xjpiGpGw5sa+qL4FQuukhDqOXieeU3g9B755Kl8F6rBe+YVGa59WrD20l8Jb| &
|BeKvcVyixxeKjcR3ZqMspdUJa9kPql+IOhAT3ay6KnXXv/9su9D8u9ANL05d2BH8c61Y4nC4| &
|Zl5yRcxgYbUedq68tH+7LkztUxlSn+yC/rerApB6ZdAcYXXjwXb9VXAHnbAC1xVGARMNE3d9| &
|O9Y/zWhvlkSI5gI3zAOHgS/SHtVCGuVBuNVHXbSIkQYQg/J76CZ8x7Jt8CGG847Bx5y0uv3i| &
|UW9/l25+janoBfHa/MKN70pGkz8KZP/VMRXFByqyeNM90WbygACDoMNK3qkWsaZjhlLzNQS3| &
|NBreqDvNRkKhzFsD1B5qwT9ikjeurszIQ+sCK+D7UJ225a55BuFnyAHUx5ErvYYyKdR5H2da| &
|nEPmTAizte5S6IBtHMrRzi9jKE5lCrleS7SwILbPKe6gFpPlzCIzJB6et/0BpJv2Sz7vW2s5| &
|4y+Cys30dAR5s5Qm8K38gYXnC4Q0yXVrBhTKCOx6Mh9NCWghLnvKnqPcEZz1nKdoBpK24CqI| &
|S4c83R7RhokOu/WJKfawdkoRtNLJ9rcBAFYgfU+TNSg/8My/wxRyutV0TU53jRqWdukLszLA| &
|NPK/yjNMhNXMDc4wN2d2iF/YhNqKPDSKSgQD/yhFrAT1gldRxqDecoi7byhKuqxSgnq8bLwm| &
|CAQ+rrzRoJ9DN/tYXK/QdP79dYIDhXgCd/1of6hbqMS6W2RG5kNhdsJW+ijMKsL627VMvidt| &
|yJSVjfcV4NyIV3huMBjDPdnON1/nsbdxl2V/Bdy29ZEyBckixg8/GMTDS6igT/6rnetmguW2| &
|+cFD1nmmDLAeS36u7ASCKDfMTIyLr/2rY6QGnJ+5SWuQuTp+gsS/7NF/CS8nQZNfvLjKWHbx| &
|18QqV/rzJ0gSgFdnMYg2ym91vawf5NAyaCu7B9oJHe5unl6vzmYwj3hjzAWyFKQ9XApiG6lt| &
|LnutI26TCAM81PQWshsQW7idv62dDBgzB1j+6mS/hSyhnCTF5yhLSOeDQUQsjxtbv0jTnQLv| &
|gWudwf7CtzAZP73VAJJz1WB9wDSRtFuosXWkxg1VIVWVWEjm3tnpsKceu+dIONdnWCilzX2c| &
|wOWTEcpCTGdehTL/cjk2awJ9i2OgZYCLi0GtvMOPP66MYQxxRnCiSaTR90CSIWKq70qZsXDt| &
|bfo42wL6Cz4jYWj4ON72Q2u7GzuH5/L+BQkL0EFJWNFt4D23bSmNzV8lnaVMybd6Sz5843oG| &
|VhtvZlVKfpqFK2K/gsRjlmwE2BbnNgb1tb17Zh0eN2D1NRufAV/P47JchlNEvj1vmAVprpko| &
|rxtJuR/zuEC0JqlLyOwBim9sPQRJgLFYxFfkxdROk5eS4c3AVZsCqPZVEY3Md8UOntYmuHwW| &
|uJLSOoNQqnJz3peDsouynZ0qlckJP7LWZRe9a4R58l9RQm+c63SMBhq2STiMjavooxjVhkCG| &
|axCc0vy90VZuQ9YbG64+7AiFtIy6B/ze2UaewVbkT/aWV9DmOUrG7Lj4XYBttbjyUVFk77Hw| &
|mlSe+OFu+f74X8383CETDi/hwJ++Nb2TLpcz53Gy24ODYZdi5vgcjGv+OCBJcpgEqM70jGlL| &
|mihjlOLr0i1xUjqdQh8YHD+flwH+PupzTHKZXzBCjZFJGW9vIYuh59wWc5o65BiUD2s3A/zv| &
|ERykEHEknsRqL6AC2wsno9kS1UjCxMTuYTwOV7Wkr8iEwsDE5W6oa9Dbdr5/dI6HkF70klkO| &
|RxGw/ELRkJ8XhnKjWpzRMtCzJph2eIS9wj129KA2rM69vcLqY/mpKjx11cbCj2HbPfURSrlL| &
|xNYTP5IEIPCrYcM4zSa6iC3j94HJT3Ytn0zepBaEdzRswABhPPAslHMIXg9rY1O0WiJ+MLu2| &
|D23hUW2tXoLc5wAw1SPB83rJTvG5XhXXqOI7KIPk4ErE1CH1+sEXhcAHSIVINNI/iiQT/5FX| &
|VUPO4NoUrxc6f/ft3ExZA+g9Kzxwb2XKUYLxER4wtgYcUQ/X7JRrz8ShrTwi7/PaFZIRl4Kh| &
|eJUw4mQZUoImNCaioTF9HQMb9jlAv2b7F0RbOk7uVSVFmJ7J4Mc83i0w+SMoC87Q112rX5L/| &
|dfI9vMzfLQ9Rkw2cFGPJmLj2dt3dOmrCDfBmjgiWWIb3Lpv2txJpkqZf78P8RlO1/XzPuipi| &
|Y08Rb3jvKFUJVLzkB3USGr6EUKCVZofPRnAygp2A02WyIiOdx+OhMfG2zC/rvwmjeahKZ5/h| &
|zxpPwjaRotB5hmlfpDwvi0QsJExCUN7VCKnEfhnHhX/6O9Dtrnoy1fphcv0nZz4SYtt245/A| &
|p7l0gZ03M3SSHno22w49xMRDYl38al5C3XZZo45J4pjMKH8IJGmB7uLsah9eqvCdB4Zr6Ujz| &
|jQf0VzFKJZiQ2j6vO6aCYxOVanS+tNJk307HlyLz5rh3y2XGV95d3ogvGaBDxphBu2zfXOe6| &
|q5oybeAIdux9Z1S415V9vSTKvWqU0yUXGRjJoDhmFCjuagXI70Nh44zkklhDxUSgEfvkdylc| &
|7F26JCGMvVQZQ4vm7jQhmBtIalS2GDMKgQZRVcTlivQb+jVCzLvThcIqjeSYYiqpS5tbgFM5| &
|CRtP9W7An8OL75syP8Qve/ePOv4lYGMRJy25/Bk9LIXJvlT1NEIg7eufF2hVL5z8Nka5ieDq| &
|nDsQUR/PtGSrgB+LKx+NqtNxgWhRRGzdseal/mEd6volDEhc2YcH38GBX2q+Hc5DAzK6IjfV| &
|oXF51wfoA9zge0LcLxiePa9vcEZ2HjjjWbEEoF87dPUefBSOqfMNu+TdFttVPr9Ovu70h8Qz| &
|IqFl8xxelslGpAhQHyWmLzCOvKU6EOqAS/Qid+03+FV3FnNF+bloWRbY6LseUIue+1WNg685| &
|W5e/HCt/PQluEK0IfzuA+PCpiKDy7n7vmUUk32l6HbJGfTcLAt9v7S+9D87hfj6DPtoEbzhB| &
|Gfh4NPs5BtS5FsVA3VNgYmYV0UCV4Qm57O459BbL7tFWKdZPKHsJayhHnsXvU66KKronWDiG| &
|d/rNzNdEi98J9lFiT5iQCI/sfbqeRkTAck8/T36m/DDTJV8hOHy/pgf9qs0Lc7B2spIAidNj| &
|orO94cVKtHI0wfHRTNrgQdMnUQwTMb3Gej1rFFDJhFZ8upy2X0Ub00Yz05BZlPcZxFp+DOiG| &
|4L68KbAvYRoVXISGe/azIW8TF24EPGNbz1DKeSfop9CBRP5U5B0WD4Ogbp6DaWQMWveBuM3F| &
|xLYYFmtCtEQaR/rAXNPYvSyHUDuztB5CcaIwoS9s7j+vYNurA/41FiokQYwxlEO4fBQMnV+g| &
|F1Kdwe1Wg02YQt8SHWG/PPtwvXyjmv+du3oUkG620y75yK7uw63IAZnzOBAzbVj+BRdxZ799| &
|7ecPD33XA7h+N5r4EE3Xr0tzUabW45DYM++CbtdnqYyB3o/YqMW+FvpUW55Awzxe4m93fuwr| &
|gX+wkPWLIAd1kvcvSXnlpTpxhqS/+tdx3zB/0xoynglMj29SvF+dEjVc8r7czMG16QIYZIRg| &
|z4/a+lFp6bw1I33jYVehF+2Jo7b9uPSz9n5TpuBhQrMVmPm/bxZgDn43+vb6E69Kh21F4LvA| &
|HOKFYrpvTqHQlYY/TEzQL1+6zV/x3bHvVuXhUiebEsApeQm/gfbHTk9Q7QUvJRVNYkYzGzjq| &
|x3N6BkioedIfnRlhfn/j4KR/wOLubvRtF/JH9eD3T57jpvLfGZl+LsbhLOPjqw5f/P5D/uBb| &
|aiz8/Ef+gIf9/WzFLdmH/dHXITswAmJ+LBUaH9D+6cjpm87U6Hq9Gy5M4iyNh1Cc4bdHjji9| &
|T1oPokW97zlPjTBwiFP399XSFOh33rksbO84z68KgflGIwsNdDe3vVIv5XSIJYqE9o99Cspo| &
|t3TfotxMSuE2CLUujNgKWNl3WlMQzNSYdwTSg1tITmV8gtxZfqTczF48pzE5c7W1nD6TkddM| &
|3ckKM6qBw9ec4Nuk7Uk2XstUAYxknDOKWmBBhl+wYjHSBIU2WYOfEJYV1tV1O6/NeU9bTBPU| &
|QSK/paQqULkfEg+5qOvHotqossQpTglwPfexPZ+RPdocvyYHvdJHwNm40jl7txH78oGcOPit| &
|DyncbcHD1l2jGLpbxj5h4MBNnZ4Tej+x6VI1gQP51XI5GN/yFfPaw3oiggC55GHG7vTjakNl| &
|bnhaz66cFAJvqqU6KdCtwXWXXywO8nzNhKnjrHzLwAmwaPLAe64wdLn9UExOCLaHy3orWzm/| &
|VO55Idw4QZMI5RZTtGEPsoZe7AXfTMo0+qnxqdLqJ/+W+pcgAVjWxS5RLJgZfYIj4s0Vih55| &
|UipZ7/dXkPBWP36a5pEWU6IrGSQMvUG45VcyVTXhpX6imtblqMszQhuQUxwmzp3kfdg1fl5H| &
|hrRd/2zIDa35fvzg4iqOtKx9ETZzX9emV2gfk3fUh6PKJmuS81D0CMwddcEqAHtz+0BYgh+i| &
|aFvJrm1dD8cz+bfAzr6ahZsvJuPirF7nSkTbCNIbQWMxTitCuE2u0WOive1njuSA14EpTuwv| &
|DinvTGRSzY4TTZESt6rFQWS92JaSeLJn7nKa0TADIaY/vjcHfvgzGg2uC67WWVYSXDzPaloA| &
|TDpoYevtQ/GDYYIZMdzjdsljdcjvF18XikTxB4tM9LV/S3CqmBDPNb1RwJL41VQdBWyrVr6F| &
|Fq3M/ACcOdatkUPqioIF5AhXAtHlptcebUmD2D7qT1tJ5h0a6ZZr6XkoEP0aZhDyzqXqDsIH| &
|4/D77V69OoYBcNLF9RC9b0Wih859I9SWJebuwwQuSPEvQqdqNF4cN8Y9pGP0g7Qv53W0koeE| &
|jw+fQY5azevAW1AbCgda/3pRjZVUli027YSvUn7/b/+7+2/lGK3MJU+/4WCAD2srpkG/GYJQ| &
|xo5U7Ajga0m4EDnh8tcOS/H01ml3xNGLbfinjtouXfGmpXpemmNxaxLLRpV8jtA4e9ppEvzX| &
|/hDdqYuANlus0oR+idO08b5U5Kk63xCt2S9E4rZRvWyVZ3DhNN6/xxwaKuP1L9O/H//A2lyR| &
|PeA0fLZPkrwjiAYmrNiZt4ojTMoV8Hi+cn3UoJ225BgnBGItv9xGfVcr/R3zF6faOyS0KHvN| &
|mkgR0k1VBVx4pygflkwpQHIOchXq5Q8MaceBMabE3W81kXmWnrCMROTST9+a8jTewH2ikmnm| &
|h2bax6cjc3omgf1SLeIOWIt8Nxy4/8fBgrfhU2PiPhIkCyTEsVnTtu3Ur05f31RI4kb1NUd+| &
|rmBZLhzwLJWEvE6o+5aGKErH9we04ZUxBxVFBnM+qBOJ3/ovKfXu7yYq0h1KZB9dfucPn3DZ| &
|61tnFXOnzi/DUkbJ9XXwno8u9mDmVdRfAE9y9+GZrbBFzjCR4HDC2pT0Nd0Ltt2/+pSMKMdM| &
|ViurCGtP1iGVof+/PUYAbKWSl+xZW4pTwWlQ6imEUDTDlKnvGke95FNZ3z2ze2fl1OtzhL8L| &
|FcypRUKYSZR9mu6U7lqk/GLkBYQ/iF1keA351+MW8HxVH14VLDg1eFqP25Pmv5Cxu/Fki+T3| &
|Yq6gkb96ZG5L0iTxyb2ttJDAaPRkQi2BmC0+gzOMfVkg00Duir8lY6b8/h4M/ObkMNFBYRCo| &
|XrgLgV99nmFWal3hnWxZdXhkgmkYnVWmvEsBD2iBNao0nOOwXpQW9CYQx3kE6fdA2eTMs4jI| &
|meQxHI6K/NbUcasxnjpns1/87EjDawrDgZrs9hGU/Q+IA+dXn8x37QZ2Ybdg4jnJzyG1EWXz| &
|Txc99Q2vWalAxdfajaxhaPsvdhz2Gshba3nW0Ob68rhVMQRAOUnuccky9++E8Wccc117j0tX| &
|P2qXVN4DuKcK6R3uO0/hUv7qfoR8560ODazhoMctnX8pS723Cyy0gim3kCbEve30e5z7/Ho0| &
|WWEz0FvZvevx6tdb03CL4WbZlxNlXGRI1+G0llqlKaZR0Gs0Lir9nY0AqdxV8kUr51MbXcwv| &
|A9unTFChMeUfHPnwlo12v0kw4LWgF9nUM0sy0rT0oheOpc7HpY+E/rqDNKPsF3AvKwxlh1s7| &
|ddXHUoweKBUSy18AFReGs3HVgtwjp5eEy9tZgH6BWHKBDwTCMNdbunqSUvf2eoq3uAIlbX1g| &
|w5QeLyN3eXyhi81de2GkqbHpOr5djSRqy0k7jOL9XtZtZOOGv1Zs2ii6fj3j7JY8zqe2HekV| &
|wFmfROXAfXx0cP++zVOKEbBBwIdDH9DkOynjReVWHdtBlFYAvtLlUShUhQbTqfpX89bRXvta| &
|Y+CFbwHgMthqC2Yb6dhRX9oP6o+Se86oC54UJYw8fZCHv1aCTTPdN93dOH6RLaBLJ8gOsYeS| &
|KVFp/SH1DpeAsyVcQVFrSEjkA0Hiv5dsI/PPHhIKIijfzqGRxjwbKjC8EHm9d2fSS7AILv1Z| &
|v8i3kDf9PO5X3JsAEtOmhU+OYG5q9+jzt+DM6s9oA4W0f6jwrNC69Xqb4oKZHGO/gFbjZ9u5| &
|xlgLw+TsNTNsY4nM5zqEFKjSF/XjjQo+ES/pyI8i6aZQk6TcZG9zU/7h1MMMHy2AjPtoZ6H7| &
|crLQox09dQpHT5wurZ8189Tn14NncI+A/Wz/b5vf/quMsuyWk+gVI//f7X9lm+jMLW4/wMbB| &
|u7ppCKzBaxIii0fT8UlvrjdMhDwNv5W4j+5t/8oUynWaUYkF4XLQYb9x7IGdgCSGoW5lWSdp| &
|pwEPsQvzm2vov1xrpmrRNpv+IEmAp7e9KT/fdjloqg0liPCZE4NXK/LD8iX4Az3HTB4Lz38R| &
|K2WBAVoWQJFjPiq9fk6x6HfWsuc56cP3AH/0cRchuVqzHL7A+mYneyCtzw97PfJJxhb6DH9b| &
|YYY9qWTU9t2D4JgAeAkJis7wj4BN5HKX0+Mzj/B+1NE0YbtPxAVLE7JeXxj5Qbdo28YTDMWa| &
|Oqo3ds1i97oTkHYpnf5eOTBWNPh/cx+LM4/p9H+yjaXH0f+RTcd6pAupnILB2B57G9PXaGoM| &
|+T04oGyQk0LW8DFZxVTq2uJilRMQ5dfViiryahcA267RJhQE5yegmAzncL5zyVJE8Qkl4J8x| &
|/CL9NW/8NGdnhaAo8u5gFMIm+vXD0cx48Q2Yl2+Z7/ZN3oGZ/rz2V8WcdnUi6FnXzoHKHvwF| &
|wfCHIEQ6wR+skvTp6iC9+ZG/Aa9iBivQ6JzBjP4hk2vK/AHlkje8APDdIdej/vadJvM3qOiY| &
|qP9l0QnA9m4iWKbvbznRB53SZUGhJtm9fo+optEvXpxoB33fYGker0peoGwCnJak8xe2RfRC| &
|v5CnI49Y2WicQl7EOUQUWoHv/HkHTYFa9RMp0PR8ND+HCu+TF53mIiSfilrNbwR5Y8DNGcgn| &
|bM/rW1FhhL7Vk3TU8k+hfhHh7iiIS8o8Hk/iu0gWJp4c/xeE9PZqFi1sixKHu6UCKMjBhjoA| &
|GapoHkRTvAKtJTdIwxMc2dIcztzDWLW6DxnF213FsFT3Rq9/THq90O/3QqoVjrLyAmnGZL35| &
|55PIC1jyaKKiz8G88J+JvEEtqeLlyoXm7Bi1w79tLGuRy2ke+hHHZPo4LA696ahGkUfvQJHA| &
|w7/2w+0IixQfABvDrDSF4V33r8DNb/+kdZmUg3v9HHn6WxPEYl/wvjNn+S/ISJpY+QnvLYRb| &
|MMLm2mYobSQu/ulVHqA8krCaqw35kTNdI9Ctit/tDLRMd/Yf9oXj8i+Uh52Z+yh5WdBAjzry| &
|2A+CYNmeGiayj8ghPV0nwLgAjlQ01R2NmwFFbetgTDwcqdCG6ekj7qNJbWxKHjk73p9ClXeo| &
|nUff9fRdzkq1ijQdHYUzT3twnQRkBuJrEvT9aKp9HqdNUhMEbQcJ0t9j7x19MQcfMHEk3ihu| &
|E90EkYqRn8FPjC30Nsbokw4O6ZTlOmJEywQ8BViyYllEfm2RnJiO7xj09SGn8xHIh+eemVuX| &
|XXupOcQJMs3Q1ziD9Zu+GvQXDNMGyETd64ykgxKA8/hwZ0zvWFMErHrS0fc26mNaJI86ePiz| &
|t8aHF3JvSO1vBTE1JjjLsKD/ydrR5R9vjkRcEW3QfjM7ADG0sMpYbZ8+L8RS/4BBRtR5G/CU| &
|PVcry5n+W8iFCbxj6pUKb6o6Sp4QuPp1k6Lz3KB13suzNOyZGgEh2qjUgV5YTmusHTtaxzG+| &
|/BFqPlLGRLxXM1bGUFw7wRZk0jiHvPhs5hKAmPGdOgYmHepnSCsXX4kA5Mb98HTQ5n1EkJsl| &
|/ReKhFY7Z7ZL48uEAQMySsFRvMPSk+OaQtVWNuNYevNIg/K0fz4hgDMDrfRjtEpfb4VjVoLE| &
|/RHucrcqyH8mAflm5R5PjFvr95YMp7IQV3JH2ApxFHbr2YriN9xrqHjM/b4vnF55A2AeX4Tp| &
|3m4S0XG8GWSlQmiWHH8JSYiOyi6u8A9Hq6X5/x10MRn+dr2onEkWEShSwozQNQklt0moAGTW| &
|5DNDpAqpHQkxwifKW/OdWB9aj62HgZZX0XYX3ylyB17fiJAWrTYKD1dfilnCjxh8jAg069L+| &
|5STwXChlfH0X6iVbA5hrzHn4F0J69lLVZ4Si/vufXmVvpn0of+e1nIWsydvncuVWckQI7JA5| &
|pVfmpi8gcUziO49XV/GsB99cftu9yzuyHXujxmkcXH2q2G4GJKG3uQldzZfKwW7gHBOUfnK7| &
|bLANplad/oMdgOrmvfcIAjNcEHaGuEqfybbgC8TJ6tfLbItHhkb9OZPm94V0DaHueBh+Csnb| &
|fne1H4H0zVGvLRuuYVzAUARONZSjFA99hL3iS5LZFGi2s+mwGqA1Z6qK50rWdqfT2faVw/hD| &
|eTzGLGD0X+CLciOUf3lmQEYFKjghiHmy/YsW8Pm9wqS4UJDog1qGNypbl5EjKzG1birzUdZ5| &
|MgfF+dsXaJbV144RygdDlW0tFTLqfxOAq5DSkYh9pqVbcG96bISsfLqqWDe7hr1mqT9mbwMu| &
|YGJbE+EV5Zp/O8XsOddEjvWPXXjsh+Ua4Jp9RXTxx3a4u7KtOYX5itEUOj2WN3IHnc24Y9W7| &
|l/eG0vFcGtuaLL9xfgPvXQKXfHhCylr+EiTDmABm+OStwZfNP6N8bddDDJSTsY96Cs6JM98F| &
|9xdG4m9vBqzf0lqVPXcijzoePBJab/91aw3S5EpRiQzg1fEhNxzbQkW36th41RjKulg5+BmZ| &
|lA7BjH1BHIozP3U/aZ0Ylr39hbKhPkk9RqBZTG25luPiek4B+wX5j+oyucHvbU8Qu1QWRd15| &
|JKUya1rKGCkevQum0IplawgehC7CSCEl5ThInTOrDyOl3TlX+DsG/hJsJC7H9Q3zrzJZQ0Rz| &
|xFSoNaFZsk/X7z7XGH5bS02D9Q8OLgyqMS6yIsWYPUIB62yk6dtdTiWA05CCcbV5ckWu8eZn| &
|JYWKLwpiI2JKs1oNbt/Qa3Pz2q73BGk3Uadcz5jIKsWyI5lOGpSlV7Ws5oXDgG43CN/KLPwR| &
|Hanh6dHhnTNmt9RnUrWJ++O8ntnXINUGB5Qik0aF8Z/1yNaetlh2uFylkLSIZebmwoAuVbPc| &
|jh8Ry9izI7jK88M5Eptp3MDCw2V3jsScb1diBkzu5bKSv7LkemwStj6rF6HApQ9AKxfnsCvg| &
|F9a5NZizdYjSU0yt9atK8SeYapzgLjns92qEdI77fNL4pVLWV0qxfRsHZL8O9RnHD6YFLF56| &
|mONIwI4bKpdik3ArwkPjlLojiwczQCl91H4E12Ev7e8Vorn3DgmQKArxt2Ecw4bv+RozgVd7| &
|Z74UW+GT2gZmW/AURWP+ulbq+dPozW5MKXTTnslbBJHchzIZ1+T2YVs526m8EB4yBbm14VpS| &
|KrAWVj0D/C+LDBCzvivsnV0zLp7SH1z56kpUnJbkiCy5L8hvrpV8L5EcSoRe35HpkWD99hnt| &
|DvqFc6nprnZ4JZZy70gH1DExzuv6juUaQq7ua4T59ApLulT8d8Hx87aiwoiJ5e1IcoJpIK/G| &
|Yrb2vRRM0dKLQuJ22B0KrisAkUCNOZ1vgvPpiwvhHBSKBQldL5otDDBoWpGpmrqD47ixZSw5| &
|Lq1/25ij2V8dslipSe5L+ZWyZcvICODTw9nY+3B0titKf1zMEiHwQ1k8CnFMpXphF7M/+EFq| &
|Egyt38OuX7siDhL6cI6E7Ws/0wynnr777MVAgV3chP0IrBppBjN/LvNRP38BesVcomDt5KVv| &
|YeZjCHkal7HTvpXta3khiZLqEvdGDzsjDHaQq45JHCD1F6GQo+ARcPNFZKUURY1fvgy5pSyd| &
|L9HP1BrzsLPTPNqKQQstv8yB/Dr4Zrb9hFCvh5DMhQTV3eUBWwAXJtv0evV4Aqvllin1CYYg| &
|kEZwd93vVKGGGUfqx+ftq8adcgbBSZK7joLuqcrVHxjObkx5UP7KgPmWwelOYyg/sd+1GaW+| &
|XT96l17Yhxvx70H2MrLZyi+azDycLmu5xlLpgwtkWsyaoH+BewwoFcrpCwH/ckG9gol7/Qx3| &
|E+VmVxr3/f4dATTTv2RqqwDXQbbfk7/tHO+hLAi+uSZIst0k8A4nIECttbvA890ZSFuVC57l| &
|UYdze614yXUPREZxMByksc8wsqF5bcqsWsU1lAujwLZkNkxJNUJ6iqHIw41+MF1FdNnqHyDq| &
|CRaV1VOxxetaUNfZZ5xGP6HreDQ1m1V1/HQeRfDepdp6TJbicPWW6764Xb5QvY5++WL9P5Vd| &
|R8+EMJK981c4EJuwh5Wa2OQmhxs559DArx++2Zmd86hVatsgIePyq/eEXbZ07Hi4mg4Ur/eF| &
|fN8fnufvVxKjpwmWs+Ry4Z0G22dV0RkWyJRgs15ndgIsMS8xyXoa9dNb0ZosqCRSYY1LEOsE| &
|ARAk/CyHN6QKSMJE4n4s6ItMfj+Bfh2w5FZQwVGfR/eXELvfAt53rltiGUdxthj477jauQxW| &
|3WIvQg5AsCTtyxoyfdfcyIRDGnsh2JbT0efhD96MRfYaG1bqf05RPpXg9yPdi1I4rK7dt4tV| &
|worCNY2lalAA7zqa99fCnLjhCMGPeuDoi5AOxM2Yv3Y9ZEPHsg2cUv48H3e35eGg6WJzILxh| &
|mDrO0JsR8YpGeXk5GICntXcyrU22NBR6BMd5ntN6PVDYo3JI45XrgxY2Tnb2elzJ06E2jczq| &
|xUu0VcuunFvIzIPS+Zl1XVkBRFxbc6W+PDsKoyxBKf8qnVG43x01/5Ub7f16pzKI1cjl9Kz2| &
|43pGM9WBkZq3EXyWuG7iyM6i957zAPvxjFrghkqQIGL6PWQEl293zvhLdY2ER3zh4IjdENK5| &
|HkUKCsFXrB7ZoUimVoj3/Vu9+Hq5ZuXTYw0Mz/Asn70pl4LJSGQLtT3ZsnkvkK+mES+e1dHl| &
|uNAT9eeDNGjlQ3J5Xxzktn3Asfjp8ryqCguCwYcRASf/Ch70xhqyC1Cy7qoTn37q56fFOv7T| &
|i5g57sPFYRX6ZiI5ZhtmFOqNKFAQhf4NypX/jtrOb2ijiS6gsysVDrdDZ8jqLIok0jAbL7MN| &
|V4rhc82Df9DxTTxlFOe6My6SA8ZV0mfVO1UGGfTAvp+HYccE5nSAc6bDebxksBQDoQUxctru| &
|xJyguBAF/eDqingky0grmkPOLZPGV7hDsXA027nq1VIk+5d+bTJPonfzBYbbNk4y3cOdfJ3/| &
|XCgiUs99MJVgIuW9xpeGw+kIIuy4DWJJ2wt9VSvkvsZxP7qfjE/d8uH4ufMO0QcGFVdYp9jK| &
|Lq0e6ol/GI6YWS7LaC9gYv6cEnyuuYymvY4xRxljzFauW2eKf66QY6QwdOCFgFZLwxMDoLr9| &
|4CM9/T70+NjwWP9Y91j7WPNY/Vj1fteerTsWNhCOOjES3RiDK7H7u+z9IklJOfJKAHkzCmPV| &
|iN+pL8ESKPKddYfmWOW6UTspEIu3xff30yA1jx1iU3Z+qH3aaEcz8PBMIV9Im1yHW8Mi7wuM| &
|t6JkdK6Cuvd4B5QsGl2Ns7aLUx68jLnf5M9sSdlEJSOdatjullAeN2sKOcbrSgpCrkCq+knR| .
 rv_data = |{ rv_data }| &
|IekrEIINweqDuEpfSR8fWFLDM/g7JcKJxUo0C/N3yN8x7NoA1jxYuRpCROj9bGKWi86TteqB| &
|yXzPPXUPzQAvB704Ux8UkQ57oUYolq9HMaoqqyY7kSUe10l7G0zC7CWd+LL21mcXGUk8Ifo+| &
|WgDbqOZNs3c53nEFdDVPe0cd2vQ5O+CcOlktOPtIt/LLLNycz8u/TIzZu/qMX/Pt5O9TJCy0| &
|8okGNscM5/hPjpd6kuBp+wWWlXL4ozRK/ewK4nD0WRSVShgSV9kMegoE9GQXP60RAX/4MRqw| &
|yoyPZmTXV4TtnG+GxPeXwpaHqdwFzNJMing/72IPxSAsdSqzkGGVKQ/yuO+JreSYfdip4HhM| &
|HgjHl3YJeu2JSXEitRoDUYebJX7V3l+SC4ALKHt0BN8fsy+ofm2feeJ306u1NGcUJp4pHd0R| &
|bn27DWFnAsrJBduHnF+JeE/gMZyJYL1P31DOKzmBnIswZyfSboLv2wDXjD7OBx9OzJD1+PCH| &
|M32dS83RKcVHDjqgA3VlfXN43Dd988JlZbFGC08US5Y7BtaIQGvwgYW6PRKuzTJtUYvm6yDe| &
|DXnGVy5mtdS3PqlULDhUL9vh7caQfcs1FH3YYv33pcyK61+WrglAdcX9OjpVBL3xCOWLD0m/| &
|KQcsfvPWh4Xj3/W36NmoGDzwn96Cy9ca+Vj4sL9Gz1ODk+CP0f+V/zKPG/5fzsJUt2Tullv5| &
|xhpzuYOHJTvdIalW3W1TFq/8quciPCD89+p9eWWOiNH968UdeDElhSPY6t/KAWDlHsLSb90Y| &
|5XrUUuURz93aWev08q9HawprY4vRMU9Qdy6/h9pfvTV53dj9bf5gFO+YrA55HioJkxoAfq2r| &
|3nT7lyxvs+vdpvg0yHCn5m5j1ltbZ2/WtxUYf9vSyLP+o7W+fykMdxjkH6nKjl4X8tzvu3db| &
|C/DC73y70pc/H+YpcabUgdLh1xZ5memXwM4nOKqFNJhsR9WbJgW1wrF/STyJ1h49lmEtyzWJ| &
|RWXgjAkBfoLBxnatMRHbbU16KFGsmRbspcvBOGHzTx6AK+iyxM+QStT/WroVVL8zTwxDtJd0| &
|xxfBJzH8gHK2AK6OvqYD575hSjL0aVGl6vaszGIeXyMM3JkbYsXrPpF1aRVTD+6z/uj5UDDr| &
|fcbNKHQFRq+4OTDF8gXoMZJcRC+7n1xkWXSQW2kT6LHx29dz0xQ9AhSxcdt36eOlewzddxT3| &
|fvw7RphSNJygfDdXjMJIXcAXsBgHTPYnVSlC0Cz5jhRlXUxVV7zAYI5n+eBPzwatbt756V43| &
|tzqS74U4HtpQh4u5vzxqZ+7IN/RTpkBa+yUhKYlIw9EONj1TccwdIru5fyF6/sZNCKadQlHJ| &
|othzK0zfc7PGjXhIyDe/mSNmvDZCXGRd0nkEnGldPvj1DAO0gJoYUguKNHKHFl9R673QER1j| &
|w2ot0kD4eju7QByz3pukQqCxyGXhgV1o3M8IJMQ7BUAvNKjqO0kcvRDnJZxzl9wp4kZyH9nf| &
|bbMP9Lw5K3KEN6xtqoo+StKqHx4e+I7Rp05v74vsyEFWBhIQ6INiGYky/+059AbyG+jdmqGZ| &
|3n5iRnwCJI+a/y4nop+Mn6KjpuOVdEc0aJoU/l6df8RrqhkWgCZGjTaOMlQBMnWPgBBq5pJx| &
|D0m+T5sf1EUQrbjsJwvo9A68xCtt0/M1DkiALkrZrX0mqBFPK+j8Bc42mf1EgNfy7B3q5XrY| &
|T49IzhiuMOoEBF0xdijpKq6c1dMi7GlVFpSPWw8LrdQmzY4TKzr4MYVfU8CLWKFPyj6Uyx+g| &
|V40xd2KjZfwVxH2BJZTobEZbFR48qwPXcknKyI5xe3DA5leggh7JFEL2WkoYUpALmOhkSFkS| &
|nFXTaQQVjkywEHMKHzTjUSra0xdShdd+YAfu8boWQv1E4jgMOqRzz91q62j+4xCnH4T8Dqw2| &
|WpgqFH0a0yqD17LjzVXsWg+Rte5jM1WochsvCvj8nMRXGu+a6pFAOojQbVUg6OxKxVeRR6IC| &
|QsAPidEfXx8RvxFyqhEjeX0JZs58aAXxg/3csVrebZ+teLuyM96tdYUrX/BWPg8LHven6Wlf| &
|EjQ/QwE40chDS2viCUtvuYj5d3lVvK2sN+G0uo58v9Hs5Zxv+seloU2Qnw7Rzd0ealK8PYti| &
|SBFxDzEDzi5BHyB1SGPHaSSIlBq6f3sSEDfImxPB05k/ndWVKnYyH6qMLYljaUSvu9Xe6ds0| &
|o3vzQiySdDQsAD7QoXfyVBjLsIYZcYLrQ1Ef2S0wGdp17hMMoCpCcvL0jHvyI/WZK4yuH3+7| &
|jnnHgKjN+bwZGKoKXTueGdAewTfxMwzLkhcI5fOl2bWLaogHQWCUGhF5Crz5wkbTwj4XNrDp| &
|8oWx0V1Xc7+7T6u/O7MyCEusWOAvk+fo7rw8PIFiWkzZlZy9ZUb5HPGXyw9izbJOY8JVH6Z1| &
|xvaMYz49nZgQLQex+r8zWfqt5cxhx4FW7RvMmiWYwXdLLL/CJcGRe2YmaX/Y8xP/8Cd6ZKHA| &
|53+p8oVAM/PNDhsTqS0+FldbWkyEtRgmPqUaoGXTGaM9Y0VED+uUtiN2xFyDRxtk83qjIOGe| &
|mX7aBK6cI7kS/bFZ9lHvFkL7DDHmI63paGCWsn2DBNDGDfwjSHWUxxqLXjzxjdb9Z79orVzO| &
|gkw1b4NEslgheRQ+9sr05FyOSAqa/p0uBgFha3Nimbhln/AAugni0o8YfGqKfDVVEGfUvofX| &
|u1eKbsk88Egx9zN76zUxzxvFaxiZC9CkaKyqJFU5XnDPRW2ZqBSbzwBPfeK6+vXzhEmhWYxz| &
|+LaQT8qZOsK9VVM14pORbfitNGdpbzP7eb9pX4R9bxvUH9zWx8MzV2hsf3XxBbbgw6GfqocF| &
|ZOurARa1UnvCg7v0rng4di3jsJZ3/dhiRBrDtuuaYKN5zlEU96xAdmUgGHgLXwSP1AOYcND7| &
|TuFUvnmJUd+bA4sIU0gu39bMV4RVezJP0r/fO6P8vBIOcxjcvIbokGSMlixWhtToSnR4L+zd| &
|A9ewLKjERPSRxWkbEbay/WqdtbVLgV/ST1Kdtqngx/PH5gdS53hZreUPklh9JxAmk8PCHaJo| &
|uTSK6QQAcd4cA+/3+BjEsxJ8uhX5kh7W+HmotrekVuigb1LYLKymLRT5aMbtZqPzSX9qwofN| &
|O+tPMiEKKLQ7F1gpYoCaQjcqWn3dRYzu0e3qvbNEizT019+5mK2Izw2pdT0qPiNHIHnoZ9/t| &
|maik1voVdttyWyeSiLLA9h7+VYFZ9IrDddf4rUXeFvMeYuNw8tqf2vs/0PT/MIXenaEJJiX6| &
|mdPk55ZPMGBUXuBNRcCmA+IjMPjobcgwMOLC5O6FyzloE3h6HVT08wLXvB6vqkeKHxPtoam6| &
|yNZaZq0SK4+3qdwAXEaaLoVoqEgn8nYrqeO+WGg+dbljOGQq9bfo8Nb4vsIaMXBO6pu3Jf3z| &
|emRZka9DomGjV6sQugrIMOjhvU/45Y0eG9XfZTh8AxHds5U/hIlq1CaI6JumgrtpoGSPGwj0| &
|anwlB5QPUFCoakcl18PXL2cCDnChg09f7Gio91AKZiDNor8wg9RbFxic6heZUuSvCuK0mEMN| &
|KBIlh7MGmuWvJD9qavU2MXtke5bnHVBcv/S7fG5UU8ARW40HESFVv+5UbMDix/+3OUwgyzCc| &
|5y9ez/854m7P139V4iSe/vcf|.



endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->GET_FONT_SIZE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_FONT_SIZE                   TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_FONT_SIZE.
  rv_font_size = me->gv_active_font_size.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->GET_MAX_FONT_SIZE_FOR_WIDTH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* | [--->] IV_MAX_WIDTH                   TYPE        I
* | [<-()] RV_SIZE                        TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_max_font_size_for_width.
  DATA lv_cur_font_size TYPE i.
  DATA lv_size          TYPE i VALUE 1.
  DATA lv_measure       TYPE i.
  DATA lv_max_width     TYPE i.



  lv_cur_font_size = me->get_font_size( ).

* Do not measure empty strings
  IF strlen( iv_text ) = 0.
    rv_size = lv_cur_font_size.
    RETURN.
  ENDIF.

  lv_max_width = iv_max_width * me->gv_scale_factor * 1000.

  DO.
    me->set_font_size( lv_size ).
    lv_measure = me->get_string_width( iv_text ).
    IF lv_measure > lv_max_width.
      rv_size = lv_size - 1.
      EXIT.
    ELSE.
      lv_size = lv_size + 1.
    ENDIF.
  ENDDO.


  me->set_font_size( lv_cur_font_size ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->GET_PAGE_HEIGHT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_VALUE                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_page_height.

  rv_value = me->gv_page_height.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_SAP_ENCODER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENCODING                    TYPE        STRING
* | [<-()] RV_SAP_ENCODING                TYPE        TYP_ENCODING_MAP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_SAP_ENCODER.
  data lv_encoding type string.
  field-symbols <encoding> type typ_encoding_map.

  lv_encoding = iv_encoding.
*  translate lv_encoding to upper case.

  read table me->gt_encoding_map with table key encoding = iv_encoding assigning <encoding>.
  if sy-subrc = 0.
    if <encoding>-encoder is not bound.
      <encoding>-encoder = cl_abap_conv_out_ce=>create( encoding = <encoding>-sap_codepage ).
    endif.
    rv_sap_encoding = <encoding>.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_SIMPLE_FONT_METRIC
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RR_SIMPLE_FM                   TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_simple_font_metric.
  DATA lv_xstr type xstring.

  FIELD-SYMBOLS <font>              TYPE typ_font.
  FIELD-SYMBOLS <ls_simple_fm_data> TYPE typ_simple_fm_data_line.

  READ TABLE me->gt_fonts WITH KEY id = me->gv_active_font_id ASSIGNING <font>.

  READ TABLE me->gt_simple_font_metric_data WITH KEY post_script_name = <font>-post_script_name ASSIGNING <ls_simple_fm_data>.

  IF <ls_simple_fm_data>-simple_fm_data IS INITIAL.
    cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = <ls_simple_fm_data>-simple_fm_data_gzip IMPORTING raw_out = lv_xstr ).
    CALL TRANSFORMATION id SOURCE XML lv_xstr RESULT root = <ls_simple_fm_data>-simple_fm_data.
    CLEAR <ls_simple_fm_data>-simple_fm_data_gzip.
  ENDIF.

  GET REFERENCE OF <ls_simple_fm_data>-simple_fm_data INTO rr_simple_fm.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_STRING_WIDTH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STR                         TYPE        STRING
* | [<-()] RV_LEN                         TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_STRING_WIDTH.
    DATA lt_char_widths   TYPE typ_char_widths_tab.

    lt_char_widths  = me->get_char_widths( iv_str ).
    rv_len = me->get_word_length( lt_char_widths ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_STYLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STYLE                       TYPE        STRING
* | [<-()] RV_STYLE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_style.
  CASE iv_style.
    WHEN 'F'.
      rv_style = 'f'.  " fill
    WHEN 'FD' OR 'DF'.
      rv_style = 'B'.  " both
    WHEN OTHERS.
      rv_style = 'S'.  " stroke
  ENDCASE.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->GET_WORD_LENGTH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_CHAR_WIDTHS                 TYPE        TYP_CHAR_WIDTHS_TAB
* | [<-()] RV_LENGTH                      TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_WORD_LENGTH.

  field-symbols <lv_width> type i.

  LOOP AT it_char_widths assigning <lv_width>.
    rv_length = rv_length + <lv_width>.
  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->INITIALIZE_ENCODING_MAP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method INITIALIZE_ENCODING_MAP.
  DATA ls_encoding type typ_encoding_map.

  ls_encoding-encoding = 'WinAnsiEncoding'.
  ls_encoding-sap_codepage = '1100'.

  INSERT ls_encoding into table me->gt_encoding_map.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->INITIALIZE_FONT_METRIC
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD initialize_font_metric.
  DATA lv_xstr             TYPE xstring.
  DATA lv_font_metric_data TYPE string.

  lv_font_metric_data = me->get_font_metric_data( ).

  CALL FUNCTION 'SSFC_BASE64_DECODE'
    EXPORTING
      b64data = lv_font_metric_data
    IMPORTING
      bindata = lv_xstr.

  cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = lv_xstr IMPORTING raw_out = lv_xstr ).

  call transformation id source xml lv_xstr result root = me->gt_simple_font_metric_data.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->INITIALIZE_PAGE_FORMATS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method INITIALIZE_PAGE_FORMATS.
  field-symbols <page_format> type typ_page_format.

  append initial line to me->gt_page_formats assigning <page_format>.
  <page_format>-name   = 'a3'.
  <page_format>-width  = '841.89'.
  <page_format>-height = '1190.55'.

  append initial line to me->gt_page_formats assigning <page_format>.
  <page_format>-name   = 'a4'.
  <page_format>-width  = '595.28'.
  <page_format>-height = '841.89'.

  append initial line to me->gt_page_formats assigning <page_format>.
  <page_format>-name   = 'a5'.
  <page_format>-width  = '420.94'.
  <page_format>-height = '595.28'.

  append initial line to me->gt_page_formats assigning <page_format>.
  <page_format>-name   = 'letter'.
  <page_format>-width  = '612'.
  <page_format>-height = '792'.

  append initial line to me->gt_page_formats assigning <page_format>.
  <page_format>-name   = 'legal'.
  <page_format>-width  = '612'.
  <page_format>-height = '1008'.



endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PDF->JOIN_STRING_TAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] LT_STR_TAB                     TYPE        STANDARD TABLE
* | [--->] LV_DELIMETER                   TYPE        ANY
* | [<-()] RV_STR                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD join_string_tab.

  TYPES ltyp_str_tab TYPE TABLE OF string WITH DEFAULT KEY.

  FIELD-SYMBOLS <tab> TYPE ltyp_str_tab.
  FIELD-SYMBOLS <str> TYPE string.

  ASSIGN  lt_str_tab TO <tab>.

  LOOP AT <tab> ASSIGNING <str>.
    rv_str = |{ rv_str }{ lv_delimeter }{ <str> }|.
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->MEASURE_LINES_HEIGHT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_LINES                       TYPE        ZCL_PDF=>TYP_TEXT_LINES
* | [<-()] RV_LEN                         TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method MEASURE_LINES_HEIGHT.
  rv_len = lines( it_lines ) * me->get_current_line_space( ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->NEW_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_OBJ_NO                      TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method NEW_OBJECT.
  field-symbols <new_i> type i.

  me->gv_object_number = me->gv_object_number + 1.
  APPEND INITIAL LINE TO me->GT_OFFSETS assigning <new_i>.
  <new_i> = me->gv_content_length.

  me->out( me->xs( |{ me->to_string( me->gv_object_number ) } 0 obj| ) ).

  rv_obj_no = gv_object_number.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->OUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XSTR                        TYPE        XSTRING
* | [--->] IV_NEW_LINE                    TYPE        ABAP_BOOL (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD out.

  FIELD-SYMBOLS <page> TYPE typ_page.
  field-symbols <xstr> type xstring.

  IF gv_out_to_pages = abap_true.
    READ TABLE gt_pages INDEX gv_page ASSIGNING <page>.
    if iv_new_line = abap_false.
      READ TABLE <page> index lines( <page> ) assigning <xstr>.
      concatenate <xstr> iv_xstr into <xstr> in byte mode.
    else.
      APPEND iv_xstr TO <page>.
    endif.
  ELSE.
    if iv_new_line = abap_false.
      READ TABLE <page> index lines( <page> ) assigning <xstr>.
      concatenate <xstr> iv_xstr into <xstr> in byte mode.
      gv_content_length = gv_content_length + xstrlen( iv_xstr ).
    else.
      APPEND iv_xstr TO gt_content.
      gv_content_length = gv_content_length + xstrlen( iv_xstr ) + 1. "+1 is for '\n' that will be used to join contents of content
    endif.

  ENDIF.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->OUTPUT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_XSTR                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD output.
  rv_xstr = me->build_document( ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PDF_ESCAPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STR                         TYPE        STRING
* | [<-()] RV_STR                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method PDF_ESCAPE.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_CATALOG
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_catalog.

  me->out( xs( |/Type /Catalog| ) ).
  me->out( xs( |/Pages 1 0 R | ) ).

* @todo: ADD zoom AND LAYOUT modes

  me->out( xs( |/OpenAction [3 0 R /FitH null]| ) ).
  me->out( xs( |/PageLayout /OneColumn| ) ).

  RAISE EVENT evt_put_catalog.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_FONT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FONT                        TYPE        TYP_FONT
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_font.
  field-symbols <font> type typ_font.

  READ TABLE me->gt_fonts with table key id = iv_font-id assigning <font>.
  <font>-object_number = me->new_object( ).

  me->out( xs( |<</BaseFont/{ iv_font-post_script_name }/Type/Font| ) ).
  me->out( xs( |/Encoding/{ iv_font-encoding }| ) ).
  me->out( xs( |/Subtype/Type1>>| ) ).
  me->out( xs( |endobj| ) ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_FONTS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_fonts.
  FIELD-SYMBOLS <font> TYPE typ_font.


  LOOP AT me->gt_fonts ASSIGNING <font>.
    me->put_font( <font> ).
  ENDLOOP.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_INFO
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_info.
  DATA lv_timestamp     type timestamp.
  DATA lv_timestamp_str type string.

  me->out( xs( |/Producer (ionoPDF { me->gv_version })| ) ).

  IF me->gv_doc_properties-title IS NOT INITIAL.
    me->out( xs( |/Title ({ me->gv_doc_properties-title })| ) ).
  ENDIF.

  IF me->gv_doc_properties-subject IS NOT INITIAL.
    me->out( xs( |/Subject ({ me->gv_doc_properties-subject  })| ) ).
  ENDIF.

  IF me->gv_doc_properties-author IS NOT INITIAL.
    me->out( xs( |/Author ({ me->gv_doc_properties-author })| ) ).
  ENDIF.

  IF me->gv_doc_properties-keywords IS NOT INITIAL.
    me->out( xs( |/Keywords ({ me->gv_doc_properties-keywords })| ) ).
  ENDIF.

  IF me->gv_doc_properties-creator IS NOT INITIAL.
    me->out( xs( |/Creator ({ me->gv_doc_properties-creator })| ) ).
  ENDIF.

  GET TIME STAMP FIELD lv_timestamp.

  me->out( xs( |/CreationDate (D:{ me->to_string( lv_timestamp ) })| ) ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_PAGES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_pages.
  DATA lv_wpt  TYPE p LENGTH 16 DECIMALS 2.
  DATA lv_hpt  TYPE p LENGTH 16 DECIMALS 2.
  DATA lv_xstr TYPE xstring.
  DATA lv_xn   TYPE xstring.
  DATA lv_xn_2 TYPE xstring.
  DATA lv_str  TYPE string.
  DATA lv_kids_val TYPE i.
  DATA lv_kids     type string.
  DATA lv_compress TYPE abap_bool value abap_false.
  DATA lv_i        type i value 0.

  FIELD-SYMBOLS <page>   TYPE typ_page.
  FIELD-SYMBOLS <xstr>   TYPE xstring.
  field-symbols <offset> type i.

  lv_str = cl_abap_char_utilities=>newline.
  lv_xn = xs( lv_str ).
  lv_xn_2 = lv_xn.

  lv_wpt = me->gv_page_width * me->gv_scale_factor.
  lv_hpt = me->gv_page_height * me->gv_scale_factor.

  LOOP AT me->gt_pages ASSIGNING <page>.
    me->new_object( ).
    me->out( xs( |<</Type /Page| ) ).
    me->out( xs( |/Parent 1 0 R| ) ).
    me->out( xs( |/Resources 2 0 R| ) ).
    me->out( xs( |/Contents {  me->to_string( me->gv_object_number + 1 ) } 0 R>>| ) ).
    me->out( xs( |endobj| ) ).

    CLEAR lv_xstr.

    if lv_compress = abap_true.
*       @todo: add compression
    else.
      lv_xn = lv_xn_2.
      LOOP AT <page> ASSIGNING <xstr>.
        AT last.
          clear lv_xn.
        endat.

        CONCATENATE lv_xstr <xstr> lv_xn INTO lv_xstr IN BYTE MODE.
      ENDLOOP.
    endif.

    me->new_object( ).
    me->out( xs( |<</Length { me->to_string( xstrlen( lv_xstr ) ) }>>| ) ).
    me->put_stream( lv_xstr ).
    me->out( xs( |endobj| ) ).
  ENDLOOP.

  read table me->gt_offsets index 2 assigning <offset>.
  <offset> = me->gv_content_length.

  me->out( xs( |1 0 obj| ) ).
  me->out( xs( |<</Type /Pages| ) ).

  lv_i = 0.
  lv_kids = |/Kids [|.
  Loop at me->gt_pages assigning <page>.
    lv_kids     = |{ lv_kids }{ me->to_string( ( 3 + 2 * lv_i ) ) } 0 R |.
    lv_i = lv_i + 1.
  endloop.
  me->out( xs( |{ lv_kids }]| ) ).

  me->out( xs( |/Count { me->gv_page }| ) ).
  me->out( xs( |/MediaBox [0 0 { + me->to_string( lv_wpt ) } { lv_hpt }]| ) ).
  me->out( xs( |>>| ) ).
  me->out( xs( |endobj| ) ).


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_RESOURCES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_resources.
  FIELD-SYMBOLS <i> TYPE i.

  me->put_fonts( ).
  RAISE EVENT evt_put_resources.

  READ TABLE me->gt_offsets INDEX 3 ASSIGNING <i>.
  <i> = me->gv_content_length.

  me->out( xs( |2 0 obj| ) ).
  me->out( xs( |<<| ) ).
  me->put_resource_dictionary( ).
  me->out( xs( |>>| ) ).
  me->out( xs( |endobj| ) ).

  RAISE EVENT evt_post_put_resources.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_RESOURCE_DICTIONARY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD PUT_RESOURCE_DICTIONARY.
  FIELD-SYMBOLS <font> TYPE typ_font.

  me->out( xs( |/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]| ) ).
  me->out( xs( |/Font <<| ) ).

  LOOP AT me->gt_fonts ASSIGNING <font>.
    me->out( xs( |/{ <font>-id } { <font>-object_number } 0 R| ) ).
  ENDLOOP.

  me->out( xs( |>>| ) ).
  me->out( xs( |/XObject <<| ) ).
  me->put_xobject_dict( ).
  me->out( xs( |>>| ) ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_STREAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XSTR                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method PUT_STREAM.

  me->out( xs( |stream| ) ).
  me->out( iv_xstr ).
  me->out( xs( |endstream| ) ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_TRAILER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_trailer.
  me->out( xs( |/Size { gv_object_number + 1 }| ) ).
  me->out( xs( |/Root { gv_object_number } 0 R| ) ).
  me->out( xs( |/Info { gv_object_number - 1 } 0 R| ) ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->PUT_XOBJECT_DICT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD put_xobject_dict.
  RAISE EVENT evt_put_xobject_dict.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->RECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_X                           TYPE        I
* | [--->] IV_Y                           TYPE        I
* | [--->] IV_WIDTH                       TYPE        I
* | [--->] IV_HEIGTH                      TYPE        I
* | [--->] IV_STYLE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method RECT.
  data lv_op type string.

  lv_op = me->get_style( iv_style ).

  me->out( me->xs( |{ me->f2( iv_x * me->gv_scale_factor ) } | &
                   |{ me->f2( ( me->gv_page_height - iv_y ) * me->gv_scale_factor ) } | &
                   |{ me->f2( iv_width * me->gv_scale_factor ) } | &
                   |{ me->f2( iv_heigth * me->gv_scale_factor * -1 ) } re { lv_op }| ) ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->SET_FONT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FONT_NAME                   TYPE        STRING
* | [--->] IV_FONT_STYLE                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD set_font.
  me->gv_active_font_id = me->get_font_from_dictionary( iv_font_name = iv_font_name iv_font_style = iv_font_style ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->SET_FONT_SIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SIZE                        TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SET_FONT_SIZE.
  me->gv_active_font_size = iv_size.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->SHRINK_FONT_TO_FIT_BOX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* | [--->] IV_MAX_WIDTH                   TYPE        I
* | [--->] IV_MAX_HEIGHT                  TYPE        I
* | [<-()] RV_FONT_SIZE                   TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD SHRINK_FONT_TO_FIT_BOX.
  DATA lv_cur_font_size TYPE i.
  DATA lv_cur_height    TYPE i.
  DATA lt_lines TYPE zcl_pdf=>typ_text_lines.

  lv_cur_font_size = me->get_font_size( ).

  rv_font_size = lv_cur_font_size.

  DO.
    lt_lines = me->split_paragraph_to_lines( iv_string = iv_text iv_maxlen =  iv_max_width ).
    lv_cur_height = me->measure_lines_height( lt_lines ).
    IF lv_cur_height > iv_max_height - me->get_current_line_space( ).
      rv_font_size = rv_font_size - 1.
      me->set_font_size( rv_font_size ).
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  me->set_font_size( lv_cur_font_size ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->SPLIT_LONG_WORD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WORD                        TYPE        STRING
* | [--->] IT_CHAR_WIDTHS                 TYPE        TYP_CHAR_WIDTHS_TAB
* | [--->] IV_CUR_WIDTH                   TYPE        I
* | [--->] IV_MAXLEN                      TYPE        I
* | [<-()] RT_LINES                       TYPE        TYP_TEXT_LINES
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD split_long_word.
  DATA lv_work      TYPE string.
  DATA lv_cur_width TYPE i.
  DATA lv_index     TYPE i.

  FIELD-SYMBOLS <lv_width> TYPE i.
  FIELD-SYMBOLS <lv_line> TYPE string.

  lv_cur_width = iv_cur_width.

  LOOP AT it_char_widths ASSIGNING <lv_width>.
    IF lv_cur_width + <lv_width> <= iv_maxlen.
      lv_work = |{ lv_work }{ iv_word+lv_index(1) }|.
      lv_cur_width = lv_cur_width + <lv_width>.
    ELSE.
      IF lv_work IS NOT INITIAL.
        APPEND lv_work TO rt_lines.
        CLEAR lv_work.
        CLEAR lv_cur_width.
      ENDIF.
    ENDIF.
    lv_index = lv_index + 1.
  ENDLOOP.

  IF lv_work IS NOT INITIAL.
    APPEND lv_work TO rt_lines.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->SPLIT_PARAGRAPH_TO_LINES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STRING                      TYPE        STRING
* | [--->] IV_MAXLEN                      TYPE        I
* | [<-()] RT_LINES                       TYPE        TYP_TEXT_LINES
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD split_paragraph_to_lines.
  DATA lt_words         TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DATA lt_paragraphs    TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DATA lv_length        TYPE i.
  DATA lt_char_widths   TYPE typ_char_widths_tab.
  DATA lv_cur_word_len  TYPE i.
  DATA lv_space_width   TYPE i.
  DATA lv_line_length   TYPE i.
  DATA lv_maxlen        TYPE i.
  DATA lv_next_width    TYPE i.
  DATA lv_index         TYPE i VALUE 0.
  data lv_paragraph_index type i.
  data lv_paragraph_count type i.
*  DATA lt_lines         TYPE TABLE OF string.

  FIELD-SYMBOLS <lv_word>      TYPE string.
  FIELD-SYMBOLS <lv_cur_line>  TYPE string.
  FIELD-SYMBOLS <lv_paragraph> TYPE string.

  lv_maxlen = iv_maxlen * me->gv_scale_factor * 1000.

  lt_char_widths = me->get_char_widths( '> <' ).
  READ TABLE lt_char_widths INDEX 2 INTO lv_space_width.

  lv_space_width = lv_space_width.

  SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_paragraphs.
  lv_paragraph_count = lines( lt_paragraphs ).

  LOOP AT lt_paragraphs ASSIGNING <lv_paragraph>.
    lv_paragraph_index = lv_paragraph_index + 1.

    SPLIT <lv_paragraph> AT space INTO TABLE lt_words.
    lv_length = lines( lt_words ).

    clear lv_index.

    LOOP AT lt_words ASSIGNING <lv_word>.
      lv_index = lv_index + 1.

*   get measurements
      lt_char_widths  = me->get_char_widths( <lv_word> ).
      lv_cur_word_len = me->get_word_length( lt_char_widths ).

*   Calc how much space horizontally we will need for including the next word.
      IF lv_index = lv_length.
        lv_next_width =  lv_line_length + lv_cur_word_len.
      ELSE.
        lv_next_width = lv_line_length + lv_space_width + lv_cur_word_len.
      ENDIF.

*   Check if current word will exceed max length
      IF lv_next_width > lv_maxlen.
        IF lv_cur_word_len > lv_maxlen. " The word is longer than max length

          APPEND LINES OF me->split_long_word( iv_word        = <lv_word>
                                               it_char_widths = lt_char_widths
                                               iv_cur_width   = lv_line_length
                                               iv_maxlen      = lv_maxlen ) TO rt_lines.

          READ TABLE rt_lines INDEX lines( rt_lines ) ASSIGNING <lv_cur_line>.
          lt_char_widths = me->get_char_widths( <lv_cur_line> ).
          lv_line_length = me->get_word_length( lt_char_widths  ).

        ELSE. "Add to new line
          APPEND <lv_word> TO rt_lines ASSIGNING <lv_cur_line>.
          lv_line_length = lv_cur_word_len.
        ENDIF.
      ELSE. " Add word directly

*     If first word, create a new line
        IF <lv_cur_line> IS NOT ASSIGNED.
          APPEND <lv_word> TO rt_lines ASSIGNING <lv_cur_line>.
        ELSE.

*     Add the word.
          <lv_cur_line> = |{ <lv_cur_line> } { <lv_word> }|.
        ENDIF.

*     Update current line length
        lv_line_length = lv_line_length + lv_cur_word_len + lv_space_width.
      ENDIF.
    ENDLOOP.
    if lv_paragraph_index < lv_paragraph_count.
      UNASSIGN <lv_cur_line>.
      lv_line_length = 0.
    endif.
  ENDLOOP. "paragraphs



ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_PDF=>STRING_PAD_LEFT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STR                         TYPE        STRING
* | [--->] IV_PAD_CHAR                    TYPE        C
* | [--->] IV_LEN                         TYPE        I
* | [<-()] RV_RET                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD STRING_PAD_LEFT.
  DATA l_len     TYPE i.
  DATA l_num_chars TYPE i.

  l_len = strlen( iv_str ).

  l_num_chars =  iv_len - l_len.

  DO l_num_chars TIMES.
    CONCATENATE rv_ret iv_pad_char INTO rv_ret.
  ENDDO.

  CONCATENATE rv_ret iv_str INTO rv_ret.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* | [--->] IV_X                           TYPE        ANY
* | [--->] IV_Y                           TYPE        ANY
* | [<-()] RV_Y                           TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD text.
  DATA ls_encoder       TYPE typ_encoding_map.
  DATA lt_str           TYPE TABLE OF string.
  DATA lv_str           TYPE string.
  data lv_escaped_str   type string.
  DATA lt_xstr          TYPE TABLE OF xstring.
  DATA lv_xstr          TYPE xstring.
  DATA lv_n             TYPE string.
  DATA lv_x             TYPE p LENGTH 10 DECIMALS 2.
  DATA lv_y             TYPE p LENGTH 10 DECIMALS 2.

  FIELD-SYMBOLS <font>  TYPE typ_font.
  FIELD-SYMBOLS <str>   TYPE string.
  FIELD-SYMBOLS <xstr>  TYPE xstring.

  lv_n = cl_abap_char_utilities=>newline.

  READ TABLE me->gt_fonts WITH KEY id = me->gv_active_font_id ASSIGNING <font>.
  ls_encoder = me->get_sap_encoder( <font>-encoding ).

  lv_x = iv_x * me->gv_scale_factor.
  lv_y = ( me->gv_page_height - iv_y ) * me->gv_scale_factor.

  lv_str  = |BT{ lv_n }/| &
            |{ me->gv_active_font_id }| & | | &
            |{ me->gv_active_font_size }| &
            | Tf{ lv_n }| &  " font face, style, size
            |{ me->gv_active_font_size } TL{ lv_n }| & " line spacing
            |{ me->gv_text_color }| &
            |{ lv_n }| & |{ me->to_string( lv_x ) }| &
            | | & |{ me->to_string( lv_y ) }| & | Td{ lv_n }(|.

  lv_xstr = xs( lv_str ).

  APPEND lv_xstr TO lt_xstr.

  SPLIT iv_text AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_str.
  IF lines( lt_str ) > 1.
    LOOP AT lt_str ASSIGNING <str>.
      lv_escaped_str = me->escape_text( <str> ).
      lv_xstr = me->xs( iv_str = <str> iv_encoding = <font>-encoding ).
      APPEND lv_xstr TO lt_xstr.
      lv_xstr = xs( |) Tj{ lv_n }T* (| ).
      APPEND lv_xstr TO lt_xstr.
    ENDLOOP.
  ELSE.
    lv_escaped_str = me->escape_text( iv_text ).
    lv_xstr = me->xs( iv_str = lv_escaped_str iv_encoding = <font>-encoding ).
    APPEND lv_xstr TO lt_xstr.
  ENDIF.

  lv_xstr = xs( |) Tj{ lv_n }ET| ).
  APPEND lv_xstr TO lt_xstr.

  clear lv_xstr.
  LOOP AT lt_xstr assigning <xstr>.
    CONCATENATE lv_xstr <xstr> into lv_xstr in byte mode.
  endloop.

  me->out( lv_xstr ).

  rv_y = iv_y + me->get_current_line_space( ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->TEXT_BOX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* | [--->] IV_X                           TYPE        I
* | [--->] IV_Y                           TYPE        I
* | [--->] IV_WIDTH                       TYPE        I
* | [--->] IV_HEIGHT                      TYPE        I
* | [--->] IV_HOR_ALIGN                   TYPE        I (default =CONST_HOR_TEXT_ALIGN_LEFT)
* | [<-()] RV_END_Y                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD text_box.

  DATA lt_lines          TYPE zcl_pdf=>typ_text_lines.
  DATA lv_y              TYPE i.
  data lv_x              type i.
  FIELD-SYMBOLS <lv_str> TYPE string.

  lt_lines = me->split_paragraph_to_lines( iv_string = iv_text iv_maxlen = iv_width ).
  lv_y = iv_y + me->get_current_line_space( ).

  LOOP AT lt_lines ASSIGNING <lv_str>.
    CASE iv_hor_align.
      when CONST_HOR_TEXT_ALIGN_LEFT.
        lv_x = iv_x.
      when CONST_HOR_TEXT_ALIGN_MIDDLE.
        lv_x = iv_x + ( ( iv_width / 2 ) - ( ( get_string_width( <lv_str> ) / ( me->gv_scale_factor * 1000  ) ) / 2 ) ).
    ENDCASE.

    me->text( iv_text = <lv_str> iv_x = lv_x iv_y = lv_y ) .

    IF lv_y + me->get_current_line_space( ) > iv_y + iv_height.
      EXIT.
    ELSE.
      lv_y = lv_y + me->get_current_line_space( ).
    ENDIF.

  ENDLOOP.

  rv_end_y = lv_y.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PDF->TO_8BIT_STREAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD to_8bit_stream.

  DATA lv_i TYPE i.

* @TODO: add support for font metric plugins...

  lv_i = strlen( iv_text ).



ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PDF->TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [<-()] RV_STR                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method TO_STRING.
  rv_str = iv_value.
  condense rv_str.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PDF->WRITE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STR                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method WRITE.
  me->out( xs( iv_str ) ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PDF->XS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STR                         TYPE        STRING
* | [--->] IV_ENCODING                    TYPE        STRING (default ='WinAnsiEncoding')
* | [<-()] RV_XSTR                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD xs.
  DATA ls_sap_encoder TYPE typ_encoding_map.
  DATA lv_encoding TYPE string.



  lv_encoding = iv_encoding.
*  TRANSLATE lv_encoding TO UPPER CASE.

  ls_sap_encoder = me->get_sap_encoder( lv_encoding ).

  ls_sap_encoder-encoder->convert( EXPORTING data = iv_str IMPORTING buffer = rv_xstr ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_PDF->_ADD_PAGE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _ADD_PAGE.

  data lv_line_width type p length 16 decimals 2.

  lv_line_width = me->gv_line_width * me->gv_scale_factor.

  me->begin_page( ).
  me->out( xs( |{  me->to_string( lv_line_width ) } w| ) ).
  me->out( xs( me->gv_draw_color ) ).

  if me->gv_line_cap_id <> 0.
    out( xs( |{ me->to_string( me->gv_line_cap_id ) } J| ) ).
  endif.

  if gv_line_join_id <> 0.
    out( xs( |{ me->to_string( gv_line_Join_ID ) } j | ) ).
  endif.

  raise event evt_add_page exporting iv_page_no = me->gv_page.


endmethod.
ENDCLASS.
