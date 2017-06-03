# zcl_pdf
A pure stand alone ABAP class for creating PDFs in SAP on any ABAP system

I created this class on my spare time on one of my projects. We ended up
using the class in the project instead of using the built in (non ADS )
capabilities in the abap stack which is very limited. IF you want more
flexibility, your option is usually to go for ADS (Adobe Document Server). If
ADS is not available, this class could be an alternative.

The class supports only basic functionality which I needed at the time, but
it should be fairly simple to add more.

Currently, the class is capable of:
* Create text in the PDF built in fonts
  * Helvetica
  * Times
  * Courier
* Measure text strings (for the supported fonts)
* Fit text within a defined box constrain with largest possible font size
* Draw rectangles
* Get the PDF as a raw string which can be stored as files or sent as http
  responses or shown directly in SAP GUI (see example).

To use:
1. Create a class named ZCL_PDF
2. Change to "Source Code Based" editing
3. Past in the code from the file zcl_abap.abap
4. Activate class

You can switch back to Form based coding if that is what you prefer.


To test:
See the report ZTEST_ZCL_PDF to see an example on how create a simple pdf and
display the pdf within SAP GUI.
