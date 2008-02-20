{*******************************************************************
*  General Turbo Circuit file structure:
*
*  Offset   Length    Description
*
*     0      len      Identification string: STR_TCFILE_IDENTIFIER
*   len      var      Sequence of records
*
*  Description of a tipical record:
*
*     0        1      Record ID
*     1        1      Record version
*     2        2      Data size (sz)
*     4        sz
*
*******************************************************************}
unit tcfileformat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  constants;

{*******************************************************************
*  Turbo Circuit file format constants
*******************************************************************}
var
  STR_TCFILE_IDENTIFIER: string = 'Turbo Circuit File ID string';

const
  INT_TCFILE_IDENTIFIER_SIZE   = 28;

  TCRECORD_BOF                 = $01;
  TCRECORD_GUI_DATA            = $02;
  TCRECORD_DOC_DATA            = $03;
  TCRECORD_COMPONENT           = $04;
  TCRECORD_WIRE                = $05;
  TCRECORD_TEXT                = $06;
  TCRECORD_EOF                 = $0F;

{  BOF record constants }
  TCRECORD_BOF_SIZE            = $00;
  TCRECORD_BOF_VER             = $00;

{  GUI_DATA record constants }
  TCRECORD_GUI_DATA_SIZE       = $00;
  TCRECORD_GUI_DATA_VER        = $00;

{  SCHEMATICS_DOC_DATA record constants }
  TCRECORD_DOC_DATA_SIZE       = $00;
  TCRECORD_DOC_DATA_VER        = $00;

{  COMPONENT record constants }
  TCRECORD_COMPONENT_SIZE      = SizeOf(TCComponent);
  TCRECORD_COMPONENT_VER       = $00;

{  WIRE record constants }
  TCRECORD_WIRE_SIZE           = SizeOf(TCWire);
  TCRECORD_WIRE_VER            = $00;

{  TEXT record constants }
  TCRECORD_TEXT_SIZE           = SizeOf(TCText);
  TCRECORD_TEXT_VER            = $00;

{  EOF record constants }
  TCRECORD_EOF_SIZE            = $00;
  TCRECORD_EOF_VER             = $00;


implementation

end.

