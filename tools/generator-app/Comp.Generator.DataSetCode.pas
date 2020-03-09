{* ------------------------------------------------------------------------
 * ♥
 * ♥ DataSet to Delphi Code (create TFDMemTable with the data)
 * ♥
 * Component: TDSGenerator
 * Project: https://github.com/bogdanpolak/datasetToDelphiCode
 * ------------------------------------------------------------------------ }

unit Comp.Generator.DataSetCode;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

type
  TGeneratorMode = (genStructure, genAppend, genFunction, genUnit);
  TDataSetType = (dstFDMemTable, dstClientDataSet);
  TAppendMode = (amMultilineAppends, amSinglelineAppends);

  TDSGenerator = class(TComponent)
  public const
    Version = '1.4';
  private const
    MaxLiteralLenght = 70;
  private
    fCode: TStrings;
    fDataSet: TDataSet;
    fIndentationText: String;
    fGeneratorMode: TGeneratorMode;
    fDataSetType: TDataSetType;
    fAppendMode: TAppendMode;
    fUnitName: string;
    fMaxRows: integer;
    function GetDataFieldPrecision(fld: TField): integer;
    function GenerateOneAppend_Multiline: string;
    function GenerateOneAppend_Singleline: string;
  protected
    function GenerateLine_FieldDefAdd(fld: TField): string;
    function GenerateLine_SetFieldValue(fld: TField): string;
    function GenerateStructure: string;
    function GenerateOneAppend: string;
    function GenerateAppendsBlock: string;
    function FormatLongStringLiterals(const Literal: string): string;
    function GenerateUnitHeader: string;
    function GenerateUnitFooter: string;
    function GenerateFunction: string;
    function GenerateAll(aMode: TGeneratorMode): string;
    class function GenetateUnit(ds: TDataSet; const aUnitName: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    class function GenerateAsString(ds: TDataSet): string;
    class function GenerateAsArray(ds: TDataSet): TArray<String>;
    class procedure GenerateAndSaveToStream(ds: TDataSet; aStream: TStream);
    class procedure GenerateAndSaveToFile(ds: TDataSet;
      const aFileName: string);
    class procedure GenerateAndSaveClipboard(ds: TDataSet);
  published
    property DataSet: TDataSet read fDataSet write fDataSet;
    property Code: TStrings read fCode;
    property IndentationText: String read fIndentationText
      write fIndentationText;
    property GeneratorMode: TGeneratorMode read fGeneratorMode
      write fGeneratorMode;
    property DataSetType: TDataSetType read fDataSetType write fDataSetType;
    property AppendMode: TAppendMode read fAppendMode write fAppendMode;
    property NameOfUnit: string read fUnitName write fUnitName;
    property MaxRows: integer read fMaxRows write fMaxRows;
  end;

implementation

uses
  System.Rtti,
  Vcl.Clipbrd;

constructor TDSGenerator.Create(AOwner: TComponent);
begin
  inherited;
  // --------------------------------
  // Default options
  fGeneratorMode := genFunction;
  fDataSetType := dstFDMemTable;
  fAppendMode := amMultilineAppends;
  fIndentationText := '  ';
  fUnitName := 'uSampleDataSet';
  fMaxRows := 100;
  // --------------------------------
  fCode := TStringList.Create;
end;

destructor TDSGenerator.Destroy;
begin
  fCode.Free;
  inherited;
end;

function FieldTypeToString(ft: TFieldType): string;
begin
  Result := System.Rtti.TRttiEnumerationType.GetName(ft);
end;

function TDSGenerator.GetDataFieldPrecision(fld: TField): integer;
begin
  System.Assert((fld is TBCDField) or (fld is TFMTBCDField) or
    (fld is TFloatField));
  if fld is TBCDField then
    Result := (fld as TBCDField).Precision
  else if fld is TFMTBCDField then
    Result := (fld as TFMTBCDField).Precision
  else
    Result := (fld as TFloatField).Precision
end;

function TDSGenerator.GenerateLine_FieldDefAdd(fld: TField): string;
begin
  (* -----------------------------------------------------------------------
    [Doc]
   TFieldType = ( ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
   ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
   ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
   ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
   ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
   ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd,
   ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval,
   ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream,
   ftTimeStampOffset, ftObject, ftSingle);
   ------------------------------------------------------------------------- *)
  if fld.DataType in [ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint,
    ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime] then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ');'
  else if (fld.DataType in [ftBCD, ftFMTBcd]) then
    Result := 'with FieldDefs.AddFieldDef do begin' + sLineBreak +
      fIndentationText + '    ' +
      Format('Name := ''%s'';  DataType := %s;  Precision := %d;  Size := %d;',
      [fld.FieldName, FieldTypeToString(fld.DataType),
      GetDataFieldPrecision(fld), fld.Size]) + sLineBreak + fIndentationText
      + '  end;'
  else if (fld.DataType in [ftString, ftWideString]) and (fld.Size > 9999) then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', 100);'
  else if (fld.DataType in [ftString, ftWideString]) then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', ' + fld.Size.ToString + ');'
  else
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', ' + fld.Size.ToString + ');';
end;

function FloatToCode(val: Extended): string;
begin
  Result := FloatToStr(val);
  Result := StringReplace(Result, ',', '.', []);
end;

function DateToCode(dt: TDateTime): string;
var
  y, m, d: word;
begin
  DecodeDate(dt, y, m, d);
  Result := Format('EncodeDate(%d,%d,%d)', [y, m, d]);
end;

function TimeToCode(dt: TDateTime): string;
var
  h, min, s, ms: word;
begin
  DecodeTime(dt, h, min, s, ms);
  Result := Format('EncodeTime(%d,%d,%d,%d)', [h, min, s, ms]);
end;

function DateTimeToCode(dt: TDateTime): string;
begin
  Result := DateToCode(dt);
  if Frac(dt) > 0 then
    Result := Result + '+' + TimeToCode(dt);
end;

function TDSGenerator.FormatLongStringLiterals(const Literal: string): string;
var
  s1: string;
  s2: string;
begin
  if Length(Literal) <= MaxLiteralLenght then
  begin
    Result := Literal
  end
  else
  begin
    s1 := Literal;
    s2 := sLineBreak;
    while s1 <> '' do
    begin
      if Length(s1) < MaxLiteralLenght then
      begin
        s2 := s2 + fIndentationText + fIndentationText + s1;
        s1 := '';
      end
      else
      begin
        s2 := s2 + fIndentationText + fIndentationText +
          s1.Substring(0, MaxLiteralLenght - 1) + '''+' + sLineBreak;
        s1 := '''' + s1.Substring(MaxLiteralLenght - 1);
      end;
    end;
    Result := s2;
  end;
end;

function TDSGenerator.GenerateLine_SetFieldValue(fld: TField): string;
var
  sByNameValue: string;
begin
  Result := '';
  if not(fld.IsNull) then
  begin
    sByNameValue := 'FieldByName(' + QuotedStr(fld.FieldName) + ').Value';
    case fld.DataType of
      ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint:
        Result := sByNameValue + ' := ' + fld.AsString + ';';
      ftBoolean:
        Result := sByNameValue + ' := ' + BoolToStr(fld.AsBoolean, true) + ';';
      ftFloat, ftCurrency, ftBCD, ftFMTBcd:
        Result := sByNameValue + ' := ' + FloatToCode(fld.AsExtended) + ';';
      ftDate:
        Result := sByNameValue + ' := ' + DateToCode(fld.AsDateTime) + ';';
      ftTime:
        Result := sByNameValue + ' := ' + TimeToCode(fld.AsDateTime) + ';';
      ftDateTime:
        Result := sByNameValue + ' := ' + DateTimeToCode(fld.AsDateTime) + ';';
      ftString, ftWideString:
        Result := sByNameValue + ' := ' + FormatLongStringLiterals
          (QuotedStr(fld.Value)) + ';';
    end;
  end;
end;

function TDSGenerator.GenerateStructure: string;
var
  fld: TField;
  sDataSetCreate: string;
  sFieldDefinitions: string;
begin
  case fDataSetType of
    dstFDMemTable:
      sDataSetCreate := 'TFDMemTable.Create(AOwner)';
    dstClientDataSet:
      sDataSetCreate := 'TClientDataSet.Create(AOwner)';
  end;
  sFieldDefinitions := '';
  if fDataSet <> nil then
    for fld in fDataSet.Fields do
      sFieldDefinitions := sFieldDefinitions +
      {} fIndentationText + fIndentationText + GenerateLine_FieldDefAdd(fld) +
        sLineBreak;
  Result :=
  {} fIndentationText + 'ds := ' + sDataSetCreate + ';' + sLineBreak +
  {} fIndentationText + 'with ds do' + sLineBreak +
  {} fIndentationText + 'begin' + sLineBreak +
  {} sFieldDefinitions +
  {} fIndentationText + fIndentationText + 'CreateDataSet;' + sLineBreak +
  {} fIndentationText + 'end;' + sLineBreak
end;

function TDSGenerator.GenerateOneAppend_Multiline: string;
var
  fld: TField;
  s1: string;
  sl: TStringList;
begin
  if (fDataSet = nil) or (fDataSet.Fields.Count = 0) then
    Exit('');
  sl := TStringList.Create;
  try
    sl.Add(fIndentationText + 'ds.Append;');
    for fld in fDataSet.Fields do
    begin
      s1 := GenerateLine_SetFieldValue(fld);
      if s1 <> '' then
        sl.Add(fIndentationText + 'ds.' + s1);
    end;
    sl.Add(fIndentationText + 'ds.Post;');
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TDSGenerator.GenerateOneAppend_Singleline: string;
var
  sFieldsValues: string;
  fld: TField;
  s1: string;
begin
  if (fDataSet = nil) or (fDataSet.Fields.Count = 0) then
    Exit('');
  sFieldsValues := '';
  for fld in fDataSet.Fields do
  begin
    if fld.IsNull then
      s1 := 'Null'
    else
      case fld.DataType of
        ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint:
          s1 := fld.AsString;
        ftBoolean:
          s1 := BoolToStr(fld.AsBoolean, true);
        ftFloat, ftCurrency, ftBCD, ftFMTBcd:
          s1 := FloatToCode(fld.AsExtended);
        ftDate:
          s1 := DateToCode(fld.AsDateTime);
        ftTime:
          s1 := TimeToCode(fld.AsDateTime);
        ftDateTime:
          s1 := DateTimeToCode(fld.AsDateTime);
        ftString, ftWideString:
          s1 := FormatLongStringLiterals(QuotedStr(fld.Value));
      end;
    if sFieldsValues = '' then
      sFieldsValues := s1
    else
      sFieldsValues := sFieldsValues + ', ' + s1;
  end;
  Result := fIndentationText + 'ds.AppendRecord([' + sFieldsValues + ']);' +
    sLineBreak;
end;

function TDSGenerator.GenerateOneAppend: string;
begin
  case fAppendMode of
    amMultilineAppends:
      Result := GenerateOneAppend_Multiline;
    amSinglelineAppends:
      Result := GenerateOneAppend_Singleline;
  else
    Result := '';
  end;
end;

function TDSGenerator.GenerateAppendsBlock: string;
var
  sDataAppend: string;
  aBookmark: TBookmark;
  aRowCounter: integer;
begin
  if (fDataSet = nil) or (fDataSet.Fields.Count = 0) then
    Exit('');
  if fMaxRows = 0 then
    aRowCounter := MaxInt
  else
    aRowCounter := fMaxRows;
  sDataAppend := '';
  if DataSet <> nil then
  begin
    DataSet.DisableControls;
    try
      DataSet.Active := true;
      aBookmark := DataSet.GetBookmark;
      try
        DataSet.First;
        while not DataSet.Eof and (aRowCounter > 0) do
        begin
          sDataAppend := sDataAppend + GenerateOneAppend;
          dec(aRowCounter);
          DataSet.Next;
        end;
      finally
        DataSet.GotoBookmark(aBookmark);
        DataSet.FreeBookmark(aBookmark);
      end;
    finally
      DataSet.EnableControls;
    end;
  end;

  Result :=
  {} sDataAppend +
  {} fIndentationText + 'ds.First;' + sLineBreak;
end;

function TDSGenerator.GenerateUnitHeader: string;
var
  sDataSetUnits: string;
begin
  case fDataSetType of
    dstFDMemTable:
      sDataSetUnits := fIndentationText + 'FireDAC.Comp.Client;';
    dstClientDataSet:
      sDataSetUnits :=
      {} fIndentationText + 'Datasnap.DBClient;'#13#10 +
      {} fIndentationText + 'MidasLib;';
  end;
  Result :=
  {} 'unit ' + fUnitName + ';' + sLineBreak +
  {} sLineBreak +
  {} 'interface' + sLineBreak +
  {} sLineBreak +
  {} 'uses' + sLineBreak +
  {} fIndentationText + 'System.Classes,' + sLineBreak +
  {} fIndentationText + 'System.SysUtils,' + sLineBreak +
  {} fIndentationText + 'System.Variants,' + sLineBreak +
  {} fIndentationText + 'Data.DB,' + sLineBreak +
  {} sDataSetUnits + sLineBreak +
  {} sLineBreak +
  {} 'function GivenDataSet (aOwner: TComponent): TDataSet;' + sLineBreak +
  {} sLineBreak +
  {} 'implementation' + sLineBreak +
  {} sLineBreak;
end;

function TDSGenerator.GenerateFunction: string;
var
  aClassName: string;
begin
  case fDataSetType of
    dstFDMemTable:
      aClassName := 'TFDMemTable';
    dstClientDataSet:
      aClassName := 'TClientDataSet';
  end;
  Result :=
  {} 'function GivenDataSet (aOwner: TComponent): TDataSet;' + sLineBreak +
  {} 'var' + sLineBreak +
  {} '  ds: ' + aClassName + ';' + sLineBreak +
  {} 'begin' + sLineBreak +
  {} GenerateStructure() +
  {} GenerateAppendsBlock() +
  {} '  Result := ds;' + sLineBreak +
  {} 'end;' + sLineBreak;
end;

function TDSGenerator.GenerateUnitFooter(): string;
begin
  Result := sLineBreak + 'end.' + sLineBreak;
end;

function TDSGenerator.GenerateAll(aMode: TGeneratorMode): string;
begin
  case aMode of
    genStructure:
      Result := GenerateStructure();
    genAppend:
      Result := GenerateAppendsBlock();
    genUnit:
      Result := GenerateUnitHeader + GenerateFunction + GenerateUnitFooter;
    genFunction:
      Result := GenerateFunction;
  else
    Result := '// Unsupported generator mode';
  end;
end;

procedure TDSGenerator.Execute;
begin
  fCode.Text := GenerateAll(fGeneratorMode);
end;

class function TDSGenerator.GenerateAsString(ds: TDataSet): string;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := gen.Code.Text;
  finally
    gen.Free;
  end;
end;

function TStringsToArray(sl: TStrings): TArray<String>;
var
  i: integer;
begin
  SetLength(Result, sl.Count);
  for i := 0 to sl.Count - 1 do
    Result[i] := sl[i];
end;

class function TDSGenerator.GenerateAsArray(ds: TDataSet): TArray<String>;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := TStringsToArray(gen.Code);
  finally
    gen.Free;
  end;
end;

class function TDSGenerator.GenetateUnit(ds: TDataSet;
  const aUnitName: string): string;
var
  aGenerator: TDSGenerator;
begin
  aGenerator := TDSGenerator.Create(nil);
  try
    aGenerator.DataSet := ds;
    aGenerator.fUnitName := aUnitName;
    Result := aGenerator.GenerateAll(genUnit);
  finally
    aGenerator.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveToStream(ds: TDataSet;
  aStream: TStream);
var
  sCode: Utf8String;
begin
  sCode := Utf8String(GenetateUnit(ds, 'uSampleDataSet'));
  aStream.Write(sCode[1], Length(sCode));
end;

function GetUnitName_FromFilePath(aFilePath: string): string;
var
  aUnitName: string;
  aExtentionLen: integer;
begin
  aUnitName := ExtractFileName(aFilePath);
  aExtentionLen := Length(ExtractFileExt(aFilePath));
  if aExtentionLen > 0 then
    Result := aUnitName.Substring(0, aUnitName.Length - aExtentionLen)
  else
    Result := aUnitName;
end;

class procedure TDSGenerator.GenerateAndSaveToFile(ds: TDataSet;
  const aFileName: string);
var
  fs: TFileStream;
  sCode: Utf8String;
  aUnitName: string;
begin
  aUnitName := GetUnitName_FromFilePath(aFileName);
  sCode := Utf8String(GenetateUnit(ds, aUnitName));
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    {
     aFilePreamble := TEncoding.UTF8.GetPreamble;
     aStream.Write(aFilePreamble[0], Length(aFilePreamble));
    }
    fs.Write(sCode[1], Length(sCode));
  finally
    fs.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveClipboard(ds: TDataSet);
var
  aGenerator: TDSGenerator;
begin
  aGenerator := TDSGenerator.Create(nil);
  try
    aGenerator.DataSet := ds;
    Clipboard.AsText := aGenerator.GenerateFunction;
  finally
    aGenerator.Free;
  end;
end;

end.
