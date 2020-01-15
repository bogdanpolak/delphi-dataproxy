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
  TGeneratorMode = (genAll, genStructure, genAppend, genUnit, genFunction);
  TDataSetType = (dstFDMemTable, dstClientDataSet);
  TAppendMode = (amMultilineAppends, amSinglelineAppends);

  TDSGenerator = class(TComponent)
  const
    // * --------------------------------------------------------------------
    // * Signature
    ReleaseVersion = '1.3';
    // * --------------------------------------------------------------------
    MaxLiteralLenght = 70;
  private
    FCode: TStrings;
    FStructureCode: TStringList;
    FAppendCode: TStringList;
    FTempCode: TStringList;
    FDataSet: TDataSet;
    FIndentationText: String;
    FGeneratorMode: TGeneratorMode;
    FDataSetType: TDataSetType;
    FAppendMode: TAppendMode;
    procedure Guard;
    function GetDataFieldPrecision(fld: TField): integer;
    procedure GenerateOneAppend_Multiline(aFields: TFields);
    procedure GenerateOneAppend_Singleline(aFields: TFields);
  protected
    function GenerateLine_FieldDefAdd(fld: TField): string;
    function GenerateLine_SetFieldValue(fld: TField): string;
    procedure GenerateStructure(dataSet: TDataSet);
    procedure GenerateOneAppend(aFields: TFields);
    procedure GenerateAppendsBlock(dataSet: TDataSet);
    function FormatLongStringLiterals(const Literal: string): string;
    function GenerateUnitHeader(const aUnitName: string): string;
    function GenerateUnitFooter(): string;
    function GenerateFunction: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    class function GenerateAsString(ds: TDataSet): string;
    class function GenerateAsArray(ds: TDataSet): TStringDynArray;
    class procedure GenerateAndSaveToStream(ds: TDataSet; aStream: TStream);
    class procedure GenerateAndSaveToFile(ds: TDataSet;
      const aFileName: string);
    class procedure GenerateAndSaveClipboard(ds: TDataSet);
  published
    property dataSet: TDataSet read FDataSet write FDataSet;
    property Code: TStrings read FCode;
    property IndentationText: String read FIndentationText
      write FIndentationText;
    property GeneratorMode: TGeneratorMode read FGeneratorMode
      write FGeneratorMode;
    property DataSetType: TDataSetType read FDataSetType write FDataSetType;
    property AppendMode: TAppendMode read FAppendMode write FAppendMode;
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
  FGeneratorMode := genAll;
  FDataSetType := dstFDMemTable;
  FAppendMode := amMultilineAppends;
  FIndentationText := '  ';
  // --------------------------------
  FCode := TStringList.Create;
  FStructureCode := TStringList.Create;
  FAppendCode := TStringList.Create;
  FTempCode := TStringList.Create;
end;

destructor TDSGenerator.Destroy;
begin
  FCode.Free;
  FStructureCode.Free;
  FAppendCode.Free;
  FTempCode.Free;
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
      IndentationText + '    ' +
      Format('Name := ''%s'';  DataType := %s;  Precision := %d;  Size := %d;',
      [fld.FieldName, FieldTypeToString(fld.DataType),
      GetDataFieldPrecision(fld), fld.Size]) + sLineBreak + IndentationText
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
        s2 := s2 + IndentationText + '    ' + s1;
        s1 := '';
      end
      else
      begin
        s2 := s2 + IndentationText + IndentationText + IndentationText +
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

procedure TDSGenerator.Guard;
begin
  Assert(dataSet <> nil, 'Property DataSet not assigned!');
end;

procedure TDSGenerator.GenerateStructure(dataSet: TDataSet);
var
  fld: TField;
  sDataSetCreate: string;
begin
  case FDataSetType of
    dstFDMemTable:
      sDataSetCreate := 'TFDMemTable.Create(AOwner)';
    dstClientDataSet:
      sDataSetCreate := 'TClientDataSet.Create(AOwner)';
  end;
  with FStructureCode do
  begin
    Add(IndentationText + 'ds := ' + sDataSetCreate + ';');
    Add(IndentationText + 'with ds do');
    Add(IndentationText + 'begin');
    for fld in dataSet.Fields do
      Add(IndentationText + IndentationText + GenerateLine_FieldDefAdd(fld));
    Add(IndentationText + IndentationText + 'CreateDataSet;');
    Add(IndentationText + 'end;');
  end;
end;

procedure TDSGenerator.GenerateOneAppend_Multiline(aFields: TFields);
var
  fld: TField;
  s1: string;
begin
  with FAppendCode do
  begin
    Add(IndentationText + 'with ds do');
    Add(IndentationText + 'begin');
    Add(IndentationText + IndentationText + 'Append;');
    for fld in aFields do
    begin
      s1 := GenerateLine_SetFieldValue(fld);
      if s1 <> '' then
        Add(IndentationText + IndentationText + s1);
    end;
    Add(IndentationText + IndentationText + 'Post;');
    Add(IndentationText + 'end;');
  end;
end;

procedure TDSGenerator.GenerateOneAppend_Singleline(aFields: TFields);
var
  sFieldsValues: string;
  fld: TField;
  s1: string;
begin
  sFieldsValues := '';
  for fld in aFields do
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
  FAppendCode.Add(IndentationText + 'ds.AppendRecord([' + sFieldsValues
    + ']);');
end;

procedure TDSGenerator.GenerateOneAppend(aFields: TFields);
begin
  case FAppendMode of
    amMultilineAppends:
      GenerateOneAppend_Multiline(aFields);
    amSinglelineAppends:
      GenerateOneAppend_Singleline(aFields);
  end;
end;

procedure TDSGenerator.GenerateAppendsBlock(dataSet: TDataSet);
begin
  FAppendCode.Add('{$REGION ''Append data''}');
  dataSet.DisableControls;
  dataSet.Open;
  dataSet.First;
  while not dataSet.Eof do
  begin
    GenerateOneAppend(dataSet.Fields);
    dataSet.Next;
  end;
  dataSet.EnableControls;
  FAppendCode.Add(IndentationText + 'ds.First;');
  FAppendCode.Add('{$ENDREGION}');
end;

function TDSGenerator.GenerateUnitHeader(const aUnitName: string): string;
var
  sDataSetUnits: string;
begin
  case FDataSetType of
    dstFDMemTable:
      sDataSetUnits := IndentationText + 'FireDAC.Comp.Client;';
    dstClientDataSet:
      sDataSetUnits :=
      (* *) IndentationText + 'Datasnap.DBClient;'#13#10 +
      (* *) IndentationText + 'MidasLib;';
  end;
  FTempCode.Clear;
  with FTempCode do
  begin
    Add('unit ' + aUnitName + ';');
    Add('');
    Add('interface');
    Add('');
    Add('uses');
    Add(IndentationText + 'System.Classes,');
    Add(IndentationText + 'System.SysUtils,');
    Add(IndentationText + 'System.Variants,');
    Add(IndentationText + 'Data.DB,');
    Add(sDataSetUnits);
    Add('');
    Add('function CreateDataSet (aOwner: TComponent): TDataSet;');
    Add('');
    Add('implementation');
    Add('');
  end;
  Result := FTempCode.Text;
end;

function TDSGenerator.GenerateFunction(): string;
begin
  FTempCode.Clear;
  with FTempCode do
  begin
    Add('function CreateDataSet (aOwner: TComponent): TDataSet;');
    Add('var');
    Add('  ds: TFDMemTable;');
    Add('begin');
    AddStrings(FStructureCode);
    AddStrings(FAppendCode);
    Add('  Result := ds;');
    Add('end;');
  end;
  Result := FTempCode.Text;
end;

function TDSGenerator.GenerateUnitFooter(): string;
begin
  FTempCode.Clear;
  with FTempCode do
  begin
    Add('');
    Add('end.');
  end;
  Result := FTempCode.Text;
end;

procedure TDSGenerator.Execute;
begin
  Guard;
  FStructureCode.Clear;
  FAppendCode.Clear;
  GenerateStructure(dataSet);
  GenerateAppendsBlock(dataSet);
  case FGeneratorMode of
    genAll:
      FCode.Text := FStructureCode.Text + FAppendCode.Text;
    genStructure:
      FCode.Text := FStructureCode.Text;
    genAppend:
      FCode.Text := FAppendCode.Text;
    genUnit:
      FCode.Text := GenerateUnitHeader('uSampleDataSet') + GenerateFunction +
        GenerateUnitFooter;
    genFunction:
      FCode.Text := GenerateFunction;
  end;
end;

class function TDSGenerator.GenerateAsString(ds: TDataSet): string;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.dataSet := ds;
    gen.Execute;
    Result := gen.Code.Text;
  finally
    gen.Free;
  end;
end;

(* This function replace (fix) standard  Delphi RTL TStrings.ToStringArray
  .    method, because ot the following issue (in Delphi XE8 and older ones):
 Delphi 10.3 Rio:
 .   - TStringDynArray = TArray<string>;
 .   - function TStrings.ToStringArray: TArray<string>;
 .   - can assign: TArray<string> --> TStringDynArray
 Delphi XE8:
 .   - TStringDynArray = array of string;
 .   - function TStrings.ToStringArray: array of string;
 .   - not able to assign: array of string --> TStringDynArray
*)
function TStringsToStringDynArray(sl: TStrings): TStringDynArray;
var
  i: integer;
begin
  SetLength(Result, sl.Count);
  for i := 0 to sl.Count - 1 do
    Result[i] := sl[i];
end;

class function TDSGenerator.GenerateAsArray(ds: TDataSet): TStringDynArray;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.dataSet := ds;
    gen.Execute;
    Result := TStringsToStringDynArray(gen.Code);
  finally
    gen.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveToStream(ds: TDataSet;
  aStream: TStream);
var
  gen: TDSGenerator;
  sCode: Utf8String;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.dataSet := ds;
    gen.GeneratorMode := genUnit;
    gen.Execute;
    sCode := Utf8String(gen.Code.Text);
    {
     aFilePreamble := TEncoding.UTF8.GetPreamble;
     aStream.Write(aFilePreamble[0], Length(aFilePreamble));
    }
    aStream.Write(sCode[1], Length(sCode));
  finally
    gen.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveToFile(ds: TDataSet;
  const aFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    GenerateAndSaveToStream(ds, fs);
  finally
    fs.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveClipboard(ds: TDataSet);
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.dataSet := ds;
    gen.GeneratorMode := genFunction;
    gen.Execute;
    Clipboard.AsText := gen.Code.Text;
  finally
    gen.Free;
  end;
end;

end.
