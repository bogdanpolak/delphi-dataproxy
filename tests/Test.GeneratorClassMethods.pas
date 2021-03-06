unit Test.GeneratorClassMethods;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB,
  FireDAC.Comp.Client,

  Comp.Generator.DataProxy,
  Helper.DUnitAssert;

{$TYPEINFO ON}

type

  [TestFixture]
  TestGeneratorClassMethods = class(TObject)
  private
    fOwner: TComponent;
    fTemporaryFileName: string;
    fStringList: TStringList;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // ---
    procedure SaveToFile_IsFileExists;
    procedure SaveToFile_CheckUnitName;
    procedure SaveToFile_CheckAllUnit;
    procedure SaveToFile_CheckIndetnationAndNamingStyle;
    procedure SaveToFile_DiffrentUnitNameAndNameOfClass;
    // ---
    procedure SaveToClipboard_ClipboardNotEmpty;
    procedure SaveToClipboard_CheckClipboardText;
    procedure SaveToClipboard_IndetationAndNamingStyle;
  end;

{$TYPEINFO OFF}

implementation

uses
  System.IOUtils,
  Vcl.Clipbrd;

// -----------------------------------------------------------------------
// Utulities
// -----------------------------------------------------------------------

function GivenDataSet_HistoricalEvents(aOwner: TComponent): TDataSet;
var
  memTable: TFDMemTable;
begin
  memTable := TFDMemTable.Create(aOwner);
  with memTable do
  begin
    FieldDefs.Add('EventID', ftInteger);
    FieldDefs.Add('Event', ftWideString, 50);
    FieldDefs.Add('Date', ftDate);
    FieldDefs.Add('Expirence', ftFloat);
    FieldDefs.Add('Income', ftCurrency);
    CreateDataSet;
    AppendRecord([1, 'Liberation of Poland', EncodeDate(1989, 06, 04),
      1.2, 120]);
    AppendRecord([2, 'Battle of Vienna', EncodeDate(1683, 09, 12),
      System.Variants.Null, Null]);
    First;
  end;
  Result := memTable;
end;

function GivenDataSet_MiniHistoricalEvents(aOwner: TComponent): TDataSet;
var
  memTable: TFDMemTable;
begin
  memTable := TFDMemTable.Create(aOwner);
  with memTable do
  begin
    FieldDefs.Add('EventID', ftInteger);
    FieldDefs.Add('Event', ftWideString, 50);
    FieldDefs.Add('Date', ftDate);
    CreateDataSet;
    AppendRecord([1, 'Liberation of Poland', EncodeDate(1989, 06, 04)]);
    AppendRecord([2, 'Battle of Vienna', EncodeDate(1683, 09, 12)]);
    First;
  end;
  Result := memTable;
end;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGeneratorClassMethods.Setup;
begin
  fOwner := TComponent.Create(nil);
  fStringList := TStringList.Create;
  fTemporaryFileName := '';
  Clipboard.Clear;
end;

procedure TestGeneratorClassMethods.TearDown;
begin
  fStringList.Free;
  fOwner.Free;
  if (fTemporaryFileName <> '') and FileExists(fTemporaryFileName) then
    DeleteFile(fTemporaryFileName);
end;

// -----------------------------------------------------------------------
// Tests: SaveToFile
// -----------------------------------------------------------------------

procedure TestGeneratorClassMethods.SaveToFile_IsFileExists;
begin
  fTemporaryFileName := TPath.GetTempPath + 'HistoricalEvents1.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents');

  Assert.IsTrue(FileExists(fTemporaryFileName),
    Format('Expected temporary file is not exist (%s)', [fTemporaryFileName]));
end;

procedure TestGeneratorClassMethods.SaveToFile_CheckUnitName;
begin
  fTemporaryFileName := TPath.GetTempPath + 'Proxy.HistoricalEvents.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'TEventsProxy');

  fStringList.LoadFromFile(fTemporaryFileName);

  Assert.AreEqual('unit Proxy.HistoricalEvents;', fStringList[0]);
end;

procedure TestGeneratorClassMethods.SaveToFile_CheckAllUnit;
begin
  fTemporaryFileName := TPath.GetTempPath + 'Proxy.HistoricalEvents.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'THistoricalEventsProxy');

  fStringList.LoadFromFile(fTemporaryFileName);

  Assert.AreMemosEqual(
  {} 'unit Proxy.HistoricalEvents;'#13 +
  {} sLineBreak +
  {} 'interface'#13 +
  {} sLineBreak +
  {} 'uses'#13 +
  {} '  Data.DB,'#13 +
  {} '  Data.DataProxy,'#13 +
  {} '  System.SysUtils,'#13 +
  {} '  System.Classes,'#13 +
  {} '  FireDAC.Comp.Client;'#13 +
  {} sLineBreak +
  {} 'type'#13 +
  {} '  THistoricalEventsProxy = class(TDatasetProxy)'#13 +
  {} '  private'#13 +
  {} '    FEventID :TIntegerField;'#13 +
  {} '    FEvent :TWideStringField;'#13 +
  {} '    FDate :TDateField;'#13 +
  {} '  protected'#13 +
  {} '    procedure ConnectFields; override;'#13 +
  {} '  public'#13 +
  {} '    property EventID :TIntegerField read FEventID;'#13 +
  {} '    property Event :TWideStringField read FEvent;'#13 +
  {} '    property Date :TDateField read FDate;'#13 +
  {} '  end;'#13 +
  {} sLineBreak +
  {} 'implementation'#13 +
  {} sLineBreak +
  {} 'procedure THistoricalEventsProxy.ConnectFields;'#13 +
  {} 'const'#13 +
  {} '  ExpectedFieldCount = 3;'#13 +
  {} 'begin'#13 +
  {} '  FEventID := FDataSet.FieldByName(''EventID'') as TIntegerField;'#13 +
  {} '  FEvent := FDataSet.FieldByName(''Event'') as TWideStringField;'#13 +
  {} '  FDate := FDataSet.FieldByName(''Date'') as TDateField;'#13 +
  {} '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13 +
  {} 'end;'#13 +
  {} sLineBreak +
  {} 'end.'#13, fStringList.Text);
end;

procedure TestGeneratorClassMethods.SaveToFile_CheckIndetnationAndNamingStyle;
begin
  fTemporaryFileName := TPath.GetTempPath + 'Proxy.HistoricalEvents.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents',
    {} ' ',
    {} fnsLowerCaseF);

  fStringList.LoadFromFile(fTemporaryFileName);

  Assert.AreEqual(' Data.DB,', fStringList[5]);
  Assert.AreEqual('  fEventID :TIntegerField;', fStringList[14]);
  Assert.AreEqual('  property EventID :TIntegerField read fEventID;',
    fStringList[20]);
end;

procedure TestGeneratorClassMethods.SaveToFile_DiffrentUnitNameAndNameOfClass;
begin
  fTemporaryFileName := TPath.GetTempPath + 'ProxyUnit.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'TFooProxy');

  fStringList.LoadFromFile(fTemporaryFileName);

  Assert.AreEqual('unit ProxyUnit;', fStringList[0]);
  Assert.AreEqual('  TFooProxy = class(TDatasetProxy)', fStringList[12]);
  Assert.AreEqual('procedure TFooProxy.ConnectFields;', fStringList[27]);
end;

// -----------------------------------------------------------------------
// Tests: SaveToClipboard
// -----------------------------------------------------------------------

procedure TestGeneratorClassMethods.SaveToClipboard_ClipboardNotEmpty;
begin
  TDataProxyGenerator.SaveToClipboard(
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'TEventsProxy');

  Assert.IsTrue(Clipboard.AsText.Length > 0,
    'Expected proxy code, but the clipboard content is empty');
end;

procedure TestGeneratorClassMethods.SaveToClipboard_CheckClipboardText;
begin
  TDataProxyGenerator.SaveToClipboard(
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'THistoricalEventsProxy');

  Assert.AreMemosEqual(
  {} 'type'#13 +
  {} '  THistoricalEventsProxy = class(TDatasetProxy)'#13 +
  {} '  private'#13 +
  {} '    FEventID :TIntegerField;'#13 +
  {} '    FEvent :TWideStringField;'#13 +
  {} '    FDate :TDateField;'#13 +
  {} '  protected'#13 +
  {} '    procedure ConnectFields; override;'#13 +
  {} '  public'#13 +
  {} '    property EventID :TIntegerField read FEventID;'#13 +
  {} '    property Event :TWideStringField read FEvent;'#13 +
  {} '    property Date :TDateField read FDate;'#13 +
  {} '  end;'#13 +
  {} sLineBreak +
  {} 'procedure THistoricalEventsProxy.ConnectFields;'#13 +
  {} 'const'#13 +
  {} '  ExpectedFieldCount = 3;'#13 +
  {} 'begin'#13 +
  {} '  FEventID := FDataSet.FieldByName(''EventID'') as TIntegerField;'#13 +
  {} '  FEvent := FDataSet.FieldByName(''Event'') as TWideStringField;'#13 +
  {} '  FDate := FDataSet.FieldByName(''Date'') as TDateField;'#13 +
  {} '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13 +
  {} 'end;'#13, Clipboard.AsText);
end;

procedure TestGeneratorClassMethods.SaveToClipboard_IndetationAndNamingStyle;
begin
  TDataProxyGenerator.SaveToClipboard(
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'THistoricalEventsProxy',
    {} '    ',
    {} fnsLowerCaseF);

  fStringList.Text := Clipboard.AsText;

  Assert.AreEqual('    private', fStringList[2]);
  Assert.AreEqual('        FDate :TDateField;', fStringList[5]);
  Assert.AreEqual('        property Date :TDateField read FDate;',
    fStringList[11]);
  Assert.AreEqual('    FDate := FDataSet.FieldByName(''Date'') as TDateField;',
    fStringList[20]);
end;

end.
