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
    procedure SavetToFile_IsFileExists;
    procedure SavetToFile_CheckUnitName;
    procedure SavetToFile_CheckAllUnit;
    procedure SavetToFile_CheckIndetnationAndNamingStyle;
    // ---
    procedure SaveToClipboard_ClipboardNotEmpty;
    procedure SaveToClipboard_CheckClipboardText;
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

procedure TestGeneratorClassMethods.SavetToFile_IsFileExists;
begin
  fTemporaryFileName := TPath.GetTempPath + 'HistoricalEvents1.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents');

  Assert.IsTrue(FileExists(fTemporaryFileName),
    Format('Expected temporary file is not exist (%s)', [fTemporaryFileName]));
end;

procedure TestGeneratorClassMethods.SavetToFile_CheckUnitName;
begin
  fTemporaryFileName := TPath.GetTempPath + 'Proxy.HistoricalEvents.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents');

  fStringList.LoadFromFile(fTemporaryFileName);

  Assert.AreEqual('unit Proxy.HistoricalEvents;', fStringList[0]);
end;

procedure TestGeneratorClassMethods.SavetToFile_CheckAllUnit;
begin
  fTemporaryFileName := TPath.GetTempPath + 'Proxy.HistoricalEvents.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents');

  fStringList.LoadFromFile(fTemporaryFileName);

  Assert.AreMemosEqual(
  {} 'unit Proxy.HistoricalEvents;'#13 +
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
  {} 'end;'#13, fStringList.Text);
end;

procedure TestGeneratorClassMethods.SavetToFile_CheckIndetnationAndNamingStyle;
begin
  fTemporaryFileName := TPath.GetTempPath + 'Proxy.HistoricalEvents.pas';

  TDataProxyGenerator.SaveToFile(
    {} fTemporaryFileName,
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents',
    {} ' ',
    {} fnsLowerCaseF);

  fStringList.LoadFromFile(fTemporaryFileName);

  Assert.AreEqual(' Data.DB,', fStringList[3]);
  Assert.AreEqual('  fEventID :TIntegerField;', fStringList[12]);
  Assert.AreEqual('  property EventID :TIntegerField read fEventID;',
    fStringList[18]);
end;

// -----------------------------------------------------------------------
// Tests: SaveToClipboard
// -----------------------------------------------------------------------

procedure TestGeneratorClassMethods.SaveToClipboard_ClipboardNotEmpty;
begin
  TDataProxyGenerator.SaveToClipboard(
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents');

  Assert.IsTrue(Clipboard.AsText.Length > 0,
    'Expected proxy code, but the clipboard content is empty');
end;

procedure TestGeneratorClassMethods.SaveToClipboard_CheckClipboardText;
begin
  TDataProxyGenerator.SaveToClipboard(
    {} GivenDataSet_MiniHistoricalEvents(fOwner),
    {} 'HistoricalEvents');

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

end.
