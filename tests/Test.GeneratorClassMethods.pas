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
  end;

{$TYPEINFO OFF}

implementation

uses
  System.IOUtils;

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


end.
