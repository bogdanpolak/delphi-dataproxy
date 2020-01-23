unit Test.DataSetProxy;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB,

  Data.DataProxy;

{$M+}

type

  [TestFixture]
  TestBookMemProxy = class(TObject)
  private
    fOwner: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Navigation_1xNext;
    procedure Navigation_Last;
    procedure Navigation_LastAndPrior;
    procedure Navigation_LastAndFirst;
    procedure Navigation_Eof;
    procedure Navigation_LastAndEof;

    procedure ProcessData_Delete;
    procedure ProcessData_EditAndPost;
    procedure ProcessData_EditAndCancel;
    procedure ProcessData_InsertAndPost;
    procedure ProcessData_Append;
    procedure ProcessData_AppendRecord;
    procedure ProcessData_InsertRecord;
    procedure ProcessData_IsEmpty;
    procedure ProcessData_UpdateStatus_Unmodified;
    procedure ProcessData_UpdateStatus_Inserted;

    procedure ControlsDB_TestPriceFormatting;
    procedure ControlsDB_DisableControls;
    procedure DataSource_BindToDataSource;
    procedure DataSource_ConstructDataSource;

    procedure Blob_CreateBlobStream;

    procedure Locate_BookTitle;
    procedure Lookup_BookISBN;
  end;

implementation

uses
  System.TypInfo,
  Vcl.DBCtrls,
  Datasnap.DBClient;


// -----------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------

type
  TUpdateStatusHelper = record helper for TUpdateStatus
    function ToString: string;
  end;

function TUpdateStatusHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TUpdateStatus), Integer(Self));
end;

function HexesIntoBytes(aHexes: TArray<string>): TBytes;
var
  i: Integer;
  b: byte;
begin
  SetLength(Result, Length(aHexes));
  for i := 0 to High(aHexes) do
  begin
    b := byte(StrToint('$' + aHexes[i]));
    Result[i] := b;
  end;
end;

// -----------------------------------------------------------------------
// DataSetProxy declarations
// -----------------------------------------------------------------------

type
  TBookProxy = class(TDatasetProxy)
  strict private
    FISBN: TWideStringField;
    FTitle: TWideStringField;
    FAuthor: TWideStringField;
    FReleseDate: TDateField;
    FPages: TIntegerField;
    FPrice: TCurrencyField;
  protected
    procedure ConnectFields; override;
  public
    property ISBN: TWideStringField read FISBN;
    property Title: TWideStringField read FTitle;
    property Author: TWideStringField read FAuthor;
    property ReleseDate: TDateField read FReleseDate;
    property Pages: TIntegerField read FPages;
    property Price: TCurrencyField read FPrice;
  end;

procedure TBookProxy.ConnectFields;
begin
  FISBN := FDataSet.FieldByName('ISBN') as TWideStringField;
  FTitle := FDataSet.FieldByName('Title') as TWideStringField;
  FAuthor := FDataSet.FieldByName('Author') as TWideStringField;
  FReleseDate := FDataSet.FieldByName('ReleseDate') as TDateField;
  FPages := FDataSet.FieldByName('Pages') as TIntegerField;
  FPrice := FDataSet.FieldByName('Price') as TCurrencyField;
end;

type
  TBlobProxy = class(TDatasetProxy)
  strict private
    fID: TIntegerField;
    fData: TBlobField;
  protected
    procedure ConnectFields; override;
  public
    property ID: TIntegerField read fID;
    property Data: TBlobField read fData;
  end;

procedure TBlobProxy.ConnectFields;
begin
  inherited;
  fID := FDataSet.FieldByName('ID') as TIntegerField;
  fData := FDataSet.FieldByName('Data') as TBlobField;
end;


// -----------------------------------------------------------------------
// DataSetProxy factories
// -----------------------------------------------------------------------

function GivenBookDataSet(aOwner: TComponent;
  aDataToInsert: TArray < TArray < Variant >> = nil): TDataSet;
var
  ds: TClientDataSet;
  i: Integer;
  j: Integer;
begin
  ds := TClientDataSet.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add('ISBN', ftWideString, 15);
    FieldDefs.Add('Title', ftWideString, 150);
    FieldDefs.Add('Author', ftWideString, 100);
    FieldDefs.Add('ReleseDate', ftDate);
    FieldDefs.Add('Pages', ftInteger);
    FieldDefs.Add('Price', ftCurrency);
    CreateDataSet;
  end;
  if aDataToInsert <> nil then
  begin
    for i := 0 to High(aDataToInsert) do
    begin
      ds.Append;
      for j := 0 to High(aDataToInsert[i]) do
        ds.Fields[j].Value := aDataToInsert[i][j];
      ds.Post;
    end;
    ds.First;
    ds.MergeChangeLog;
  end;
  Result := ds;
end;

function GivenBlobDataSet(aOwner: TComponent;
  const aDataToInsert: TArray<TBytes>): TDataSet;
var
  ds: TClientDataSet;
  i: Integer;
begin
  ds := TClientDataSet.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add('ID', ftInteger);
    FieldDefs.Add('Data', ftBlob);
    CreateDataSet;
  end;
  if aDataToInsert <> nil then
  begin
    for i := 0 to High(aDataToInsert) do
    begin
      ds.Append;
      ds.FieldByName('ID').AsInteger := i + 1;
      (ds.FieldByName('Data') as TBlobField).Value := aDataToInsert[i];
      ds.Post;
    end;
    ds.First;
    ds.MergeChangeLog;
  end;
  Result := ds;
end;

function GivienDBEdit(aDataSet: TDataSet; aDataFieldName: string): TDBEdit;
var
  aDBEdit: TDBEdit;
begin
  aDBEdit := TDBEdit.Create(aDataSet);
  aDBEdit.DataSource := TDataSource.Create(aDataSet);
  aDBEdit.DataSource.DataSet := aDataSet;
  aDBEdit.DataField := aDataFieldName;
  Result := aDBEdit;
end;


// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestBookMemProxy.Setup;
begin
  fOwner := TComponent.Create(nil);
end;

procedure TestBookMemProxy.TearDown;
begin
  fOwner.Free;
end;

// -----------------------------------------------------------------------
// Tests: Navigation
// -----------------------------------------------------------------------

procedure TestBookMemProxy.Navigation_1xNext;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [
    { 1 }['00000000'],
    { 2 }['978-0201485677',
    'Refactoring: Improving the Design of Existing Code',
    'Martin Fowler, Kent Beck, John Brant, William Opdyke, Don Roberts',
    EncodeDate(1999, 7, 1), 464, 52.98]]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Next;

  Assert.AreEqual('978-0201485677', aBookProxy.ISBN.Value);
  Assert.AreEqual('Refactoring: Improving the Design of Existing Code',
    aBookProxy.Title.Value);
  Assert.AreEqual
    ('Martin Fowler, Kent Beck, John Brant, William Opdyke, Don Roberts',
    aBookProxy.Author.Value);
  Assert.AreEqual(464, aBookProxy.Pages.Value);
  Assert.AreEqual(52.98, aBookProxy.Price.Value, 0.000001);
end;

procedure TestBookMemProxy.Navigation_Last;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [
    { 1 }['978-0201633610',
    'Design Patterns: Elements of Reusable Object-Oriented Software',
    'Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides', EncodeDate(1994,
    11, 1), 395, 54.90],
    { 2 }['978-0201485677',
    'Refactoring: Improving the Design of Existing Code',
    'Martin Fowler, Kent Beck, John Brant, William Opdyke, Don Roberts',
    EncodeDate(1999, 7, 1), 464, 52.98],
    { 3 }['978-0131177055', 'Working Effectively with Legacy Code',
    'Michael Feathers', EncodeDate(2004, 10, 1), 464, 52.69],
    { 4 }['978-0321127426', 'Patterns of Enterprise Application Architecture',
    'Martin Fowler', EncodeDate(2002, 11, 1), 560, 55.99]]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;

  Assert.AreEqual('978-0321127426', aBookProxy.ISBN.Value);
  Assert.AreEqual('Patterns of Enterprise Application Architecture',
    aBookProxy.Title.Value);
  Assert.AreEqual('Martin Fowler', aBookProxy.Author.Value);
  Assert.AreEqual(560, aBookProxy.Pages.Value);
  Assert.AreEqual(55.99, aBookProxy.Price.Value, 0.000001);
end;

procedure TestBookMemProxy.Navigation_LastAndPrior;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610'], ['978-0201485677'],
    ['978-0131177055'], ['978-0321127426']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;
  aBookProxy.Prior;

  Assert.AreEqual({3}'978-0131177055', aBookProxy.ISBN.Value);
end;

procedure TestBookMemProxy.Navigation_LastAndFirst;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610'], ['978-0201485677'],
    ['978-0131177055'], ['978-0321127426']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;
  aBookProxy.First;

  Assert.AreEqual({1}'978-0201633610', aBookProxy.ISBN.Value);
end;

procedure TestBookMemProxy.Navigation_Eof;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610'], ['978-0201485677'],
    ['978-0131177055'], ['978-0321127426']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  Assert.AreEqual(False, aBookProxy.Eof);
end;

procedure TestBookMemProxy.Navigation_LastAndEof;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610'], ['978-0201485677'],
    ['978-0131177055'], ['978-0321127426']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;
  Assert.AreEqual(True, aBookProxy.Eof);
end;

// -----------------------------------------------------------------------
// Tests: Process data: inster, update, delete, etc.
// -----------------------------------------------------------------------

procedure TestBookMemProxy.ProcessData_Delete;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610'], ['978-0201485677'],
    ['978-0131177055'], ['978-0321127426']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Delete;

  Assert.AreEqual({2}'978-0201485677', aBookProxy.ISBN.Value);
  Assert.AreEqual(3, aBookProxy.RecordCount);
end;

procedure TestBookMemProxy.ProcessData_EditAndPost;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Edit;
  aBookProxy.Author.Value := 'Anonymous author';
  aBookProxy.Post;

  Assert.AreEqual('Anonymous author', aDataSet.FieldByName('Author').AsString);
end;

procedure TestBookMemProxy.ProcessData_EditAndCancel;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  actualAutorInDataSet: String;
begin
  aDataSet := GivenBookDataSet(fOwner,
    [['978-1788625456', 'Delphi High Performance', 'Primož Gabrijelčič',
    EncodeDate(2018, 2, 1), 336, 25.83]]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Edit;
  aBookProxy.Author.Value := 'Bogdan Polak';
  aBookProxy.Cancel;
  actualAutorInDataSet := aDataSet.FieldByName('Author').AsString;

  Assert.AreEqual('Primož Gabrijelčič', actualAutorInDataSet);
end;

procedure TestBookMemProxy.ProcessData_InsertAndPost;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Insert;
  aBookProxy.ISBN.Value := '978-1788621304';
  aBookProxy.Title.Value := 'Delphi Cookbook - Third Edition';
  aBookProxy.Author.Value := 'Daniele Spinetti, Daniele Teti';
  aBookProxy.ReleseDate.Value := EncodeDate(2018, 7, 1);
  aBookProxy.Pages.Value := 668;
  aBookProxy.Price.Value := 29.99;
  aBookProxy.Post;

  Assert.AreEqual(2, aDataSet.RecordCount);
  Assert.AreEqual(1, aDataSet.RecNo);
  Assert.AreEqual('Daniele Spinetti, Daniele Teti',
    aDataSet.FieldByName('Author').AsString);
  Assert.AreEqual(668, aDataSet.FieldByName('Pages').AsInteger);
end;

procedure TestBookMemProxy.ProcessData_Append;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610'],
    ['978-0201485677']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Append;
  aBookProxy.ISBN.Value := '978-1788621304';
  aBookProxy.Post;

  Assert.AreEqual(3, aDataSet.RecordCount);
  Assert.AreEqual(3, aDataSet.RecNo);
  Assert.AreEqual('978-1788621304', aDataSet.FieldByName('ISBN').AsString);
end;

procedure TestBookMemProxy.ProcessData_AppendRecord;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.AppendRecord(['978-1788621304', 'Delphi Cookbook - Third Edition',
    'Daniele Spinetti, Daniele Teti', EncodeDate(2018, 7, 1), 668, 29.99]);

  Assert.AreEqual('978-1788621304', aDataSet.FieldByName('ISBN').AsString);
  Assert.AreEqual(EncodeDate(2018, 7, 1), aDataSet.FieldByName('ReleseDate')
    .AsDateTime);
  Assert.AreEqual(29.99, aDataSet.FieldByName('Price').AsFloat, 0.000001);
  Assert.AreEqual(1, aDataSet.RecordCount);
end;

procedure TestBookMemProxy.ProcessData_InsertRecord;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.InsertRecord(['978-1788621304']);

  Assert.AreEqual('978-1788621304', aDataSet.FieldByName('ISBN').AsString);
  Assert.AreEqual(1, aDataSet.RecordCount);
end;

procedure TestBookMemProxy.ProcessData_IsEmpty;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610'], ['978-0201485677'],
    ['978-0131177055'], ['978-0321127426']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  Assert.AreEqual(False, aBookProxy.IsEmpty);
  aDataSet.Delete;
  aDataSet.Delete;
  aDataSet.Delete;
  aDataSet.Delete;
  Assert.AreEqual(True, aBookProxy.IsEmpty);
end;

procedure TestBookMemProxy.ProcessData_UpdateStatus_Unmodified;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  Assert.AreEqual('usUnmodified', aBookProxy.UpdateStatus.ToString);
end;

procedure TestBookMemProxy.ProcessData_UpdateStatus_Inserted;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-0201633610']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aDataSet.InsertRecord(['978-1788621304']);

  Assert.AreEqual('usInserted', aBookProxy.UpdateStatus.ToString);
end;


// -----------------------------------------------------------------------
// Tests: DB aware controls  / TDataSource factories
// -----------------------------------------------------------------------

procedure TestBookMemProxy.ControlsDB_TestPriceFormatting;
var
  aFormatSettings: TFormatSettings;
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  aDBEdit: TDBEdit;
begin
  aFormatSettings := FormatSettings;
  FormatSettings := TFormatSettings.Create('en-us');
  try
    aDataSet := GivenBookDataSet(fOwner, [
      { 1 }['978-1111111111', NULL, NULL, NULL, 395, 54.90],
      { 2 }['978-0201485677', NULL, NULL, NULL, 256, 55.99]]);
    aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;
    aDBEdit := GivienDBEdit(aDataSet, 'Price');

    Assert.AreEqual('$54.90', aDBEdit.Text);
    aBookProxy.Next;
    Assert.AreEqual('$55.99', aDBEdit.Text);
  finally
    FormatSettings := aFormatSettings;
  end;
end;

procedure TestBookMemProxy.ControlsDB_DisableControls;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  aDBEdit: TDBEdit;
begin
  aDataSet := GivenBookDataSet(fOwner, [
    { 1 }['978-0201633610',
    'Design Patterns: Elements of Reusable Object-Oriented Software',
    'Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides', EncodeDate(1994,
    11, 1), 395, 54.90],
    { 2 }['978-0201485677',
    'Refactoring: Improving the Design of Existing Code',
    'Martin Fowler, Kent Beck, John Brant, William Opdyke, Don Roberts',
    EncodeDate(1999, 7, 1), 464, 52.98],
    { 3 }['978-0131177055', 'Working Effectively with Legacy Code',
    'Michael Feathers', EncodeDate(2004, 10, 1), 464, 52.69],
    { 4 }['978-0321127426', 'Patterns of Enterprise Application Architecture',
    'Martin Fowler', EncodeDate(2002, 11, 1), 560, 55.99]]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;
  aDBEdit := GivienDBEdit(aDataSet, 'Author');

  aBookProxy.DisableControls;
  aBookProxy.Next;

  Assert.AreEqual('Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides',
    aDBEdit.Text);
  Assert.AreEqual(True, aBookProxy.ControlsDisabled);

  aBookProxy.EnableControls;

  Assert.AreEqual
    ('Martin Fowler, Kent Beck, John Brant, William Opdyke, Don Roberts',
    aDBEdit.Text);
  Assert.AreEqual(False, aBookProxy.ControlsDisabled);
end;

procedure TestBookMemProxy.DataSource_BindToDataSource;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  aDBEdit: TDBEdit;
begin
  aDataSet := GivenBookDataSet(fOwner, [['978-1788621304'],
    ['978-0201485677']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;
  aDBEdit := TDBEdit.Create(fOwner);
  aDBEdit.DataSource := TDataSource.Create(fOwner);
  aDBEdit.DataField := 'ISBN';

  aBookProxy.BindToDataSource(aDBEdit.DataSource);

  Assert.AreEqual('978-1788621304', aDBEdit.Text);
  aBookProxy.Next;
  Assert.AreEqual('978-0201485677', aDBEdit.Text);
end;

procedure TestBookMemProxy.DataSource_ConstructDataSource;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  aDBEdit: TDBEdit;
begin
  aDataSet := GivenBookDataSet(fOwner, [['11111'], ['222222']]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;
  aDBEdit := TDBEdit.Create(fOwner);
  aDBEdit.DataField := 'ISBN';

  aDBEdit.DataSource := aBookProxy.ConstructDataSource(fOwner);

  Assert.AreEqual('11111', aDBEdit.Text);
  aBookProxy.Next;
  Assert.AreEqual('222222', aDBEdit.Text);
end;

// -----------------------------------------------------------------------
// Tests: Blob
// -----------------------------------------------------------------------

procedure TestBookMemProxy.Blob_CreateBlobStream;
var
  aDataSet: TDataSet;
  aBlobProxy: TBlobProxy;
  stream: TStream;
  aBuffer: TBytes;
begin
  aDataSet := GivenBlobDataSet(fOwner,
    [HexesIntoBytes(['f0', '22', '3e', '7a', 'c4', '6b', '00', '00', '01']),
    HexesIntoBytes(['00', '00', '70', '4b', '00', '00', '02'])]);
  aBlobProxy := TBlobProxy.Create(fOwner).WithDataSet(aDataSet) as TBlobProxy;

  stream := aBlobProxy.CreateBlobStream(aBlobProxy.Data, bmRead);
  SetLength(aBuffer, stream.Size);
  stream.Read(aBuffer, stream.Size);
  stream.Free;

  Assert.AreEqual(byte(15 * 16), aBuffer[0]);
end;


// -----------------------------------------------------------------------
// Tests: Locate
// -----------------------------------------------------------------------

procedure TestBookMemProxy.Locate_BookTitle;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner, [
    {1} ['978-1941266038', 'Coding in Delphi', 'Nick Hodges', EncodeDate(2014,
    4, 1), 236, 24.99],
    {2} ['978-1941266106', 'More Coding in Delphi', 'Nick Hodges',
    EncodeDate(2015, 12, 1), 246, 25.99],
    {3} ['978-1785287428', 'Delphi Cookbook - Second Edition', 'Daniele Teti',
    EncodeDate(2016, 6, 1), 470, 30.13],
    {4} ['978-1941266229', 'Dependency Injection In Delphi', 'Nick Hodges',
    EncodeDate(2017, 2, 1), 132, 18.18],
    {5} ['978-1546391272', 'Delphi in Depth: FireDAC', 'Cary Jensen',
    EncodeDate(2017, 5, 1), 556, 52.43],
    {6} ['978-1786460165', 'Expert Delphi', 'Paweł Głowacki', EncodeDate(2017,
    6, 1), 506, 32.71],
    {7} ['978-1788625456', 'Delphi High Performance', 'Primož Gabrijelčič',
    EncodeDate(2018, 2, 1), 336, 25.83],
    {8} ['978-1788621304', 'Delphi Cookbook - Third Edition',
    'Daniele Spinetti, Daniele Teti', EncodeDate(2018, 7, 1), 668, 30.13],
    {9} ['978-1789343243', 'Hands-On Design Patterns with Delphi',
    'Primož Gabrijelčič', EncodeDate(2019, 2, 27), 476, 35.99],
    {10} ['978-1788624176', 'Delphi GUI Programming with FireMonkey',
    'Andrea Magni', EncodeDate(2020, 04, 11), 437, 29.27]]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Locate('Title', 'Expert Delphi', []);

  Assert.AreEqual(6, aDataSet.RecNo);
  Assert.AreEqual('Paweł Głowacki', aBookProxy.Author.Value);
end;

procedure TestBookMemProxy.Lookup_BookISBN;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  sActual: TArray<Variant>;
begin
  aDataSet := GivenBookDataSet(fOwner, [
    {1} ['978-1941266038', 'Coding in Delphi', 'Nick Hodges', EncodeDate(2014,
    4, 1), 236, 24.99],
    {2} ['978-1941266106', 'More Coding in Delphi', 'Nick Hodges',
    EncodeDate(2015, 12, 1), 246, 25.99],
    {3} ['978-1785287428', 'Delphi Cookbook - Second Edition', 'Daniele Teti',
    EncodeDate(2016, 6, 1), 470, 30.13],
    {4} ['978-1941266229', 'Dependency Injection In Delphi', 'Nick Hodges',
    EncodeDate(2017, 2, 1), 132, 18.18],
    {5} ['978-1546391272', 'Delphi in Depth: FireDAC', 'Cary Jensen',
    EncodeDate(2017, 5, 1), 556, 52.43],
    {6} ['978-1786460165', 'Expert Delphi', 'Paweł Głowacki', EncodeDate(2017,
    6, 1), 506, 32.71],
    {7} ['978-1788625456', 'Delphi High Performance', 'Primož Gabrijelčič',
    EncodeDate(2018, 2, 1), 336, 25.83],
    {8} ['978-1788621304', 'Delphi Cookbook - Third Edition',
    'Daniele Spinetti, Daniele Teti', EncodeDate(2018, 7, 1), 668, 30.13],
    {9} ['978-1789343243', 'Hands-On Design Patterns with Delphi',
    'Primož Gabrijelčič', EncodeDate(2019, 2, 27), 476, 35.99],
    {10} ['978-1788624176', 'Delphi GUI Programming with FireMonkey',
    'Andrea Magni', EncodeDate(2020, 04, 11), 437, 29.27]]);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Next;
  sActual := aBookProxy.Lookup('ISBN', '978-1788625456', 'Author;Title;Price');

  Assert.AreEqual(2, aDataSet.RecNo);
  Assert.AreEqual('Primož Gabrijelčič', String(sActual[0]));
  Assert.AreEqual('Delphi High Performance', String(sActual[1]));
  Assert.AreEqual(25.83, Extended(sActual[2]), 0.0000001);
end;

end.
