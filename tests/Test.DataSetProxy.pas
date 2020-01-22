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

    procedure ControlsDB_DisableControls;
    procedure DataSource_BindToDataSource;

    procedure Locate_BookTitle;
  end;

implementation

uses
  System.TypInfo,
  Vcl.DBCtrls,
  Datasnap.DBClient;


// -----------------------------------------------------------------------
// DataSetProxy factories
// -----------------------------------------------------------------------

type
  TUpdateStatusHelper = record helper for TUpdateStatus
    function ToString: string;
  end;

function TUpdateStatusHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TUpdateStatus), Integer(Self));
end;

// -----------------------------------------------------------------------
// DataSetProxy factories
// -----------------------------------------------------------------------

type
  TBookProxy = class(TDatasetProxy)
  private
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
  if aDataToInsert = nil then
  begin
    ds.AppendRecord(['978-0201633610',
      'Design Patterns: Elements of Reusable Object-Oriented Software',
      'Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides',
      EncodeDate(1994, 11, 1), 395, 54.90]);
    ds.AppendRecord(['978-0201485677',
      'Refactoring: Improving the Design of Existing Code',
      'Martin Fowler,' + ' Kent Beck,' + ' John Brant,' + ' William Opdyke,' +
      ' Don Roberts', EncodeDate(1999, 7, 1), 464, 52.98]);
    ds.AppendRecord(['978-0131177055', 'Working Effectively with Legacy Code',
      'Michael Feathers', EncodeDate(2004, 10, 1), 464, 52.69]);
    ds.AppendRecord(['978-0321127426',
      'Patterns of Enterprise Application Architecture', 'Martin Fowler',
      EncodeDate(2002, 11, 1), 560, 55.99]);
  end
  else
  begin
    for i := 0 to High(aDataToInsert) do
    begin
      ds.Append;
      for j := 0 to High(aDataToInsert[i]) do
        ds.Fields[j].Value := aDataToInsert[i][j];
      ds.Post;
    end;
  end;
  ds.First;
  ds.MergeChangeLog;
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
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Next;

  Assert.AreEqual(2, aDataSet.RecNo);
end;

procedure TestBookMemProxy.Navigation_Last;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;

  Assert.AreEqual(4, aDataSet.RecNo);
end;

procedure TestBookMemProxy.Navigation_LastAndPrior;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;
  aBookProxy.Prior;

  Assert.AreEqual(3, aDataSet.RecNo);
end;

procedure TestBookMemProxy.Navigation_LastAndFirst;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;
  aBookProxy.First;

  Assert.AreEqual(1, aDataSet.RecNo);
end;

procedure TestBookMemProxy.Navigation_Eof;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  actual: Boolean;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  actual := aBookProxy.Eof;

  Assert.AreEqual(False, actual);
end;

procedure TestBookMemProxy.Navigation_LastAndEof;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  actual: Boolean;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Last;
  actual := aBookProxy.Eof;

  Assert.AreEqual(True, actual);
end;

// -----------------------------------------------------------------------
// Tests: Process data: inster, update, delete, etc.
// -----------------------------------------------------------------------

procedure TestBookMemProxy.ProcessData_Delete;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Delete;

  Assert.AreEqual(3, aDataSet.RecordCount);
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
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Edit;
  aBookProxy.Author.Value := 'Anonymous author';
  aBookProxy.Cancel;

  Assert.AreEqual('Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides',
    aDataSet.FieldByName('Author').AsString);
end;

procedure TestBookMemProxy.ProcessData_InsertAndPost;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Insert;
  aBookProxy.ISBN.Value := '978-1788621304';
  aBookProxy.Title.Value := 'Delphi Cookbook - Third Edition';
  aBookProxy.Author.Value := 'Daniele Spinetti, Daniele Teti';
  aBookProxy.ReleseDate.Value := EncodeDate(2018, 7, 1);
  aBookProxy.Pages.Value := 668;
  aBookProxy.Price.Value := 29.99;
  aBookProxy.Post;

  Assert.AreEqual(5, aDataSet.RecordCount);
  Assert.AreEqual(1, aDataSet.RecNo);
end;

procedure TestBookMemProxy.ProcessData_Append;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Append;
  aBookProxy.ISBN.Value := '978-1788621304';
  aBookProxy.Post;

  Assert.AreEqual(5, aDataSet.RecordCount);
  Assert.AreEqual(5, aDataSet.RecNo);
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

  Assert.AreEqual(5, aDataSet.RecordCount);
  Assert.AreEqual('978-1788621304', aDataSet.FieldByName('ISBN').AsString);
  Assert.AreEqual(EncodeDate(2018, 7, 1), aDataSet.FieldByName('ReleseDate')
    .AsDateTime);
  Assert.AreEqual(29.99, aDataSet.FieldByName('Price').AsFloat, 0.000001);
end;

procedure TestBookMemProxy.ProcessData_InsertRecord;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.InsertRecord(['978000']);

  Assert.AreEqual(5, aDataSet.RecordCount);
  Assert.AreEqual(1, aDataSet.RecNo);
  Assert.AreEqual('978000', aDataSet.FieldByName('ISBN').AsString);
end;

procedure TestBookMemProxy.ProcessData_IsEmpty;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
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
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  Assert.AreEqual('usUnmodified', aBookProxy.UpdateStatus.ToString);
end;

procedure TestBookMemProxy.ProcessData_UpdateStatus_Inserted;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aDataSet.InsertRecord(['123456789', 'Title', 'Author', Int(Now)]);

  Assert.AreEqual('usInserted', aBookProxy.UpdateStatus.ToString);
end;


// -----------------------------------------------------------------------
// Tests: DB aware controls  / TDataSource factories
// -----------------------------------------------------------------------

procedure TestBookMemProxy.ControlsDB_DisableControls;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
  aDBEdit: TDBEdit;
begin
  aDataSet := GivenBookDataSet(fOwner, [
    { 1 }['978-0201633610', 'Design Patterns', 'Gang of Four', EncodeDate(1994,
    11, 1), 395, 54.90],
    { 2 }['978-0201485677', 'Refactoring: ...', 'Martin Fowler',
    EncodeDate(1999, 7, 1), 464, 52.98],
    { 3 }['978-0131177055', 'Working Effectively with Legacy Code',
    'Michael Feathers', EncodeDate(2004, 10, 1), 464, 52.69],
    { 4 }['978-0321127426', 'Patterns of Enterprise Application Architecture',
    'Martin Fowler', EncodeDate(2002, 11, 1), 560, 55.99]]);

  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;
  aDBEdit := GivienDBEdit(aDataSet, 'Author');

  aBookProxy.DisableControls;
  aBookProxy.Next;

  Assert.AreEqual('Gang of Four', aDBEdit.Text);
  Assert.AreEqual(True, aBookProxy.ControlsDisabled);

  aBookProxy.EnableControls;

  Assert.AreEqual('Martin Fowler', aDBEdit.Text);
  Assert.AreEqual(False, aBookProxy.ControlsDisabled);
end;

procedure TestBookMemProxy.DataSource_BindToDataSource;
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
      { 1 }['978-1788621304', 'Delphi Cookbook', NULL, NULL, NULL, 29.99],
      { 2 }['978-0201485677', 'Refactoring', NULL, NULL, NULL, 52.98]]);
    aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;
    aDBEdit := TDBEdit.Create(fOwner);
    aDBEdit.DataSource := TDataSource.Create(fOwner);
    aDBEdit.DataField := 'Price';

    aBookProxy.BindToDataSource(aDBEdit.DataSource);

    Assert.AreEqual('$29.99', aDBEdit.Text);
    aBookProxy.Next;
    Assert.AreEqual('$52.98', aDBEdit.Text);
  finally
    FormatSettings := aFormatSettings;
  end;
end;

(* Test to deliver:
  ------------ ------------ ------------ ------------ ------------
 procedure BindToDataSource(DataSource: TDataSource);
 function ConstructDataSource(AOwner: TComponent): TDataSource;

 function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;

 function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
 function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
 ------------ ------------ ------------ ------------ ------------
*)

(* Verify implementation opportiunity:
  ------------ ------------ ------------ ------------ ------------
 procedure Refresh;
 //  doc: http://docwiki.embarcadero.com/Libraries/Rio/en/Data.DB.TDataSet.Refresh

 TDataProxy.Reload; = dataset.Close & dataset.Open
 procedure Close;
 ------------ ------------ ------------ ------------ ------------
*)

// -----------------------------------------------------------------------
// Tests: Locate
// -----------------------------------------------------------------------

procedure TestBookMemProxy.Locate_BookTitle;
var
  aDataSet: TDataSet;
  aBookProxy: TBookProxy;
begin
  aDataSet := GivenBookDataSet(fOwner);
  aBookProxy := TBookProxy.Create(fOwner).WithDataSet(aDataSet) as TBookProxy;

  aBookProxy.Locate('Title', 'Working Effectively with Legacy Code', []);

  Assert.AreEqual('Michael Feathers', aBookProxy.Author.Value);
  Assert.AreEqual('Michael Feathers', String(aDataSet.FieldValues['Author']));
end;

end.
