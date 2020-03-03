unit Test.ProxyGenerator;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB,
  FireDAC.Comp.Client,

  Comp.Generator.DataProxy,
  Wrapper.TProxyGenerator,
  Helper.DUnitAssert;

{$M+}

type
  TMatrixOfVariants = TArray<TArray<Variant>>;

  [TestFixture]
  TestGenerator = class(TObject)
  private
    fOwner: TComponent;
    fGenerator: TTestProxyDataGenerator;
    function GivenDataset(aFieldsDef: TMatrixOfVariants): TDataSet;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // ---
    procedure GenUnitHeader_IsEmpty;
    procedure GenUsesSection;
    procedure GenUsesSection_Identation_4spaces;
    // ---
    procedure GenClassFields_Integer;
    procedure GenClassFields_Integer_LowerCaseStyle;
    procedure GenClassFields_String;
    procedure GenProperty_Date;
    procedure GenFieldAssigment_Currency;
    // ---
    procedure GenClass_DataSet_Nil;
    procedure GenClass_Dataset_OneInteger;
    procedure GenClass_With2Fields_Identation4;
    procedure GenClass_TwoFields_LowerCaseStyle;
    procedure GenClass_AccessToDataSet_InComments;
    procedure GenClass_AccessToDataSet_Full;
    // ---
    procedure GenMethod_ConnectFields_DataSet_Nil;
    procedure GenMethod_ConnectFields_DataSet_OneString;
    procedure GenMethod_ConnectFields_TwoFields_LowerCaseStyle;
    procedure GenMethod_ConnectFields_Identation4;
    // ---
    procedure Generate_BooksProxy_Unit;
    procedure Generate_BooksProxy_Class;
  end;

implementation

// -----------------------------------------------------------------------
// Dataset factories
// -----------------------------------------------------------------------

function GivenField(aOwner: TComponent; const fieldName: string;
  fieldType: TFieldType; size: integer = 0): TField;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  ds.FieldDefs.Add(fieldName, fieldType, size);
  ds.CreateDataSet;
  Result := ds.Fields[0];
end;

function TestGenerator.GivenDataset(aFieldsDef: TMatrixOfVariants): TDataSet;
var
  aTable: TFDMemTable;
  i: integer;
begin
  aTable := TFDMemTable.Create(fOwner);
  for i := 0 to High(aFieldsDef) do
    aTable.FieldDefs.Add(aFieldsDef[i][0], aFieldsDef[i][1]);
  aTable.CreateDataSet;
  Result := aTable;
end;


// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGenerator.Setup;
begin
  fOwner := TComponent.Create(nil);
  fGenerator := TTestProxyDataGenerator.Create(fOwner);
end;

procedure TestGenerator.TearDown;
begin
  fOwner.Free;
end;


// -----------------------------------------------------------------------
// Tests: Unit Header / Uses Section
// -----------------------------------------------------------------------

procedure TestGenerator.GenUnitHeader_IsEmpty;
begin
  fGenerator.Generate_UnitHeader;
  Assert.AreEqual('', fGenerator.Code.Text);
end;

procedure TestGenerator.GenUsesSection;
var
  actualCode: string;
begin
  actualCode := fGenerator.Generate_UsesSection;

  Assert.AreEqual(
    (* *) 'uses'#13#10 +
    (* *) '  Data.DB,'#13#10 +
    (* *) '  Data.DataProxy,'#13#10 +
    (* *) '  System.SysUtils,'#13#10 +
    (* *) '  System.Classes,'#13#10 +
    (* *) '  FireDAC.Comp.Client;'#13#10, actualCode);
end;

procedure TestGenerator.GenUsesSection_Identation_4spaces;
var
  actualCode: string;
begin
  fGenerator.IdentationText := '    ';

  actualCode := fGenerator.Generate_UsesSection;

  Assert.AreEqual(
    (* *) 'uses'#13#10 +
    (* *) '    Data.DB,'#13#10 +
    (* *) '    Data.DataProxy,'#13#10 +
    (* *) '    System.SysUtils,'#13#10 +
    (* *) '    System.Classes,'#13#10 +
    (* *) '    FireDAC.Comp.Client;'#13#10, actualCode);
end;


// -----------------------------------------------------------------------
// Tests: Field generation in class definition
// -----------------------------------------------------------------------

procedure TestGenerator.GenClassFields_Integer;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Level', ftInteger);

  actualCode := fGenerator.Generate_PrivateField(fld);

  Assert.AreEqual('FLevel :TIntegerField;', actualCode);
end;

procedure TestGenerator.GenClassFields_Integer_LowerCaseStyle;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Level', ftInteger);
  fGenerator.FieldNamingStyle := fnsLowerCaseF;

  actualCode := fGenerator.Generate_PrivateField(fld);

  Assert.AreEqual('FLevel :TIntegerField;', actualCode);
end;

procedure TestGenerator.GenClassFields_String;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Captal', ftString, 20);

  actualCode := fGenerator.Generate_PrivateField(fld);

  Assert.AreEqual('FCaptal :TStringField;', actualCode);
end;


// -----------------------------------------------------------------------
// Tests: Property generation in class definition
// -----------------------------------------------------------------------

procedure TestGenerator.GenProperty_Date;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'BirthDate', ftDate);

  actualCode := fGenerator.Generate_PublicProperty(fld);

  Assert.AreEqual('property BirthDate :TDateField read FBirthDate;',
    actualCode);
end;


// -----------------------------------------------------------------------
// Tests: Generate one private field assigment
// -----------------------------------------------------------------------

procedure TestGenerator.GenFieldAssigment_Currency;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Budget', ftCurrency);

  actualCode := fGenerator.Generate_FieldAssigment(fld);

  Assert.AreEqual
    ('FBudget := FDataSet.FieldByName(''Budget'') as TCurrencyField;',
    actualCode);
end;


// -----------------------------------------------------------------------
// Tests: Class Declaration
// -----------------------------------------------------------------------

procedure TestGenerator.GenClass_DataSet_Nil;
var
  actualCode: string;
begin
  fGenerator.DataSet := nil;
  fGenerator.ObjectName := 'Something1';

  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreMemosEqual(
    (* *) 'type'#13#10 +
    (* *) '  TSomething1Proxy = class(TDatasetProxy)'#13#10 +
    (* *) '  private'#13#10 +
    (* *) '  protected'#13#10 +
    (* *) '    procedure ConnectFields; override;'#13#10 +
    (* *) '  public'#13#10 +
    (* *) '  end;'#13#10, actualCode);
end;

procedure TestGenerator.GenClass_Dataset_OneInteger;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['FieldInteger', ftInteger]]);
  fGenerator.ObjectName := 'Something2';

  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreMemosEqual(
    (* *) 'type'#13#10 +
    (* *) '  TSomething2Proxy = class(TDatasetProxy)'#13#10 +
    (* *) '  private'#13#10 +
    (* *) '    FFieldInteger :TIntegerField;'#13#10 +
    (* *) '  protected'#13#10 +
    (* *) '    procedure ConnectFields; override;'#13#10 +
    (* *) '  public'#13#10 +
    (* *) '    property FieldInteger :TIntegerField read FFieldInteger;'#13#10 +
    (* *) '  end;'#13#10, actualCode);
end;

procedure TestGenerator.GenClass_With2Fields_Identation4;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([
    {} ['CustomerID', ftInteger],
    {} ['CompanyName', ftString]]);
  fGenerator.ObjectName := 'TwoField';
  fGenerator.IdentationText := '    ';

  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreMemosEqual(
    {} 'type'#13#10 +
    {} '    TTwoFieldProxy = class(TDatasetProxy)'#13#10 +
    {} '    private'#13#10 +
    {} '        FCustomerID :TIntegerField;'#13#10 +
    {} '        FCompanyName :TStringField;'#13#10 +
    {} '    protected'#13#10 +
    {} '        procedure ConnectFields; override;'#13#10 +
    {} '    public'#13#10 +
    {} '        property CustomerID :TIntegerField read FCustomerID;'#13#10 +
    {} '        property CompanyName :TStringField read FCompanyName;'#13#10 +
    {} '    end;'#13#10, actualCode);
end;

procedure TestGenerator.GenClass_TwoFields_LowerCaseStyle;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['CustomerID', ftInteger],
    ['CompanyName', ftString]]);
  fGenerator.ObjectName := 'Something';
  fGenerator.FieldNamingStyle := fnsLowerCaseF;

  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreMemosEqual(
    (* *) 'type'#13#10 +
    (* *) '  TSomethingProxy = class(TDatasetProxy)'#13#10 +
    (* *) '  private'#13#10 +
    (* *) '    fCustomerID :TIntegerField;'#13#10 +
    (* *) '    fCompanyName :TStringField;'#13#10 +
    (* *) '  protected'#13#10 +
    (* *) '    procedure ConnectFields; override;'#13#10 +
    (* *) '  public'#13#10 +
    (* *) '    property CustomerID :TIntegerField read fCustomerID;'#13#10 +
    (* *) '    property CompanyName :TStringField read fCompanyName;'#13#10 +
    (* *) '  end;'#13#10, actualCode);
end;

procedure TestGenerator.GenClass_AccessToDataSet_InComments;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['FullName', ftString]]);
  fGenerator.ObjectName := 'Something';
  fGenerator.DataSetAccess := dsaGenComment;

  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreMemosEqual(
    (* *) 'type'#13#10
    (* *) + '  TSomethingProxy = class(TDatasetProxy)'#13#10
    (* *) + '  private'#13#10
    (* *) + '    FFullName :TStringField;'#13#10
    (* *) + '  protected'#13#10
    (* *) + '    procedure ConnectFields; override;'#13#10
    (* *) + '  public'#13#10
    (* *) + '    property FullName :TStringField read FFullName;'#13#10
    (* *) + '    // the following property should be hidden (uncomment if required)'#13#10
    (* *) + '    // property DataSet: TDataSet read FDataSet;'#13#10
    (* *) + '  end;'#13#10, actualCode);
end;

procedure TestGenerator.GenClass_AccessToDataSet_Full;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['FullName', ftString]]);
  fGenerator.ObjectName := 'Something';
  fGenerator.DataSetAccess := dsaFullAccess;

  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreMemosEqual(
    (* *) 'type'#13#10
    (* *) + '  TSomethingProxy = class(TDatasetProxy)'#13#10
    (* *) + '  private'#13#10
    (* *) + '    FFullName :TStringField;'#13#10
    (* *) + '  protected'#13#10
    (* *) + '    procedure ConnectFields; override;'#13#10
    (* *) + '  public'#13#10
    (* *) + '    property FullName :TStringField read FFullName;'#13#10
    (* *) + '    property DataSet: TDataSet read FDataSet;'#13#10
    (* *) + '  end;'#13#10, actualCode);
end;


// -----------------------------------------------------------------------
// Tests: Method ConnectFields
// -----------------------------------------------------------------------

procedure TestGenerator.GenMethod_ConnectFields_DataSet_Nil;
var
  actualCode: string;
begin
  fGenerator.DataSet := nil;

  actualCode := fGenerator.Generate_MethodConnectFields;

  Assert.AreMemosEqual(
    (* *) 'procedure TSomethingProxy.ConnectFields;'#13#10 +
    (* *) 'const'#13#10 +
    (* *) '  ExpectedFieldCount = 0;'#13#10 +
    (* *) 'begin'#13#10 +
    (* *) '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13#10 +
    (* *) 'end;'#13#10, actualCode);
end;

procedure TestGenerator.GenMethod_ConnectFields_DataSet_OneString;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['FullName', ftString]]);

  actualCode := fGenerator.Generate_MethodConnectFields;

  Assert.AreMemosEqual(
    (* *) 'procedure TSomethingProxy.ConnectFields;'#13#10 +
    (* *) 'const'#13#10 +
    (* *) '  ExpectedFieldCount = 1;'#13#10 +
    (* *) 'begin'#13#10 +
    (* *) '  FFullName := FDataSet.FieldByName(''FullName'') as TStringField;'#13#10
    (* *) + '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13#10 +
    (* *) 'end;'#13#10, actualCode);
end;

procedure TestGenerator.GenMethod_ConnectFields_TwoFields_LowerCaseStyle;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['CustomerID', ftInteger],
    ['CompanyName', ftString]]);
  fGenerator.ObjectName := 'Something';
  fGenerator.FieldNamingStyle := fnsLowerCaseF;

  actualCode := fGenerator.Generate_MethodConnectFields;

  Assert.AreMemosEqual(
    (* *) 'procedure TSomethingProxy.ConnectFields;'#13#10
    (* *) + 'const'#13#10
    (* *) + '  ExpectedFieldCount = 2;'#13#10
    (* *) + 'begin'#13#10
    (* *) + '  fCustomerID := FDataSet.FieldByName(''CustomerID'') as TIntegerField;'#13#10
    (* *) + '  fCompanyName := FDataSet.FieldByName(''CompanyName'') as TStringField;'#13#10
    (* *) + '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13#10
    (* *) + 'end;'#13#10, actualCode);
end;

procedure TestGenerator.GenMethod_ConnectFields_Identation4;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([
    {} ['CustomerID', ftInteger],
    {} ['CompanyName', ftString]]);
  fGenerator.ObjectName := 'Foo';
  fGenerator.IdentationText := '    ';

  actualCode := fGenerator.Generate_MethodConnectFields;

  Assert.AreMemosEqual(
    {} 'procedure TFooProxy.ConnectFields;'#13#10
    {} + 'const'#13#10
    {} + '    ExpectedFieldCount = 2;'#13#10
    {} + 'begin'#13#10
    {} + '    FCustomerID := FDataSet.FieldByName(''CustomerID'') as TIntegerField;'#13#10
    {} + '    FCompanyName := FDataSet.FieldByName(''CompanyName'') as TStringField;'#13#10
    {} + '    Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13#10
    {} + 'end;'#13#10, actualCode);
end;

// -----------------------------------------------------------------------
// Tests: Generate books proxy
// -----------------------------------------------------------------------

procedure TestGenerator.Generate_BooksProxy_Unit;
begin
  fGenerator.DataSet := GivenDataset([
    {} ['ISBN', ftWideString, 20],
    {} ['Title', ftWideString, 100],
    {} ['Authors', ftWideString, 100],
    {} ['ReleseDate', ftDate],
    {} ['Pages', ftInteger],
    {} ['Price', ftBCD, 12, 2]]);
  fGenerator.ObjectName := 'Books';

  fGenerator.Execute;

  Assert.AreMemosEqual(
    {} 'unit Proxy.Books;'#13#10
    {} + sLineBreak
    {} + 'uses'#13#10
    {} + '  Data.DB,'#13#10
    {} + '  Data.DataProxy,'#13#10
    {} + '  System.SysUtils,'#13#10
    {} + '  System.Classes,'#13#10
    {} + '  FireDAC.Comp.Client;'#13#10
    {} + ''#13#10
    {} + 'type'#13#10
    {} + '  TBooksProxy = class(TDatasetProxy)'#13#10
    {} + '  private'#13#10
    {} + '    FISBN :TWideStringField;'#13#10
    {} + '    FTitle :TWideStringField;'#13#10
    {} + '    FAuthors :TWideStringField;'#13#10
    {} + '    FReleseDate :TDateField;'#13#10
    {} + '    FPages :TIntegerField;'#13#10
    {} + '    FPrice :TBCDField;'#13#10
    {} + '  protected'#13#10
    {} + '    procedure ConnectFields; override;'#13#10
    {} + '  public'#13#10
    {} + '    property ISBN :TWideStringField read FISBN;'#13#10
    {} + '    property Title :TWideStringField read FTitle;'#13#10
    {} + '    property Authors :TWideStringField read FAuthors;'#13#10
    {} + '    property ReleseDate :TDateField read FReleseDate;'#13#10
    {} + '    property Pages :TIntegerField read FPages;'#13#10
    {} + '    property Price :TBCDField read FPrice;'#13#10
    {} + '  end;'#13#10
    {} + sLineBreak
    {} + 'implementation'#13#10
    {} + sLineBreak
    {} + 'procedure TBooksProxy.ConnectFields;'#13#10
    {} + 'const'#13#10
    {} + '  ExpectedFieldCount = 6;'#13#10
    {} + 'begin'#13#10
    {} + '  FISBN := FDataSet.FieldByName(''ISBN'') as TWideStringField;'#13#10
    {} + '  FTitle := FDataSet.FieldByName(''Title'') as TWideStringField;'#13#10
    {} + '  FAuthors := FDataSet.FieldByName(''Authors'') as TWideStringField;'#13#10
    {} + '  FReleseDate := FDataSet.FieldByName(''ReleseDate'') as TDateField;'#13#10
    {} + '  FPages := FDataSet.FieldByName(''Pages'') as TIntegerField;'#13#10
    {} + '  FPrice := FDataSet.FieldByName(''Price'') as TBCDField;'#13#10
    {} + '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13#10
    {} + 'end;'#13#10, fGenerator.Code.Text);
end;

procedure TestGenerator.Generate_BooksProxy_Class;
begin
  fGenerator.DataSet := GivenDataset([
    {} ['ISBN', ftWideString, 20],
    {} ['Pages', ftInteger]]);
  fGenerator.ObjectName := 'Books2';

  fGenerator.GeneratorMode := pgmClass;
  fGenerator.Execute;

  Assert.AreMemosEqual(
    {} 'type'#13#10
    {} + '  TBooks2Proxy = class(TDatasetProxy)'#13#10
    {} + '  private'#13#10
    {} + '    FISBN :TWideStringField;'#13#10
    {} + '    FPages :TIntegerField;'#13#10
    {} + '  protected'#13#10
    {} + '    procedure ConnectFields; override;'#13#10
    {} + '  public'#13#10
    {} + '    property ISBN :TWideStringField read FISBN;'#13#10
    {} + '    property Pages :TIntegerField read FPages;'#13#10
    {} + '  end;'#13#10
    {} + sLineBreak
    {} + 'procedure TBooks2Proxy.ConnectFields;'#13#10
    {} + 'const'#13#10
    {} + '  ExpectedFieldCount = 2;'#13#10
    {} + 'begin'#13#10
    {} + '  FISBN := FDataSet.FieldByName(''ISBN'') as TWideStringField;'#13#10
    {} + '  FPages := FDataSet.FieldByName(''Pages'') as TIntegerField;'#13#10
    {} + '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13#10
    {} + 'end;'#13#10, fGenerator.Code.Text);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerator);

end.
