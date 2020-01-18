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
  Wrapper.TProxyGenerator;

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
    procedure Test_UnitHeader_IsEmpty;
    procedure Test_UsesSection;
    // ---
    procedure Test_ClassDeclaration_DataSetNil;
    procedure Test_ClassDeclaration_DataSetOneField;
    procedure GenerateClass_TwoFields_LowerCase;
    procedure Gen_ProxyClass_DataSetAccessAsComment;
    procedure Gen_ProxyClass_FullDataSetAccess;
    // ---
    procedure Test_MethodConnectFields_DataSetNil;
    procedure Test_MethodConnectFields_DataSetOneField;
    procedure Gen_MethodConnectFields_TwoFields_LowerCase;
  end;

implementation

// -----------------------------------------------------------------------
// Utils section
// -----------------------------------------------------------------------

function TestGenerator.GivenDataset(aFieldsDef: TMatrixOfVariants): TDataSet;
var
  aTable: TFDMemTable;
  i: Integer;
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
// Templates
// -----------------------------------------------------------------------

type
  TProxyTemplates = class
  const
    SingeCodeIndentation = '  ';
    Section_MethodConnectFields =
    (* *) 'procedure TSomethingProxy.ConnectFields;→' +
    (* *) 'const→' +
    (* *) '◇ExpectedFieldCount = 0;→' +
    (* *) 'begin→' +
    (* *) '◇Assert(FDataSet.Fields.Count = ExpectedFieldCount);→' +
    (* *) 'end;→';
    Section_MethodConnectFields_WithIntegerField =
    (* *) 'procedure TSomethingProxy.ConnectFields;→' +
    (* *) 'const→' +
    (* *) '◇ExpectedFieldCount = 1;→' +
    (* *) 'begin→' +
    (* *) '◇FFieldInteger := FDataSet.FieldByName(''FieldInteger'') as TIntegerField;→'
      +
    (* *) '◇Assert(FDataSet.Fields.Count = ExpectedFieldCount);→' +
    (* *) 'end;→';
  strict private
    class var Expected: string;
    class function ReplaceArrowsAndDiamonds(const s: String): string;
  public
    class procedure Assert_MethodConnectFields(Code: TStrings);
    class procedure Assert_MethodConnectFields_WithIntegerField(Code: TStrings);
  end;

class function TProxyTemplates.ReplaceArrowsAndDiamonds
  (const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '◇', SingeCodeIndentation, [rfReplaceAll])
end;

class procedure TProxyTemplates.Assert_MethodConnectFields(Code: TStrings);
begin
  Expected := ReplaceArrowsAndDiamonds(Section_MethodConnectFields);
  Assert.AreEqual(Expected, Code.Text);
end;

class procedure TProxyTemplates.Assert_MethodConnectFields_WithIntegerField
  (Code: TStrings);
begin
  Expected := ReplaceArrowsAndDiamonds
    (Section_MethodConnectFields_WithIntegerField);
  Assert.AreEqual(Expected, Code.Text);
end;


// -----------------------------------------------------------------------
// Tests: Unit Header / Uses Section
// -----------------------------------------------------------------------

procedure TestGenerator.Test_UnitHeader_IsEmpty;
begin
  fGenerator.Generate_UnitHeader;
  Assert.AreEqual('', fGenerator.Code.Text);
end;

procedure TestGenerator.Test_UsesSection;
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


// -----------------------------------------------------------------------
// Tests: Class Declaration
// -----------------------------------------------------------------------

procedure TestGenerator.Test_ClassDeclaration_DataSetNil;
var
  actualCode: string;
begin
  fGenerator.DataSet := nil;

  fGenerator.ObjectName := 'Something1';
  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreEqual(
    (* *) 'type'#13#10 +
    (* *) '  TSomething1Proxy = class(TDatasetProxy)'#13#10 +
    (* *) '  private'#13#10 +
    (* *) '  protected'#13#10 +
    (* *) '    procedure ConnectFields; override;'#13#10 +
    (* *) '  public'#13#10 +
    (* *) '  end;'#13#10, actualCode);
end;

procedure TestGenerator.Test_ClassDeclaration_DataSetOneField;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['FieldInteger', ftInteger]]);

  fGenerator.ObjectName := 'Something2';
  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreEqual(
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

procedure TestGenerator.GenerateClass_TwoFields_LowerCase;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['CustomerID', ftInteger],
    ['CompanyName', ftString]]);

  fGenerator.ObjectName := 'Something';
  fGenerator.FieldNamingStyle := fnsLowerCaseF;
  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreEqual(
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
    (* *) '  end;'#13#10, actualCode, false);
end;

procedure TestGenerator.Gen_ProxyClass_DataSetAccessAsComment;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['FullName', ftString]]);

  fGenerator.ObjectName := 'Something';
  fGenerator.DataSetAccess := dsaGenComment;
  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreEqual(
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
    (* *) + '  end;'#13#10, actualCode, false);
end;

procedure TestGenerator.Gen_ProxyClass_FullDataSetAccess;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['FullName', ftString]]);

  fGenerator.ObjectName := 'Something';
  fGenerator.DataSetAccess := dsaFullAccess;
  actualCode := fGenerator.Generate_ClassDeclaration;

  Assert.AreEqual(
    (* *) 'type'#13#10
    (* *) + '  TSomethingProxy = class(TDatasetProxy)'#13#10
    (* *) + '  private'#13#10
    (* *) + '    FFullName :TStringField;'#13#10
    (* *) + '  protected'#13#10
    (* *) + '    procedure ConnectFields; override;'#13#10
    (* *) + '  public'#13#10
    (* *) + '    property FullName :TStringField read FFullName;'#13#10
    (* *) + '    property DataSet: TDataSet read FDataSet;'#13#10
    (* *) + '  end;'#13#10, actualCode, false);
end;

// -----------------------------------------------------------------------
// Tests: Method ConnectFields
// -----------------------------------------------------------------------

procedure TestGenerator.Test_MethodConnectFields_DataSetNil;
begin
  fGenerator.DataSet := nil;
  fGenerator.Generate_MethodConnectFields;
  TProxyTemplates.Assert_MethodConnectFields(fGenerator.Code);
end;

procedure TestGenerator.Test_MethodConnectFields_DataSetOneField;
begin
  fGenerator.DataSet := GivenDataset([['FieldInteger', ftInteger]]);

  fGenerator.Generate_MethodConnectFields;

  TProxyTemplates.Assert_MethodConnectFields_WithIntegerField(fGenerator.Code);
end;

procedure TestGenerator.Gen_MethodConnectFields_TwoFields_LowerCase;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataset([['CustomerID', ftInteger],
    ['CompanyName', ftString]]);

  fGenerator.ObjectName := 'Something';
  fGenerator.FieldNamingStyle := fnsLowerCaseF;
  fGenerator.Generate_MethodConnectFields;
  actualCode := fGenerator.Code.Text;

  Assert.AreEqual(
    (* *) 'procedure TSomethingProxy.ConnectFields;'#13#10
    (* *) + 'const'#13#10
    (* *) + '  ExpectedFieldCount = 2;'#13#10
    (* *) + 'begin'#13#10
    (* *) + '  fCustomerID := FDataSet.FieldByName(''CustomerID'') as TIntegerField;'#13#10
    (* *) + '  fCompanyName := FDataSet.FieldByName(''CompanyName'') as TStringField;'#13#10
    (* *) + '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13#10
    (* *) + 'end;'#13#10, actualCode, false);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerator);

end.
