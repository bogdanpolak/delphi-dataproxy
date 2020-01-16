unit Test.ProxyGenerator;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils, System.Variants,
  Data.DB,
  FireDAC.Comp.Client,
  Wrapper.TProxyGenerator;

{$M+}

type

  [TestFixture]
  ProxyGenerator = class(TObject)
  private
    OwnerComponent: TComponent;
    ProxyCodeGenerator: TTestProxyDataGenerator;
    MemDataSet: TFDMemTable;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure Test_UnitHeader_IsEmpty;
    procedure Test_UsesSection;
    procedure Test_ClassDeclaration_DataSetNil;
    procedure Test_ClassDeclaration_DataSetOneField;
    procedure Test_MethodConnectFields_DataSetNil;
    procedure Test_MethodConnectFields_DataSetOneField;
  end;

implementation

// -----------------------------------------------------------------------
// Utils section
// -----------------------------------------------------------------------

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure ProxyGenerator.Setup;
begin
  OwnerComponent := TComponent.Create(nil);
  ProxyCodeGenerator := TTestProxyDataGenerator.Create(OwnerComponent);
  with ProxyCodeGenerator do
  begin
    GenCommentsWithPublicDataSet := false;
  end;
  MemDataSet := TFDMemTable.Create(OwnerComponent);
end;

procedure ProxyGenerator.TearDown;
begin
  OwnerComponent.Free;
end;

// -----------------------------------------------------------------------
// Templates
// -----------------------------------------------------------------------

type
  TProxyTemplates = class
  const
    SingeCodeIndentation = '  ';
    Section_Uses =
    (* *) 'uses→' +
    (* *) '◇Data.DB,→' +
    (* *) '◇Data.DataProxy,→' +
    (* *) '◇System.SysUtils,→' +
    (* *) '◇System.Classes,→' +
    (* *) '◇FireDAC.Comp.Client;→';
    Section_ClassDeclatarion =
    (* *) 'type→' +
    (* *) '◇T{ObjectName}Proxy = class(TDatasetProxy)→' +
    (* *) '◇private→' +
    (* *) '◇protected→' +
    (* *) '◇◇procedure ConnectFields; override;→' +
    (* *) '◇public→' +
    (* *) '◇end;→';
    Section_ClassDeclatarion_WithIntField =
    (* *) 'type→' +
    (* *) '◇T{ObjectName}Proxy = class(TDatasetProxy)→' +
    (* *) '◇private→' +
    (* *) '◇◇FFieldInteger :TIntegerField;→' +
    (* *) '◇protected→' +
    (* *) '◇◇procedure ConnectFields; override;→' +
    (* *) '◇public→' +
    (* *) '◇◇property FieldInteger :TIntegerField read FFieldInteger;→' +
    (* *) '◇end;→';
    Section_MethodConnectFields =
    (* *) 'procedure T{ObjectName}Proxy.ConnectFields;→' +
    (* *) 'const→' +
    (* *) '◇ExpectedFieldCount = 0;→' +
    (* *) 'begin→' +
    (* *) '◇Assert(FDataSet.Fields.Count = ExpectedFieldCount);→' +
    (* *) 'end;→';
    Section_MethodConnectFields_WithIntegerField =
    (* *) 'procedure T{ObjectName}Proxy.ConnectFields;→' +
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
    class procedure Asset_UsesSection(Code: TStrings);
    class procedure Assert_ClassDeclaration(Code: TStrings);
    class procedure Assert_ClassDeclaration_WithIntegerField(Code: TStrings);
    class procedure Assert_MethodConnectFields(Code: TStrings);
    class procedure Assert_MethodConnectFields_WithIntegerField(Code: TStrings);
  end;

class function TProxyTemplates.ReplaceArrowsAndDiamonds
  (const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '◇', SingeCodeIndentation, [rfReplaceAll])
end;

class procedure TProxyTemplates.Asset_UsesSection(Code: TStrings);
begin
  Expected := ReplaceArrowsAndDiamonds(Section_Uses);
  Assert.AreEqual(Expected, Code.Text);
end;

class procedure TProxyTemplates.Assert_ClassDeclaration(Code: TStrings);
begin
  Expected := ReplaceArrowsAndDiamonds(Section_ClassDeclatarion);
  Assert.AreEqual(Expected, Code.Text);
end;

class procedure TProxyTemplates.Assert_ClassDeclaration_WithIntegerField
  (Code: TStrings);
begin
  Expected := ReplaceArrowsAndDiamonds(Section_ClassDeclatarion_WithIntField);
  Assert.AreEqual(Expected, Code.Text);
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
// Test Unit Header and Uses Section
// -----------------------------------------------------------------------
{$REGION 'Test Unit Header and Uses Section'}

procedure ProxyGenerator.Test_UnitHeader_IsEmpty;
begin
  ProxyCodeGenerator.Generate_UnitHeader;
  Assert.AreEqual('', ProxyCodeGenerator.Code.Text);
end;

procedure ProxyGenerator.Test_UsesSection;
begin
  ProxyCodeGenerator.Generate_UsesSection;
  TProxyTemplates.Asset_UsesSection(ProxyCodeGenerator.Code);
end;
{$ENDREGION}


// -----------------------------------------------------------------------
// Test Class Declaration Section
// -----------------------------------------------------------------------
{$REGION 'Test Class Declaration Section'}

procedure ProxyGenerator.Test_ClassDeclaration_DataSetNil;
begin
  ProxyCodeGenerator.Generate_ClassDeclaration;
  TProxyTemplates.Assert_ClassDeclaration(ProxyCodeGenerator.Code);
end;

procedure ProxyGenerator.Test_ClassDeclaration_DataSetOneField;
begin
  with MemDataSet do
  begin
    FieldDefs.Add('FieldInteger', ftInteger);
    CreateDataSet;
  end;
  ProxyCodeGenerator.DataSet := MemDataSet;
  ProxyCodeGenerator.Generate_ClassDeclaration;
  TProxyTemplates.Assert_ClassDeclaration_WithIntegerField
    (ProxyCodeGenerator.Code);
end;
{$ENDREGION}


// -----------------------------------------------------------------------
// Test Method ConnectFields Section
// -----------------------------------------------------------------------
{$REGION 'Test Method ConnectFields Section'}

procedure ProxyGenerator.Test_MethodConnectFields_DataSetNil;
begin
  ProxyCodeGenerator.Generate_MethodConnectFields;
  TProxyTemplates.Assert_MethodConnectFields(ProxyCodeGenerator.Code);
end;

procedure ProxyGenerator.Test_MethodConnectFields_DataSetOneField;
begin
  with MemDataSet do
  begin
    FieldDefs.Add('FieldInteger', ftInteger);
    CreateDataSet;
  end;
  ProxyCodeGenerator.DataSet := MemDataSet;
  ProxyCodeGenerator.Generate_MethodConnectFields;
  TProxyTemplates.Assert_MethodConnectFields_WithIntegerField
    (ProxyCodeGenerator.Code);
end;
{$ENDREGION}


initialization

TDUnitX.RegisterTestFixture(ProxyGenerator);

end.
