unit Test.SqlDataSetProxy;

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
  TestSqDemoProxy = class(TObject)
  private const
    TestUsingFireDefinitionName = 'SQLite_Demo';
  private
    fOwner: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure CheckFireDAC_ConnectionDef;
  end;

implementation

uses
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestSqDemoProxy.Setup;
begin
  fOwner := TComponent.Create(nil);
end;

procedure TestSqDemoProxy.TearDown;
begin
  fOwner.Free;
end;

// -----------------------------------------------------------------------
// Tests: CheckFireDAC
// -----------------------------------------------------------------------

procedure TestSqDemoProxy.CheckFireDAC_ConnectionDef;
begin
  Assert.IsTrue(FDManager.ConnectionDefs.FindConnectionDef
    (TestUsingFireDefinitionName) <> nil, 'Test fixture ' + Self.ClassName +
    ' required FireDAC to work. ' + 'Expected connction definition "' +
    TestUsingFireDefinitionName + '" not found.');
end;

end.
