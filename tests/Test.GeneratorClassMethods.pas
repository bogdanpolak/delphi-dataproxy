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
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // ---
    procedure Test_01;
  end;

{$TYPEINFO OFF}

implementation

// -----------------------------------------------------------------------
// Dataset factories
// -----------------------------------------------------------------------

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGeneratorClassMethods.Setup;
begin
  fOwner := TComponent.Create(nil);
end;

procedure TestGeneratorClassMethods.TearDown;
begin
  fOwner.Free;
end;

// -----------------------------------------------------------------------
// Tests: --
// -----------------------------------------------------------------------

procedure TestGeneratorClassMethods.Test_01;
begin
  Assert.Fail();
end;

end.
