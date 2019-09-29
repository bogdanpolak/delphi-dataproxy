unit Test.ProxyGenerator;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils, System.Variants,
  Data.DB,
  Wrapper.TProxyGenerator;

{$M+}

type

  [TestFixture]
  ProxyGenerator = class(TObject)
  private
    OwnerComponent: TComponent;
    ProxyCodeGenerator: TProxyCodeGenerator_AUT;
    function ReplaceArrowsAndDiamonds(const s: String): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure Test_UnitHeader_IsEmpty;
    procedure Test_UsesSection;
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
  ProxyCodeGenerator := TProxyCodeGenerator_AUT.Create(OwnerComponent);
end;

procedure ProxyGenerator.TearDown;
begin
  OwnerComponent.Free;
end;

// -----------------------------------------------------------------------
// Templates
// -----------------------------------------------------------------------

const
  SingeCodeIndentation = '  ';
  Section_Uses =
  (* *) 'uses→' +
  (* *) '◇Data.DB,→' +
  (* *) '◇Data.DataProxy,→' +
  (* *) '◇System.SysUtils,→' +
  (* *) '◇System.Classes,→' +
  (* *) '◇FireDAC.Comp.Client;→';

procedure Temp_procedure_required_because_of_formatter_error;
begin
end;

// -----------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------

function ProxyGenerator.ReplaceArrowsAndDiamonds(const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '◇', SingeCodeIndentation, [rfReplaceAll])
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
  Assert.AreEqual(ReplaceArrowsAndDiamonds(Section_Uses),
    ProxyCodeGenerator.Code.Text);
end;

{$ENDREGION}

initialization

TDUnitX.RegisterTestFixture(ProxyGenerator);

end.
