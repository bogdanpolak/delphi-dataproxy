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
  strict private
    class var Expected: string;
    class function ReplaceArrowsAndDiamonds(const s: String): string; static;
  public
    class procedure Asset_UsesSection(Code: TStrings);
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

initialization

TDUnitX.RegisterTestFixture(ProxyGenerator);

end.
