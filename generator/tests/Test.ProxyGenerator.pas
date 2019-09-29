unit Test.ProxyGenerator;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils, System.Variants,
  Data.DB;

{$M+}

type

  [TestFixture]
  ProxyGenerator = class(TObject)
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure Test001;
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
  //
end;

procedure ProxyGenerator.TearDown;
begin
  //
end;

// -----------------------------------------------------------------------
// Templates
// -----------------------------------------------------------------------

// -----------------------------------------------------------------------
// Test001
// -----------------------------------------------------------------------
{$REGION 'Test001'}

procedure ProxyGenerator.Test001;
begin
end;

{$ENDREGION}

initialization

TDUnitX.RegisterTestFixture(ProxyGenerator);

end.
