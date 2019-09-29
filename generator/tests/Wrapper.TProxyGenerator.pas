unit Wrapper.TProxyGenerator;

interface

uses
  Comp.Generator.ProxyCode;

type
  TProxyCodeGenerator_AUT = class(TProxyCodeGenerator)
  public
    procedure Generate_UnitHeader;
    procedure Generate_UsesSection;
    procedure Generate_ClassDeclaration;
    procedure Generate_MethodConnectFields;
  end;

implementation

{ TProxyCodeGeneratorCUT }

procedure TProxyCodeGenerator_AUT.Generate_UnitHeader;
begin
  DoGenerate_UnitHeader;
end;

procedure TProxyCodeGenerator_AUT.Generate_UsesSection;
begin
  DoGenerate_UsesSection;

end;

procedure TProxyCodeGenerator_AUT.Generate_ClassDeclaration;
begin
  DoGenerate_ClassDeclaration;
end;

procedure TProxyCodeGenerator_AUT.Generate_MethodConnectFields;
begin
  DoGenerate_MethodConnectFields;
end;

end.
