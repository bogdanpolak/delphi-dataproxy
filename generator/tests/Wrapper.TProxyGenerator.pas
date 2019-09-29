unit Wrapper.TProxyGenerator;

interface

uses
  Comp.Generator.ProxyCode;

type
  TProxyCodeGeneratorCUT = class (TProxyCodeGenerator)
  public
    procedure Generate_UnitHeader;
    procedure Generate_UsesSection;
    procedure Generate_ClassDeclaration;
    procedure Generate_MethodConnectFields;
  end;

implementation

{ TProxyCodeGeneratorCUT }

procedure TProxyCodeGeneratorCUT.Generate_UnitHeader;
begin
  DoGenerate_UnitHeader;
end;

procedure TProxyCodeGeneratorCUT.Generate_UsesSection;
begin
  DoGenerate_UsesSection;

end;

procedure TProxyCodeGeneratorCUT.Generate_ClassDeclaration;
begin
  DoGenerate_ClassDeclaration;
end;

procedure TProxyCodeGeneratorCUT.Generate_MethodConnectFields;
begin
  DoGenerate_MethodConnectFields;
end;

end.
