unit Wrapper.TProxyGenerator;

interface

uses
  Comp.Generator.DataProxy;

type
  TProxyCodeGenerator_AUT = class(TDataProxyGenerator)
  public
    procedure Generate_UnitHeader;
    procedure Generate_UsesSection;
    procedure Generate_ClassDeclaration;
    procedure Generate_PrivateFieldList;
    procedure Generate_PublicPropertyList;
    procedure Generate_FieldAssigments;
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

procedure TProxyCodeGenerator_AUT.Generate_PrivateFieldList;
begin
  DoGenerate_PrivateFieldList;
end;

procedure TProxyCodeGenerator_AUT.Generate_PublicPropertyList;
begin
  DoGenerate_PublicPropertyList;
end;

procedure TProxyCodeGenerator_AUT.Generate_FieldAssigments;
begin
  DoGenerate_FieldAssigments;
end;

procedure TProxyCodeGenerator_AUT.Generate_MethodConnectFields;
begin
  DoGenerate_MethodConnectFields;
end;

end.
