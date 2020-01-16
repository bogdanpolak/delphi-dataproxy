unit Wrapper.TProxyGenerator;

interface

uses
  Comp.Generator.DataProxy;

type
  TTestProxyDataGenerator = class(TDataProxyGenerator)
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

procedure TTestProxyDataGenerator.Generate_UnitHeader;
begin
  DoGenerate_UnitHeader;
end;

procedure TTestProxyDataGenerator.Generate_UsesSection;
begin
  DoGenerate_UsesSection;

end;

procedure TTestProxyDataGenerator.Generate_ClassDeclaration;
begin
  DoGenerate_ClassDeclaration;
end;

procedure TTestProxyDataGenerator.Generate_PrivateFieldList;
begin
  DoGenerate_PrivateFieldList;
end;

procedure TTestProxyDataGenerator.Generate_PublicPropertyList;
begin
  DoGenerate_PublicPropertyList;
end;

procedure TTestProxyDataGenerator.Generate_FieldAssigments;
begin
  DoGenerate_FieldAssigments;
end;

procedure TTestProxyDataGenerator.Generate_MethodConnectFields;
begin
  DoGenerate_MethodConnectFields;
end;

end.
