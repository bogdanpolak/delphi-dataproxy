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

procedure TTestProxyDataGenerator.Generate_UnitHeader;
begin
  Fill_FieldList;
  Code.Text := Gen_UnitHeader;
end;

procedure TTestProxyDataGenerator.Generate_UsesSection;
begin
  Fill_FieldList;
  Code.Text := Gen_UsesSection;
end;

procedure TTestProxyDataGenerator.Generate_ClassDeclaration;
begin
  Fill_FieldList;
  Code.Text := Gen_ClassDeclaration;
end;

procedure TTestProxyDataGenerator.Generate_PrivateFieldList;
begin
  Fill_FieldList;
  Code.Text := Gen_PrivateFieldList;
end;

procedure TTestProxyDataGenerator.Generate_PublicPropertyList;
begin
  Fill_FieldList;
  Code.Text := Gen_PublicPropertyList;
end;

procedure TTestProxyDataGenerator.Generate_FieldAssigments;
begin
  Fill_FieldList;
  Code.Text := Gen_FieldAssigments;
end;

procedure TTestProxyDataGenerator.Generate_MethodConnectFields;
begin
  Fill_FieldList;
  Code.Text := Gen_MethodConnectFields;
end;

end.
