unit Wrapper.TProxyGenerator;

interface

uses
  Comp.Generator.DataProxy;

type
  TTestProxyDataGenerator = class(TDataProxyGenerator)
  public
    function Generate_UnitHeader: string;
    function Generate_UsesSection: string;
    function Generate_ClassDeclaration: string;
    procedure Generate_PrivateFieldList;
    procedure Generate_PublicPropertyList;
    procedure Generate_FieldAssigments;
    procedure Generate_MethodConnectFields;
  end;

implementation

function TTestProxyDataGenerator.Generate_UnitHeader: string;
begin
  Result := Gen_UnitHeader;
end;

function TTestProxyDataGenerator.Generate_UsesSection: string;
begin
  Result := Gen_UsesSection;
end;

function TTestProxyDataGenerator.Generate_ClassDeclaration: string;
begin
  Result := Gen_ClassDeclaration;
end;

procedure TTestProxyDataGenerator.Generate_PrivateFieldList;
begin
  Code.Text := Gen_PrivateFieldList;
end;

procedure TTestProxyDataGenerator.Generate_PublicPropertyList;
begin
  Code.Text := Gen_PublicPropertyList;
end;

procedure TTestProxyDataGenerator.Generate_FieldAssigments;
begin
  Code.Text := Gen_FieldAssigments;
end;

procedure TTestProxyDataGenerator.Generate_MethodConnectFields;
begin
  Code.Text := Gen_MethodConnectFields;
end;

end.
