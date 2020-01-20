unit Wrapper.TProxyGenerator;

interface

uses
  Data.DB,
  Comp.Generator.DataProxy;

type
  TTestProxyDataGenerator = class(TDataProxyGenerator)
  public
    function Generate_UnitHeader: string;
    function Generate_UsesSection: string;
    function Generate_ClassDeclaration: string;
    procedure Generate_PrivateFieldList(aFields: TFields);
    procedure Generate_PublicPropertyList(aFields: TFields);
    procedure Generate_FieldAssigments(aFields: TFields);
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

procedure TTestProxyDataGenerator.Generate_PrivateFieldList(aFields: TFields);
begin
  Code.Text := Gen_PrivateFieldList(aFields);
end;

procedure TTestProxyDataGenerator.Generate_PublicPropertyList(aFields: TFields);
begin
  Code.Text := Gen_PublicPropertyList(aFields);
end;

procedure TTestProxyDataGenerator.Generate_FieldAssigments(aFields: TFields);
begin
  Code.Text := Gen_FieldAssigments(aFields);
end;

procedure TTestProxyDataGenerator.Generate_MethodConnectFields;
begin
  Code.Text := Gen_MethodConnectFields;
end;

end.
