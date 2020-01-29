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
    function Generate_PrivateField(fld: TField): string;
    function Generate_PublicProperty(fld: TField): string;
    function Generate_FieldAssigment(fld: TField): string;
    function Generate_MethodConnectFields: string;
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

function TTestProxyDataGenerator.Generate_PrivateField(fld: TField): string;
begin
  Result := Gen_PrivateField(fld);
end;

function TTestProxyDataGenerator.Generate_PublicProperty(fld: TField): string;
begin
  Result := Gen_PublicProperty(fld);
end;

function TTestProxyDataGenerator.Generate_FieldAssigment(fld: TField): string;
begin
  Result := Gen_FieldAssigment(fld);
end;

function TTestProxyDataGenerator.Generate_MethodConnectFields: string;
begin
  Result := Gen_MethodConnectFields;
end;

end.
