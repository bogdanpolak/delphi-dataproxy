unit Comp.Generator.ProxyCode;

interface

uses
  Data.DB,
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  TProxyCodeGenerator = class(TComponent)
  private
    Fields: TList<TField>;
    FDataSet: TDataSet;
    FCode: TStringList;
    FGenCommentsWithPublicDataSet: boolean;
    procedure FillFieldsFromDataSet(ds: TDataSet);
    procedure Guard;
    class function GetFieldClassName(fld: TField): string;
  protected
    procedure DoGenerate_UnitHeader;
    procedure DoGenerate_UsesSection;
    procedure DoGenerate_ClassDeclaration;
    procedure DoGenerate_MethodConnectFields;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property Code: TStringList read FCode;
    property DataSet: TDataSet read FDataSet write FDataSet;
    property GenCommentsWithPublicDataSet: boolean
      read FGenCommentsWithPublicDataSet write FGenCommentsWithPublicDataSet;
    procedure Execute;
  end;

implementation

constructor TProxyCodeGenerator.Create(Owner: TComponent);
begin
  inherited;
  Fields := TList<TField>.Create();
  FCode := TStringList.Create;
  GenCommentsWithPublicDataSet := true;
end;

destructor TProxyCodeGenerator.Destroy;
begin
  Fields.Free;
  FCode.Free;
  inherited;
end;

procedure TProxyCodeGenerator.FillFieldsFromDataSet(ds: TDataSet);
var
  i: integer;
begin
  Fields.Clear;
  for i := 0 to ds.Fields.Count - 1 do
    Fields.Add(ds.Fields[i]);
end;

procedure TProxyCodeGenerator.Guard;
begin
  Assert(DataSet <> nil);
  Assert(DataSet.Active);
end;

procedure TProxyCodeGenerator.DoGenerate_UnitHeader;
begin
end;

procedure TProxyCodeGenerator.DoGenerate_UsesSection;
begin
  Code.Add('uses');
  Code.Add('  Data.DB,');
  Code.Add('  Data.DataProxy,');
  Code.Add('  System.SysUtils,');
  Code.Add('  System.Classes,');
  Code.Add('  FireDAC.Comp.Client;');
end;

class function TProxyCodeGenerator.GetFieldClassName(fld: TField): string;
begin
  Result := Data.DB.DefaultFieldClasses[fld.DataType].ClassName;
end;

procedure TProxyCodeGenerator.DoGenerate_ClassDeclaration;
var
  fld: TField;
begin
  Code.Add('type');
  Code.Add('  T{ObjectName}Proxy = class(TDatasetProxy)');
  Code.Add('  private');
  for fld in Fields do
    Code.Add('    F' + fld.FieldName + ' :' + GetFieldClassName(fld) + ';');
  Code.Add('  protected');
  Code.Add('    procedure ConnectFields; override;');
  Code.Add('  public');
  for fld in Fields do
    Code.Add('    property ' + fld.FieldName + ' :' + GetFieldClassName(fld) +
      ' read F' + fld.FieldName + ';');
  if GenCommentsWithPublicDataSet then
  begin
    Code.Add('    // this property should be hidden, but during migration can be usefull');
    Code.Add('    // property DataSet: TDataSet read FDataSet;');
  end;
  Code.Add('  end;');
end;

procedure TProxyCodeGenerator.DoGenerate_MethodConnectFields;
var
  fld: TField;
begin
  Code.Add('procedure T{ObjectName}Proxy.ConnectFields;');
  Code.Add('const');
  Code.Add('  ExpectedFieldCount = ' + Fields.Count.ToString + ';');
  Code.Add('begin');
  for fld in Fields do
    Code.Add('  F' + fld.FieldName + ' := FDataSet.FieldByName(' +
      QuotedStr(fld.FieldName) + ') as ' + GetFieldClassName(fld) + ';');
  Code.Add('  Assert(FDataSet.Fields.Count = ExpectedFieldCount);');
  Code.Add('end;');
end;

procedure TProxyCodeGenerator.Execute;
begin
  Guard;
  FillFieldsFromDataSet(DataSet);
  DoGenerate_UnitHeader;
  DoGenerate_UsesSection;
  Code.Add('');
  DoGenerate_ClassDeclaration;
  Code.Add('');
  Code.Add('implementation');
  Code.Add('');
  DoGenerate_MethodConnectFields;
end;

end.
