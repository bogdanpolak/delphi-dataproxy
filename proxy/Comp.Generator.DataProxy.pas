unit Comp.Generator.DataProxy;

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
    procedure Fill_FieldList;
    procedure Guard;
    class function GetFieldClassName(fld: TField): string;
  protected
    procedure DoGenerate_UnitHeader;
    procedure DoGenerate_UsesSection;
    procedure DoGenerate_ClassDeclaration;
    procedure DoGenerate_PrivateFieldList;
    procedure DoGenerate_PublicPropertyList;
    procedure DoGenerate_FieldAssigments;
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
  FDataSet := nil;
  GenCommentsWithPublicDataSet := true;
end;

destructor TProxyCodeGenerator.Destroy;
begin
  Fields.Free;
  FCode.Free;
  inherited;
end;

procedure TProxyCodeGenerator.Fill_FieldList;
var
  i: integer;
  IsEqual: boolean;
  fld: TField;
begin
  // ------------------------------------
  // Checks if Field List is equal to DataSet's Fields
  //
  IsEqual := (DataSet <> nil) and (DataSet.Fields.Count = Fields.Count);
  if not IsEqual then
  begin
    for i := 0 to Fields.Count - 1 do
      if Fields[i] <> DataSet.Fields[i] then
      begin
        IsEqual := false;
        Break;
      end;
  end;
  // ------------------------------------
  // if not equal then fill Fields list
  //
  if IsEqual then
    exit;
  Fields.Clear;
  if DataSet <> nil then
    for fld in DataSet.Fields do
      Fields.Add(fld);
  // ------------------------------------
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

procedure TProxyCodeGenerator.DoGenerate_PrivateFieldList;
var
  fld: TField;
begin
  Fill_FieldList;
  for fld in Fields do
    Code.Add('    F' + fld.FieldName + ' :' + GetFieldClassName(fld) + ';');
end;

procedure TProxyCodeGenerator.DoGenerate_PublicPropertyList;
var
  fld: TField;
begin
  Fill_FieldList;
  for fld in Fields do
    Code.Add('    property ' + fld.FieldName + ' :' + GetFieldClassName(fld) +
      ' read F' + fld.FieldName + ';');
end;

procedure TProxyCodeGenerator.DoGenerate_FieldAssigments;
var
  fld: TField;
begin
  Fill_FieldList;
  for fld in Fields do
    Code.Add('  F' + fld.FieldName + ' := FDataSet.FieldByName(' +
      QuotedStr(fld.FieldName) + ') as ' + GetFieldClassName(fld) + ';');
end;

procedure TProxyCodeGenerator.DoGenerate_ClassDeclaration;
begin
  Code.Add('type');
  Code.Add('  T{ObjectName}Proxy = class(TDatasetProxy)');
  Code.Add('  private');
  DoGenerate_PrivateFieldList;
  Code.Add('  protected');
  Code.Add('    procedure ConnectFields; override;');
  Code.Add('  public');
  DoGenerate_PublicPropertyList;
  if GenCommentsWithPublicDataSet then
  begin
    Code.Add('    // this property should be hidden, but during migration can be usefull');
    Code.Add('    // property DataSet: TDataSet read FDataSet;');
  end;
  Code.Add('  end;');
end;

procedure TProxyCodeGenerator.DoGenerate_MethodConnectFields;
begin
  Fill_FieldList;
  Code.Add('procedure T{ObjectName}Proxy.ConnectFields;');
  Code.Add('const');
  Code.Add('  ExpectedFieldCount = ' + Fields.Count.ToString + ';');
  Code.Add('begin');
  DoGenerate_FieldAssigments;
  Code.Add('  Assert(FDataSet.Fields.Count = ExpectedFieldCount);');
  Code.Add('end;');
end;

procedure TProxyCodeGenerator.Execute;
begin
  Guard;
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
