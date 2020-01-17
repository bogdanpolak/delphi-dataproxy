unit Comp.Generator.DataProxy;

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  Data.DB,
  System.Generics.Collections;

type
  TFieldNamingStyle = (fnsUpperCaseF, fnsLowerCaseF);

  TDataProxyGenerator = class(TComponent)
  private
    Fields: TList<TField>;
    FDataSet: TDataSet;
    fCode: TStringList;
    FGenCommentsWithPublicDataSet: boolean;
    fFieldNamingStyle: TFieldNamingStyle;
    procedure Guard;
  protected
    procedure Fill_FieldList;
    function Gen_UnitHeader: string;
    function Gen_UsesSection: string;
    function Gen_ClassDeclaration: string;
    function Gen_PrivateFieldList: string;
    function Gen_PublicPropertyList: string;
    function Gen_FieldAssigments: string;
    function Gen_MethodConnectFields: string;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property Code: TStringList read fCode;
    property DataSet: TDataSet read FDataSet write FDataSet;
    property GenCommentsWithPublicDataSet: boolean
      read FGenCommentsWithPublicDataSet write FGenCommentsWithPublicDataSet;
    property FieldNamingStyle: TFieldNamingStyle read fFieldNamingStyle
      write fFieldNamingStyle;
  end;

implementation

constructor TDataProxyGenerator.Create(Owner: TComponent);
begin
  inherited;
  Fields := TList<TField>.Create();
  fCode := TStringList.Create;
  FDataSet := nil;
  GenCommentsWithPublicDataSet := true;
end;

destructor TDataProxyGenerator.Destroy;
begin
  Fields.Free;
  fCode.Free;
  inherited;
end;

procedure TDataProxyGenerator.Fill_FieldList;
var
  i: integer;
  IsEqual: boolean;
  fld: TField;
begin
  // ------------------------------------
  // Checks if Field List is equal to DataSet's Fields
  //
  IsEqual := (DataSet <> nil) and (DataSet.Fields.Count = Fields.Count);
  if IsEqual then
  begin
    for i := 0 to Fields.Count - 1 do
      if Fields[i] <> DataSet.Fields[i] then
      begin
        IsEqual := false;
        Break;
      end;
  end;
  // ------------------------------------
  if not IsEqual then
  begin
    Fields.Clear;
    if DataSet <> nil then
      for fld in DataSet.Fields do
        Fields.Add(fld);
  end;
end;

procedure TDataProxyGenerator.Guard;
begin
  Assert(DataSet <> nil);
  Assert(DataSet.Active);
end;

function TDataProxyGenerator.Gen_UnitHeader: string;
begin
end;

function TDataProxyGenerator.Gen_UsesSection: string;
begin
  Result :=
  (* *) 'uses'#13 +
  (* *) '  Data.DB,'#13 +
  (* *) '  Data.DataProxy,'#13 +
  (* *) '  System.SysUtils,'#13 +
  (* *) '  System.Classes,'#13 +
  (* *) '  FireDAC.Comp.Client;'#13;
end;

function GetFieldClassName(fld: TField): string;
begin
  Result := Data.DB.DefaultFieldClasses[fld.DataType].ClassName;
end;

function TDataProxyGenerator.Gen_PrivateFieldList: string;
var
  fld: TField;
begin
  Result := '';
  for fld in Fields do
    Result := Result + Format('    F%s :%s;'#13,
      [fld.FieldName, GetFieldClassName(fld)]);
end;

function TDataProxyGenerator.Gen_PublicPropertyList: string;
var
  fld: TField;
begin
  Result := '';
  for fld in Fields do
    Result := Result + Format('    property %s :%s read F%s;'#13,
      [fld.FieldName, GetFieldClassName(fld), fld.FieldName]);
end;

function TDataProxyGenerator.Gen_FieldAssigments: string;
var
  fld: TField;
begin
  Result := '';
  for fld in Fields do
    Result := Result + Format('  F%s := FDataSet.FieldByName(''%s'') as %s;'#13,
      [fld.FieldName, fld.FieldName, GetFieldClassName(fld)]);
end;

function TDataProxyGenerator.Gen_ClassDeclaration: string;
var
  aDatasePropertyCode: string;
begin
  aDatasePropertyCode := IfThen(GenCommentsWithPublicDataSet,
    (*   *) '    // this property should be hidden, but during migration can be usefull'#13
    (* *) + '    // property DataSet: TDataSet read FDataSet;'#13, '');
  Result :=
  (* *) 'type'#13 +
  (* *) '  T{ObjectName}Proxy = class(TDatasetProxy)'#13 +
  (* *) '  private'#13 +
  (* *) Gen_PrivateFieldList +
  (* *) '  protected'#13 +
  (* *) '    procedure ConnectFields; override;'#13 +
  (* *) '  public'#13 +
  (* *) Gen_PublicPropertyList +
  (* *) aDatasePropertyCode +
  (* *) '  end;';
end;

function TDataProxyGenerator.Gen_MethodConnectFields: string;
begin
  Result :=
  (* *) 'procedure T{ObjectName}Proxy.ConnectFields;'#13 +
  (* *) 'const'#13 +
  (* *) '  ExpectedFieldCount = ' + Fields.Count.ToString + ';'#13 +
  (* *) 'begin'#13 +
  (* *) Gen_FieldAssigments +
  (* *) '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);'#13 +
  (* *) 'end;'#13;
end;

procedure TDataProxyGenerator.Execute;
begin
  Guard;
  Fill_FieldList;
  fCode.Text :=
  (* *) Gen_UnitHeader +
  (* *) Gen_UsesSection +
  (* *) #13 +
  (* *) Gen_ClassDeclaration +
  (* *) #13 +
  (* *) 'implementation'#13 +
  (* *) #13 +
  (* *) Gen_MethodConnectFields;
end;

end.
