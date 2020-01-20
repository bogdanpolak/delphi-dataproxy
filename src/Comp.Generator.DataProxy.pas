unit Comp.Generator.DataProxy;

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Math,
  Data.DB,
  System.Generics.Collections;

type
  TFieldNamingStyle = (fnsUpperCaseF, fnsLowerCaseF);
  TDataSetAccess = (dsaNoAccess, dsaGenComment, dsaFullAccess);

  TDataProxyGenerator = class(TComponent)
  private const
    Version = '0.9';
  private
    fDataSet: TDataSet;
    fCode: TStringList;
    fDataSetAccess: TDataSetAccess;
    fFieldNamingStyle: TFieldNamingStyle;
    fObjectName: string;
    procedure Guard;
    function GetFieldPrefix: string;
  protected
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
    property DataSet: TDataSet read fDataSet write fDataSet;
    property DataSetAccess: TDataSetAccess read fDataSetAccess
      write fDataSetAccess;
    property FieldNamingStyle: TFieldNamingStyle read fFieldNamingStyle
      write fFieldNamingStyle;
    property ObjectName: string read fObjectName write fObjectName;
  end;

implementation

constructor TDataProxyGenerator.Create(Owner: TComponent);
begin
  inherited;
  fCode := TStringList.Create;
  fDataSet := nil;
  fObjectName := 'Something';
  fDataSetAccess := dsaNoAccess;
end;

destructor TDataProxyGenerator.Destroy;
begin
  fCode.Free;
  inherited;
end;

procedure TDataProxyGenerator.Guard;
begin
  Assert(fDataSet <> nil);
  Assert(fDataSet.Active);
end;

function TDataProxyGenerator.Gen_UnitHeader: string;
begin
end;

function TDataProxyGenerator.Gen_UsesSection: string;
begin
  Result :=
  (* *) 'uses' + sLineBreak +
  (* *) '  Data.DB,' + sLineBreak +
  (* *) '  Data.DataProxy,' + sLineBreak +
  (* *) '  System.SysUtils,' + sLineBreak +
  (* *) '  System.Classes,' + sLineBreak +
  (* *) '  FireDAC.Comp.Client;' + sLineBreak;
end;

function GetFieldClassName(fld: TField): string;
begin
  Result := Data.DB.DefaultFieldClasses[fld.DataType].ClassName;
end;

function TDataProxyGenerator.GetFieldPrefix: string;
begin
  case fFieldNamingStyle of
    fnsUpperCaseF:
      Result := 'F';
    fnsLowerCaseF:
      Result := 'f';
  end;
end;

function TDataProxyGenerator.Gen_PrivateFieldList: string;
var
  fld: TField;
begin
  Result := '';
  if fDataSet <> nil then
    for fld in fDataSet.Fields do
      Result := Result + Format(
        (* *) '    %s :%s;' + sLineBreak,
        (* *) [GetFieldPrefix + fld.FieldName, GetFieldClassName(fld)]);
end;

function TDataProxyGenerator.Gen_PublicPropertyList: string;
var
  fld: TField;
begin
  Result := '';
  if fDataSet <> nil then
    for fld in fDataSet.Fields do
      Result := Result + Format(
        (* *) '    property %s :%s read %s;' + sLineBreak,
        (* *) [fld.FieldName, GetFieldClassName(fld),
        GetFieldPrefix + fld.FieldName]);
end;

function TDataProxyGenerator.Gen_FieldAssigments: string;
var
  fld: TField;
begin
  Result := '';
  if fDataSet <> nil then
    for fld in fDataSet.Fields do
      Result := Result + Format(
        (* *) '  %s := FDataSet.FieldByName(''%s'') as %s;' + sLineBreak,
        (* *) [GetFieldPrefix + fld.FieldName, fld.FieldName,
        GetFieldClassName(fld)]);
end;

function TDataProxyGenerator.Gen_ClassDeclaration: string;
var
  aDatasePropertyCode: string;
begin
  case fDataSetAccess of
    dsaNoAccess:
      aDatasePropertyCode := '';
    dsaGenComment:
      aDatasePropertyCode :=
      (* *) '    // the following property should be hidden (uncomment if required)'
        + sLineBreak +
      (* *) '    // property DataSet: TDataSet read FDataSet;' + sLineBreak;
    dsaFullAccess:
      aDatasePropertyCode :=
      (* *) '    property DataSet: TDataSet read FDataSet;' + sLineBreak;
  end;
  Result :=
  (* *) 'type' + sLineBreak +
  (* *) '  T' + fObjectName + 'Proxy = class(TDatasetProxy)' + sLineBreak +
  (* *) '  private' + sLineBreak +
  (* *) Gen_PrivateFieldList +
  (* *) '  protected' + sLineBreak +
  (* *) '    procedure ConnectFields; override;' + sLineBreak +
  (* *) '  public' + sLineBreak +
  (* *) Gen_PublicPropertyList +
  (* *) aDatasePropertyCode +
  (* *) '  end;' + sLineBreak;
end;

function TDataProxyGenerator.Gen_MethodConnectFields: string;
var
  aFieldCount: Integer;
begin
  if fDataSet <> nil then
    aFieldCount := fDataSet.Fields.Count
  else
    aFieldCount := 0;
  Result :=
  (* *) 'procedure T' + fObjectName + 'Proxy.ConnectFields;' + sLineBreak +
  (* *) 'const' + sLineBreak +
  (* *) '  ExpectedFieldCount = ' + aFieldCount.ToString + ';' + sLineBreak +
  (* *) 'begin' + sLineBreak +
  (* *) Gen_FieldAssigments +
  (* *) '  Assert(FDataSet.Fields.Count = ExpectedFieldCount);' + sLineBreak +
  (* *) 'end;' + sLineBreak;
end;

procedure TDataProxyGenerator.Execute;
begin
  Guard;
  fCode.Text :=
  (* *) Gen_UnitHeader +
  (* *) Gen_UsesSection +
  (* *) sLineBreak +
  (* *) Gen_ClassDeclaration +
  (* *) sLineBreak +
  (* *) 'implementation' + sLineBreak +
  (* *) sLineBreak +
  (* *) Gen_MethodConnectFields;
end;

end.
