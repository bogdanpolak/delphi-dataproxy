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
    Version = '1.0';
  private
    fDataSet: TDataSet;
    fCode: TStringList;
    fDataSetAccess: TDataSetAccess;
    fFieldNamingStyle: TFieldNamingStyle;
    fObjectName: string;
    fIdentationText: string;
    procedure Guard;
    function GetFieldPrefix: string;
  protected
    function Gen_UnitHeader: string;
    function Gen_UsesSection: string;
    function Gen_ClassDeclaration: string;
    function Gen_PrivateField(fld: TField): string;
    function Gen_PublicProperty(fld: TField): string;
    function Gen_FieldAssigment(fld: TField): string;
    function Gen_MethodConnectFields: string;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    class procedure SaveToFile(const aFileName: string; aDataSet: TDataSet;
      const aSubjectName: string; const aIdentationText: string = '  ';
      aNamingStyle: TFieldNamingStyle = fnsUpperCaseF);
  published
    property Code: TStringList read fCode;
    property DataSet: TDataSet read fDataSet write fDataSet;
    // ---- options ----
    property DataSetAccess: TDataSetAccess read fDataSetAccess
      write fDataSetAccess;
    property FieldNamingStyle: TFieldNamingStyle read fFieldNamingStyle
      write fFieldNamingStyle;
    property ObjectName: string read fObjectName write fObjectName;
    property IdentationText: string read fIdentationText write fIdentationText;
  end;

implementation

constructor TDataProxyGenerator.Create(Owner: TComponent);
begin
  inherited;
  fCode := TStringList.Create;
  fDataSet := nil;
  fObjectName := 'Something';
  fDataSetAccess := dsaNoAccess;
  fIdentationText := '  ';
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
  Result :=
    {} 'unit Proxy.' + fObjectName + ';' + sLineBreak +
    {} sLineBreak;
end;

function TDataProxyGenerator.Gen_UsesSection: string;
begin
  Result :=
  (* *) 'uses' + sLineBreak +
  (* *) fIdentationText + 'Data.DB,' + sLineBreak +
  (* *) fIdentationText + 'Data.DataProxy,' + sLineBreak +
  (* *) fIdentationText + 'System.SysUtils,' + sLineBreak +
  (* *) fIdentationText + 'System.Classes,' + sLineBreak +
  (* *) fIdentationText + 'FireDAC.Comp.Client;' + sLineBreak;
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

function TDataProxyGenerator.Gen_PrivateField(fld: TField): string;
begin
  Result := GetFieldPrefix + fld.FieldName + ' :' +
    GetFieldClassName(fld) + ';';
end;

function TDataProxyGenerator.Gen_PublicProperty(fld: TField): string;
begin
  Result := 'property ' + fld.FieldName + ' :' + GetFieldClassName(fld) +
    ' read ' + GetFieldPrefix + fld.FieldName + ';';
end;

function TDataProxyGenerator.Gen_FieldAssigment(fld: TField): string;
begin
  Result := GetFieldPrefix + fld.FieldName + ' := FDataSet.FieldByName(''' +
    fld.FieldName + ''') as ' + GetFieldClassName(fld) + ';';
end;

function TDataProxyGenerator.Gen_ClassDeclaration: string;
var
  fld: TField;
  aPrivateFields: string;
  aPublicProperties: string;
  aDatasePropertyCode: string;
begin
  aPrivateFields := '';
  aPublicProperties := '';
  if fDataSet <> nil then
  begin
    for fld in fDataSet.Fields do
    begin
      aPrivateFields := aPrivateFields + fIdentationText + fIdentationText +
        Gen_PrivateField(fld) + sLineBreak;
      aPublicProperties := aPublicProperties + fIdentationText + fIdentationText
        + Gen_PublicProperty(fld) + sLineBreak;
    end;
  end;
  // ----
  case fDataSetAccess of
    dsaNoAccess:
      aDatasePropertyCode := '';
    dsaGenComment:
      aDatasePropertyCode :=
      {} fIdentationText + fIdentationText + '// the following property' +
        ' should be hidden (uncomment if required)' + sLineBreak +
      {} fIdentationText + fIdentationText + '// property DataSet: TDataSet' +
        ' read FDataSet;' + sLineBreak;
    dsaFullAccess:
      aDatasePropertyCode :=
      {} fIdentationText + fIdentationText + 'property DataSet: TDataSet' +
        ' read FDataSet;' + sLineBreak;
  end;
  // ----
  Result :=
  {} 'type' + sLineBreak +
  {} fIdentationText + 'T' + fObjectName + 'Proxy = class(TDatasetProxy)' +
    sLineBreak +
  {} fIdentationText + 'private' + sLineBreak +
  {} aPrivateFields +
  {} fIdentationText + 'protected' + sLineBreak +
  {} fIdentationText + fIdentationText + 'procedure ConnectFields; override;' +
    sLineBreak +
  {} fIdentationText + 'public' + sLineBreak +
  {} aPublicProperties +
  {} aDatasePropertyCode +
  {} fIdentationText + 'end;' + sLineBreak;
end;

function TDataProxyGenerator.Gen_MethodConnectFields: string;
var
  aFieldCount: Integer;
  fld: TField;
  aFieldAssigments: string;
begin
  if fDataSet <> nil then
  begin
    aFieldCount := fDataSet.Fields.Count;
    for fld in fDataSet.Fields do
      aFieldAssigments := aFieldAssigments + fIdentationText +
        Gen_FieldAssigment(fld) + sLineBreak;
  end
  else
  begin
    aFieldCount := 0;
    aFieldAssigments := '';
  end;
  Result :=
  {} 'procedure T' + fObjectName + 'Proxy.ConnectFields;' + sLineBreak +
  {} 'const' + sLineBreak +
  {} fIdentationText + 'ExpectedFieldCount = ' + aFieldCount.ToString + ';' +
    sLineBreak +
  {} 'begin' + sLineBreak +
  {} aFieldAssigments +
  {} fIdentationText + 'Assert(FDataSet.Fields.Count = ExpectedFieldCount);' +
    sLineBreak +
  {} 'end;' + sLineBreak;
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

class procedure TDataProxyGenerator.SaveToFile(const aFileName: string;
  aDataSet: TDataSet; const aSubjectName: string; const aIdentationText: string;
  aNamingStyle: TFieldNamingStyle);
var
  ss: TStringStream;
  aGenerator: TDataProxyGenerator;
  aUnitName: string;
  sCode: string;
  aExtLength: Integer;
begin
  aGenerator := TDataProxyGenerator.Create(nil);
  try
    aExtLength := Length(ExtractFileExt(aFileName));
    aUnitName := ExtractFileName(aFileName);
    aUnitName := aUnitName.Substring(0,Length(aUNitName)-aExtLength);
    sCode := 'unit ' + aUnitName + ';' + sLineBreak + 'interface' + sLineBreak +
      sLineBreak + 'implementation' + sLineBreak + sLineBreak + 'end.';
    ss := TStringStream.Create(sCode, TEncoding.UTF8);
    try
      ss.SaveToFile(aFileName);
    finally
      ss.Free;
    end;
  finally
    aGenerator.Free;
  end;
end;

end.
