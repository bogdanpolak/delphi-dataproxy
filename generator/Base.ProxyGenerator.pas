unit Base.ProxyGenerator;

interface

uses
  Data.DB,
  System.Classes,
  System.SysUtils;

type
  EProxyGenError = class(Exception);

type
  TProxyGenerator = class (TComponent)
  private
    FDataSet: TDataSet;
    FCode: String;
    function FieldToClass (aField: TField): TClass;
    procedure SetDataSet(const aDataSet: TDataSet);
    procedure SetCode(const aCode: String);
  protected
    function DoGenerateProxy (ds: TDataSet): String;
  public
    property Code: String read FCode write SetCode;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    procedure Generate;
  end;

implementation

resourcestring
  ErrDataSetIsRequired = 'DataSet is required to generate new proxy';
  ErrDataSetNotActive = 'DataSet have to be active!';

{ TProxyGenerator }

function TProxyGenerator.FieldToClass (aField: TField): TClass;
begin
  Result := Data.DB.DefaultFieldClasses[aField.DataType];
end;

function TProxyGenerator.DoGenerateProxy(ds: TDataSet): String;
var
  code: TStringList;
  aField: TField;
  i: Integer;
  DataClass: TClass;
begin
  code := TStringList.Create;
  try
    for i := 0 to ds.Fields.Count-1 do
    begin
      aField := ds.Fields[i];
      DataClass := FieldToClass( aField );
      code.Add('  property '+aField.FieldName+' :'+DataClass.ClassName);
    end;
    Result := code.Text;
  finally
    code.Free;
  end;
end;

procedure TProxyGenerator.Generate;
begin
  if DataSet=nil then
    raise EProxyGenError.Create(ErrDataSetIsRequired);
  if not DataSet.Active then
    raise EProxyGenError.Create(ErrDataSetNotActive);
  Code := DoGenerateProxy(DataSet);
end;

procedure TProxyGenerator.SetCode(const aCode: String);
begin
  FCode := aCode;
end;

procedure TProxyGenerator.SetDataSet(const aDataSet: TDataSet);
begin
  FDataSet := aDataSet;
end;

end.
