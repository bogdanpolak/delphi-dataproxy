unit Command.GenerateProxy;

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode,
  Comp.Proxy.CodeGenerator;

type
  TProxyGeneratorCommand = class(TComponent)
  private
    GeneratedCode: TStringList;
    ProxyGenerator: TProxyCodeGenerator;
    DataSetGenerator: TGenerateDataSetCode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destory;
    function Execute(dataset: TDataSet): String;
  end;

implementation

{ TProxyGeneratorCommand }

constructor TProxyGeneratorCommand.Create(AOwner: TComponent);
begin
  inherited;
  GeneratedCode := TStringList.Create;
  ProxyGenerator := TProxyCodeGenerator.Create (Self);
  DataSetGenerator := TGenerateDataSetCode.Create (Self);
end;

destructor TProxyGeneratorCommand.Destory;
begin
  GeneratedCode.Free;
end;

function TProxyGeneratorCommand.Execute(dataset: TDataSet): String;
begin
  ProxyGenerator.DataSet := dataset;
  ProxyGenerator.Execute;
  // -----------
  DataSetGenerator.dataSet := dataset;
  DataSetGenerator.IndentationText := '  ';
  DataSetGenerator.Execute;
  // -----------
  GeneratedCode.Clear;
  with GeneratedCode do begin
    AddStrings( ProxyGenerator.Code );
    Add('');
    Add('// -----------------------------------------------------------');
    Add('');
    Add('class function T{ObjectName}Proxy.CreateMockTable (AOwner: TComponent): TFDMemTable;');
    Add('var');
    Add('  ds: TFDMemTable;');
    Add('begin');
    AddStrings(DataSetGenerator.CodeWithStructure);
    AddStrings(DataSetGenerator.CodeWithAppendData);
    Add('  Result := ds;');
    Add('end;');
  end;
  Result :=  GeneratedCode.Text;
end;

end.
