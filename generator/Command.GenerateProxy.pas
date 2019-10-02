unit Command.GenerateProxy;

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode,
  Comp.Generator.ProxyCode;

type
  TProxyGeneratorCommand = class(TComponent)
  const
    BaseProxyName = '{ObjectName}';
  private
    GeneratedCode: TStringList;
    ProxyGenerator: TProxyCodeGenerator;
    DataSetGenerator: TGenerateDataSetCode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(dataset: TDataSet): String;
  end;

implementation

uses
  App.AppInfo;

type
  TStringListHelper = class helper for TStringList
    procedure LineReplace(const LineToReplace, NewLine: string);
  end;

  { TStringListHelper }

procedure TStringListHelper.LineReplace(const LineToReplace, NewLine: string);
begin
  Self.Text := StringReplace(Self.Text, LineToReplace, NewLine, [rfReplaceAll]);
end;

{ TProxyGeneratorCommand }

constructor TProxyGeneratorCommand.Create(AOwner: TComponent);
begin
  inherited;
  GeneratedCode := TStringList.Create;
  ProxyGenerator := TProxyCodeGenerator.Create(Self);
  DataSetGenerator := TGenerateDataSetCode.Create(Self);
end;

destructor TProxyGeneratorCommand.Destroy;
begin
  GeneratedCode.Free;
  inherited;
end;

function TProxyGeneratorCommand.Execute(dataset: TDataSet): String;
begin
  ProxyGenerator.dataset := dataset;
  ProxyGenerator.Execute;
  GeneratedCode.Clear;
  with GeneratedCode do
  begin
    Add('// Generated by ' + TAppInfo.AppName + ' at ' +
      FormatDateTime('yyyy-mm-dd hh:nn', Now));
    Add('');
    Add('unit proxy.{ObjectName};');
    Add('');
    AddStrings(ProxyGenerator.Code);
  end;
  // -----------
  DataSetGenerator.dataset := dataset;
  DataSetGenerator.IndentationText := '  ';
  DataSetGenerator.Execute;
  // -----------
  with GeneratedCode do
  begin
    LineReplace('  public', '  public' + sLineBreak +
      '    class function CreateMockTable (AOwner: TComponent): TFDMemTable;');
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
    Add('');
    Add('end.');
  end;
  Result := GeneratedCode.Text;
end;

end.
