unit DataModule.Main;

interface

{--$Define FULL_FIREDAC_ACCESS}

uses
  System.SysUtils, System.Classes, System.Types,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.Def,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Stan.Param,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.Phys.MySQLDef, FireDAC.Phys.MySQL,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.IBDef, FireDAC.Phys.IB,
  FireDAC.Phys.FBDef, FireDAC.Phys.FB,
  FireDAC.Phys.PGDef, FireDAC.Phys.PG,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite,
  {$IFDEF FULL_FIREDAC_ACCESS}
  FireDAC.Phys.OracleDef, FireDAC.Phys.Oracle,
  FireDAC.Phys.DB2Def, FireDAC.Phys.DB2,
  FireDAC.Phys.MSSQLDef, FireDAC.Phys.MSSQL,
  {$ENDIF}
  FireDAC.Phys.ODBCBase;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
  private
  public
    function IsConnected: boolean;
    procedure OpenConnection (const ConnDefName: String);
    procedure CloseConnection;
    function GetMainDataQuery : TDataSet;
    procedure ExecuteSQL (const TextSQL: String);
    function GetTablesAndViewsNames: TStringDynArray;
    function GetFieldNames (const TableName: string): TStringDynArray;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  Helper.TStrings,
  Helper.TFDCustomManager;

{$R *.dfm}

function TDataModule1.IsConnected: boolean;
begin
  Result := FDConnection1.Connected;
end;

procedure TDataModule1.OpenConnection(const ConnDefName: String);
begin
  FDConnection1.ConnectionDefName := ConnDefName;
  FDConnection1.Open();
end;

procedure TDataModule1.CloseConnection;
begin
  FDConnection1.Close;
end;

function TDataModule1.GetMainDataQuery: TDataSet;
begin
  Result := FDQuery1;
end;

procedure TDataModule1.ExecuteSQL(const TextSQL: String);
begin
  FDQuery1.SQL.Text := '';
  FDQuery1.Open(TextSQL);
end;

function TDataModule1.GetTablesAndViewsNames: TStringDynArray;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    FDConnection1.GetTableNames('','','',sl);
    Result := sl.ToStringDynArray;
  finally
    sl.Free;
  end;
end;

function TDataModule1.GetFieldNames(const TableName: string): TStringDynArray;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    FDConnection1.GetFieldNames('','',TableName,'',sl);
    Result := sl.ToStringDynArray;
  finally
    sl.Free;
  end;
end;

end.
