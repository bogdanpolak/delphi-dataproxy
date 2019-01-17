unit DataModule.Main;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Phys.MySQLDef, FireDAC.Phys.FBDef, FireDAC.Phys.PGDef,
  FireDAC.Phys.IBDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.OracleDef, FireDAC.Phys.DB2Def, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.MSSQL, FireDAC.Phys.ODBCBase, FireDAC.Phys.DB2,
  FireDAC.Phys.Oracle, FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.Phys.MySQL, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TStringArray = array of String;

  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDPhysDB2DriverLink1: TFDPhysDB2DriverLink;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
  private
  public
    function GetConnectionDefList: TStringArray;
    function IsConnected: boolean;
    procedure OpenConnection (const ConnDefName: String);
    procedure CloseConnection;
    function GetMainDataQuery : TDataSet;
    procedure ExecuteSQL (const TextSQL: String);
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataModule1 }

function TDataModule1.GetConnectionDefList: TStringArray;
var
  i: Integer;
  ConnectionDef: IFDStanConnectionDef;
begin
  SetLength(Result, FDManager.ConnectionDefs.Count);
  for i := 0 to FDManager.ConnectionDefs.Count-1 do
  begin
    ConnectionDef := FDManager.ConnectionDefs.Items[i];
    Result[i] := ConnectionDef.Name;
  end;
end;

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

end.
