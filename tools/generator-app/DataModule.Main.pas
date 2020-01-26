unit DataModule.Main;

interface

{--$Define FULL_FIREDAC_ACCESS}  // avaliable only in Delphi Entrprise version

uses
  System.SysUtils,
  System.Classes,
  System.Types,
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
  // -----------------------------
  // TODO: Convert to class accesed by singleton factory method
  // class: TDatabaseModule
  // interface: GetDatabaseModule: IDatabaseModule;
  // -----------------------------
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
  private
  public
    function GetMainDataQuery : TDataSet;
    procedure ExecuteSQL (const TextSQL: String);
    function GetConnection: TFDConnection;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function TDataModule1.GetConnection: TFDConnection;
begin
  Result := FDConnection1;
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
