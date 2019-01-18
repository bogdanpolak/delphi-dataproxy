unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Data.DB,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.DBGrids, Vcl.ComCtrls, Vcl.ActnList, Vcl.StdCtrls,
  Plus.ProxyGenerator, Vcl.Menus;

type
  TFormMain = class(TForm)
    grbxAppCommands: TGroupBox;
    Button1: TButton;
    PageControl1: TPageControl;
    tshDataSet: TTabSheet;
    tshDaoCode: TTabSheet;
    GridPanel2: TGridPanel;
    btnGenerateDAO: TButton;
    DBGrid1: TDBGrid;
    tmrReady: TTimer;
    mmProxyCode: TMemo;
    Label1: TLabel;
    Splitter1: TSplitter;
    Button2: TButton;
    DataSource1: TDataSource;
    // --------------------------------------------------------------------
    ActionList1: TActionList;
    actGenerateProxy: TAction;
    actConnect: TAction;
    actSelectConnectionDef: TAction;
    actExecSQL: TAction;
    actQueryBuilder: TAction;
    Action5: TAction;
    // --------------------------------------------------------------------
    Panel1: TPanel;
    Label2: TLabel;
    mmSqlStatement: TMemo;
    GridPanel1: TGridPanel;
    pmnRecentConnections: TPopupMenu;
    Button3: TButton;
    Label3: TLabel;
    Button4: TButton;
    // --------------------------------------------------------------------
    // Startup
    procedure FormCreate(Sender: TObject);
    procedure tmrReadyTimer(Sender: TObject);
    // --------------------------------------------------------------------
    // Action events
    procedure actSelectConnectionDefExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actExecSQLExecute(Sender: TObject);
    procedure actGenerateProxyExecute(Sender: TObject);
    procedure actQueryBuilderExecute(Sender: TObject);
    procedure Action5Execute(Sender: TObject);
    // --------------------------------------------------------------------
  private
    CurrentConnDefName: string;
    ProxyGenerator: TProxyGenerator;
    FMainDataSet: TDataSet;
    procedure InitializeControls;
    procedure UpdateActionEnable;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  DataModule.Main,
  Dialog.SelectDefinition,
  Helper.TApplication,
  App.AppInfo, Helper.TDBGrid, Dialog.QueryBuilder;

const
  AUTOOPEN_Application = True;

  // --------------------------------------------------------------------------
  // Applicationo start-up
  // --------------------------------------------------------------------------

procedure TFormMain.InitializeControls;
begin
  PageControl1.ActivePageIndex := 0;
  PageControl1.Align := alClient;
  mmProxyCode.Align := alClient;
  mmSqlStatement.Clear;
  mmProxyCode.Clear;
  Self.Caption := TAppInfo.AppName + ' - ' + TAppInfo.Version;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ProxyGenerator := TProxyGenerator.Create(Self);
  FMainDataSet := DataModule1.GetMainDataQuery;
  DataSource1.DataSet := FMainDataSet;
  InitializeControls;
  // -------------------------------------------------------
  // Inititialize actions
  // -------------------------------------------------------
  if Application.InDeveloperMode then
    ReportMemoryLeaksOnShutdown := True;
  actConnect.Enabled := False;
  UpdateActionEnable;
end;

procedure TFormMain.tmrReadyTimer(Sender: TObject);
begin
  tmrReady.Enabled := False;
  if Application.InDeveloperMode and AUTOOPEN_Application
  then
  begin
    CurrentConnDefName := 'SQLite_Demo';
    actSelectConnectionDef.Caption := 'Definition: ' + CurrentConnDefName;
    actConnect.Enabled := True;
    actConnect.Execute;
    actQueryBuilder.Execute;
    // actExecSQL.Execute;
    // actGenerateProxy.Execute;
  end;
end;

procedure TFormMain.UpdateActionEnable();
var
  IsConnected: Boolean;
  IsDataSetActive: Boolean;
begin
  IsConnected := DataModule1.IsConnected;
  IsDataSetActive := FMainDataSet.Active;
  actQueryBuilder.Enabled := IsConnected;
  actExecSQL.Enabled := IsConnected;
  actGenerateProxy.Enabled := IsDataSetActive;
end;

// --------------------------------------------------------------------------
// Action events
// --------------------------------------------------------------------------

procedure TFormMain.actSelectConnectionDefExecute(Sender: TObject);
var
  ConnDefName: string;
  IsSelectedDef: Boolean;
begin
  ConnDefName := TDialogSelectDefinition.Execute;
  if (CurrentConnDefName = '') or (ConnDefName <> '') then
  begin
    CurrentConnDefName := ConnDefName;
    IsSelectedDef := (CurrentConnDefName <> '');
    if IsSelectedDef then
      actSelectConnectionDef.Caption := 'Definition: ' + CurrentConnDefName
    else
      actSelectConnectionDef.Caption := 'Select Connection';
    actConnect.Enabled := IsSelectedDef;
  end;
end;

procedure TFormMain.actConnectExecute(Sender: TObject);
begin
  if not DataModule1.IsConnected then
  begin
    DataModule1.OpenConnection(CurrentConnDefName);
    actConnect.Caption := 'Disconnect';
  end
  else
  begin
    DataModule1.CloseConnection;
    actConnect.Caption := 'Connect';
    PageControl1.ActivePageIndex := 0;
    mmProxyCode.Clear;
  end;
  UpdateActionEnable();
end;

procedure TFormMain.actExecSQLExecute(Sender: TObject);
begin
  DataModule1.ExecuteSQL(mmSqlStatement.Text);
  Self.UpdateActionEnable;
  DBGrid1.AutoSizeColumns();
end;

procedure TFormMain.actGenerateProxyExecute(Sender: TObject);
begin
  PageControl1.ActivePage := tshDaoCode;
  ProxyGenerator.DataSet := DataSource1.DataSet;
  ProxyGenerator.Generate;
  mmProxyCode.Text := ProxyGenerator.Code;
end;

procedure TFormMain.actQueryBuilderExecute(Sender: TObject);
var
  sql: string;
begin
  sql := TDialogQueryBuilder.Execute;
  if sql<>'' then
    mmSqlStatement.Text := sql;
end;

procedure TFormMain.Action5Execute(Sender: TObject);
begin
  // TODO: Place for new feature (implement later)
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

end.
