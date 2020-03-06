unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.StrUtils, System.Types,
  Data.DB,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.DBGrids, Vcl.ComCtrls, Vcl.ActnList, Vcl.StdCtrls, Vcl.Menus,

  Comp.Generator.DataProxy,
  Comp.Generator.DataSetCode;

type
  TFormMain = class(TForm)
    grbxAppCommands: TGroupBox;
    Button1: TButton;
    PageControl1: TPageControl;
    tshDataSet: TTabSheet;
    tshProxyCode: TTabSheet;
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
    // --------------------------------------------------------------------
    Panel1: TPanel;
    Label2: TLabel;
    mmSqlStatement: TMemo;
    GridPanel1: TGridPanel;
    pmnRecentConnections: TPopupMenu;
    Button3: TButton;
    Label3: TLabel;
    Button4: TButton;
    grbxProxyGenOptions: TGroupBox;
    edtUnitName: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    tshFakeDataset: TTabSheet;
    gxbxFakeGenOptions: TGroupBox;
    rbtnFakeOptionFDMemTable: TRadioButton;
    rbtnFakeOptionClientDataSet: TRadioButton;
    rbtnFakeOptionAppendMultiline: TRadioButton;
    rbtnFakeOptionAppendSingleline: TRadioButton;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    mmFakeDataSetCode: TMemo;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    rbtnProxyOptionFieldLowerCase: TRadioButton;
    rbtnProxyOptionFieldUpperCase: TRadioButton;
    rbtnProxyOptionNoDataSetAccess: TRadioButton;
    rbtnProxyOptionCommnetedDataSet: TRadioButton;
    GroupBox7: TGroupBox;
    cbxProxyOptionIdentation: TComboBox;
    Label6: TLabel;
    edtClassName: TEdit;
    Label7: TLabel;
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
    procedure edtUnitNameKeyPress(Sender: TObject; var Key: Char);
    procedure rbtnFakeOptionFDMemTableClick(Sender: TObject);
    procedure rbtnFakeOptionClientDataSetClick(Sender: TObject);
    procedure rbtnFakeOptionAppendMultilineClick(Sender: TObject);
    procedure rbtnFakeOptionAppendSinglelineClick(Sender: TObject);
    procedure rbtnProxyOptionFieldLowerCaseClick(Sender: TObject);
    procedure rbtnProxyOptionFieldUpperCaseClick(Sender: TObject);
    procedure rbtnProxyOptionNoDataSetAccessClick(Sender: TObject);
    procedure rbtnProxyOptionCommnetedDataSetClick(Sender: TObject);
    procedure cbxProxyOptionIdentationChange(Sender: TObject);
    procedure edtClassNameKeyPress(Sender: TObject; var Key: Char);
  private
    fProxyGenerator: TDataProxyGenerator;
    fDataSetGenerator: TDSGenerator;
    fCurrentConnDefName: string;
    fDataSet: TDataSet;
    fConnectionMruList: string;
    procedure InitializeControls;
    procedure UpdateActionEnable;
    procedure StoreConnectionDefinitionInMRUList(const ConnDefName: string);
    function GetConnectionDefinitionMRUList: TStringDynArray;
    procedure FillConnectionMRUPopupMenu;
    procedure PopupMenuRecentConnectionsItemClick(Sender: TObject);
    function UpdateMRUList(const ConnDefName: string): boolean;
    procedure SetCurrentConnectionDefinition(ConnDefName: string);
    procedure AutomateMainForm;
    procedure UpdateFakeCode_AfterOptionChange;
    procedure UpdateProxyCode_AfterOptionChange;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.Win.Registry,

  Helper.TDBGrid,
  Helper.TApplication,
  Helper.TFDConnection,

  App.AppInfo,
  DataModule.Main,
  Dialog.SelectDefinition,
  Dialog.QueryBuilder,
  Utils.Timer.Interval;

const
  AppRegistryKey = 'Software\DelphiPower\DataSetProxyGenerator';


  // --------------------------------------------------------------------------
  // Connection Definition MRU List
  // * Storage level
  // * Domain level
  // --------------------------------------------------------------------------
  // TODO: Extract because of SOLID #1: SRP (Single Responsibility)

function TFormMain.UpdateMRUList(const ConnDefName: string): boolean;
var
  list: System.Types.TStringDynArray;
  len: Integer;
  i: Integer;
  j: Integer;
begin
  // TODO: Separate mru-list as an independent component
  if fConnectionMruList = '' then
  begin
    fConnectionMruList := ConnDefName;
    Result := True;
  end
  else
  begin
    list := System.StrUtils.SplitString(fConnectionMruList, ',');
    len := Length(list);
    if (list[0] = ConnDefName) then
      Result := False
    else
    begin
      i := 1;
      while (i < len) and (list[i] <> ConnDefName) do
        inc(i);
      if i < len then
      begin
        for j := i + 1 to len - 1 do
          list[j - 1] := list[j - 1];
        SetLength(list, len - 1);
      end;
      fConnectionMruList := ConnDefName + ',' + String.Join(',', list);
      Result := True;
    end;
  end;
end;

procedure TFormMain.SetCurrentConnectionDefinition(ConnDefName: string);
var
  IsSelectedDef: boolean;
begin
  if (fCurrentConnDefName = '') or (ConnDefName <> '') then
  begin
    fCurrentConnDefName := ConnDefName;
    IsSelectedDef := (fCurrentConnDefName <> '');
    if IsSelectedDef then
      actSelectConnectionDef.Caption := 'Definition: ' + fCurrentConnDefName
    else
      actSelectConnectionDef.Caption := 'Select Connection';
    actConnect.Enabled := IsSelectedDef;
  end;
end;

procedure TFormMain.AutomateMainForm;
begin
  // Level1: Select Connection
  fCurrentConnDefName := 'SQLite_Demo';
  actSelectConnectionDef.Caption := 'Definition: ' + fCurrentConnDefName;
  actConnect.Enabled := True;
  // Level2: Connect
  if TAplicationAutomation.IsLevelSupported(2) then
    actConnect.Execute;
  // Level3: Open QueryBuilder dialog | Select demo query | Close dialog
  if TAplicationAutomation.IsLevelSupported(3) then
    actQueryBuilder.Execute;
  // Level4: Execute SQL command and shor results in grid
  if TAplicationAutomation.IsLevelSupported(4) then
    actExecSQL.Execute;
  // Level5: Generate proxy
  if TAplicationAutomation.IsLevelSupported(5) then
    actGenerateProxy.Execute;
end;

procedure TFormMain.StoreConnectionDefinitionInMRUList
  (const ConnDefName: string);
var
  reg: TRegistry;
begin
  if UpdateMRUList(ConnDefName) then
  begin
    // ----
    // Store MRU list
    // ----
    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_CURRENT_USER;
      if not reg.KeyExists(AppRegistryKey) then
      begin
        reg.CreateKey(AppRegistryKey);
        // TODO: Check if CreateKey = True and log error
      end;
      reg.Access := KEY_WRITE;
      if reg.OpenKey(AppRegistryKey, False) then
      begin
        reg.WriteString('ConnectionMruList', fConnectionMruList);
      end;
    finally
      reg.Free;
    end;
  end;
end;

function TFormMain.GetConnectionDefinitionMRUList: TStringDynArray;
var
  reg: TRegistry;
begin
  if fConnectionMruList = '' then
  begin
    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_CURRENT_USER;
      if reg.KeyExists(AppRegistryKey) then
        if reg.OpenKey(AppRegistryKey, False) then
          fConnectionMruList := reg.ReadString('ConnectionMruList');
    finally
      reg.Free;
    end;
  end;
  if fConnectionMruList = '' then
    Result := nil
  else
    Result := System.StrUtils.SplitString(fConnectionMruList, ',')
end;

procedure TFormMain.FillConnectionMRUPopupMenu;
var
  list: System.Types.TStringDynArray;
  i: Integer;
  item: TMenuItem;
begin
  list := GetConnectionDefinitionMRUList;
  pmnRecentConnections.Items.Clear;
  for i := 0 to High(list) do
  begin
    item := TMenuItem.Create(pmnRecentConnections);
    with item do
    begin
      Caption := list[i];
      Tag := i;
      OnClick := PopupMenuRecentConnectionsItemClick;
    end;
    pmnRecentConnections.Items.Add(item);
  end;
end;

procedure TFormMain.PopupMenuRecentConnectionsItemClick(Sender: TObject);
var
  ConnDefName: string;
begin
  if Sender is TMenuItem then
  begin
    ConnDefName := Vcl.Menus.StripHotkey((Sender as TMenuItem).Caption);
    if DataModule1.GetConnection.IsConnected then
      actConnect.Execute;
    SetCurrentConnectionDefinition(ConnDefName);
  end;
end;


// --------------------------------------------------------------------------
// Application start-up
// --------------------------------------------------------------------------

procedure TFormMain.InitializeControls;
begin
  PageControl1.ActivePageIndex := 0;
  PageControl1.Align := alClient;
  mmProxyCode.Align := alClient;
  mmSqlStatement.Clear;
  mmProxyCode.Clear;
  mmFakeDataSetCode.Clear;
  Self.Caption := TAppInfo.AppName + ' - ' + TAppInfo.Version;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // -------------------------------------------------------
  fProxyGenerator := TDataProxyGenerator.Create(Self);
  fDataSetGenerator := TDSGenerator.Create(Self);
  // -------------------------------------------------------
  fDataSet := DataModule1.GetMainDataQuery;
  DataSource1.DataSet := fDataSet;
  fProxyGenerator.NameOfUnit := 'Proxy.Foo';
  edtUnitName.Text := fProxyGenerator.NameOfUnit;
  fProxyGenerator.NameOfClass := 'TFooProxy';
  edtClassName.Text := fProxyGenerator.NameOfClass;
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
  FillConnectionMRUPopupMenu;
  if TAplicationAutomation.IsActive then
    AutomateMainForm;
end;

procedure TFormMain.UpdateActionEnable();
begin
  actQueryBuilder.Enabled := DataModule1.GetConnection.IsConnected;
  actExecSQL.Enabled := DataModule1.GetConnection.IsConnected;
  actGenerateProxy.Enabled := fDataSet.Active;
end;


// --------------------------------------------------------------------------
// Action events
// --------------------------------------------------------------------------

procedure TFormMain.actSelectConnectionDefExecute(Sender: TObject);
begin
  if TDialogSelectDefinition.Execute then
  begin
    if DataModule1.GetConnection.IsConnected then
      actConnect.Execute;
    SetCurrentConnectionDefinition(TDialogSelectDefinition.ConnectionDef);
  end;
end;

procedure TFormMain.actConnectExecute(Sender: TObject);
begin
  if not DataModule1.GetConnection.IsConnected then
  begin
    DataModule1.GetConnection.Open(fCurrentConnDefName);
    // TODO: misleading method name (2 responsibilities)
    // * AddOrUpdateConnection_MruList = UpdateMRUList
    // * WriteConnectionMruList
    StoreConnectionDefinitionInMRUList(fCurrentConnDefName);
    FillConnectionMRUPopupMenu;
    actConnect.Caption := 'Disconnect';
  end
  else
  begin
    DataModule1.GetConnection.Close;
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
  PageControl1.ActivePage := tshProxyCode;
  // ----------------------------------------------------
  // DataSet Fake generator
  // ----------------------------------------------------
  fProxyGenerator.DataSet := DataSource1.DataSet;
  fProxyGenerator.NameOfUnit := edtUnitName.Text;
  fProxyGenerator.NameOfClass := edtClassName.Text;
  fProxyGenerator.DataSetAccess := dsaNoAccess;
  fProxyGenerator.FieldNamingStyle := fnsUpperCaseF;
  fProxyGenerator.IndentationText := '  ';
  fProxyGenerator.Execute;
  mmProxyCode.Lines.Text := fProxyGenerator.Code.Text;
  // ----------------------------------------------------
  // DataSet Fake generator
  // ----------------------------------------------------
  fDataSetGenerator.DataSet := DataSource1.DataSet;
  fDataSetGenerator.GeneratorMode := genUnit;
  fDataSetGenerator.AppendMode := amMultilineAppends;
  fDataSetGenerator.DataSetType := dstFDMemTable;
  fDataSetGenerator.IndentationText := '  ';
  fDataSetGenerator.Execute;
  mmFakeDataSetCode.Lines.Text := fDataSetGenerator.Code.Text;
  // ----------------------------------------------------
end;

procedure TFormMain.actQueryBuilderExecute(Sender: TObject);
var
  sql: string;
begin
  sql := TDialogQueryBuilder.Execute;
  if sql <> '' then
    mmSqlStatement.Text := sql;
end;

procedure TFormMain.UpdateFakeCode_AfterOptionChange;
begin
  if DataSource1.DataSet.Active then
  begin
    mmFakeDataSetCode.Color := clBtnFace;
    TIntervalTimer.SetTimeout(200,
      procedure
      begin
        fDataSetGenerator.Execute;
        mmFakeDataSetCode.Lines.Text := fDataSetGenerator.Code.Text;
        mmFakeDataSetCode.Color := clWindow;
      end);
  end;
end;

procedure TFormMain.UpdateProxyCode_AfterOptionChange;
begin
  if DataSource1.DataSet.Active then
  begin
    mmProxyCode.Color := clBtnFace;
    TIntervalTimer.SetTimeout(200,
      procedure
      begin
        fProxyGenerator.Execute;
        mmProxyCode.Lines.Text := fProxyGenerator.Code.Text;
        mmProxyCode.Color := clWindow;
      end);
  end;
end;

procedure TFormMain.edtClassNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    fProxyGenerator.NameOfUnit := edtUnitName.Text;
    fProxyGenerator.NameOfClass := edtClassName.Text;
    UpdateProxyCode_AfterOptionChange;
    Key := #0;
  end;
end;

procedure TFormMain.edtUnitNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    fProxyGenerator.NameOfUnit := edtUnitName.Text;
    fProxyGenerator.NameOfClass := edtClassName.Text;
    UpdateProxyCode_AfterOptionChange;
    Key := #0;
  end;
end;

procedure TFormMain.cbxProxyOptionIdentationChange(Sender: TObject);
begin
  case cbxProxyOptionIdentation.ItemIndex of
    0:
      fProxyGenerator.IndentationText := '  ';
    1:
      fProxyGenerator.IndentationText := '    ';
  end;
  UpdateProxyCode_AfterOptionChange;
end;

procedure TFormMain.rbtnProxyOptionCommnetedDataSetClick(Sender: TObject);
begin
  fProxyGenerator.DataSetAccess := dsaGenComment;
  UpdateProxyCode_AfterOptionChange;
end;

procedure TFormMain.rbtnProxyOptionNoDataSetAccessClick(Sender: TObject);
begin
  fProxyGenerator.DataSetAccess := dsaNoAccess;
  UpdateProxyCode_AfterOptionChange;
end;

procedure TFormMain.rbtnProxyOptionFieldLowerCaseClick(Sender: TObject);
begin
  fProxyGenerator.FieldNamingStyle := fnsLowerCaseF;
  UpdateProxyCode_AfterOptionChange;
end;

procedure TFormMain.rbtnProxyOptionFieldUpperCaseClick(Sender: TObject);
begin
  fProxyGenerator.FieldNamingStyle := fnsUpperCaseF;
  UpdateProxyCode_AfterOptionChange;
end;

procedure TFormMain.rbtnFakeOptionAppendMultilineClick(Sender: TObject);
begin
  fDataSetGenerator.AppendMode := amMultilineAppends;
  UpdateFakeCode_AfterOptionChange;
end;

procedure TFormMain.rbtnFakeOptionAppendSinglelineClick(Sender: TObject);
begin
  fDataSetGenerator.AppendMode := amSinglelineAppends;
  UpdateFakeCode_AfterOptionChange;
end;

procedure TFormMain.rbtnFakeOptionFDMemTableClick(Sender: TObject);
begin
  fDataSetGenerator.DataSetType := dstFDMemTable;
  UpdateFakeCode_AfterOptionChange;
end;

procedure TFormMain.rbtnFakeOptionClientDataSetClick(Sender: TObject);
begin
  fDataSetGenerator.DataSetType := dstClientDataSet;
  UpdateFakeCode_AfterOptionChange;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

end.
