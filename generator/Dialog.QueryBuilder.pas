unit Dialog.QueryBuilder;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ActnList,
  Plus.Types;

type
  TDialogQueryBuilder = class(TForm)
    pnTables: TPanel;
    pnCommands: TPanel;
    Label1: TLabel;
    cbxMainTables: TComboBox;
    lbxJoinTables: TListBox;
    Label2: TLabel;
    mmSqlPreview: TMemo;
    // ------------------------------------------------------------------
    // Actions
    ActionList1: TActionList;
    actUseSQL: TAction;
    actCancel: TAction;
    actDemoSelect: TAction;
    actMainTableSelected: TAction;
    actJoinTableSelected: TAction;
    // ------------------------------------------------------------------
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actDemoSelectExecute(Sender: TObject);
    procedure actJoinTableSelectedExecute(Sender: TObject);
    procedure actMainTableSelectedExecute(Sender: TObject);
    procedure actUseSQLExecute(Sender: TObject);
    procedure actUseSQLUpdate(Sender: TObject);
    procedure cbxTablesMainChange(Sender: TObject);
  private
    FTables: TStringArray;
    procedure PaintBox1Paint(Sender: TObject);
    procedure DrawInfoListBoxNotImplemented(APaintBox: TPaintBox);
  public
    class function Execute: string;
  end;

implementation

{$R *.dfm}

uses
  DataModule.Main;

class function TDialogQueryBuilder.Execute: string;
var
  dlg: TDialogQueryBuilder;
  mr: Integer;
begin
  dlg := TDialogQueryBuilder.Create(Application);
  Result := '';
  try
    mr := dlg.ShowModal;
    if mr = mrOK then
      Result := dlg.mmSqlPreview.Text;
  finally
    dlg.Free;
  end;
end;

procedure TDialogQueryBuilder.FormCreate(Sender: TObject);
var
  s: String;
begin
  FTables := DataModule1.GetTablesAndViewsNames;
  // -------------------------------------------------------------------
  // Configure dialog controls
  // -------------------------------------------------------------------
  mmSqlPreview.Align := alClient;
  mmSqlPreview.Text := '';
  // -------------------------------------------------------------------
  // Configure cbxMainTables
  // -------------------------------------------------------------------
  cbxMainTables.OnChange := nil;
  cbxMainTables.Style := csDropDownList;
  cbxMainTables.AddItem('<select object>', nil);
  cbxMainTables.ItemIndex := 0;
  cbxMainTables.DropDownCount := 25;
  for s in FTables do
    cbxMainTables.Items.Add(s);
  cbxMainTables.OnChange := cbxTablesMainChange;
  // -------------------------------------------------------------------
  // Not implemented join tables selection
  // -------------------------------------------------------------------
  lbxJoinTables.Visible := False;
  with TPaintBox.Create(Self) do
  begin;
    AlignWithMargins := True;
    Align := alClient;
    OnPaint := PaintBox1Paint;
    Parent := pnTables;
  end;
end;

// ------------------------------------------------------------------------
// Actions
// ------------------------------------------------------------------------

procedure TDialogQueryBuilder.actDemoSelectExecute(Sender: TObject);
var
  sql: string;
begin
  sql := 'SELECT Orders.OrderID, ' + sLineBreak +
    '  Orders.CustomerID, Customers.CompanyName,  Orders.EmployeeID, ' +
    sLineBreak +
    '  Employees.FirstName||'' ''||Employees.LastName EmployeeName, ' +
    sLineBreak + '  Orders.OrderDate, Orders.RequiredDate, Orders.ShippedDate, '
    + sLineBreak + '  Orders.ShipVia, Orders.Freight ' + sLineBreak +
    'FROM {id Orders} Orders ' + sLineBreak +
    '  INNER JOIN {id Employees} Employees ' + sLineBreak +
    '    ON Orders.EmployeeID = Employees.EmployeeID ' + sLineBreak +
    '  INNER JOIN {id Customers} Customers ' + sLineBreak +
    '    ON Orders.CustomerID = Customers.CustomerID ' + sLineBreak +
    'WHERE {year(OrderDate)} = 1997 ' + sLineBreak + 'ORDER BY Orders.OrderID ';
  mmSqlPreview.Text := sql;
  ModalResult := mrOK;
end;

procedure TDialogQueryBuilder.actJoinTableSelectedExecute(Sender: TObject);
begin
  // TODO: Implementation required
end;

procedure TDialogQueryBuilder.actMainTableSelectedExecute(Sender: TObject);
var
  aTableName: String;
  Fields: TStringArray;
  fldName: String;
  FieldCount: Integer;
  sFieldsList: string;
begin
  if cbxMainTables.ItemIndex > 0 then
  begin
    aTableName := cbxMainTables.Text;
    Fields := DataModule1.GetFieldNames(aTableName);
    FieldCount := Length(Fields);
    mmSqlPreview.Clear;
    if FieldCount > 0 then
    begin
      sFieldsList := '';
      for fldName in Fields do
      begin
        if sFieldsList = '' then
          sFieldsList := fldName
        else
          sFieldsList := sFieldsList + ', ' + fldName;
      end;
      mmSqlPreview.Lines.Add('SELECT ');
      mmSqlPreview.Lines.Add('  ' + sFieldsList + ' ');
      mmSqlPreview.Lines.Add('FROM ');
      mmSqlPreview.Lines.Add('  ' + aTableName);
    end;
  end;
  // TODO: Add foreign keys analysis for table aTableName (comments below)
  // it looks that it requires additional list with objects
  // * list of TTableForeignKey (FKName, FKTableName, [ReferenceFields])
  // * basic version: just foreign tables names
end;

procedure TDialogQueryBuilder.actUseSQLExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TDialogQueryBuilder.actUseSQLUpdate(Sender: TObject);
begin
  actUseSQL.Enabled := Trim(mmSqlPreview.Text) <> '';
end;

procedure TDialogQueryBuilder.actCancelExecute(Sender: TObject);
begin
  // TODO: Add veryfication if something is build?
  ModalResult := mrCancel;
end;

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------

procedure TDialogQueryBuilder.cbxTablesMainChange(Sender: TObject);
begin
  actMainTableSelected.Execute;
end;

procedure TDialogQueryBuilder.PaintBox1Paint(Sender: TObject);
begin
  Self.DrawInfoListBoxNotImplemented(Sender as TPaintBox);
end;

procedure TDialogQueryBuilder.DrawInfoListBoxNotImplemented
  (APaintBox: TPaintBox);
var
  Canv: TCanvas;
  rectA: TRect;
  xc: Integer;
  yc: Integer;
  sLine: string;
  hgText: Integer;
  wdText: Integer;
begin
  Canv := APaintBox.Canvas;
  rectA := Rect(1, 1, APaintBox.Width - 1, APaintBox.Height - 1);
  Canv.Pen.Color := $E0E0E0;
  Canv.Pen.Width := 3;
  Canv.MoveTo(rectA.Left, rectA.Top);
  Canv.LineTo(rectA.Right, rectA.Bottom);
  Canv.MoveTo(rectA.Right, rectA.Top);
  Canv.LineTo(rectA.Left, rectA.Bottom);
  Canv.Brush.Style := bsClear;
  Canv.Pen.Width := 2;
  Canv.Pen.Color := $707070;
  Canv.Rectangle(rectA);
  xc := rectA.Left + (rectA.Right - rectA.Left) div 2;
  yc := rectA.Top + (rectA.Bottom - rectA.Top) div 2;
  Canv.Font.Color := $505050;
  Canv.Font.Style := [];
  sLine := 'ListBox: ' + lbxJoinTables.Name;
  hgText := Canv.TextHeight(sLine);
  wdText := Canv.TextWidth(sLine);
  Canv.TextOut(xc - wdText div 2, yc - hgText - 2, sLine);
  sLine := 'Not implemented yet';
  Canv.Font.Style := [fsItalic];
  wdText := Canv.TextWidth(sLine);
  Canv.TextOut(xc - wdText div 2, yc + 2, sLine);
end;

end.
