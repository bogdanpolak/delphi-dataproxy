unit Form.Main;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  {Project uses}
  Data.Proxy.Book,
  Data.Mock.Book;

type
  TDatasetKind = (dskMemory, dskSQL);

  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    Button1: TButton;
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    Button2: TButton;
    GroupBox2: TGroupBox;
    rbtnSqlDataset: TRadioButton;
    rbtnMemoryDataset: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure rbtnMemoryDatasetClick(Sender: TObject);
    procedure rbtnSqlDatasetClick(Sender: TObject);
  private
    fDatasetKind: TDatasetKind;
    fBookProxy: TBookProxy;
    procedure InitializeButtonCaptions;
    procedure CreateBookProxy;
    procedure ResetDemo;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Data.DataProxy;

function CreateSQLDataSet_Book(AOwner: TComponent; AConnection: TFDConnection)
  : TDataSet;
var
  fdq: TFDQuery;
begin
  fdq := TFDQuery.Create(AOwner);
  with fdq do
  begin
    Connection := AConnection;
    SQL.Text := 'SELECT ISBN, Title, Authors, Status, ReleseDate,' +
      ' Pages, Price, Currency, Imported, Description FROM Books';
    Open;
  end;
  Result := fdq;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fDatasetKind := dskMemory;
  fBookProxy := nil;
  InitializeButtonCaptions;
end;

procedure TForm1.CreateBookProxy;
begin
  fBookProxy := TBookProxy.Create(Self);
  case fDatasetKind of
    dskMemory:
      fBookProxy.WithDataSet(CreateMockTableBook(fBookProxy));
    dskSQL:
      fBookProxy.WithDataSet(CreateSQLDataSet_Book(fBookProxy, FDConnection1));
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if fBookProxy = nil then
    CreateBookProxy;
  ListBox1.ItemIndex := -1;
  InitializeButtonCaptions;
  ListBox1.Clear;
  fBookProxy.ForEach(
    procedure
    begin
      ListBox1.Items.Add(fBookProxy.ISBN.Value + ' ' + fBookProxy.ToString);
    end);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button2.Caption := Format('More expensive books = %d',
    [fBookProxy.CountMoreExpensiveBooks]);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  s: string;
  ISBN: string;
begin
  if (ListBox1.ItemIndex >= 0) then
  begin
    s := ListBox1.Items[ListBox1.ItemIndex];
    ISBN := s.Substring(0, 14);
    fBookProxy.LocateISBN(ISBN);
    Self.Caption := fBookProxy.Title.Value;
    InitializeButtonCaptions;
  end;
end;

procedure TForm1.rbtnMemoryDatasetClick(Sender: TObject);
begin
  fDatasetKind := dskMemory;
  ResetDemo;
end;

procedure TForm1.rbtnSqlDatasetClick(Sender: TObject);
begin
  fDatasetKind := dskSQL;
  ResetDemo;
end;

procedure TForm1.ResetDemo;
begin
  FreeAndNil(fBookProxy);
  InitializeButtonCaptions;
  ListBox1.Clear;
end;

procedure TForm1.InitializeButtonCaptions;
begin
  Button2.Enabled := (fBookProxy <> nil);
  if fBookProxy = nil then
  begin
    Button2.Caption := 'Load books and select one of them'
  end
  else
  begin
    Button2.Caption := 'Count more expensive books then: ' +
      fBookProxy.Price.AsString + ' ' + fBookProxy.Currency.Value;
  end;
end;

end.
