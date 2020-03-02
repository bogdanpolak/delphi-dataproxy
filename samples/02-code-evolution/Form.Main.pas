unit Form.Main;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Model.Books,
  Proxy.Books,
  Data.DataProxy;

type
  TFormMain = class(TForm)
    ListBox1: TListBox;
    FDConnection1: TFDConnection;
    Splitter1: TSplitter;
    Memo1: TMemo;
    FlowPanel1: TFlowPanel;
    btnBeforeModernization: TButton;
    btnPhase1: TButton;
    btnPhase2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBeforeModernizationClick(Sender: TObject);
    procedure btnPhase1Click(Sender: TObject);
    procedure btnPhase2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    fdqBook: TFDQuery;
    fProxyBooks: TBooksProxy;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.DateUtils;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ListBox1.Clear;
  fdqBook := TFDQuery.Create(Self);
  fdqBook.Connection := FDConnection1;
  fdqBook.Open('SELECT ISBN, Title, Authors, Status, ReleaseDate,' +
    '  Pages, Price, Currency FROM {id Books}');
  fProxyBooks := TBooksProxy.Create(Self);
  fProxyBooks.WithDataSet(fdqBook);
end;

procedure TFormMain.ListBox1Click(Sender: TObject);
var
  aBook: TBook;
begin
  aBook := ListBox1.Items.Objects[ListBox1.ItemIndex] as TBook;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('ISBN: ' + aBook.ISBN);
  Memo1.Lines.Add('Title: ' + aBook.Title);
  Memo1.Lines.Add('Authors: ' + aBook.GetAuthorsList);
  Memo1.Lines.Add('ReleaseDate: ' + aBook.GetReleaseDate);
end;

const
  MonthToRoman: array [1 .. 12] of string = ('I', 'II', 'III', 'IV', 'V', 'VI',
    'VII', 'VIII', 'IX', 'X', 'XI', 'XII');

function ConvertReleaseDate(const aReleseDate: string;
  out isDatePrecise: boolean): TDateTime;
var
  idxSeparator: integer;
  yy: word;
  i: integer;
  sMonth: string;
  mm: word;
begin
  isDatePrecise := false;
  if aReleseDate = '' then
    Exit(0);
  idxSeparator := aReleseDate.IndexOf(' ');
  if idxSeparator = -1 then
  begin
    Result := ISO8601ToDate(aReleseDate);
    isDatePrecise := true;
  end
  else
  begin
    yy := StrToInt(aReleseDate.Substring(idxSeparator + 1));
    sMonth := aReleseDate.Substring(0, idxSeparator);
    mm := 0;
    for i := 1 to 12 do
      if sMonth = MonthToRoman[i] then
        mm := i;
    Result := EncodeDate(yy, mm, 1);
  end;
end;

procedure ValidateCurrency(aPriceCurrency: string);
begin
end;

procedure TFormMain.btnBeforeModernizationClick(Sender: TObject);
var
  aIndex: integer;
  aBookmark: TBookmark;
  aBook: TBook;
  isDatePrecise: boolean;
  aBookCaption: string;
begin
  ListBox1.ItemIndex := -1;
  for aIndex := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Items.Objects[aIndex].Free;
  ListBox1.Clear;
  aBookmark := fdqBook.GetBookmark;
  try
    fdqBook.DisableControls;
    try
      while not fdqBook.Eof do
      begin
        aBookCaption := fdqBook.FieldByName('ISBN').AsString + ' - ' +
          fdqBook.FieldByName('Title').AsString;
        aBook := TBook.Create;
        ListBox1.AddItem(aBookCaption, aBook);
        aBook.ISBN := fdqBook.FieldByName('ISBN').AsString;
        aBook.Title := fdqBook.FieldByName('Title').AsString;
        aBook.ReleaseDate := ConvertReleaseDate
          (fdqBook.FieldByName('ReleaseDate').AsString, isDatePrecise);
        aBook.IsPreciseReleaseDate := isDatePrecise;
        aBook.Price := fdqBook.FieldByName('Price').AsCurrency;
        aBook.PriceCurrency := fdqBook.FieldByName('Currency').AsString;
        ValidateCurrency(aBook.PriceCurrency);
        fdqBook.Next;
      end;
    finally
      fdqBook.EnableControls;
    end
  finally
    fdqBook.FreeBookmark(aBookmark);
  end;
end;

procedure TFormMain.btnPhase1Click(Sender: TObject);
var
  aIndex: integer;
  aBook: TBook;
  isDatePrecise: boolean;
begin
  ListBox1.ItemIndex := -1;
  for aIndex := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Items.Objects[aIndex].Free;
  ListBox1.Clear;

  fProxyBooks.ForEach(
    procedure
    begin
      aBook := TBook.Create;
      ListBox1.AddItem(fProxyBooks.ISBN.Value + ' - ' +
        fProxyBooks.Title.Value, aBook);
      aBook.ISBN := fProxyBooks.ISBN.Value;
      aBook.Title := fProxyBooks.Title.Value;
      aBook.ReleaseDate := ConvertReleaseDate(fProxyBooks.ReleaseDate.Value,
        isDatePrecise);
      aBook.IsPreciseReleaseDate := isDatePrecise;
      aBook.Price := fProxyBooks.Price.AsCurrency;
      aBook.PriceCurrency := fProxyBooks.Currency.Value;
      ValidateCurrency(aBook.PriceCurrency);
    end);
end;

procedure TFormMain.btnPhase2Click(Sender: TObject);
begin
  // TODO: Phase 2
end;

end.
