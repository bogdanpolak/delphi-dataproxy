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

type
  TBook = class
  strict private
    FISBN: string;
    FTitle: String;
    FAuthors: TList<string>;
    FReleaseDate: TDateTime;
    FIsPreciseReleaseDate: boolean;
    FPrice: Currency;
    FPriceCurrency: string;
    FPages: integer;
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BuildAuhtorsList(const aAutlorsList: string);
    property ISBN: string read FISBN write FISBN;
    property Title: String read FTitle write FTitle;
    property Authors: TList<string> read FAuthors write FAuthors;
    property ReleaseDate: TDateTime read FReleaseDate write FReleaseDate;
    property IsPreciseReleaseDate: boolean read FIsPreciseReleaseDate
      write FIsPreciseReleaseDate;
    property Price: Currency read FPrice write FPrice;
    property PriceCurrency: string read FPriceCurrency write FPriceCurrency;
    property Pages: integer read FPages write FPages;
  end;

constructor TBook.Create;
begin
  FAuthors := TList<string>.Create;
end;

destructor TBook.Destroy;
begin
  FAuthors.Free;
  inherited;
end;

procedure TBook.BuildAuhtorsList(const aAutlorsList: string);
begin
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
  Result := 0;
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
        aBook.BuildAuhtorsList(fdqBook.FieldByName('Authors').AsString);
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
  aDataSet: TDataSet;
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
      aBook.BuildAuhtorsList(fProxyBooks.Authors.Value);
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
