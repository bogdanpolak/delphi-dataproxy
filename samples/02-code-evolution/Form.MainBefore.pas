unit Form.MainBefore;

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
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    FDConnection1: TFDConnection;
    Splitter1: TSplitter;
    Memo1: TMemo;
    FlowPanel1: TFlowPanel;
    btnPhase1: TButton;
    btnPhase2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPhase1Click(Sender: TObject);
    procedure btnPhase2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.DateUtils,
  Proxy.Books,
  Data.DataProxy;

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

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TForm1.btnPhase1Click(Sender: TObject);
var
  aIndex: integer;
  aBookmark: TBookmark;
  aBookDataSet: TDataSet;
  aBook: TBook;
  isDatePrecise: boolean;
  aBookCaption: string;
begin
  ListBox1.ItemIndex := -1;
  for aIndex := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Items.Objects[aIndex].Free;
  ListBox1.Clear;
  FDConnection1.ExecSQL('SELECT ISBN, Title, Authors, Status, ReleaseDate,' +
    '  Pages, Price, Currency FROM {id Books}', aBookDataSet);
  aBookmark := aBookDataSet.GetBookmark;
  try
    aBookDataSet.DisableControls;
    try
      while not aBookDataSet.Eof do
      begin
        aBookCaption := aBookDataSet.FieldByName('ISBN').AsString + ' - ' +
          aBookDataSet.FieldByName('Title').AsString;
        aBook := TBook.Create;
        ListBox1.AddItem(aBookCaption, aBook);
        aBook.ISBN := aBookDataSet.FieldByName('ISBN').AsString;
        aBook.BuildAuhtorsList(aBookDataSet.FieldByName('Authors').AsString);
        aBook.Title := aBookDataSet.FieldByName('Title').AsString;
        aBook.ReleaseDate := ConvertReleaseDate
          (aBookDataSet.FieldByName('ReleaseDate').AsString, isDatePrecise);
        aBook.IsPreciseReleaseDate := isDatePrecise;
        aBook.Price := aBookDataSet.FieldByName('Price').AsCurrency;
        aBook.PriceCurrency := aBookDataSet.FieldByName('Currency').AsString;
        ValidateCurrency(aBook.PriceCurrency);
        aBookDataSet.Next;
      end;
    finally
      aBookDataSet.EnableControls;
    end
  finally
    aBookDataSet.FreeBookmark(aBookmark);
  end;
end;

procedure TForm1.btnPhase2Click(Sender: TObject);
var
  aIndex: integer;
  aDataSet: TDataSet;
  aBook: TBook;
  isDatePrecise: boolean;
  aProxyBooks: TBooksProxy;
begin
  ListBox1.ItemIndex := -1;
  for aIndex := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Items.Objects[aIndex].Free;
  ListBox1.Clear;

  FDConnection1.ExecSQL('SELECT ISBN, Title, Authors, Status, ReleaseDate,' +
    '  Pages, Price, Currency FROM {id Books}', aDataSet);

  aProxyBooks := TBooksProxy.Create(Self);
  aProxyBooks.WithDataSet(aDataSet);

  aProxyBooks.ForEach(
    procedure
    begin
      aBook := TBook.Create;
      ListBox1.AddItem(aProxyBooks.ISBN.Value + ' - ' +
        aProxyBooks.Title.Value, aBook);
      aBook.ISBN := aProxyBooks.ISBN.Value;
      aBook.BuildAuhtorsList(aProxyBooks.Authors.Value);
      aBook.Title := aProxyBooks.Title.Value;
      aBook.ReleaseDate := ConvertReleaseDate(aProxyBooks.ReleaseDate.Value,
        isDatePrecise);
      aBook.IsPreciseReleaseDate := isDatePrecise;
      aBook.Price := aProxyBooks.Price.AsCurrency;
      aBook.PriceCurrency := aProxyBooks.Currency.Value;
      ValidateCurrency(aBook.PriceCurrency);
    end);
end;

end.
