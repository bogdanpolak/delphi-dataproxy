unit Form.Main;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.Net.HttpClient,

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
  Data.DataProxy,
  Procesor.Currency.Intf;

type
  EInvalidCurrency = class(Exception);

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
    fCurrencyProcessor: ICurrencyProcessor;

    function BuildAuhtorsList(const aAuthorList: string): TArray<String>;
    function ConvertReleaseDate(const aReleseDate: string;
      out isDatePrecise: boolean): TDateTime;
    procedure ValidateCurrency(const aPriceCurrency: string);
    {
    procedure DownloadCurrencyRates;
    function LocateRate(aCurrencyCode: string): integer;
    }
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.StrUtils,
  System.DateUtils,

  Procesor.Currency;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // OLD: fCurrencyRates := nil;
  fCurrencyProcessor := TCurrencyProcessor.Create;

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
  Memo1.Lines.Add('Local Price: ' + FormatFloat('###,###,###.00',
    // OLD: aBook.GetPrice('PLN', fCurrencyRates)) + ' PLN zloty');
    aBook.GetPrice('PLN', fCurrencyProcessor)) + ' PLN zloty');
  Memo1.Lines.Add('Original Price: ' + FormatFloat('###,###,###.00',
    aBook.Price) + ' ' + aBook.PriceCurrency);
end;

const
  MonthToRoman: array [1 .. 12] of string = ('I', 'II', 'III', 'IV', 'V', 'VI',
    'VII', 'VIII', 'IX', 'X', 'XI', 'XII');

function TFormMain.BuildAuhtorsList(const aAuthorList: string): TArray<String>;
var
  aAuthors: TArray<String>;
  idx: integer;
begin
  aAuthors := SplitString(aAuthorList, ',');
  SetLength(Result, Length(aAuthors));
  for idx := 0 to High(aAuthors) do
    Result[idx] := aAuthors[idx].Trim;
end;

function TFormMain.ConvertReleaseDate(const aReleseDate: string;
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

{
procedure TFormMain.DownloadCurrencyRates;
var
  aStringStream: TStringStream;
  aHTTPClient: THTTPClient;
  jsResult: TJSONObject;
  jsRates: TJSONObject;
  idx: integer;
begin
  aStringStream := TStringStream.Create('', TEncoding.UTF8);
  try
    aHTTPClient := THTTPClient.Create;
    try
      aHTTPClient.Get('https://api.exchangeratesapi.io/latest', aStringStream);
      jsResult := TJSONObject.ParseJSONValue(aStringStream.DataString)
        as TJSONObject;
      try
        jsRates := jsResult.GetValue('rates') as TJSONObject;
        SetLength(fCurrencyRates, jsRates.Count + 1);
        fCurrencyRates[0].Code := 'EUR';
        fCurrencyRates[0].Rate := 1.0000;
        for idx := 0 to jsRates.Count - 1 do
        begin
          fCurrencyRates[idx + 1].Code := jsRates.Pairs[idx].JsonString.Value;
          fCurrencyRates[idx + 1].Rate := jsRates.Pairs[idx]
            .JsonValue.AsType<double>;
        end;
      finally
        jsResult.Free;
      end;
    finally
      aHTTPClient.Free;
    end;
  finally
    aStringStream.Free;
  end;
end;

function TFormMain.LocateRate(aCurrencyCode: string): integer;
var
  idx: integer;
begin
  for idx := 0 to High(fCurrencyRates) do
    if fCurrencyRates[idx].Code = aCurrencyCode then
      Exit(idx);
  Result := -1;
end;
}

procedure TFormMain.ValidateCurrency(const aPriceCurrency: string);
begin
  // OLD:
  {
  if fCurrencyRates = nil then
    DownloadCurrencyRates;
  if LocateRate(aPriceCurrency) = -1 then
    raise EInvalidCurrency.Create('Invalid currency in book price: ' +
      aPriceCurrency);
  }
  if not fCurrencyProcessor.IsInitialiased then
    fCurrencyProcessor.Download('https://api.exchangeratesapi.io/latest');
  if not fCurrencyProcessor.IsCurrencySupported(aPriceCurrency) then
    raise EInvalidCurrency.Create('Invalid currency in book price: ' +
      aPriceCurrency);
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
        aBook.Authors.AddRange(BuildAuhtorsList(fdqBook.FieldByName('Authors')
          .AsString));
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
      aBook.Authors.AddRange(BuildAuhtorsList(fProxyBooks.Authors.Value));
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
