unit Proxy.Books;

interface

uses
  Data.DB,
  Data.DataProxy,
  System.SysUtils,
  System.Classes,
  // ---- Stage 2 code:
  System.DateUtils,
  System.StrUtils,
  System.Generics.Collections,
  // ----
  FireDAC.Comp.Client,

  // ---- Stage 2 code:
  Procesor.Currency.Intf,
  Model.Books;
  // ----

type
  TBooksProxy = class(TDatasetProxy)
  private
    FISBN: TWideStringField;
    FTitle: TWideStringField;
    FAuthors: TWideStringField;
    FStatus: TWideStringField;
    FReleaseDate: TWideStringField;
    FPages: TIntegerField;
    FPrice: TBCDField;
    FCurrency: TWideStringField;
    // ---- Stage 2 code:
    fCurrencyProcessor: ICurrencyProcessor;
    fBooksList: TObjectList<TBook>;
    procedure ValidateCurrency(const aPriceCurrency: string);
    function BuildAuhtorsList(const aAuthorList: string): TArray<String>;
    function ConvertReleaseDate(const aReleseDate: string;
      out isDatePrecise: boolean): TDateTime;
    // ----
  protected
    procedure ConnectFields; override;
  public
    // ---- Stage 2 code:
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure SetCurrencyProcessor(aCurrencyProc: ICurrencyProcessor);
    procedure FillStringsWithBooks(aStrings: TStrings);
    procedure LoadAndValidate;
    // ----
    property ISBN: TWideStringField read FISBN;
    property Title: TWideStringField read FTitle;
    property Authors: TWideStringField read FAuthors;
    property Status: TWideStringField read FStatus;
    property ReleaseDate: TWideStringField read FReleaseDate;
    property Pages: TIntegerField read FPages;
    property Price: TBCDField read FPrice;
    property Currency: TWideStringField read FCurrency;
  end;

implementation

procedure TBooksProxy.ConnectFields;
const
  ExpectedFieldCount = 8;
begin
  FISBN := FDataSet.FieldByName('ISBN') as TWideStringField;
  FTitle := FDataSet.FieldByName('Title') as TWideStringField;
  FAuthors := FDataSet.FieldByName('Authors') as TWideStringField;
  FStatus := FDataSet.FieldByName('Status') as TWideStringField;
  FReleaseDate := FDataSet.FieldByName('ReleaseDate') as TWideStringField;
  FPages := FDataSet.FieldByName('Pages') as TIntegerField;
  FPrice := FDataSet.FieldByName('Price') as TBCDField;
  FCurrency := FDataSet.FieldByName('Currency') as TWideStringField;
  Assert(FDataSet.Fields.Count = ExpectedFieldCount);
end;


// --------------------------------------------------------
// ---- Stage 2 code:

const
  ThisIsObjectsOwner = true;

constructor TBooksProxy.Create(Owner: TComponent);
begin
  inherited;
  fBooksList := TObjectList<TBook>.Create(ThisIsObjectsOwner);
  fCurrencyProcessor := nil;
end;

destructor TBooksProxy.Destroy;
begin
  fBooksList.Free;
  inherited;
end;

const
  MonthToRoman: array [1 .. 12] of string = ('I', 'II', 'III', 'IV', 'V', 'VI',
    'VII', 'VIII', 'IX', 'X', 'XI', 'XII');

function TBooksProxy.BuildAuhtorsList(const aAuthorList: string)
  : TArray<String>;
var
  aAuthors: TArray<String>;
  idx: integer;
begin
  aAuthors := SplitString(aAuthorList, ',');
  SetLength(Result, Length(aAuthors));
  for idx := 0 to High(aAuthors) do
    Result[idx] := aAuthors[idx].Trim;
end;

function TBooksProxy.ConvertReleaseDate(const aReleseDate: string;
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

procedure TBooksProxy.ValidateCurrency(const aPriceCurrency: string);
begin
  if not fCurrencyProcessor.IsInitialiased then
    fCurrencyProcessor.Download('https://api.exchangeratesapi.io/latest');
  if not fCurrencyProcessor.IsCurrencySupported(aPriceCurrency) then
    raise EInvalidCurrency.Create('Invalid currency in book price: ' +
      aPriceCurrency);
end;

procedure TBooksProxy.SetCurrencyProcessor(aCurrencyProc: ICurrencyProcessor);
begin
  fCurrencyProcessor := aCurrencyProc;
end;

procedure TBooksProxy.FillStringsWithBooks(aStrings: TStrings);
var
  aBook: TBook;
begin
  aStrings.Clear;
  for aBook in fBooksList do
    aStrings.AddObject(aBook.ISBN + ' - ' + aBook.Title, aBook);
end;

procedure TBooksProxy.LoadAndValidate;
var
  aBook: TBook;
  isDatePrecise: boolean;
begin
  fBooksList.Clear;
  ForEach(
    procedure
    begin
      aBook := TBook.Create;
      fBooksList.Add(aBook);
      aBook.ISBN := ISBN.Value;
      aBook.Authors.AddRange(BuildAuhtorsList(Authors.Value));
      aBook.Title := Title.Value;
      aBook.ReleaseDate := ConvertReleaseDate(ReleaseDate.Value, isDatePrecise);
      aBook.IsPreciseReleaseDate := isDatePrecise;
      aBook.Price := Price.AsCurrency;
      aBook.PriceCurrency := Currency.Value;
      ValidateCurrency(aBook.PriceCurrency);
    end);
end;

// --------------------------------------------------------

end.
