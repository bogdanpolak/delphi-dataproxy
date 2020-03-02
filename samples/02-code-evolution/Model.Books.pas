unit Model.Books;

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections;

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
    // ---
    function GetAuthorsList: string;
    function GetReleaseDate: string;
    // ---
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

implementation

constructor TBook.Create;
begin
  FAuthors := TList<string>.Create;
end;

destructor TBook.Destroy;
begin
  FAuthors.Free;
  inherited;
end;

function TBook.GetAuthorsList: string;
var
  idx: integer;
begin
  if FAuthors.Count = 0 then
    Exit('');
  Result := FAuthors[0];
  for idx := 1 to FAuthors.Count - 1 do
    Result := Result + ', ' + FAuthors[idx];
end;

function TBook.GetReleaseDate: string;
begin
  if FReleaseDate = 0 then
    Result := '---'
  else if FIsPreciseReleaseDate then
    Result := DateToStr(FReleaseDate)
  else
    Result := FormatDateTime('mm yyyy', FReleaseDate);
end;

end.
