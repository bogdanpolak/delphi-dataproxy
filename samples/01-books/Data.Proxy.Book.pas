unit Data.Proxy.Book;

interface

uses
  Data.DB,
  Data.DataProxy;

type
  TBookProxy = class(TDatasetProxy)
  private
    FISBN: TWideStringField;
    FTitle: TWideStringField;
    FAuthors: TWideStringField;
    FStatus: TWideStringField;
    FReleseDate: TDateField;
    FPages: TIntegerField;
    FPrice: TBCDField;
    FCurrency: TWideStringField;
    FImported: TDateTimeField;
    FDescription: TWideStringField;
  protected
    procedure ConnectFields; override;
  public
    function ToString: String; override;
    function CountMoreExpensiveBooks: integer;
    function LocateISBN(const ISBN: string): boolean;
    property ISBN: TWideStringField read FISBN;
    property Title: TWideStringField read FTitle;
    property Authors: TWideStringField read FAuthors;
    property Status: TWideStringField read FStatus;
    property ReleseDate: TDateField read FReleseDate;
    property Pages: TIntegerField read FPages;
    property Price: TBCDField read FPrice;
    property Currency: TWideStringField read FCurrency;
    property Imported: TDateTimeField read FImported;
    property Description: TWideStringField read FDescription;
    // this property should be hidden, but during migration can be usefull
    // property DataSet: TDataSet read FDataSet;
  end;

implementation

uses
  System.SysUtils;

procedure TBookProxy.ConnectFields;
const
  ExpectedFieldCount = 10;
begin
  FISBN := fDataSet.FieldByName('ISBN') as TWideStringField;
  FTitle := fDataSet.FieldByName('Title') as TWideStringField;
  FAuthors := fDataSet.FieldByName('Authors') as TWideStringField;
  FStatus := fDataSet.FieldByName('Status') as TWideStringField;
  FReleseDate := fDataSet.FieldByName('ReleseDate') as TDateField;
  FPages := fDataSet.FieldByName('Pages') as TIntegerField;
  FPrice := fDataSet.FieldByName('Price') as TBCDField;
  FCurrency := fDataSet.FieldByName('Currency') as TWideStringField;
  FImported := fDataSet.FieldByName('Imported') as TDateTimeField;
  FDescription := fDataSet.FieldByName('Description') as TWideStringField;
  Assert(fDataSet.Fields.Count = ExpectedFieldCount);
end;

function TBookProxy.CountMoreExpensiveBooks: integer;
var
  CurrentPrice: Extended;
  Count: integer;
begin
  Count := 0;
  CurrentPrice := Price.Value;
  self.ForEach(
    procedure
    begin
      if self.Price.Value > CurrentPrice then
        Count := Count + 1;
    end);
  Result := Count;
end;

function TBookProxy.LocateISBN(const ISBN: string): boolean;
begin
  Result := fDataSet.Locate('ISBN', ISBN, []);
end;

function TBookProxy.ToString: String;
begin
  Result := Format('%s (%.2f %s)', [Title.Value, Price.Value, Currency.Value]);
end;

end.
