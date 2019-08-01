unit Data.Proxy.Book;

interface
uses
  Data.DB,
  Data.DataProxy;

type
  TBookProxy = class(TDatasetProxy)
  private
    FISBN :TWideStringField;
    FTitle :TWideStringField;
    FAuthors :TWideStringField;
    FStatus :TWideStringField;
    FReleseDate :TDateField;
    FPages :TIntegerField;
    FPrice :TBCDField;
    FCurrency :TWideStringField;
    FImported :TDateTimeField;
    FDescription :TWideStringField;
  protected
    procedure ConnectFields; override;
  public
    function ToString: String;  override;
    function CountMoreExpensiveBooks: integer;
    function LocateISBN (const ISBN: string): boolean;
    property ISBN :TWideStringField read FISBN;
    property Title :TWideStringField read FTitle;
    property Authors :TWideStringField read FAuthors;
    property Status :TWideStringField read FStatus;
    property ReleseDate :TDateField read FReleseDate;
    property Pages :TIntegerField read FPages;
    property Price :TBCDField read FPrice;
    property Currency :TWideStringField read FCurrency;
    property Imported :TDateTimeField read FImported;
    property Description :TWideStringField read FDescription;
    // this property should be hidden, but during migration can be usefull
    // property DataSet: TDataSet read FDataSet;
  end;

implementation

uses
  System.SysUtils;

procedure TBookProxy.ConnectFields;
begin
  FISBN := FDataSet.FieldByName('ISBN') as TWideStringField;
  FTitle := FDataSet.FieldByName('Title') as TWideStringField;
  FAuthors := FDataSet.FieldByName('Authors') as TWideStringField;
  FStatus := FDataSet.FieldByName('Status') as TWideStringField;
  FReleseDate := FDataSet.FieldByName('ReleseDate') as TDateField;
  FPages := FDataSet.FieldByName('Pages') as TIntegerField;
  FPrice := FDataSet.FieldByName('Price') as TBCDField;
  FCurrency := FDataSet.FieldByName('Currency') as TWideStringField;
  FImported := FDataSet.FieldByName('Imported') as TDateTimeField;
  FDescription := FDataSet.FieldByName('Description') as TWideStringField;
end;

function TBookProxy.CountMoreExpensiveBooks: integer;
var
  CurrentPrice: Extended;
  Count: Integer;
begin
  Count := 0;
  CurrentPrice := Price.Value;
  self.ForEach(
    procedure
    begin
      if Self.Price.Value > CurrentPrice then
        Count := Count + 1;
    end);
  Result := Count;
end;

function TBookProxy.LocateISBN(const ISBN: string): boolean;
begin
  Result := FDataSet.Locate('ISBN',ISBN,[]);
end;

function TBookProxy.ToString: String;
begin
  Result := Format('%s (%.2f %s)',[Title.Value,Price.Value,Currency.Value]);
end;

end.
