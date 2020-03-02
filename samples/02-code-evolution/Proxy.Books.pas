unit Proxy.Books;

interface

uses
  Data.DB,
  Data.DataProxy,
  System.SysUtils,
  System.Classes,
  FireDAC.Comp.Client;

type
  TBooksProxy = class(TDatasetProxy)
  private
    FISBN :TWideStringField;
    FTitle :TWideStringField;
    FAuthors :TWideStringField;
    FStatus :TWideStringField;
    FReleaseDate :TWideStringField;
    FPages :TIntegerField;
    FPrice :TBCDField;
    FCurrency :TWideStringField;
  protected
    procedure ConnectFields; override;
  public
    property ISBN :TWideStringField read FISBN;
    property Title :TWideStringField read FTitle;
    property Authors :TWideStringField read FAuthors;
    property Status :TWideStringField read FStatus;
    property ReleaseDate :TWideStringField read FReleaseDate;
    property Pages :TIntegerField read FPages;
    property Price :TBCDField read FPrice;
    property Currency :TWideStringField read FCurrency;
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

end.
