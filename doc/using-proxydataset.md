# Using TProxyDataSet in action

The modernization process includes 4 steps: 
1. The proxy generation
2. Moving the behavior to the proxy (optional)
3. Create the proxy
4. Replace `TDataSet` with the proxy

## 1. The proxy generation 

Using the generator application it's possible to create automatically proxy unit. Sample unit `Data.Proxy.Book.pas` can looks like that:

```pas
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
    FPrice :TBCDField;
    FCurrency :TWideStringField;
  protected
    procedure ConnectFields; override;
  public
    property ISBN :TWideStringField read FISBN;
    property Title :TWideStringField read FTitle;
    property Authors :TWideStringField read FAuthors;
    property Price :TBCDField read FPrice;
    property Currency :TWideStringField read FCurrency;
    // this property should be hidden, but during migration can be usefull
    // property DataSet: TDataSet read FDataSet;
  end;

implementation

uses
  System.SysUtils;

procedure TBookProxy.ConnectFields;
const
  ExpectedFieldCount = 5;
begin
  FISBN := FDataSet.FieldByName('ISBN') as TWideStringField;
  FTitle := FDataSet.FieldByName('Title') as TWideStringField;
  FAuthors := FDataSet.FieldByName('Authors') as TWideStringField;
  FStatus := FDataSet.FieldByName('Status') as TWideStringField;
  FPrice := FDataSet.FieldByName('Price') as TBCDField;
  FCurrency := FDataSet.FieldByName('Currency') as TWideStringField;
  Assert(FDataSet.Fields.Count = ExpectedFieldCount);
end;

end.
```

## 2. Moving the behavior to the proxy (optional)

You can immediately replace the classic `TDataSet` with the generated object, but the recommended previous step is to transfer the domain code to the proxy object (behavior). Thanks to this step, you can write some simple unit tests from the very beginning - even using the TDD. Sample behavior for the book data:

```pas
function TBookProxy.ToString: String;
begin
  Result := Format('%s (%.2f %s)',[Title.Value,Price.Value,Currency.Value]);
end;

function TBookProxy.LocateISBN(const ISBN: string): boolean;
begin
  Result := FDataSet.Locate('ISBN',ISBN,[]);
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
```

## 3. Create the proxy

```pas
uses
  Data.Proxy.Book;

type
  TDataModule1 = class(TDataModule)
    procedure TDataModule1.DataModuleCreate(Sender: TObject);
  private
    function CreateSQLDataSet_Book(AOwner: TComponent; 
      AConnection: TFDConnection): TDataSet;
  public
    BookProxy: TBookProxy;
  end;

function TDataModule1.CreateSQLDataSet_Book(AOwner: TComponent; 
  AConnection: TFDConnection): TDataSet;
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

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
begin
  BookProxy := TDataProxyFactory.CreateProxy<TBookProxy>(Self,
    CreateSQLDataSet_Book(Self, FDConnection1));
end;
```

## 4. Replace `TDataSet` with the proxy


```pas
procedure TForm1.Button1Click(Sender: TObject);
var
 ABook: TBookProxy;
begin
  ListBox1.ItemIndex := -1;
  InitializeMoreExpensiveButtons(nil);
  ListBox1.Clear;
  ABook := DataModule1.BookProxy;
  ABook.ForEach(
    procedure
    begin
      ListBox1.Items.Add(ABook.ISBN.Value + ' ' +ABook.ToString);
    end);
end;
```

```pas
procedure TForm1.ListBox1Click(Sender: TObject);
var
  s: string;
  ISBN: string;
begin
  if (ListBox1.ItemIndex >= 0) then
  begin
    s := ListBox1.Items[ListBox1.ItemIndex];
    ISBN := s.Substring(0, 14);  // TODO: refactor this ugly code
    DataModule1.BookProxy.LocateISBN(ISBN);
    Self.Caption := DataModule1.BookProxy.Title.Value;
  end;
end;
```

```pas
procedure TForm1.Button2Click(Sender: TObject);
begin
  Button2.Caption := Format('More expensive books = %d',
    [DataModule1.BookProxy.CountMoreExpensiveBooks]);
end;
```
