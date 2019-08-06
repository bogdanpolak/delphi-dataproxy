# DataProxy Pattern for Delphi

## Overview

TDataSetProxy is a wrapper component for the TDataSet component (Delphi). The proxy allows to replace any dataset (TDataSet descendant) with a mock dataset - memory table. Solution can be used to separate a business class from  datasets during unit testing. Another use is to allow easy replacement of one DAC's components with another.

![](./doc/resources/datasetproxy-01.png)

**Inspiration**. Idea is based on Proxy GoF pattern and Active Record pattern (from: Martin Fowler - Patterns of Enterprise Application Architecture). See article: [Evolving Toward a Persistence Layer by Patkos Csaba](https://code.tutsplus.com/tutorials/evolving-toward-a-persistence-layer--net-27138)

## Generator Application

The Generator application automatically creates Delphi source code based od sample SQL query (eg. SELECT statement). The project contains its source code (foder `/generator`). In current release the `Generator App` uses FireDAC to connect to RDBMS server and execute query, but it's possible to extend support to other Delphi DAC components (eg. AnyDAC). 

![](./doc/resources/generator-app.png)

Main generator's goals are:
* Receive a SQL statement 
  * Connects to RDBMS database with `FireDAC`, paste, enter or edit a SQL statement
  * Checks a structure and data in the result data set
* Generate a proxy
  * Using a SQL statement structure creates a Delphi code with new DAO class based on the TProxyDataSet
* Generate a dataset mock `MemTable`
  * Creates a Delphi code which builds a `TFDMemTable` component with the same structure as the input dataset
  * Creates a Delphi code that clones data using the `Append` procedure
  
Supported Delphi versions: XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo, 10.3 Rio

## Samples

Check `samples` subfolder

1) Books sample
    1) see the setup documentation: [Samples README](./samples/README.md)
    1) `TDatasetProxy` class source code is in the `/samples/base` folder
    1) Generated proxy = `TBookProxy` in (`Data.Proxy.Book.pas` unit)
    1) Generated mock factory = `function CreateMockTableBook` in (`Data.Mock.Book.pas` unit)

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
    // --------
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
  end;
  ...
```
[... more code - Gist sample (Data.Proxy.Book.pas)](https://gist.github.com/bogdanpolak/b13f0c5a677c3401734918dbfa7ae755)

```pas
unit Data.Mock.Book;

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

function CreateMockTableBook(AOwner: TComponent): TFDMemTable;

implementation

function CreateMockTableBook(AOwner: TComponent): TFDMemTable;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(AOwner);
  with ds do
  begin
    FieldDefs.Add('ISBN', ftWideString, 20);
    FieldDefs.Add('Title', ftWideString, 100);
    FieldDefs.Add('Authors', ftWideString, 100);
    FieldDefs.Add('Status', ftWideString, 15);
    FieldDefs.Add('ReleseDate', ftDate);
    FieldDefs.Add('Pages', ftInteger);
    with FieldDefs.AddFieldDef do begin
      Name := 'Price';  DataType := ftBCD;  Precision := 12;  Size := 2;
    end;
    FieldDefs.Add('Currency', ftWideString, 10);
    FieldDefs.Add('Imported', ftDateTime);
    FieldDefs.Add('Description', ftWideString, 2000);
    CreateDataSet;
  end;
  ...
  Result := ds;
end;
```
[... more code - Gist sample (Data.Mock.Book.pas)](https://gist.github.com/bogdanpolak/1622fcc3e4f1185fb4ead8263c9b8b31)

## Documentation

1. [Proxy Generator User Guide](doc/generator-guide.md)
1. [Using TProxyDataSet in the project](doc/using-proxydataset.md)

## More

[TBD]
