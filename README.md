# DataProxy Pattern for Delphi

![ Delphi Support ](https://img.shields.io/badge/Delphi%20Support-%20XE8%20..%2010.3%20Rio-blue.svg)
![ version ](https://img.shields.io/badge/version-%200.9-yellow.svg)

-------------------------------------------------------------------
## TBD in ver. 1.0 (plan)

Tasks:
1) Remove Generator App from main `README`
   - move to Generator App documentation to separate document
   - update diagrams
2) Merge [Using TProxyDataSet in action](https://github.com/bogdanpolak/delphi-dataproxy/blob/master/doc/using-proxy.md) with main README
3) Merge [TDataSet vs TDataSetProxy](https://github.com/bogdanpolak/delphi-dataproxy/blob/master/doc/compare-dataset-vs-proxy.md) with main README as introduction
4) Separate memory dataset generation as separate subject for unit testing
5) Describe TDataSetProxy as a tool for extracting logic form legacy projects
6) Separate two sections:
   * refactoring dataset into `TDataSetProxy`
   * inserting extracted code into unit test harness
7) Point [DataSet Generator](https://github.com/bogdanpolak/dataset-generator) as a supportive project
8) Remove `TDataProxyFactory` use `WithDataSet`
9) Update docs according to current folder structure:
   * `proxy` -> `src`
   * `generator` -> `tools\generator-app`
10) Expnlain fake datasets generator as supportive project
   * point source repo: https://github.com/bogdanpolak/dataset-generator


-------------------------------------------------------------------
## Overview

TDataSetProxy is a wrapper component for the classic Delphi dataset component. Proxy allows to replace any dataset (TDataSet class descendant) with a fake dataset (in-memory table). Solution can be used to separate a business class from  datasets during unit testing. Another use is to allow easy replacement of one DAC's components with another.

![](./doc/resources/datasetproxy-diagram.png)

**Inspiration**. Idea is based on Proxy GoF pattern and Active Record pattern (from: Martin Fowler - Patterns of Enterprise Application Architecture). See article: [Evolving Toward a Persistence Layer by Patkos Csaba](https://code.tutsplus.com/tutorials/evolving-toward-a-persistence-layer--net-27138)

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
  TBookProxy = class(Data.DataProxy.TDatasetProxy)
  private
    FISBN :TWideStringField;
    FTitle :TWideStringField;
    FReleseDate :TDateField;
    FPages :TIntegerField;
    FPrice :TBCDField;
  protected
    procedure ConnectFields; override;
  public
    property ISBN :TWideStringField read FISBN;
    property Title :TWideStringField read FTitle;
    property ReleseDate :TDateField read FReleseDate;
    property Pages :TIntegerField read FPages;
    property Price :TBCDField read FPrice;
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

function CreateMemDataSet_Book(AOwner: TComponent): TDataSet;

implementation

function CreateMemDataSet_Book(AOwner: TComponent): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(AOwner);
  with ds do
  begin
    FieldDefs.Add('ISBN', ftWideString, 20);
    FieldDefs.Add('Title', ftWideString, 100);
    FieldDefs.Add('ReleseDate', ftDate);
    FieldDefs.Add('Pages', ftInteger);
    with FieldDefs.AddFieldDef do begin
      Name := 'Price';  DataType := ftBCD;  Precision := 12;  Size := 2;
    end;
    CreateDataSet;
  end;
  with ds do
  begin
    Append;
    FieldByName('ISBN').Value := '978-0131177055';
    FieldByName('Title').Value := 'Working Effectively with Legacy Code';
    FieldByName('ReleseDate').Value := EncodeDate(2004,10,1);
    FieldByName('Pages').Value := 464;
    FieldByName('Price').Value := 52.69;
    Post;
  end;
  // ... more rows appended here ...
  Result := ds;
end;
```
[... more code - Gist sample (Data.Mock.Book.pas)](https://gist.github.com/bogdanpolak/1622fcc3e4f1185fb4ead8263c9b8b31)

## Documentation

1. [Proxy Generator User Guide](doc/generator-app-guide.md)
1. [Using TProxyDataSet in the project](doc/using-proxy.md) *(in progress)*
1. [Why using TDataSetProxy](./doc/compare-dataset-vs-proxy.md)
