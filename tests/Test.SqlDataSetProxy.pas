unit Test.SqlDataSetProxy;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB,

  Data.DataProxy;

{$M+}

type

  [TestFixture]
  TestSqDemoProxy = class(TObject)
  private const
    TestUsingFireDefinitionName = 'SQLite_Demo';
  private
    fOwner: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure CheckFireDAC_ConnectionDef;
    procedure WithSql_CustomerOrders;
    procedure WithSql_Orders_Year1998_Month01;
  end;

implementation

uses
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite;


// -----------------------------------------------------------------------
// Proxy: CustomerOrders
// -----------------------------------------------------------------------

type
  TCustomerOrdersProxy = class(TDatasetProxy)
  strict private
    FOrderID: TAutoIncField;
    FCustomerID: TStringField;
    FCompanyName: TStringField;
    FEmployeeID: TIntegerField;
    FEmployeeName: TWideStringField;
    FOrderDate: TDateTimeField;
    FRequiredDate: TDateTimeField;
    FShippedDate: TDateTimeField;
    FShipVia: TIntegerField;
    FFreight: TCurrencyField;
  strict protected
    procedure ConnectFields; override;
  public
    property OrderID: TAutoIncField read FOrderID;
    property CustomerID: TStringField read FCustomerID;
    property CompanyName: TStringField read FCompanyName;
    property EmployeeID: TIntegerField read FEmployeeID;
    property EmployeeName: TWideStringField read FEmployeeName;
    property OrderDate: TDateTimeField read FOrderDate;
    property RequiredDate: TDateTimeField read FRequiredDate;
    property ShippedDate: TDateTimeField read FShippedDate;
    property ShipVia: TIntegerField read FShipVia;
    property Freight: TCurrencyField read FFreight;
  end;

procedure TCustomerOrdersProxy.ConnectFields;
const
  ExpectedFieldCount = 10;
begin
  FOrderID := FDataSet.FieldByName('OrderID') as TAutoIncField;
  FCustomerID := FDataSet.FieldByName('CustomerID') as TStringField;
  FCompanyName := FDataSet.FieldByName('CompanyName') as TStringField;
  FEmployeeID := FDataSet.FieldByName('EmployeeID') as TIntegerField;
  FEmployeeName := FDataSet.FieldByName('EmployeeName') as TWideStringField;
  FOrderDate := FDataSet.FieldByName('OrderDate') as TDateTimeField;
  FRequiredDate := FDataSet.FieldByName('RequiredDate') as TDateTimeField;
  FShippedDate := FDataSet.FieldByName('ShippedDate') as TDateTimeField;
  FShipVia := FDataSet.FieldByName('ShipVia') as TIntegerField;
  FFreight := FDataSet.FieldByName('Freight') as TCurrencyField;
  System.Assert(FDataSet.Fields.Count = ExpectedFieldCount);
end;

type
  TMiniOrdersProxy = class(TDatasetProxy)
  strict private
    FOrderID: TAutoIncField;
    FCustomerID: TStringField;
    FOrderDate: TDateTimeField;
    FFreight: TCurrencyField;
  strict protected
    procedure ConnectFields; override;
  public
    property OrderID: TAutoIncField read FOrderID;
    property CustomerID: TStringField read FCustomerID;
    property OrderDate: TDateTimeField read FOrderDate;
    property Freight: TCurrencyField read FFreight;
  end;

procedure TMiniOrdersProxy.ConnectFields;
const
  ExpectedFieldCount = 4;
begin
  FOrderID := FDataSet.FieldByName('OrderID') as TAutoIncField;
  FCustomerID := FDataSet.FieldByName('CustomerID') as TStringField;
  FOrderDate := FDataSet.FieldByName('OrderDate') as TDateTimeField;
  FFreight := FDataSet.FieldByName('Freight') as TCurrencyField;
  System.Assert(FDataSet.Fields.Count = ExpectedFieldCount);
end;


// -----------------------------------------------------------------------
// Utilities
// -----------------------------------------------------------------------

function GivenConnection(aOwner: TComponent): TFDConnection;
begin
  Result := TFDConnection.Create(aOwner);
  Result.ConnectionName := TestSqDemoProxy.TestUsingFireDefinitionName;
  Result.FetchOptions.RowsetSize := 100;
end;


// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestSqDemoProxy.Setup;
begin
  FDManager.SilentMode := True;
  fOwner := TComponent.Create(nil);
end;

procedure TestSqDemoProxy.TearDown;
begin
  fOwner.Free;
end;


// -----------------------------------------------------------------------
// Tests: CheckFireDAC
// -----------------------------------------------------------------------

procedure TestSqDemoProxy.CheckFireDAC_ConnectionDef;
begin
  Assert.IsTrue(FDManager.ConnectionDefs.FindConnectionDef
    (TestUsingFireDefinitionName) <> nil, 'Test fixture ' + Self.ClassName +
    ' required FireDAC to work. ' + 'Expected connction definition "' +
    TestUsingFireDefinitionName + '" not found.');
end;


// -----------------------------------------------------------------------
// Tests: WithSql
// -----------------------------------------------------------------------

procedure TestSqDemoProxy.WithSql_CustomerOrders;
var
  aCustOrdersProxy: TCustomerOrdersProxy;
begin
  aCustOrdersProxy := TCustomerOrdersProxy.Create(fOwner);

  aCustOrdersProxy.WithFiredacSQL(GivenConnection(fOwner),
    {} 'SELECT Orders.OrderID,'#13#10 +
    {} '  Orders.CustomerID, Customers.CompanyName, Orders.EmployeeID,'#13#10 +
    {} '  Employees.FirstName||'' ''||Employees.LastName EmployeeName,'#13#10 +
    {} '  Orders.OrderDate, Orders.RequiredDate, Orders.ShippedDate,'#13#10 +
    {} '  Orders.ShipVia, Orders.Freight'#13#10 +
    {} 'FROM {id Orders} Orders'#13#10 +
    {} '  INNER JOIN {id Employees} Employees'#13#10 +
    {} '    ON Orders.EmployeeID = Employees.EmployeeID'#13#10 +
    {} '  INNER JOIN {id Customers} Customers'#13#10 +
    {} '    ON Orders.CustomerID = Customers.CustomerID'#13#10 +
    {} 'WHERE {year(OrderDate)} = 1997 and {month(OrderDate)} = 11 '#13#10 +
    {} 'ORDER BY Orders.OrderID');

  Assert.AreEqual(34, aCustOrdersProxy.RecordCount);
end;


procedure TestSqDemoProxy.WithSql_Orders_Year1998_Month01;
var
  aOrdersProxy: TMiniOrdersProxy;
begin
  aOrdersProxy := TMiniOrdersProxy.Create(fOwner);

  aOrdersProxy.WithFiredacSQL(GivenConnection(fOwner),
    {} 'SELECT OrderID, CustomerID, OrderDate, Freight' +
    {} ' FROM {id Orders} ' +
    {} ' WHERE {year(OrderDate)} = :AYear and {month(OrderDate)} = :AMonth',
    [1998, 01]);

  Assert.AreEqual(55, aOrdersProxy.RecordCount);
end;

end.
