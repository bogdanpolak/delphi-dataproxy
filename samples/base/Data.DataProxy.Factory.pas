{ * ------------------------------------------------------------------------
  * ♥  Delphi DataProxy Project © 2019  ♥
  * https://github.com/bogdanpolak/delphi-dataproxy
  *  ----------------------------------------------------------------------- * }
unit Data.DataProxy.Factory;

interface

uses
  System.Classes,
  Data.DataProxy, Data.DB,
  FireDAC.Comp.Client;

type
  TDatasetProxyClass = class of TDatasetProxy;

  TDataProxyFactory = class
    class function CreateAndOpenProxy(AClass: TDatasetProxyClass;
      Owner: TComponent; Connection: TFDConnection; const ASqlStatement: string): TDatasetProxy; overload;
    class function CreateAndOpenProxy(AClass: TDatasetProxyClass;
      Owner: TComponent; Connection: TFDConnection; const ASqlStatement: string;  const AParams: array of Variant): TDatasetProxy; overload;
  end;

implementation

class function TDataProxyFactory.CreateAndOpenProxy(AClass: TDatasetProxyClass;
  Owner: TComponent; Connection: TFDConnection; const ASqlStatement: string): TDatasetProxy;
begin
  Result := CreateAndOpenProxy(AClass, Owner, Connection, ASqlStatement, []);
end;

class function TDataProxyFactory.CreateAndOpenProxy(AClass: TDatasetProxyClass;
  Owner: TComponent; Connection: TFDConnection; const ASqlStatement: string;
  const AParams: array of Variant): TDatasetProxy;
var
  ds: TDataSet;
begin
  Result := AClass.Create(Owner);
  Connection.ExecSQL('SELECT ISBN, Title, Authors, Status, ' +
    'ReleseDate, Pages, Price, Currency, Imported, Description FROM Books', ds);
  Result.ConnectWithDataSet(ds);
  Result.Open;
end;

end.
