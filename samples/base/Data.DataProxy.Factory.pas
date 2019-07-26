{ * ------------------------------------------------------------------------
  * ♥  Delphi DataProxy Project © 2019  ♥
  * https://github.com/bogdanpolak/delphi-dataproxy
  *  ----------------------------------------------------------------------- * }
unit Data.DataProxy.Factory;

interface

uses
  Data.DataProxy;

type
  TDatasetProxyClass = class of TDatasetProxy;

  TDataProxyFactory = class
    class procedure RegisterSQLProxy(AClass: TDatasetProxyClass;
      const SqlStmnt: string);
    class function CreateAndOpenProxy(AClass: TDatasetProxyClass)
      : TDatasetProxy; overload;
    class function CreateAndOpenProxy(AClass: TDatasetProxyClass;
      const AParams: array of Variant): TDatasetProxy; overload;
  end;

implementation

class function TDataProxyFactory.CreateAndOpenProxy(AClass: TDatasetProxyClass;
  const AParams: array of Variant): TDatasetProxy;
begin

end;

class function TDataProxyFactory.CreateAndOpenProxy(AClass: TDatasetProxyClass)
  : TDatasetProxy;
begin

end;

class procedure TDataProxyFactory.RegisterSQLProxy(AClass: TDatasetProxyClass;
  const SqlStmnt: string);
begin

end;

end.
