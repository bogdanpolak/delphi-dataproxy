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
  TDataProxyFactory = class
    class function CreateProxy<T: TDataSetProxy> (Owner: TComponent;
      ADataSet: TDataSet): T;
  end;

implementation

class function TDataProxyFactory.CreateProxy<T>(Owner: TComponent;
  ADataSet: TDataSet): T;
begin
  Result := T.Create(Owner);
  Result.ConnectWithDataSet(ADataSet);
  Result.Open;
end;

end.
