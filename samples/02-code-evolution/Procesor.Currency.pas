unit Procesor.Currency;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.Net.HttpClient,

  Procesor.Currency.Intf;

type
  ECurrencyProcessorError = class(Exception);

  TCurrencyProcessor = class(TInterfacedObject, ICurrencyProcessor)
  private
    fCurrencyRates: TArray<TCurrencyRate>;
    function GetCurrencyRate(const aCode: string): Currency;
  public
    constructor Create;
    function IsInitialiased: boolean;
    procedure Download(const aURL: string);
    function IsCurrencySupported(const aCode: string): boolean;
    function Convert(aValue: Currency; const aFromCurrency: string;
      const aToCurrency: string): Currency;
  end;

implementation

constructor TCurrencyProcessor.Create;
begin
  fCurrencyRates := nil;
end;

function TCurrencyProcessor.IsInitialiased: boolean;
begin
  Result := (fCurrencyRates <> nil);
end;

procedure TCurrencyProcessor.Download(const aURL: string);
var
  aStringStream: TStringStream;
  aHTTPClient: THTTPClient;
  jsResult: TJSONObject;
  jsRates: TJSONObject;
  idx: integer;
begin
  aStringStream := TStringStream.Create('', TEncoding.UTF8);
  try
    aHTTPClient := THTTPClient.Create;
    try
      aHTTPClient.Get(aURL, aStringStream);
      jsResult := TJSONObject.ParseJSONValue(aStringStream.DataString)
        as TJSONObject;
      try
        jsRates := jsResult.GetValue('rates') as TJSONObject;
        SetLength(fCurrencyRates, jsRates.Count + 1);
        fCurrencyRates[0].Code := 'EUR';
        fCurrencyRates[0].Rate := 1.0000;
        for idx := 0 to jsRates.Count - 1 do
        begin
          fCurrencyRates[idx + 1].Code := jsRates.Pairs[idx].JsonString.Value;
          fCurrencyRates[idx + 1].Rate := jsRates.Pairs[idx]
            .JsonValue.AsType<double>;
        end;
      finally
        jsResult.Free;
      end;
    finally
      aHTTPClient.Free;
    end;
  finally
    aStringStream.Free;
  end;
end;

function TCurrencyProcessor.IsCurrencySupported(const aCode: string): boolean;
var
  idx: integer;
begin
  for idx := 0 to High(fCurrencyRates) do
    if fCurrencyRates[idx].Code = aCode then
      Exit(True);
  Result := False;
end;

function TCurrencyProcessor.GetCurrencyRate(const aCode: string): Currency;
var
  idx: integer;
begin
  for idx := 0 to High(fCurrencyRates) do
    if fCurrencyRates[idx].Code = aCode then
      Exit(fCurrencyRates[idx].Rate);
  raise ECurrencyProcessorError.Create('Unregistered currency code: ' + aCode);
end;

function TCurrencyProcessor.Convert(aValue: Currency;
  const aFromCurrency, aToCurrency: string): Currency;
begin
  Result := aValue / GetCurrencyRate(aFromCurrency) *
    GetCurrencyRate(aToCurrency);
end;

end.
