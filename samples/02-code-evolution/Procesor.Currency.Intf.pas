unit Procesor.Currency.Intf;

interface

type
  TCurrencyRate = record
    Code: String;
    Rate: Currency;
  end;

  ICurrencyProcessor = interface(IInvokable)
    ['{08CE1219-BC8A-45C9-B704-9D8D3B25FDFB}']
    function IsInitialiased: boolean;
    procedure Download(const aURL: string);
    function IsCurrencySupported(const aCode: string): boolean;
    function Convert(aValue: Currency; const aFromCurrency: string;
      const aToCurrency: string): Currency;
  end;

implementation

end.
