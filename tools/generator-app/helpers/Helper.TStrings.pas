unit Helper.TStrings;

interface

uses
  System.Classes, System.Types;

type
  TStringsHelper = class helper for TStrings
    // due to compatibility:
    // Delphi 10.3 Rio:
    // ... - TStringDynArray = TArray<string>;
    // ... - function TStrings.ToStringArray: TArray<string>;
    // Delphi XE8:
    // ... - TStringDynArray = array of string;
    // ... - function TStrings.ToStringArray: array of string;
    // ---
    // * can assign: TArray<string> := TArray<string>
    // * can't assign: array of string := array of string
    // ...   (requires the same type definition)
    function ToStringDynArray: System.Types.TStringDynArray;
  end;

implementation

function TStringsHelper.ToStringDynArray: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Self.Count);
  for i := 0 to Self.Count - 1 do
    Result[i] := Self[i];
end;

end.
