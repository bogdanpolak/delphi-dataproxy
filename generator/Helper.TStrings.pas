unit Helper.TStrings;

interface

uses
  System.Classes, System.Types;

type
  TStringsHelper = class helper for TStrings
    function ToStringArray: TStringDynArray;
  end;

implementation

function TStringsHelper.ToStringArray: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result,Self.Count);
  for i := 0 to Self.Count-1 do
    Result[i] := Self[i];
end;

end.
