unit Helper.TFDCustomManager;

interface

uses
  FireDAC.Comp.Client;

type
  TFDCustomManagerHelper = class helper for TFDCustomManager
  public
    function GetConnectionNamesAsArrray: TArray<string>;
  end;

implementation

function TFDCustomManagerHelper.GetConnectionNamesAsArrray: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, Self.ConnectionDefs.Count);
  for i := 0 to Self.ConnectionDefs.Count-1 do
    Result[i] := Self.ConnectionDefs.Items[i].Name;
end;

end.
