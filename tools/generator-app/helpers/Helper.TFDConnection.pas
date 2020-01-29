unit Helper.TFDConnection;

interface

uses
  System.Classes,
  FireDAC.Comp.Client;

type
  TFDConnectionHelper = class helper for TFDConnection
  public
    function WithConnectionDef(const aDefinitionName: string): TFDConnection;
    function IsConnected: boolean;
    function GetFieldNamesAsArray(const aTableName: string): TArray<String>;
    function GetTableNamesAsArray: TArray<String>;
  end;

implementation

uses
  FireDAC.Phys.Intf;

function TFDConnectionHelper.GetTableNamesAsArray: TArray<String>;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    Self.GetTableNames('', '', '', sl, //.
      // TFDPhysObjectScopes = [osMy, osOther, osSystem]
      [osMy],
      // TFDPhysTableKinds = (tkSynonym, tkTable, tkView, tkTempTable, tkLocalTable)
      [tkSynonym, tkTable, tkView],
      // aFullName: Boolean
      True);
    SetLength(Result, sl.Count);
    for i := 0 to sl.Count - 1 do
      Result[i] := sl[i];
  finally
    sl.Free;
  end;
end;

function TFDConnectionHelper.IsConnected: boolean;
begin
  Result := Self.Connected;
end;

function TFDConnectionHelper.WithConnectionDef(const aDefinitionName: string)
  : TFDConnection;
begin
  Self.ConnectionDefName := aDefinitionName;
  Result := Self;
end;

function TFDConnectionHelper.GetFieldNamesAsArray(const aTableName: string)
  : TArray<String>;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    Self.GetFieldNames('', '', aTableName, '', sl);
    SetLength(Result, sl.Count);
    for i := 0 to sl.Count - 1 do
      Result[i] := sl[i];
  finally
    sl.Free;
  end;
end;

end.
