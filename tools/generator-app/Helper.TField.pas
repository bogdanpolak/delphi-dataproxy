unit Helper.TField;

interface

uses
  Data.DB;

type
  TFieldHelper = class helper for TField
    function ToClass: TClass;
  end;

implementation

function TFieldHelper.ToClass: TClass;
begin
  Result := Data.DB.DefaultFieldClasses[Self.DataType];
end;

end.
