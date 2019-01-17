unit Helper.TApplication;

interface

uses
  Vcl.Forms;

type
  THelperApplication = class helper for TApplication
    { TODO 99: XML Documentation is required }
    function InDeveloperMode: boolean;
  end;

implementation

uses
  System.SysUtils;

function THelperApplication.InDeveloperMode: boolean;
var
  Extention: string;
  AExeName: string;
  ProjectFileName: string;
begin
{$IFDEF DEBUG}
  Extention := '.dpr';
  AExeName := ExtractFileName(Application.ExeName);
  ProjectFileName := ChangeFileExt(AExeName, Extention);
  Result := FileExists(ProjectFileName) or
    FileExists('..\..\' + ProjectFileName);
{$ELSE}
  Result := False;
{$ENDIF}
end;

end.
