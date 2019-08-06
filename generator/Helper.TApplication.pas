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
{$IFDEF DEBUG}
var
  Extention: string;
  AExeName: string;
  ProjectFileName: string;
begin
  Extention := '.dpr';
  AExeName := ExtractFileName(Application.ExeName);
  ProjectFileName := ChangeFileExt(AExeName, Extention);
  Result := FileExists(ProjectFileName) or
    FileExists('..\..\' + ProjectFileName);
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

end.
