unit App.AppInfo;

interface

type
  TAppInfo = class
  const
    AppName = 'Proxy Generator for FireDAC';
    Version = '1.1';
    ReleaseDate = '2020-03-09';
  end;

// -----------------------------------------------------
// Release History:
// -----------------------------------------------------
(*
[
  {"releaseDate": "2020-03-09", "version": "1.1"},
  {"releaseDate": "2020-01-29", "version": "1.0"},
  {"releaseDate": "2020-01-18", "version": "0.9"},
  {"releaseDate": "2019-10-02", "version": "0.8"},
  {"releaseDate": "2019-09-12", "version": "0.7"},
  {"releaseDate": "2019-08-26", "version": "0.6"},
  {"releaseDate": "2019-08-06", "version": "0.5.1"},
  {"releaseDate": "2019-08-05", "version": "0.5"},
  {"releaseDate": "2019-08-01", "version": "0.4"},
  {"releaseDate": "2019-07-25", "version": "0.3"},
  {"releaseDate": "2019-01-17", "version": "0.2"}
]
*)

type
  TAplicationAutomation = class
  public
    // ------------
    // Automation for quicker application testing (avaliable only in Developer Mode)
    class function IsActive: boolean;
    class function IsLevelSupported(level: integer): boolean;
    class procedure SpeedSlowDown;
  private const
    AutomationActive = False;
    AutomationLevel = 5;
    // 1: Select Connection
    // 2: Connect
    // 3: QueryBuilder & demo query
    // 4: Execute sql command
    // 5: Generate proxy
    AutomationSpeed = 3;
    // 0: max speed (no delay) otherwise:  AutomationSpeed x 100 ms delay
  end;

implementation

uses
  System.SysUtils,
  Vcl.Forms,
  Helper.TApplication;

// Automation for faster application testing (active only in Developer Mode)
class function TAplicationAutomation.IsActive: boolean;
begin
  Result := AutomationActive and Application.InDeveloperMode;
end;

class function TAplicationAutomation.IsLevelSupported(level: integer): boolean;
begin
  SpeedSlowDown;
  Result := (level <= AutomationLevel);
end;

class procedure TAplicationAutomation.SpeedSlowDown;
var
  i: integer;
begin
  for i := 1 to AutomationSpeed do
  begin
    Application.ProcessMessages;
    Sleep(100);
  end;
end;


end.
