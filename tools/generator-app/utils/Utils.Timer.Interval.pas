unit Utils.Timer.Interval;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.ExtCtrls;

type
  // --------------------------------------------------------------------
  // TIntervalTimer
  // * class method Run(Owner, Interval, OnTimerProc);
  // * class method RunOnce(Owner, DelayMs, OnTimerProc);
  // --------------------------------------------------------------------
  TTimerMode = (tmDefault, tmOnce, tmRepeat);

  TIntervalTimer = class(TComponent)
  private
    class var fOwner: TComponent;
  private
    fTimerMode: TTimerMode;
    fTimer: TTimer;
    fOnTimerProc: TProc;
    procedure OnTimerEvent(Sender: TObject);
    class function InternalSetTimer(aTimerMode: TTimerMode; aInterval: Integer;
      aOnTimerProc: TProc): TIntervalTimer;
    class procedure GarbageCollector;
  public
    function Timer: TTimer;
    class function SetInterval(aInterval: Integer; aOnTimerProc: TProc)
      : TIntervalTimer;
    class procedure ClearInterval(aIntervalTimer: TIntervalTimer);
    class procedure SetTimeout(aInterval: Integer; aOnTimerProc: TProc);
  end;

implementation

class procedure TIntervalTimer.ClearInterval(aIntervalTimer: TIntervalTimer);
var
  i: Integer;
begin
  for i := fOwner.ComponentCount - 1 downto 0 do
    if fOwner.Components[i] = aIntervalTimer then
      aIntervalTimer.Timer.Enabled := False;
  GarbageCollector;
end;

class procedure TIntervalTimer.GarbageCollector;
var
  i: Integer;
  aIntervalTimer: TIntervalTimer;
begin
  for i := fOwner.ComponentCount - 1 downto 0 do
  begin
    aIntervalTimer := fOwner.Components[i] as TIntervalTimer;
    if aIntervalTimer.Timer.Enabled = False then
      aIntervalTimer.Free;
  end;
end;

class function TIntervalTimer.InternalSetTimer(aTimerMode: TTimerMode;
  aInterval: Integer; aOnTimerProc: TProc): TIntervalTimer;
begin
  if fOwner = nil then
    fOwner := TComponent.Create(nil);
  GarbageCollector;
  Result := TIntervalTimer.Create(fOwner);
  Result.fTimerMode := tmOnce;
  Result.fOnTimerProc := aOnTimerProc;
  Result.fTimer := TTimer.Create(Result);
  Result.fTimer.OnTimer := Result.OnTimerEvent;
  Result.fTimerMode := aTimerMode;
  Result.fTimer.Interval := aInterval;
  Result.fTimer.Enabled := True;
end;

procedure TIntervalTimer.OnTimerEvent(Sender: TObject);
begin
  if Assigned(fOnTimerProc) then
    fOnTimerProc();
  if (fTimerMode = tmOnce) then
    fTimer.Enabled := False;
end;

class function TIntervalTimer.SetInterval(aInterval: Integer;
  aOnTimerProc: TProc): TIntervalTimer;
begin
  Result := InternalSetTimer(tmRepeat, aInterval, aOnTimerProc);
end;

class procedure TIntervalTimer.SetTimeout(aInterval: Integer;
  aOnTimerProc: TProc);
begin
  InternalSetTimer(tmOnce, aInterval, aOnTimerProc);
end;

function TIntervalTimer.Timer: TTimer;
begin
  Result := fTimer;
end;

initialization

TIntervalTimer.fOwner := nil;

finalization

if TIntervalTimer.fOwner <> nil then
  TIntervalTimer.fOwner.Free;

end.
