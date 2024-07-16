unit DeepStar.SDL2.Timer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  libSDL2;

type
  PClock = ^TClock;
  TClock = object
  private type
    TEventProc = procedure of object;

  private
    _Enabled: boolean;
    _Interval: cardinal;
    _OnStartTicks: TEventProc;
    _OnStopTicks: TEventProc;
    _OnTicks: TEventProc;
    _StartTime: cardinal;
    _timer: cardinal;

    function __GetElapsedTime: cardinal;
    function __GetElapsedTimeAsSeconds: single;
    function __GetSeconds: single;
    function __GetTicks: cardinal;
    procedure __SetOnTimer(const Value: TEventProc);
    procedure __SetSeconds(const Value: single);

  public
    constructor Init;
    destructor Done;

    class function Create: TClock; static;
    class function CreatePtr: PClock; static;

    procedure Restart;
    procedure Tick;

    property Enabled: boolean read _Enabled write _Enabled;
    property Interval: cardinal read _Interval write _Interval;
    property Seconds: single read __GetSeconds write __SetSeconds;
    property ElapsedTime: cardinal read __GetElapsedTime;
    property ElapsedTimeAsSeconds: single read __GetElapsedTimeAsSeconds;

    property OnTick: TEventProc read _OnTicks write __SetOnTimer;
    property OnStartTick: TEventProc read _OnStartTicks write _OnStartTicks;
    property OnStopTick: TEventProc read _OnStopTicks write _OnStopTicks;
  end;

  PFrames = ^TFrames;
  // 获取程序每秒帧率的 Object
  TFrames  = object
  private
    _FpsTimer: TClock;
    _CapTimer: TClock;
    _CountedFrames: uint32;
    _FramerateLimit: integer;

    function __GetAvgFPS: single;
    procedure __SetFramerateLimit(Value: integer);

  public
    constructor Init;
    destructor Done;

    // 获取当前平均帧率
    property AvgFPS: single read __GetAvgFPS;

    // 读取或设置帧率限制
    property FramerateLimit: integer read _FramerateLimit write __SetFramerateLimit;
  end;

implementation

{ TClock }

constructor TClock.Init;
begin
  _Interval := 1000;
  _Enabled := true;
  _timer := _Interval;
  _StartTime := __GetTicks;
end;

class function TClock.Create: TClock;
begin
  Result.Init;
end;

class function TClock.CreatePtr: PClock;
begin
  New(Result, Init);
end;

destructor TClock.Done;
begin
  _timer := 0;

  inherited;
end;

procedure TClock.Restart;
begin
  Self._StartTime := __GetTicks;
end;

procedure TClock.Tick;
var
  time: cardinal;
begin
  if not _Enabled then Exit;

  time := Self.ElapsedTime;
  Restart;

  _timer += time;

  if _timer > _Interval then
  begin
    OnTick;
    _timer := 0;
  end;
end;

function TClock.__GetElapsedTime: cardinal;
begin
  Result := __GetTicks - _StartTime;
end;

function TClock.__GetElapsedTimeAsSeconds: single;
begin
  Result := (__GetTicks - _StartTime) / 1000;
end;

function TClock.__GetSeconds: single;
begin
  Result := _Interval / 1000;
end;

function TClock.__GetTicks: cardinal;
begin
  Result := SDL_GetTicks();
end;

procedure TClock.__SetOnTimer(const Value: TEventProc);
begin
  if _OnTicks = Value then Exit;
  _OnTicks := Value;
end;

procedure TClock.__SetSeconds(const Value: single);
begin
  _Interval := trunc(Value * 1000);
end;

{ TFrames }

constructor TFrames.Init();
begin
  _FpsTimer.Init;
  _CapTimer.Init;
end;

destructor TFrames.Done;
begin
  _CapTimer.Done;
  _FpsTimer.Done;
end;

function TFrames.__GetAvgFPS: single;
var
  res: single;
begin
  res := single(0);
  res := _CountedFrames / (_FpsTimer.ElapsedTime / 1000);
  if res > 2000000 then
  begin
    res := 0;
  end;

  _CountedFrames += 1;

  Result := res;
end;

procedure TFrames.__SetFramerateLimit(Value: integer);
var
  frameTicks, ScreenTicksPerFrame: integer;
begin
  if _FramerateLimit <> Value then
    _FramerateLimit := Value;

  ScreenTicksPerFrame := trunc(1000 / _FramerateLimit);

  // If frame finished early
  frameTicks := integer(0);
  frameTicks := _CapTimer.ElapsedTime;
  if frameTicks < ScreenTicksPerFrame then
  begin
    // Wait remaining time
    SDL_Delay(ScreenTicksPerFrame - frameTicks);
  end;

  _CapTimer.Restart;
end;

end.
