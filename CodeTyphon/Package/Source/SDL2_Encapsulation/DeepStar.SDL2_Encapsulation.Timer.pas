unit DeepStar.SDL2_Encapsulation.Timer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  libSDL2;

type
  PClock = ^TClock;
  TClock = object(TObj_Base)
  private type
    TEventProc = procedure of object;

  private
    _enabled: boolean;
    _interval: cardinal;
    _OnStartTicks: TEventProc;
    _OnStopTicks: TEventProc;
    _OnTicks: TEventProc;
    _startTime: cardinal;
    _timer: cardinal;

    function __GetElapsedTime: cardinal;
    function __GetElapsedTimeAsSeconds: single;
    function __GetSeconds: single;
    function __GetTicks: cardinal;
    procedure __SetOnTimer(const Value: TEventProc);
    procedure __SetSeconds(const Value: single);

  public
    constructor Init;
    destructor Done; virtual;
    class function Create: TClock; static;
    class function CreatePtr: PClock; static;

    procedure Restart;
    procedure Tick;

    property Enabled: boolean read _enabled write _enabled;
    property Interval: cardinal read _interval write _interval;
    property Seconds: single read __GetSeconds write __SetSeconds;
    property ElapsedTime: cardinal read __GetElapsedTime;
    property ElapsedTimeAsSeconds: single read __GetElapsedTimeAsSeconds;

    property OnTick: TEventProc read _OnTicks write __SetOnTimer;
    property OnStartTick: TEventProc read _OnStartTicks write _OnStartTicks;
    property OnStopTick: TEventProc read _OnStopTicks write _OnStopTicks;
  end;

  PFrames = ^TFrames;
  // 获取程序每秒帧率的 Object
  TFrames  = object(TObj_Base)
  private
    _fpsTimer: TClock;
    _capTimer: TClock;
    _countedFrames: uint32;
    _framerateLimit: integer;

    function __GetAvgFPS: single;
    procedure __SetFramerateLimit(Value: integer);

  public
    constructor Init;
    destructor Done; virtual;
    class function Create: TFrames; static;
    class function CreatePtr: PFrames; static;

    // 获取当前平均帧率
    property AvgFPS: single read __GetAvgFPS;

    // 读取或设置帧率限制
    property FramerateLimit: integer read _framerateLimit write __SetFramerateLimit;
  end;

implementation

{ TClock }

constructor TClock.Init;
begin
  _interval := 1000;
  _enabled := true;
  _timer := _interval;
  _startTime := __GetTicks;
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
  Self._startTime := __GetTicks;
end;

procedure TClock.Tick;
var
  time: cardinal;
begin
  if not _enabled then Exit;

  time := Self.ElapsedTime;
  Restart;

  _timer += time;

  if _timer > _interval then
  begin
    OnTick;
    _timer := 0;
  end;
end;

function TClock.__GetElapsedTime: cardinal;
begin
  Result := __GetTicks - _startTime;
end;

function TClock.__GetElapsedTimeAsSeconds: single;
begin
  Result := (__GetTicks - _startTime) / 1000;
end;

function TClock.__GetSeconds: single;
begin
  Result := _interval / 1000;
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
  _interval := trunc(Value * 1000);
end;

{ TFrames }

constructor TFrames.Init();
begin
  _fpsTimer.Init;
  _capTimer.Init;
end;

class function TFrames.Create: TFrames;
begin
  Result.Init;
end;

class function TFrames.CreatePtr: PFrames;
begin
  New(Result, Init);
end;

destructor TFrames.Done;
begin
  _capTimer.Done;
  _fpsTimer.Done;
end;

function TFrames.__GetAvgFPS: single;
var
  res: single;
begin
  res := single(0);
  res := _countedFrames / (_fpsTimer.ElapsedTime / 1000);
  if res > 2000000 then
  begin
    res := 0;
  end;

  _countedFrames += 1;

  Result := res;
end;

procedure TFrames.__SetFramerateLimit(Value: integer);
var
  frameTicks, ScreenTicksPerFrame: integer;
begin
  if _framerateLimit <> Value then
    _framerateLimit := Value;

  ScreenTicksPerFrame := trunc(1000 / _framerateLimit);

  // If frame finished early
  frameTicks := integer(0);
  frameTicks := _capTimer.ElapsedTime;
  if frameTicks < ScreenTicksPerFrame then
  begin
    // Wait remaining time
    SDL_Delay(ScreenTicksPerFrame - frameTicks);
  end;

  _capTimer.Restart;
end;

end.
