type
  TClock = class(TInterfacedObject)
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
    constructor Create;
    destructor Destroy; override;

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

  // 获取程序每秒帧率
  TFrames  = class(TInterfacedObject)
  private
    _fpsTimer: TClock;
    _capTimer: TClock;
    _countedFrames: uint32;
    _framerateLimit: integer;

    function __GetAvgFPS: single;
    procedure __SetFramerateLimit(Value: integer);

  public
    constructor Create;
    destructor Destroy; override;

    // 获取当前平均帧率
    property AvgFPS: single read __GetAvgFPS;

    // 读取或设置帧率限制
    property FramerateLimit: integer read _framerateLimit write __SetFramerateLimit;
  end;

