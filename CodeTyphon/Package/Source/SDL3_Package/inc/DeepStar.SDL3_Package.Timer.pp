type

  { TClock }

  TClock = class(TInterfacedObject)
  private type
    TEventProc = procedure of object;

  private
    fEnabled: boolean;
    fInterval: cardinal;
    fOnStartTicks: TEventProc;
    fOnStopTicks: TEventProc;
    fOnTicks: TEventProc;
    fStartTime: cardinal;
    fTimer: cardinal;

    function GetElapsedTime: cardinal;
    function GetElapsedTimeAsSeconds: single;
    function GetSeconds: single;
    function GetTicks: cardinal;
    procedure SetOnTimer(const Value: TEventProc);
    procedure SetSeconds(const Value: single);
    function GetEnabled: boolean;
    procedure SetEnabled(const value: boolean);
    function GetInterval: cardinal;
    procedure SetInterval(const value: cardinal);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Restart;
    procedure Tick;

    property Enabled: boolean read GetEnabled write SetEnabled;
    property Interval: cardinal read GetInterval write SetInterval;
    property Seconds: single read GetSeconds write SetSeconds;
    property ElapsedTime: cardinal read GetElapsedTime;
    property ElapsedTimeAsSeconds: single read GetElapsedTimeAsSeconds;

    property OnTick: TEventProc read fOnTicks write SetOnTimer;
    property OnStartTick: TEventProc read fOnStartTicks write fOnStartTicks;
    property OnStopTick: TEventProc read fOnStopTicks write fOnStopTicks;
  end;

  // 获取程序每秒帧率
  TFrames  = class(TInterfacedObject)
  private
    fFpsTimer: TClock;
    fCapTimer: TClock;
    fCountedFrames: uint32;
    fFramerateLimit: integer;

    function GetAvgFPS: single;
    procedure SetFramerateLimit(Value: integer);

  public
    constructor Create;
    destructor Destroy; override;

    // 获取当前平均帧率
    property AvgFPS: single read GetAvgFPS;

    // 读取或设置帧率限制
    property FramerateLimit: integer read fFramerateLimit write SetFramerateLimit;
  end;

