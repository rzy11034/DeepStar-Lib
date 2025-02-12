type
  TCustomMixer = class abstract(TInterfacedObject)
  protected class var
    _MixerRefCount: integer;

  private
    procedure __MixerOpenAudio;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromFile(fileName: string); virtual; abstract;
    procedure Play; virtual; abstract;
  end;

  TMusic = class(TCustomMixer)
  private
    _Music: PMix_Music;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromFile(fileName: string); override;
    procedure Play; override;
  end;

  TChunk = class(TCustomMixer)
  private
    _Chunk: PMix_Chunk;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromFile(fileName: string); override;
    procedure Play; override;
  end;

