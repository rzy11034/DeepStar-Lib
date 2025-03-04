type
  TCustomMixer = class abstract(TInterfacedObject)
  protected class var
    fMixerRefCount: integer;

  private
    procedure MixerOpenAudio;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromFile(fileName: string); virtual; abstract;
    procedure Play; virtual; abstract;
  end;

  TMusic = class(TCustomMixer)
  private
    fMusic: PMix_Music;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromFile(fileName: string); override;
    procedure Play; override;
  end;

  TChunk = class(TCustomMixer)
  private
    fChunk: PMix_Chunk;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromFile(fileName: string); override;
    procedure Play; override;
  end;

