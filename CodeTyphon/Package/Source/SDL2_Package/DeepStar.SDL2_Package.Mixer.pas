unit DeepStar.SDL2_Package.Mixer;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  libSDL2,
  libSDL2_mixer;

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

implementation

{ TCustomMixer }

constructor TCustomMixer.Create;
begin
  if _MixerRefCount <= 0 then
  begin
    __MixerOpenAudio;
  end;

  _MixerRefCount += 1;
end;

destructor TCustomMixer.Destroy;
begin
  _MixerRefCount -= 1;

  if _MixerRefCount <= 0 then
  begin
    Mix_CloseAudio;
    Mix_Quit;
  end;

  inherited Destroy;
end;

procedure TCustomMixer.__MixerOpenAudio;
var
  errStr: String;
begin
  // Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048)
  if Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT,
    MIX_DEFAULT_CHANNELS, 2048) < 0 then
  begin
    errStr := '';
    errStr := 'SDL_mixer could not initialize! SDL_mixer: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

{ TMusic }

constructor TMusic.Create;
begin
  inherited Create;
  _Music := PMix_Music(nil);
end;

destructor TMusic.Destroy;
begin
  if _Music <> nil then
  begin
    Mix_FreeMusic(_Music);
    _Music := nil;
  end;

  inherited Destroy;
end;

procedure TMusic.LoadFromFile(fileName: string);
var
  errStr: String;
begin
  _Music := Mix_LoadMUS(CrossFixFileName(fileName).ToPAnsiChar);
  if _Music = nil then
  begin
    errStr := string('Failed to load ' + fileName);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TMusic.Play;
begin
  Mix_PlayMusic(_Music, -1);
end;

{ TChunk }

constructor TChunk.Create;
begin
  inherited Create;
  _Chunk := PMix_Chunk(nil);
end;

destructor TChunk.Destroy;
begin
  if _Chunk <> nil then
  begin
    Mix_FreeChunk(_Chunk);
    _Chunk := nil;
  end;

  inherited Destroy;
end;

procedure TChunk.LoadFromFile(fileName: string);
var
  errStr: String;
begin
  _Chunk := Mix_LoadWAV(CrossFixFileName(fileName).ToPAnsiChar);
  if _Chunk = nil then
  begin
    errStr := string('Failed to load ' + fileName);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TChunk.Play;
begin
  Mix_PlayChannel(-1, _Chunk, 0);
end;

end.

