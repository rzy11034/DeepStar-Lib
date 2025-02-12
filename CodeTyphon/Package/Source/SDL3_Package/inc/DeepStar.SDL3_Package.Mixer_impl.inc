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
  spec: TSDL_AudioSpec;
begin
  spec := Default(TSDL_AudioSpec);
  spec.format := MIX_DEFAULT_FORMAT;
  spec.channels := MIX_DEFAULT_CHANNELS;
  spec.freq := MIX_DEFAULT_FREQUENCY;

  if Mix_OpenAudio(0, @spec) < 0 then
  begin
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


