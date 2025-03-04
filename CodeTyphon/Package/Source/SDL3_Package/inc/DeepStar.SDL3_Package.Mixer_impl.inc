{ TCustomMixer }

constructor TCustomMixer.Create;
begin
  if fMixerRefCount <= 0 then
  begin
    MixerOpenAudio;
  end;

  fMixerRefCount += 1;
end;

destructor TCustomMixer.Destroy;
begin
  fMixerRefCount -= 1;

  if fMixerRefCount <= 0 then
  begin
    Mix_CloseAudio;
    Mix_Quit;
  end;

  inherited Destroy;
end;

procedure TCustomMixer.MixerOpenAudio;
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
  fMusic := PMix_Music(nil);
end;

destructor TMusic.Destroy;
begin
  if fMusic <> nil then
  begin
    Mix_FreeMusic(fMusic);
    fMusic := nil;
  end;

  inherited Destroy;
end;

procedure TMusic.LoadFromFile(fileName: string);
var
  errStr: String;
begin
  fMusic := Mix_LoadMUS(CrossFixFileName(fileName).ToPAnsiChar);
  if fMusic = nil then
  begin
    errStr := string('Failed to load ' + fileName);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TMusic.Play;
begin
  Mix_PlayMusic(fMusic, -1);
end;

{ TChunk }

constructor TChunk.Create;
begin
  inherited Create;
  fChunk := PMix_Chunk(nil);
end;

destructor TChunk.Destroy;
begin
  if fChunk <> nil then
  begin
    Mix_FreeChunk(fChunk);
    fChunk := nil;
  end;

  inherited Destroy;
end;

procedure TChunk.LoadFromFile(fileName: string);
var
  errStr: String;
begin
  fChunk := Mix_LoadWAV(CrossFixFileName(fileName).ToPAnsiChar);
  if fChunk = nil then
  begin
    errStr := string('Failed to load ' + fileName);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TChunk.Play;
begin
  Mix_PlayChannel(-1, fChunk, 0);
end;


