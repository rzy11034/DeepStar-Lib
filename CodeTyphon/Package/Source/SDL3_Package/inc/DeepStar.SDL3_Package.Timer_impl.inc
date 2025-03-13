{ TClock }

constructor TClock.Create;
begin
  fInterval := 1000;
  fEnabled := true;
  fTimer := fInterval;
  fStartTime := GetTicks;
end;

destructor TClock.Destroy;
begin
  fTimer := 0;
  inherited Destroy;
end;

procedure TClock.Restart;
begin
  Self.fStartTime := GetTicks;
end;

procedure TClock.Tick;
var
  time: cardinal;
begin
  if not fEnabled then Exit;

  time := Self.ElapsedTime;
  Restart;

  fTimer += time;

  if fTimer > fInterval then
  begin
    OnTick;
    fTimer := 0;
  end;
end;

function TClock.GetElapsedTime: cardinal;
begin
  Result := GetTicks - fStartTime;
end;

function TClock.GetElapsedTimeAsSeconds: single;
begin
  Result := (GetTicks - fStartTime) / 1000;
end;

function TClock.GetSeconds: single;
begin
  Result := fInterval / 1000;
end;

function TClock.GetTicks: cardinal;
begin
  Result := SDL_GetTicks();
end;

procedure TClock.SetOnTimer(const Value: TEventProc);
begin
  if fOnTicks = Value then Exit;
  fOnTicks := Value;
end;

procedure TClock.SetSeconds(const Value: single);
begin
  fInterval := trunc(Value * 1000);
end;

function TClock.GetEnabled: boolean;
begin
  Result := fEnabled;
end;

procedure TClock.SetEnabled(const value: boolean);
begin
  if fEnabled = value then Exit;
  fEnabled := value;
end;

function TClock.GetInterval: cardinal;
begin
  Result := Interval;
end;

procedure TClock.SetInterval(const value: cardinal);
begin
  if fInterval = value then Exit;
  fInterval := value;
end;

{ TFrames }

constructor TFrames.Create;
begin
  fFpsTimer := TClock.Create;
  fCapTimer := TClock.Create;
end;

destructor TFrames.Destroy;
begin
  if fFpsTimer <> nil then
    FreeAndNil(fFpsTimer);

  if fCapTimer <> nil then
    FreeAndNil(fCapTimer);

  inherited Destroy;
end;

function TFrames.GetAvgFPS: single;
var
  res: single;
begin
  res := single(0);
  res := fCountedFrames / (fFpsTimer.ElapsedTime / 1000);
  if res > 2000000 then
  begin
    res := 0;
  end;

  fCountedFrames += 1;

  Result := res;
end;

procedure TFrames.SetFramerateLimit(Value: integer);
var
  frameTicks, ScreenTicksPerFrame: integer;
begin
  if fFramerateLimit <> Value then
    fFramerateLimit := Value;

  ScreenTicksPerFrame := trunc(1000 / fFramerateLimit);

  // If frame finished early
  frameTicks := integer(0);
  frameTicks := fCapTimer.ElapsedTime;
  if frameTicks < ScreenTicksPerFrame then
  begin
    // Wait remaining time
    SDL_Delay(ScreenTicksPerFrame - frameTicks);
  end;

  fCapTimer.Restart;
end;

