{ TClock }

constructor TClock.Create;
begin
  _interval := 1000;
  _enabled := true;
  _timer := _interval;
  _startTime := __GetTicks;
end;

destructor TClock.Destroy;
begin
  _timer := 0;
  inherited Destroy;
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

constructor TFrames.Create;
begin
  _fpsTimer := TClock.Create;
  _capTimer := TClock.Create;
end;

destructor TFrames.Destroy;
begin
  if _fpsTimer <> nil then
    FreeAndNil(_fpsTimer);

  if _capTimer <> nil then
    FreeAndNil(_capTimer);

  inherited Destroy;
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

