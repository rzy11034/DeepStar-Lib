unit DeepStar.OpenGL.Camera;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM;

type
  TCamera_Movement = (FORWARD, BACKWARD, LEFT, RIGHT);

  TCamera = class(TObject)
  private
    _Yaw: GLfloat;
    _Pitch: GLfloat;
    _MovementSpeed: GLfloat;
    _MouseSensitivity: GLfloat;
    _Zoom: GLfloat;

    _Position: TVec3;
    _Front: TVec3;
    _Up: TVec3;
    _Right: TVec3;
    _WorldUp: TVec3;

    procedure __UpdateCameraVectors;

  public
    constructor Create;
    constructor Create(aposition: TVec3);
    constructor Create(aposition, aup: TVec3);
    constructor Create(posX, posY, posZ, upX, upY, upZ, aYaw, aPitch: GLfloat);
    destructor Destroy; override;

    function GetViewMatrix: TMat4;
    procedure ProcessKeyboard(direction: TCamera_Movement; deltaTime: GLfloat);
    procedure ProcessMouseMovement(xoffset, yoffset: GLfloat;
      constrainPitch: GLboolean = GLboolean(true));
    procedure ProcessMouseScroll(yoffset: GLfloat);

    property Zoom: GLfloat read _Zoom write _Zoom;
  end;


implementation

{ TCamera }

constructor TCamera.Create(posX, posY, posZ, upX, upY, upZ, aYaw, aPitch: GLfloat);
begin
  _Yaw := aYaw;
  _Pitch := aPitch;
  _MovementSpeed := 2.5;
  _MouseSensitivity := 0.1;
  _Zoom := 45;

  _Position := TGLM.Vec3(posX, posY, posZ);
  _WorldUp := TGLM.Vec3(upX, upY, upZ);
  _Front := TGLM.Vec3(0, 0, -1);

  __UpdateCameraVectors;
end;

constructor TCamera.Create(aposition, aup: TVec3);
begin
  _Yaw := -90.0;
  _Pitch := 0.0;
  _MovementSpeed := 2.5;
  _MouseSensitivity := 0.1;
  _Zoom := 45;

  _Position := aposition;
  _WorldUp := aup;
  _Front := TGLM.Vec3(0, 0, -1);

  __UpdateCameraVectors;
end;

constructor TCamera.Create(aposition: TVec3);
begin
  Create(aposition, TGLM.Vec3(0, 1, 0));
end;

constructor TCamera.Create;
begin
  Create(TGLM.Vec3(0, 0, 0), TGLM.Vec3(0, 1, 0));
end;

destructor TCamera.Destroy;
begin
  inherited Destroy;
end;

function TCamera.GetViewMatrix: TMat4;
begin
  Result := TGLM.LookAt(_Position, _Position + _Front, _Up);
end;

procedure TCamera.ProcessKeyboard(direction: TCamera_Movement; deltaTime: GLfloat);
var
  velocity: GLfloat;
begin
  velocity := _MovementSpeed * deltaTime;

  case direction of
    FORWARD: _Position += _Front * velocity;
    BACKWARD: _Position -= _Front * velocity;
    LEFT: _Position -= _Right * velocity;
    RIGHT: _Position += _Right * velocity;
  end;
end;

procedure TCamera.ProcessMouseMovement(xoffset, yoffset: GLfloat; constrainPitch: GLboolean);
begin
  xoffset *= _MouseSensitivity;
  yoffset *= _MouseSensitivity;

  _Yaw += xoffset;
  _Pitch += yoffset;

  // make sure that when _Pitch is out of bounds, screen doesn't get flipped
  if constrainPitch.ToBoolean then
  begin
    if _Pitch > 89 then _Pitch := 89;
    if _Pitch < -89 then _Pitch := -89;
  end;

  // update Front, Right and Up Vectors using the updated Euler angles
  __UpdateCameraVectors;
end;

procedure TCamera.ProcessMouseScroll(yoffset: GLfloat);
begin
  _Zoom -= yoffset;
  if _Zoom < 1 then
    _Zoom := 1;
  if _Zoom > 45 then
    _Zoom := 45;
end;

procedure TCamera.__UpdateCameraVectors;
var
  f: TVec3;
begin
  f := TGLM.Vec3(0, 0, 0);
  f.x := Cos(TGLM.Radians(_Yaw)) * Cos(TGLM.Radians(_Pitch));
  f.y := Sin(TGLM.Radians(_Pitch));
  f.z := Sin(TGLM.Radians(_Yaw)) * Cos(TGLM.Radians(_Pitch));
  _Front := TGLM.Normalize(f);

  // also re-calculate the Right and Up vector
  // normalize the vectors, because their length gets closer to 0 the more you
  // look up or down which results in slower movement.
  _Right := TGLM.Normalize(TGLM.Cross(_Front, _WorldUp));
  _Up := TGLM.Normalize(TGLM.Cross(_Right, _Front));
end;

end.
