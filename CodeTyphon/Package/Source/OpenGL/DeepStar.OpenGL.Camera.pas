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

type
  TCamera = class(TObject)
  private const
    // Default camera values
    YAW = -90.0;
    PITCH = 0.0;
    SPEED = 2.5;
    SENSITIVITY = 0.1;
    ZOOM_ = 45.0;

  private
    _yaw: GLfloat;
    _pitch: GLfloat;
    _movementSpeed: GLfloat;
    _mouseSensitivity: GLfloat;
    _zoom: GLfloat;
    _position: TVec3;
    _front: TVec3;
    _up: TVec3;
    _right: TVec3;
    _worldUp: TVec3;

    function __GetZoom: GLfloat;
    procedure __SetZoom(const value: GLfloat);
    procedure __UpdateCameraVectors;

  public
    constructor Create;
    constructor Create(aPosition: TVec3);
    constructor Create(aPosition, aUp: TVec3);
    constructor Create(posX, posY, posZ, upX, upY, upZ: GLfloat; aYaw: GLfloat = YAW;
      aPitch: GLfloat = PITCH);
    destructor Destroy; override;

    function GetViewMatrix: TMat4;

    procedure ProcessKeyboard(direction: TCamera_Movement; deltaTime: GLfloat);
    procedure ProcessMouseMovement(xoffset, yoffset: GLfloat;
      constrainPitch: GLboolean = GLboolean(true));
    procedure ProcessMouseScroll(yoffset: GLfloat);

    property Zoom: GLfloat read __GetZoom write __SetZoom;
  end;


implementation

{ TCamera }

constructor TCamera.Create(posX, posY, posZ, upX, upY, upZ: GLfloat;
  aYaw: GLfloat; aPitch: GLfloat);
begin
  _yaw := aYaw;
  _pitch := aPitch;
  _movementSpeed := SPEED;
  _mouseSensitivity := SENSITIVITY;
  _zoom := ZOOM_;

  _position := TGLM.Vec3(posX, posY, posZ);
  _worldUp := TGLM.Vec3(upX, upY, upZ);
  _front := TGLM.Vec3(0, 0, -1);

  __UpdateCameraVectors;
end;

constructor TCamera.Create(aPosition, aUp: TVec3);
begin
  Create(aPosition.x, aPosition.y, aPosition.z, aUp.x, aUp.y, aUp.z);
end;

constructor TCamera.Create(aPosition: TVec3);
begin
  Create(aPosition, TGLM.Vec3(0, 1, 0));
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
  Result := TGLM.LookAt(_position, _position + _front, _up);
end;

procedure TCamera.ProcessKeyboard(direction: TCamera_Movement; deltaTime: GLfloat);
var
  velocity: GLfloat;
begin
  velocity := _movementSpeed * deltaTime;

  case direction of
    FORWARD: _position += _front * velocity;
    BACKWARD: _position -= _front * velocity;
    LEFT: _position -= _right * velocity;
    RIGHT: _position += _right * velocity;
  end;
end;

procedure TCamera.ProcessMouseMovement(xoffset, yoffset: GLfloat; constrainPitch: GLboolean);
begin
  xoffset *= _mouseSensitivity;
  yoffset *= _mouseSensitivity;

  _yaw += xoffset;
  _pitch += yoffset;

  // make sure that when _pitch is out of bounds, screen doesn't get flipped
  if constrainPitch.ToBoolean then
  begin
    if _pitch > 89 then _pitch := 89;
    if _pitch < -89 then _pitch := -89;
  end;

  // update Front, Right and Up Vectors using the updated Euler angles
  __UpdateCameraVectors;
end;

procedure TCamera.ProcessMouseScroll(yoffset: GLfloat);
begin
  _zoom -= yoffset;
  if _zoom < 1 then
    _zoom := 1;
  if _zoom > 45 then
    _zoom := 45;
end;

function TCamera.__GetZoom: GLfloat;
begin
  Result := _zoom;
end;

procedure TCamera.__SetZoom(const value: GLfloat);
begin
  _zoom := value;
end;

procedure TCamera.__UpdateCameraVectors;
var
  f: TVec3;
begin
  f := TGLM.Vec3(0, 0, 0);
  f.x := Cos(TGLM.Radians(_yaw)) * Cos(TGLM.Radians(_pitch));
  f.y := Sin(TGLM.Radians(_pitch));
  f.z := Sin(TGLM.Radians(_yaw)) * Cos(TGLM.Radians(_pitch));
  _front := TGLM.Normalize(f);

  // also re-calculate the Right and Up vector
  // normalize the vectors, because their length gets closer to 0 the more you
  // look up or down which results in slower movement.
  _right := TGLM.Normalize(TGLM.Cross(_front, _worldUp));
  _up := TGLM.Normalize(TGLM.Cross(_right, _front));
end;

end.
