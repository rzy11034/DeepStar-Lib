unit DeepStar.OpenGL.Shader;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Utils;

type
  TShaderTypes = (vertexObj, fragmentObj, programObj);

  TShaderProgram = class(TObject)
  private
    _id: GLuint;

    procedure __CheckShader(shaderID: GLubyte; shaderType: TShaderTypes);
    function __GetId: GLuint;

  public
    constructor Create();
    destructor Destroy; override;

    procedure LoadShaderFile(const vertexFile, fragmentFile: string);

    // 使用/激活程序
    procedure UseProgram;

    // uniform工具函数
    procedure SetUniformInt(uniform: string; Value: TArr_GLint);

    procedure SetUniformFloat(uniform: string; Value: TArr_GLfloat);
    procedure SetUniformFloat(uniform: string; Value: TVec3);
    procedure SetUniformFloat(uniform: string; Value: TVec4);

    procedure SetUniformMatrix4fv(uniform: string; const mat: TMat4);

    property ID: GLuint read __GetId;
  end;

implementation

{ TShaderProgram }

constructor TShaderProgram.Create();
begin
  inherited;
end;

destructor TShaderProgram.Destroy;
begin
  glDeleteProgram(_id);

  inherited Destroy;
end;

procedure TShaderProgram.LoadShaderFile(const vertexFile, fragmentFile: string);
var
  sl: TStringList;
  vertexStr, fragmentStr: PGLchar;
  vertexShader, fragmentShader, shaderProgram: GLuint;
begin
  sl := TStringList.Create();
  try
    try
      sl.LoadFromFile(CrossFixFileName(vertexFile).ToAnsiString);
    except
      raise Exception.Create('Vertex File load Error!');
    end;
    vertexStr := string('');
    vertexStr := string(sl.Text).ToPAnsiChar;

    sl.Clear;

    try
      sl.LoadFromFile(CrossFixFileName(fragmentFile).ToAnsiString);
    except
      raise Exception.Create('Fragment File load Error!');
    end;
    fragmentStr := string('');
    fragmentStr := string(sl.Text).ToPAnsiChar;
  finally
    sl.Free;
  end;

  vertexShader := GLuint(0);
  fragmentShader := GLuint(0);
  shaderProgram := GLuint(0);

  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertexShader, 1, @vertexStr, nil);
  glCompileShader(vertexShader);
  __CheckShader(vertexShader, TShaderTypes.vertexObj);

  fragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragmentShader, 1, @fragmentStr, nil);
  glCompileShader(fragmentShader);
  __CheckShader(fragmentShader, TShaderTypes.fragmentObj);

  shaderProgram := glCreateProgram();
  glAttachShader(shaderProgram, vertexShader);
  glAttachShader(shaderProgram, fragmentShader);
  glLinkProgram(shaderProgram);
  __CheckShader(shaderProgram, TShaderTypes.programObj);

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  _id := shaderProgram;
end;

procedure TShaderProgram.SetUniformFloat(uniform: string; Value: TArr_GLfloat);
var
  uniformLocation, len: GLint;
begin
  uniformLocation := GLint(0);
  uniformLocation := glGetUniformLocation(_id, uniform.ToPAnsiChar);

  len := GLint(0);
  len := Length(Value);
  if not (len in [1..4]) then
    raise Exception.Create('uniform is illegal!');

  case len of
    1: glUniform1f(uniformLocation, Value[0]);
    2: glUniform2f(uniformLocation, Value[0], Value[1]);
    3: glUniform3f(uniformLocation, Value[0], Value[1], Value[2]);
    4: glUniform4f(uniformLocation, Value[0], Value[1], Value[2], Value[3]);
  end;
end;

procedure TShaderProgram.SetUniformFloat(uniform: string; Value: TVec3);
begin
  SetUniformFloat(uniform, [Value.x, Value.y, Value.z]);
end;

procedure TShaderProgram.SetUniformFloat(uniform: string; Value: TVec4);
begin
  Self.SetUniformFloat(uniform, [Value.x, Value.y, Value.z, Value.w]);
end;

procedure TShaderProgram.SetUniformInt(uniform: string; Value: TArr_GLint);
var
  uniformLocation, len: GLint;
begin
  uniformLocation := GLint(0);
  uniformLocation := glGetUniformLocation(_id, uniform.ToPAnsiChar);

  len := GLint(0);
  len := Length(Value);
  if not (len in [1..4]) then
    raise Exception.Create('uniform is illegal!');

  case len of
    1: glUniform1i(uniformLocation, Value[0]);
    2: glUniform2i(uniformLocation, Value[0], Value[1]);
    3: glUniform3i(uniformLocation, Value[0], Value[1], Value[2]);
    4: glUniform4i(uniformLocation, Value[0], Value[1], Value[2], Value[3]);
  end;
end;

procedure TShaderProgram.SetUniformMatrix4fv(uniform: string; const mat: TMat4);
var
  uniformLocation: GLint;
begin
  uniformLocation := GLint(0);
  uniformLocation := glGetUniformLocation(_id, uniform.ToPAnsiChar);
  glUniformMatrix4fv(uniformLocation, 1, GL_FALSE, TGLM.ValuePtr(mat));
end;

procedure TShaderProgram.UseProgram;
begin
  glUseProgram(_id);
end;

procedure TShaderProgram.__CheckShader(shaderID: GLubyte; shaderType: TShaderTypes);
var
  success: GLint;
  infoLog: TArr_GLchar;
  s: string;
  strInfoLog: string;
begin
  success := GLint(false.ToInteger);
  infoLog := TArr_GLchar(nil);
  SetLength(infoLog, 512);
  case shaderType of
    vertexObj, fragmentObj:
    begin
      if shaderType = TShaderTypes.vertexObj then
        s := 'VERTEX'
      else
        s := 'FRAGMENT';

      glGetShaderiv(shaderID, GL_COMPILE_STATUS, @success);
      if not success.ToBoolean then
      begin
        glGetShaderInfoLog(shaderID, 512, nil, @infoLog[0]);
        strInfoLog := string(PAnsiChar(infoLog));
        WriteLn('ERROR::SHADER::' + s + '::COMPILATION_FAILED' + LE, strInfoLog);
      end;
    end;

    programObj:
    begin
      s := 'PROGRAM';

      glGetProgramiv(shaderID, GL_LINK_STATUS, @success);
      if not success.ToBoolean then
      begin
        glGetProgramInfoLog(shaderID, 512, nil, @infoLog[0]);
        strInfoLog := string(PAnsiChar(infoLog));
        WriteLn('ERROR::SHADER::' + s + '::COMPILATION_FAILED' + LE, strInfoLog);
      end;
    end;
  end;
end;

function TShaderProgram.__GetId: GLuint;
begin
  Result := _id;
end;

end.
