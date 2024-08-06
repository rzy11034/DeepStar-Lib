unit DeepStar.OpenGL.Mesh;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.Shader;

const
  MAX_BONE_INFLUENCE = 4;

type
  TVertex = packed record
    Position: TVec3;
    Normal: TVec3;
    TexCoords: TVec2;
    Tangent: TVec3;
    Bitangent: TVec3;
    BoneIDs: array[0..MAX_BONE_INFLUENCE - 1] of integer;
    Weights: array[0..MAX_BONE_INFLUENCE - 1] of single;
  end;

  TTexture = packed record
    ID: cardinal;
    Type_: string;
    Path: string;
  end;

  TList_TVertex = specialize TArrayList<TVertex>;
  TList_Gluint = specialize TArrayList<GLuint>;
  TList_TTexture = specialize TArrayList<TTexture>;

type
  TMesh = class(TObject)
  private
    _ebo: GLuint;
    _vao: GLuint;
    _vbo: GLuint;
    _vertices: TList_TVertex;
    _indices: TList_Gluint;
    _textures: TList_TTexture;

    function __GetEBO: GLuint;
    function __GetIndices: TList_Gluint;
    function __GetTextures: TList_TTexture;
    function __GetVAO: GLuint;
    function __GetVBO: GLuint;
    function __GetVertices: TList_TVertex;

    // Set the vertex buffers and its attribute pointers
    procedure __SetMesh;

  public
    constructor Create(aVertices: TList_TVertex; aIndices: TList_Gluint; aTexture: TList_TTexture);
    destructor Destroy; override;

    // render the mesh
    procedure Draw(shader: TShaderProgram);

    property Vertices: TList_TVertex read __GetVertices;
    property Indices: TList_Gluint read __GetIndices;
    property Textures: TList_TTexture read __GetTextures;
    property VAO: GLuint read __GetVAO;
    property VBO: GLuint read __GetVBO;
    property EBO: GLuint read __GetEBO;
  end;

implementation

{ TMesh }

constructor TMesh.Create(aVertices: TList_TVertex; aIndices: TList_Gluint; aTexture: TList_TTexture);
begin
  inherited Create;

  Self._vertices := aVertices;
  Self._indices := aIndices;
  Self._textures := aTexture;

  __SetMesh;
end;

destructor TMesh.Destroy;
begin
  Self._vertices.Free;
  Self._indices.Free;
  Self._textures.Free;

  inherited Destroy;
end;

procedure TMesh.Draw(shader: TShaderProgram);
var
  diffuseNr, specularNr, normalNr, heightNr: GLuint;
  i: integer;
  name, number: string;
begin
  // bind appropriate textures
  diffuseNr  := GLuint(1);
  specularNr := GLuint(1);
  normalNr   := GLuint(1);
  heightNr   := GLuint(1);

  for i := 0 to _textures.Count - 1 do
  begin
    // active proper texture unit before binding
    glActiveTexture(GL_TEXTURE0 + i);

    // retrieve texture number (the N in diffuse_textureN)
    name := _textures[i].Type_;

    case name of
      'texture_diffuse':
      begin
        number := string(diffuseNr.ToString);
        diffuseNr += 1;
      end;

      'texture_specular':
      begin
        number := string(specularNr.ToString);
        specularNr += 1;
      end;

      'texture_normal':
      begin
        number := string(normalNr.ToString);
        normalNr += 1;
      end;

      'texture_height':
      begin
        number := string(heightNr.ToString);
        heightNr += 1;
      end;
    end;

    // now set the sampler to the correct texture unit
    glUniform1i(glGetUniformLocation(shader.ID, (Name + number).ToPAnsiChar), i);
    // and finally bind the texture
    glBindTexture(GL_TEXTURE_2D, _textures[i].ID);
  end;

  // draw mesh
  glBindVertexArray(_vao);
  glDrawElements(GL_TRIANGLES, _indices.Count, GL_UNSIGNED_INT, Pointer(0));
  glBindVertexArray(0);

  // always good practice to set everything back to defaults once configured.
  glActiveTexture(GL_TEXTURE0);
end;

function TMesh.__GetEBO: GLuint;
begin
  Result := _ebo;
end;

function TMesh.__GetIndices: TList_Gluint;
begin
  Result := _indices;
end;

function TMesh.__GetTextures: TList_TTexture;
begin
  Result := _textures;
end;

function TMesh.__GetVAO: GLuint;
begin
  Result := _vao;
end;

function TMesh.__GetVBO: GLuint;
begin
  Result := _vbo;
end;

function TMesh.__GetVertices: TList_TVertex;
begin
  Result := _vertices;
end;

procedure TMesh.__SetMesh;
const
  SIZE_VTX = SizeOf(TVertex);
  SIZE_C = SizeOf(cardinal);
var
  offsetPtr: Pointer;
  arrVTX: TList_TVertex.TArr;
  arrIDS: TList_Gluint.TArr;
begin
  // create buffers/arrays
  glGenVertexArrays(1, @_vao);
  glGenBuffers(1, @_vbo);
  glGenBuffers(1, @_ebo);

  glBindVertexArray(_vao);

  arrVTX := _vertices.ToArray;
  arrIDS := _indices.ToArray;

  // load data into vertex buffers
  glBindBuffer(GL_ARRAY_BUFFER, _vbo);
  glBufferData(GL_ARRAY_BUFFER, Length(arrVTX) * SIZE_VTX, @arrVTX[0], GL_STATIC_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ebo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(arrIDS) * SIZE_C, @arrIDS[0], GL_STATIC_DRAW);

  // set the vertex attribute pointers
  // Positions: TVec3;
  offsetPtr := Pointer(0) + OffsetOf(arrVTX[0], arrVTX[0].Position);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(TVertex), offsetPtr);

  // Normal: TVec3;
  offsetPtr := Pointer(0) + OffsetOf(arrVTX[0], arrVTX[0].Normal);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, SizeOf(TVertex), offsetPtr);

  // TexCoords: TVec2;
  offsetPtr := Pointer(0) + OffsetOf(arrVTX[0], arrVTX[0].TexCoords);
  glEnableVertexAttribArray(2);
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(TVertex), offsetPtr);

  // Tangent: TVec3;
  offsetPtr := Pointer(0) + OffsetOf(arrVTX[0], arrVTX[0].Tangent);
  glEnableVertexAttribArray(3);
  glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, SizeOf(TVertex), offsetPtr);

  // Bitangent: TVec3;
  offsetPtr := Pointer(0) + OffsetOf(arrVTX[0], arrVTX[0].Bitangent);
  glEnableVertexAttribArray(4);
  glVertexAttribPointer(4, 3, GL_FLOAT, GL_FALSE, SizeOf(TVertex), offsetPtr);

  // BoneIDs: array[0..MAX_BONE_INFLUENCE - 1] of integer;
  offsetPtr := Pointer(0) + OffsetOf(arrVTX[0], arrVTX[0].BoneIDs);
  glEnableVertexAttribArray(5);
  glVertexAttribPointer(5, 4, GL_INT, GL_FALSE, SizeOf(TVertex), offsetPtr);

  // Weights: array[0..MAX_BONE_INFLUENCE - 1] of single;
  offsetPtr := Pointer(0) + OffsetOf(arrVTX[0], arrVTX[0].Weights);
  glEnableVertexAttribArray(6);
  glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, SizeOf(TVertex), offsetPtr);
end;

end.
