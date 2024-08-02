unit DeepStar.OpenGL.Mesh;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.Assimp;

const
  MAX_BONE_INFLUENCE = 4;

type
  TVertex = record
    Position: TVec3;
    Normal: TVec3;
    TexCoords: TVec2;
    Tangent: TVec3;
    Bitangent: TVec3;
    BoneIDs: array[0..MAX_BONE_INFLUENCE - 1] of integer;
    Weights: array[0..MAX_BONE_INFLUENCE - 1] of single;
  end;

  TTexture = record
    ID: cardinal;
    Type_: string;
    Path: string;
  end;

  TVertexList = specialize TArrayList<TVertex>;
  TCardinalList = specialize TArrayList<GLuint>;
  TTextureList = specialize TArrayList<TTexture>;

type
  TMesh = class(TObject)
  private
    _vertices: TVertexList;
    _indices: TCardinalList;
    _textures: TTextureList;
    _vao: GLuint;

  public
    constructor Create(aVertices:TVertexList; aIndices: TCardinalList; aTexture: TTextureList);
    destructor Destroy; override;

  end;

implementation

{ TMesh }

constructor TMesh.Create(aVertices: TVertexList; aIndices: TCardinalList; aTexture: TTextureList);
begin
  Self._vertices := aVertices;
  Self._indices := aIndices;
  Self._textures := aTexture;
end;

destructor TMesh.Destroy;
begin
  inherited Destroy;
end;

end.
