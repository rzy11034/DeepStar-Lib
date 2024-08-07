﻿//The macros are now a big enumeration
//handled by the implementation of aiGetMaterial*

const
// ---------------------------------------------------------------------------
  AI_MATKEY_NAME = 0;
  AI_MATKEY_TWOSIDED = 1;
  AI_MATKEY_SHADING_MODEL = 2;
  AI_MATKEY_ENABLE_WIREFRAME = 3;
  AI_MATKEY_BLEND_FUNC = 4;
  AI_MATKEY_OPACITY = 5;
  AI_MATKEY_BUMPSCALING = 6;
  AI_MATKEY_SHININESS = 7;
  AI_MATKEY_REFLECTIVITY = 8;
  AI_MATKEY_SHININESS_STRENGTH = 9;
  AI_MATKEY_REFRACTI = 10;
  AI_MATKEY_COLOR_DIFFUSE = 11;
  AI_MATKEY_COLOR_AMBIENT = 12;
  AI_MATKEY_COLOR_SPECULAR = 13;
  AI_MATKEY_COLOR_EMISSIVE = 14;
  AI_MATKEY_COLOR_TRANSPARENT = 15;
  AI_MATKEY_COLOR_REFLECTIVE = 16;
  AI_MATKEY_GLOBAL_BACKGROUND_IMAGE = 17;

type
  TaiMatKey = Byte;

const
  AI_MATKEY_TEXTURE_DIFFUSE = 0;
  AI_MATKEY_TEXTURE_SPECULAR = 1;
  AI_MATKEY_TEXTURE_AMBIENT = 2;
  AI_MATKEY_TEXTURE_EMISSIVE = 3;
  AI_MATKEY_TEXTURE_NORMALS = 4;
  AI_MATKEY_TEXTURE_HEIGHT = 5;
  AI_MATKEY_TEXTURE_SHININESS = 6;
  AI_MATKEY_TEXTURE_OPACITY = 7;
  AI_MATKEY_TEXTURE_DISPLACEMENT = 8;
  AI_MATKEY_TEXTURE_LIGHTMAP = 9;
  AI_MATKEY_TEXTURE_REFLECTION = 10;
  AI_MATKEY_UVWSRC_DIFFUSE = 11;
  AI_MATKEY_UVWSRC_SPECULAR = 12;
  AI_MATKEY_UVWSRC_AMBIENT = 13;
  AI_MATKEY_UVWSRC_EMISSIVE = 14;
  AI_MATKEY_UVWSRC_NORMALS = 15;
  AI_MATKEY_UVWSRC_HEIGHT = 16;
  AI_MATKEY_UVWSRC_SHININESS = 17;
  AI_MATKEY_UVWSRC_OPACITY = 18;
  AI_MATKEY_UVWSRC_DISPLACEMENT = 19;
  AI_MATKEY_UVWSRC_LIGHTMAP = 20;
  AI_MATKEY_UVWSRC_REFLECTION = 21;
  AI_MATKEY_TEXOP_DIFFUSE = 22;
  AI_MATKEY_TEXOP_SPECULAR = 23;
  AI_MATKEY_TEXOP_AMBIENT = 24;
  AI_MATKEY_TEXOP_EMISSIVE = 25;
  AI_MATKEY_TEXOP_NORMALS = 26;
  AI_MATKEY_TEXOP_HEIGHT = 27;
  AI_MATKEY_TEXOP_SHININESS = 28;
  AI_MATKEY_TEXOP_OPACITY = 29;
  AI_MATKEY_TEXOP_DISPLACEMENT = 30;
  AI_MATKEY_TEXOP_LIGHTMAP = 31;
  AI_MATKEY_TEXOP_REFLECTION = 32;
  AI_MATKEY_MAPPING_DIFFUSE = 33;
  AI_MATKEY_MAPPING_SPECULAR = 34;
  AI_MATKEY_MAPPING_AMBIENT = 35;
  AI_MATKEY_MAPPING_EMISSIVE = 36;
  AI_MATKEY_MAPPING_NORMALS = 37;
  AI_MATKEY_MAPPING_HEIGHT = 38;
  AI_MATKEY_MAPPING_SHININESS = 39;
  AI_MATKEY_MAPPING_OPACITY = 40;
  AI_MATKEY_MAPPING_DISPLACEMENT = 41;
  AI_MATKEY_MAPPING_LIGHTMAP = 42;
  AI_MATKEY_MAPPING_REFLECTION = 43;
  AI_MATKEY_TEXBLEND_DIFFUSE = 44;
  AI_MATKEY_TEXBLEND_SPECULAR = 45;
  AI_MATKEY_TEXBLEND_AMBIENT = 46;
  AI_MATKEY_TEXBLEND_EMISSIVE = 47;
  AI_MATKEY_TEXBLEND_NORMALS = 48;
  AI_MATKEY_TEXBLEND_HEIGHT = 49;
  AI_MATKEY_TEXBLEND_SHININESS = 50;
  AI_MATKEY_TEXBLEND_OPACITY = 51;
  AI_MATKEY_TEXBLEND_DISPLACEMENT = 52;
  AI_MATKEY_TEXBLEND_LIGHTMAP = 53;
  AI_MATKEY_TEXBLEND_REFLECTION = 54;
  AI_MATKEY_MAPPINGMODE_U_DIFFUSE = 55;
  AI_MATKEY_MAPPINGMODE_U_SPECULAR = 56;
  AI_MATKEY_MAPPINGMODE_U_AMBIENT = 57;
  AI_MATKEY_MAPPINGMODE_U_EMISSIVE = 58;
  AI_MATKEY_MAPPINGMODE_U_NORMALS = 59;
  AI_MATKEY_MAPPINGMODE_U_HEIGHT = 60;
  AI_MATKEY_MAPPINGMODE_U_SHININESS = 61;
  AI_MATKEY_MAPPINGMODE_U_OPACITY = 62;
  AI_MATKEY_MAPPINGMODE_U_DISPLACEMENT = 63;
  AI_MATKEY_MAPPINGMODE_U_LIGHTMAP = 64;
  AI_MATKEY_MAPPINGMODE_U_REFLECTION = 65;
  AI_MATKEY_MAPPINGMODE_V_DIFFUSE = 66;
  AI_MATKEY_MAPPINGMODE_V_SPECULAR = 67;
  AI_MATKEY_MAPPINGMODE_V_AMBIENT = 68;
  AI_MATKEY_MAPPINGMODE_V_EMISSIVE = 69;
  AI_MATKEY_MAPPINGMODE_V_NORMALS = 70;
  AI_MATKEY_MAPPINGMODE_V_HEIGHT = 71;
  AI_MATKEY_MAPPINGMODE_V_SHININESS = 72;
  AI_MATKEY_MAPPINGMODE_V_OPACITY = 73;
  AI_MATKEY_MAPPINGMODE_V_DISPLACEMENT = 74;
  AI_MATKEY_MAPPINGMODE_V_LIGHTMAP = 75;
  AI_MATKEY_MAPPINGMODE_V_REFLECTION = 76;
  AI_MATKEY_TEXMAP_AXIS_DIFFUSE = 77;
  AI_MATKEY_TEXMAP_AXIS_SPECULAR = 78;
  AI_MATKEY_TEXMAP_AXIS_AMBIENT = 79;
  AI_MATKEY_TEXMAP_AXIS_EMISSIVE = 80;
  AI_MATKEY_TEXMAP_AXIS_NORMALS = 81;
  AI_MATKEY_TEXMAP_AXIS_HEIGHT = 82;
  AI_MATKEY_TEXMAP_AXIS_SHININESS = 83;
  AI_MATKEY_TEXMAP_AXIS_OPACITY = 84;
  AI_MATKEY_TEXMAP_AXIS_DISPLACEMENT = 85;
  AI_MATKEY_TEXMAP_AXIS_LIGHTMAP = 86;
  AI_MATKEY_TEXMAP_AXIS_REFLECTION = 87;
  AI_MATKEY_UVTRANSFORM_DIFFUSE = 88;
  AI_MATKEY_UVTRANSFORM_SPECULAR = 89;
  AI_MATKEY_UVTRANSFORM_AMBIENT = 90;
  AI_MATKEY_UVTRANSFORM_EMISSIVE = 91;
  AI_MATKEY_UVTRANSFORM_NORMALS = 92;
  AI_MATKEY_UVTRANSFORM_HEIGHT = 93;
  AI_MATKEY_UVTRANSFORM_SHININESS = 94;
  AI_MATKEY_UVTRANSFORM_OPACITY = 95;
  AI_MATKEY_UVTRANSFORM_DISPLACEMENT = 96;
  AI_MATKEY_UVTRANSFORM_LIGHTMAP = 97;
  AI_MATKEY_UVTRANSFORM_REFLECTION = 98;
  AI_MATKEY_TEXFLAGS_DIFFUSE = 99;
  AI_MATKEY_TEXFLAGS_SPECULAR = 100;
  AI_MATKEY_TEXFLAGS_AMBIENT = 101;
  AI_MATKEY_TEXFLAGS_EMISSIVE = 102;
  AI_MATKEY_TEXFLAGS_NORMALS = 103;
  AI_MATKEY_TEXFLAGS_HEIGHT = 104;
  AI_MATKEY_TEXFLAGS_SHININESS = 105;
  AI_MATKEY_TEXFLAGS_OPACITY = 106;
  AI_MATKEY_TEXFLAGS_DISPLACEMENT = 107;
  AI_MATKEY_TEXFLAGS_LIGHTMAP = 108;
  AI_MATKEY_TEXFLAGS_REFLECTION = 109;

type
  TaiTextureMatKey = Byte;

{
//not translated yet

// ---------------------------------------------------------------------------
#define AI_MATKEY_TEXTURE(type, N) _AI_MATKEY_TEXTURE_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_TEXTURE_DIFFUSE(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_TEXTURE_SPECULAR(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_SPECULAR,N)

#define AI_MATKEY_TEXTURE_AMBIENT(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_AMBIENT,N)

#define AI_MATKEY_TEXTURE_EMISSIVE(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_TEXTURE_NORMALS(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_NORMALS,N)

#define AI_MATKEY_TEXTURE_HEIGHT(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_HEIGHT,N)

#define AI_MATKEY_TEXTURE_SHININESS(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_SHININESS,N)

#define AI_MATKEY_TEXTURE_OPACITY(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_OPACITY,N)

#define AI_MATKEY_TEXTURE_DISPLACEMENT(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_TEXTURE_LIGHTMAP(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_TEXTURE_REFLECTION(N)	\
	AI_MATKEY_TEXTURE(aiTextureType_REFLECTION,N)

//! @endcond

// ---------------------------------------------------------------------------
#define AI_MATKEY_UVWSRC(type, N) _AI_MATKEY_UVWSRC_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_UVWSRC_DIFFUSE(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_UVWSRC_SPECULAR(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_SPECULAR,N)

#define AI_MATKEY_UVWSRC_AMBIENT(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_AMBIENT,N)

#define AI_MATKEY_UVWSRC_EMISSIVE(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_UVWSRC_NORMALS(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_NORMALS,N)

#define AI_MATKEY_UVWSRC_HEIGHT(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_HEIGHT,N)

#define AI_MATKEY_UVWSRC_SHININESS(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_SHININESS,N)

#define AI_MATKEY_UVWSRC_OPACITY(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_OPACITY,N)

#define AI_MATKEY_UVWSRC_DISPLACEMENT(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_UVWSRC_LIGHTMAP(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_UVWSRC_REFLECTION(N)	\
	AI_MATKEY_UVWSRC(aiTextureType_REFLECTION,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_TEXOP(type, N) _AI_MATKEY_TEXOP_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_TEXOP_DIFFUSE(N)	\
	AI_MATKEY_TEXOP(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_TEXOP_SPECULAR(N)	\
	AI_MATKEY_TEXOP(aiTextureType_SPECULAR,N)

#define AI_MATKEY_TEXOP_AMBIENT(N)	\
	AI_MATKEY_TEXOP(aiTextureType_AMBIENT,N)

#define AI_MATKEY_TEXOP_EMISSIVE(N)	\
	AI_MATKEY_TEXOP(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_TEXOP_NORMALS(N)	\
	AI_MATKEY_TEXOP(aiTextureType_NORMALS,N)

#define AI_MATKEY_TEXOP_HEIGHT(N)	\
	AI_MATKEY_TEXOP(aiTextureType_HEIGHT,N)

#define AI_MATKEY_TEXOP_SHININESS(N)	\
	AI_MATKEY_TEXOP(aiTextureType_SHININESS,N)

#define AI_MATKEY_TEXOP_OPACITY(N)	\
	AI_MATKEY_TEXOP(aiTextureType_OPACITY,N)

#define AI_MATKEY_TEXOP_DISPLACEMENT(N)	\
	AI_MATKEY_TEXOP(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_TEXOP_LIGHTMAP(N)	\
	AI_MATKEY_TEXOP(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_TEXOP_REFLECTION(N)	\
	AI_MATKEY_TEXOP(aiTextureType_REFLECTION,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_MAPPING(type, N) _AI_MATKEY_MAPPING_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_MAPPING_DIFFUSE(N)	\
	AI_MATKEY_MAPPING(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_MAPPING_SPECULAR(N)	\
	AI_MATKEY_MAPPING(aiTextureType_SPECULAR,N)

#define AI_MATKEY_MAPPING_AMBIENT(N)	\
	AI_MATKEY_MAPPING(aiTextureType_AMBIENT,N)

#define AI_MATKEY_MAPPING_EMISSIVE(N)	\
	AI_MATKEY_MAPPING(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_MAPPING_NORMALS(N)	\
	AI_MATKEY_MAPPING(aiTextureType_NORMALS,N)

#define AI_MATKEY_MAPPING_HEIGHT(N)	\
	AI_MATKEY_MAPPING(aiTextureType_HEIGHT,N)

#define AI_MATKEY_MAPPING_SHININESS(N)	\
	AI_MATKEY_MAPPING(aiTextureType_SHININESS,N)

#define AI_MATKEY_MAPPING_OPACITY(N)	\
	AI_MATKEY_MAPPING(aiTextureType_OPACITY,N)

#define AI_MATKEY_MAPPING_DISPLACEMENT(N)	\
	AI_MATKEY_MAPPING(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_MAPPING_LIGHTMAP(N)	\
	AI_MATKEY_MAPPING(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_MAPPING_REFLECTION(N)	\
	AI_MATKEY_MAPPING(aiTextureType_REFLECTION,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_TEXBLEND(type, N) _AI_MATKEY_TEXBLEND_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_TEXBLEND_DIFFUSE(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_TEXBLEND_SPECULAR(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_SPECULAR,N)

#define AI_MATKEY_TEXBLEND_AMBIENT(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_AMBIENT,N)

#define AI_MATKEY_TEXBLEND_EMISSIVE(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_TEXBLEND_NORMALS(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_NORMALS,N)

#define AI_MATKEY_TEXBLEND_HEIGHT(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_HEIGHT,N)

#define AI_MATKEY_TEXBLEND_SHININESS(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_SHININESS,N)

#define AI_MATKEY_TEXBLEND_OPACITY(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_OPACITY,N)

#define AI_MATKEY_TEXBLEND_DISPLACEMENT(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_TEXBLEND_LIGHTMAP(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_TEXBLEND_REFLECTION(N)	\
	AI_MATKEY_TEXBLEND(aiTextureType_REFLECTION,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_MAPPINGMODE_U(type, N) _AI_MATKEY_MAPPINGMODE_U_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_MAPPINGMODE_U_DIFFUSE(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_MAPPINGMODE_U_SPECULAR(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_SPECULAR,N)

#define AI_MATKEY_MAPPINGMODE_U_AMBIENT(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_AMBIENT,N)

#define AI_MATKEY_MAPPINGMODE_U_EMISSIVE(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_MAPPINGMODE_U_NORMALS(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_NORMALS,N)

#define AI_MATKEY_MAPPINGMODE_U_HEIGHT(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_HEIGHT,N)

#define AI_MATKEY_MAPPINGMODE_U_SHININESS(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_SHININESS,N)

#define AI_MATKEY_MAPPINGMODE_U_OPACITY(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_OPACITY,N)

#define AI_MATKEY_MAPPINGMODE_U_DISPLACEMENT(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_MAPPINGMODE_U_LIGHTMAP(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_MAPPINGMODE_U_REFLECTION(N)	\
	AI_MATKEY_MAPPINGMODE_U(aiTextureType_REFLECTION,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_MAPPINGMODE_V(type, N) _AI_MATKEY_MAPPINGMODE_V_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_MAPPINGMODE_V_DIFFUSE(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_MAPPINGMODE_V_SPECULAR(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_SPECULAR,N)

#define AI_MATKEY_MAPPINGMODE_V_AMBIENT(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_AMBIENT,N)

#define AI_MATKEY_MAPPINGMODE_V_EMISSIVE(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_MAPPINGMODE_V_NORMALS(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_NORMALS,N)

#define AI_MATKEY_MAPPINGMODE_V_HEIGHT(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_HEIGHT,N)

#define AI_MATKEY_MAPPINGMODE_V_SHININESS(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_SHININESS,N)

#define AI_MATKEY_MAPPINGMODE_V_OPACITY(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_OPACITY,N)

#define AI_MATKEY_MAPPINGMODE_V_DISPLACEMENT(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_MAPPINGMODE_V_LIGHTMAP(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_MAPPINGMODE_V_REFLECTION(N)	\
	AI_MATKEY_MAPPINGMODE_V(aiTextureType_REFLECTION,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_TEXMAP_AXIS(type, N) _AI_MATKEY_TEXMAP_AXIS_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_TEXMAP_AXIS_DIFFUSE(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_TEXMAP_AXIS_SPECULAR(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_SPECULAR,N)

#define AI_MATKEY_TEXMAP_AXIS_AMBIENT(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_AMBIENT,N)

#define AI_MATKEY_TEXMAP_AXIS_EMISSIVE(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_TEXMAP_AXIS_NORMALS(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_NORMALS,N)

#define AI_MATKEY_TEXMAP_AXIS_HEIGHT(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_HEIGHT,N)

#define AI_MATKEY_TEXMAP_AXIS_SHININESS(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_SHININESS,N)

#define AI_MATKEY_TEXMAP_AXIS_OPACITY(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_OPACITY,N)

#define AI_MATKEY_TEXMAP_AXIS_DISPLACEMENT(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_TEXMAP_AXIS_LIGHTMAP(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_TEXMAP_AXIS_REFLECTION(N)	\
	AI_MATKEY_TEXMAP_AXIS(aiTextureType_REFLECTION,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_UVTRANSFORM(type, N) _AI_MATKEY_UVTRANSFORM_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_UVTRANSFORM_DIFFUSE(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_UVTRANSFORM_SPECULAR(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_SPECULAR,N)

#define AI_MATKEY_UVTRANSFORM_AMBIENT(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_AMBIENT,N)

#define AI_MATKEY_UVTRANSFORM_EMISSIVE(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_UVTRANSFORM_NORMALS(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_NORMALS,N)

#define AI_MATKEY_UVTRANSFORM_HEIGHT(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_HEIGHT,N)

#define AI_MATKEY_UVTRANSFORM_SHININESS(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_SHININESS,N)

#define AI_MATKEY_UVTRANSFORM_OPACITY(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_OPACITY,N)

#define AI_MATKEY_UVTRANSFORM_DISPLACEMENT(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_UVTRANSFORM_LIGHTMAP(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_UVTRANSFORM_REFLECTION(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_REFLECTION,N)

#define AI_MATKEY_UVTRANSFORM_UNKNOWN(N)	\
	AI_MATKEY_UVTRANSFORM(aiTextureType_UNKNOWN,N)

//! @endcond
// ---------------------------------------------------------------------------
#define AI_MATKEY_TEXFLAGS(type, N) _AI_MATKEY_TEXFLAGS_BASE,type,N

// For backward compatibility and simplicity
//! @cond MATS_DOC_FULL
#define AI_MATKEY_TEXFLAGS_DIFFUSE(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_DIFFUSE,N)

#define AI_MATKEY_TEXFLAGS_SPECULAR(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_SPECULAR,N)

#define AI_MATKEY_TEXFLAGS_AMBIENT(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_AMBIENT,N)

#define AI_MATKEY_TEXFLAGS_EMISSIVE(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_EMISSIVE,N)

#define AI_MATKEY_TEXFLAGS_NORMALS(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_NORMALS,N)

#define AI_MATKEY_TEXFLAGS_HEIGHT(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_HEIGHT,N)

#define AI_MATKEY_TEXFLAGS_SHININESS(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_SHININESS,N)

#define AI_MATKEY_TEXFLAGS_OPACITY(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_OPACITY,N)

#define AI_MATKEY_TEXFLAGS_DISPLACEMENT(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_DISPLACEMENT,N)

#define AI_MATKEY_TEXFLAGS_LIGHTMAP(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_LIGHTMAP,N)

#define AI_MATKEY_TEXFLAGS_REFLECTION(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_REFLECTION,N)

#define AI_MATKEY_TEXFLAGS_UNKNOWN(N)	\
	AI_MATKEY_TEXFLAGS(aiTextureType_UNKNOWN,N)
}
