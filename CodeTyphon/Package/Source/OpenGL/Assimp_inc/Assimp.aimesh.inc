﻿//from mesh.h

// ---------------------------------------------------------------------------
// Limits. These values are required to match the settings Assimp was
// compiled against. Therfore, do not redefine them unless you build the
// library from source using the same definitions.
// ---------------------------------------------------------------------------

const
  {** @def AI_MAX_FACE_INDICES
   *  Maximum number of indices per face (polygon). *}
  AI_MAX_FACE_INDICES = $7fff;

  {** @def AI_MAX_BONE_WEIGHTS
   *  Maximum number of indices per face (polygon). *}
  AI_MAX_BONE_WEIGHTS = $7fffffff;

  {** @def AI_MAX_VERTICES
   *  Maximum number of vertices per mesh.  *}
  AI_MAX_VERTICES = $7fffffff;

  {** @def AI_MAX_FACES
   *  Maximum number of faces per mesh. *}
  AI_MAX_FACES = $7fffffff;

  {** @def AI_MAX_NUMBER_OF_COLOR_SETS
   *  Supported number of vertex color sets per mesh. *}
  AI_MAX_NUMBER_OF_COLOR_SETS = $8;

  {** @def AI_MAX_NUMBER_OF_TEXTURECOORDS
   *  Supported number of texture coord sets (UV(W) channels) per mesh *}
  AI_MAX_NUMBER_OF_TEXTURECOORDS = $8;

// ---------------------------------------------------------------------------
  {** @brief A single face in a mesh, referring to multiple vertices.
   *
   * If mNumIndices is 3, we call the face 'triangle', for mNumIndices > 3
   * it's called 'polygon' (hey, that's just a definition!).
   * <br>
   * aiMesh::mPrimitiveTypes can be queried to quickly examine which types of
   * primitive are actually present in a mesh. The #aiProcess_SortByPType flag
   * executes a special post-processing algorithm which splits meshes with
   * *different* primitive types mixed up (e.g. lines and triangles) in several
   * 'clean' submeshes. Furthermore there is a configuration option (
   * #AI_CONFIG_PP_SBP_REMOVE) to force #aiProcess_SortByPType to remove
   * specific kinds of primitives from the imported scene, completely and forever.
   * In many cases you'll probably want to set this setting to
   * @code
   * aiPrimitiveType_LINE|aiPrimitiveType_POINT
   * @endcode
   * Together with the #aiProcess_Triangulate flag you can then be sure that
   * #aiFace::mNumIndices is always 3.
   * @note Take a look at the @link data Data Structures page @endlink for
   * more information on the layout and winding order of a face.
   *}
type
  TaiFace = record
    //! Number of indices defining this face.
    //! The maximum value for this member is #AI_MAX_FACE_INDICES.
    mNumIndices: CUInt;

    //! Pointer to the indices array. Size of the array is given in numIndices.
    mIndices: PCUInt;
  end;
  PaiFace = ^TaiFace;
  PPaiFace = ^PaiFace;

// ---------------------------------------------------------------------------
  {** @brief A single influence of a bone on a vertex. *}
  TaiVertexWeight = record
    //! Index of the vertex which is influenced by the bone.
    mVertexId: CUInt;

    //! The strength of the influence in the range (0...1).
    //! The influence from all bones at one vertex amounts to 1.
    mWeight: Single;
  end;
  PaiVertexWeight = ^TaiVertexWeight;
  PPaiVertexWeight = ^PaiVertexWeight;

// ---------------------------------------------------------------------------
  {** @brief A single bone of a mesh.
   *
   *  A bone has a name by which it can be found in the frame hierarchy and by
   *  which it can be addressed by animations. In addition it has a number of
   *  influences on vertices.
   *}
  TaiBone = record
    //! The name of the bone.
    mName: TaiString;

    //! The number of vertices affected by this bone
    //! The maximum value for this member is #AI_MAX_BONE_WEIGHTS.
    mNumWeights: CUInt;

    //! The vertices affected by this bone
    mWeights: PaiVertexWeight;

    //! Matrix that transforms from mesh space to bone space in bind pose
    mOffsetMatrix: TaiMatrix4x4;
  end;
  PaiBone = ^TaiBone;
  PPaiBone = ^PaiBone;

// ---------------------------------------------------------------------------
  {** @brief Enumerates the types of geometric primitives supported by Assimp.
   *
   *  @see aiFace Face data structure
   *  @see aiProcess_SortByPType Per-primitive sorting of meshes
   *  @see aiProcess_Triangulate Automatic triangulation
   *  @see AI_CONFIG_PP_SBP_REMOVE Removal of specific primitive types.
   *}
const
  {** A point primitive.
   *
   * This is just a single vertex in the virtual world,
   * #aiFace contains just one index for such a primitive.
   *}
  aiPrimitiveType_POINT       = $1;

  {** A line primitive.
   *
   * This is a line defined through a start and an end position.
   * #aiFace contains exactly two indices for such a primitive.
   *}
  aiPrimitiveType_LINE        = $2;

  {** A triangular primitive.
   *
   * A triangle consists of three indices.
   *}
  aiPrimitiveType_TRIANGLE    = $4;

  {** A higher-level polygon with more than 3 edges.
   *
   * A triangle is a polygon, but polygon in this context means
   * "all polygons that are not triangles". The "Triangulate"-Step
   * is provided for your convenience, it splits all polygons in
   * triangles (which are much easier to handle).
   *}
  aiPrimitiveType_POLYGON     = $8;

type
  TaiPrimitiveType = Integer;

// Get the #aiPrimitiveType flag for a specific number of face indices
//#define AI_PRIMITIVE_TYPE_FOR_N_INDICES(n) \
//	((n) > 3 ? aiPrimitiveType_POLYGON : (aiPrimitiveType)(1u << ((n)-1)))

// ---------------------------------------------------------------------------
type
  {** @brief NOT CURRENTLY IN USE. An AnimMesh is an attachment to an #aiMesh stores per-vertex
   *  animations for a particular frame.
   *
   *  You may think of an #aiAnimMesh as a `patch` for the host mesh, which
   *  replaces only certain vertex data streams at a particular time.
   *  Each mesh stores n attached attached meshes (#aiMesh::mAnimMeshes).
   *  The actual relationship between the time line and anim meshes is
   *  established by #aiMeshAnim, which references singular mesh attachments
   *  by their ID and binds them to a time offset.
   *}
  TaiAnimMesh = record
    {** Replacement for aiMesh::mVertices. If this array is non-NULL,
     *  it *must* contain mNumVertices entries. The corresponding
     *  array in the host mesh must be non-NULL as well - animation
     *  meshes may neither add or nor remove vertex components (if
     *  a replacement array is NULL and the corresponding source
     *  array is not, the source data is taken instead)*}
    mVertices: PaiVector3D;

    {** Replacement for aiMesh::mNormals.  *}
    mNormals: PaiVector3D;

    {** Replacement for aiMesh::mTangents. *}
    mTangents: PaiVector3D;

    {** Replacement for aiMesh::mBitangents. *}
    mBitangents: PaiVector3D;

    {** Replacement for aiMesh::mColors *}
    mColors: array[0..AI_MAX_NUMBER_OF_COLOR_SETS-1] of PaiColor4D;

    {** Replacement for aiMesh::mTextureCoords *}
    mTextureCoords: array[0..AI_MAX_NUMBER_OF_TEXTURECOORDS-1] of PaiVector3D;

    {** The number of vertices in the aiAnimMesh, and thus the length of all
     * the member arrays.
     *
     * This has always the same value as the mNumVertices property in the
     * corresponding aiMesh. It is duplicated here merely to make the length
     * of the member arrays accessible even if the aiMesh is not known, e.g.
     * from language bindings.
     *}
    mNumVertices: CUInt;
  end;
  PaiAnimMesh = ^TaiAnimMesh;
  PPaiAnimMesh = ^PaiAnimMesh;

// ---------------------------------------------------------------------------
  {** @brief A mesh represents a geometry or model with a single material.
  *
  * It usually consists of a number of vertices and a series of primitives/faces
  * referencing the vertices. In addition there might be a series of bones, each
  * of them addressing a number of vertices with a certain weight. Vertex data
  * is presented in channels with each channel containing a single per-vertex
  * information such as a set of texture coords or a normal vector.
  * If a data pointer is non-null, the corresponding data stream is present.
  * From C++-programs you can also use the comfort functions Has*() to
  * test for the presence of various data streams.
  *
  * A Mesh uses only a single material which is referenced by a material ID.
  * @note The mPositions member is usually not optional. However, vertex positions
  * *could* be missing if the #AI_SCENE_FLAGS_INCOMPLETE flag is set in
  * @code
  * aiScene::mFlags
  * @endcode
  *}
  TaiMesh = record
    {** Bitwise combination of the members of the #aiPrimitiveType enum.
     * This specifies which types of primitives are present in the mesh.
     * The "SortByPrimitiveType"-Step can be used to make sure the
     * output meshes consist of one primitive type each.
     *}
    mPrimitiveTypes: CUInt;

    {** The number of vertices in this mesh.
     * This is also the size of all of the per-vertex data arrays.
     * The maximum value for this member is #AI_MAX_VERTICES.
     *}
    mNumVertices: CUInt;

    {** The number of primitives (triangles, polygons, lines) in this  mesh.
     * This is also the size of the mFaces array.
     * The maximum value for this member is #AI_MAX_FACES.
     *}
    mNumFaces: CUInt;

    {** Vertex positions.
     * This array is always present in a mesh. The array is
     * mNumVertices in size.
     *}
    mVertices: PaiVector3D;

    {** Vertex normals.
     * The array contains normalized vectors, NULL if not present.
     * The array is mNumVertices in size. Normals are undefined for
     * point and line primitives. A mesh consisting of points and
     * lines only may not have normal vectors. Meshes with mixed
     * primitive types (i.e. lines and triangles) may have normals,
     * but the normals for vertices that are only referenced by
     * point or line primitives are undefined and set to QNaN (WARN:
     * qNaN compares to inequal to *everything*, even to qNaN itself.
     * Using code like this to check whether a field is qnan is:
     * @code
     * #define IS_QNAN(f) (f != f)
     * @endcode
     * still dangerous because even 1.f == 1.f could evaluate to false! (
     * remember the subtleties of IEEE754 artithmetics). Use stuff like
     * @c fpclassify instead.
     * @note Normal vectors computed by Assimp are always unit-length.
     * However, this needn't apply for normals that have been taken
     *   directly from the model file.
     *}
    mNormals: PaiVector3D;

    {** Vertex tangents.
     * The tangent of a vertex points in the direction of the positive
     * X texture axis. The array contains normalized vectors, NULL if
     * not present. The array is mNumVertices in size. A mesh consisting
     * of points and lines only may not have normal vectors. Meshes with
     * mixed primitive types (i.e. lines and triangles) may have
     * normals, but the normals for vertices that are only referenced by
     * point or line primitives are undefined and set to qNaN.  See
     * the #mNormals member for a detailled discussion of qNaNs.
     * @note If the mesh contains tangents, it automatically also
     * contains bitangents.
     *}
    mTangents: PaiVector3D;

    {** Vertex bitangents.
    * The bitangent of a vertex points in the direction of the positive
    * Y texture axis. The array contains normalized vectors, NULL if not
    * present. The array is mNumVertices in size.
    * @note If the mesh contains tangents, it automatically also contains
    * bitangents.
    *}
    mBitangents: PaiVector3D;

    {** Vertex color sets.
    * A mesh may contain 0 to #AI_MAX_NUMBER_OF_COLOR_SETS vertex
    * colors per vertex. NULL if not present. Each array is
    * mNumVertices in size if present.
    *}
    mColors: array[0..AI_MAX_NUMBER_OF_COLOR_SETS-1] of PaiColor4D;

    {** Vertex texture coords, also known as UV channels.
    * A mesh may contain 0 to AI_MAX_NUMBER_OF_TEXTURECOORDS per
    * vertex. NULL if not present. The array is mNumVertices in size.
    *}
    mTextureCoords: array[0..AI_MAX_NUMBER_OF_TEXTURECOORDS-1] of PaiVector3D;

    {** Specifies the number of components for a given UV channel.
    * Up to three channels are supported (UVW, for accessing volume
    * or cube maps). If the value is 2 for a given channel n, the
    * component p.z of mTextureCoords[n][p] is set to 0.0f.
    * If the value is 1 for a given channel, p.y is set to 0.0f, too.
    * @note 4D coords are not supported
    *}
    mNumUVComponents: array[0..AI_MAX_NUMBER_OF_TEXTURECOORDS-1] of CUInt;

    {** The faces the mesh is constructed from.
    * Each face refers to a number of vertices by their indices.
    * This array is always present in a mesh, its size is given
    * in mNumFaces. If the #AI_SCENE_FLAGS_NON_VERBOSE_FORMAT
    * is NOT set each face references an unique set of vertices.
    *}
    mFaces: PaiFace;

    {** The number of bones this mesh contains.
    * Can be 0, in which case the mBones array is NULL.
    *}
    mNumBones: CUInt;

    {** The bones of this mesh.
    * A bone consists of a name by which it can be found in the
    * frame hierarchy and a set of vertex weights.
    *}
    mBones: PPaiBone;

    {** The material used by this mesh.
     * A mesh does use only a single material. If an imported model uses
     * multiple materials, the import splits up the mesh. Use this value
     * as index into the scene's material list.
     *}
    mMaterialIndex: CUInt;

    {** Name of the mesh. Meshes can be named, but this is not a
     *  requirement and leaving this field empty is totally fine.
     *  There are mainly three uses for mesh names:
     *   - some formats name nodes and meshes independently.
     *   - importers tend to split meshes up to meet the
     *      one-material-per-mesh requirement. Assigning
     *      the same (dummy) name to each of the result meshes
     *      aids the caller at recovering the original mesh
     *      partitioning.
     *   - Vertex animations refer to meshes by their names.
     **}
    mName: TaiString;

    {** NOT CURRENTLY IN USE. The number of attachment meshes *}
    mNumAnimMeshes: CUInt;

    {** NOT CURRENTLY IN USE. Attachment meshes for this mesh, for vertex-based animation.
     *  Attachment meshes carry replacement data for some of the
     *  mesh'es vertex components (usually positions, normals). *}
    mAnimMeshes: PPaiAnimMesh;
  end;
  PaiMesh = ^TaiMesh;
  PPaiMesh = ^PaiMesh;

