private with Rho.Buffers;

with Rho.Objects;
with Rho.Matrices;
with Rho.Renderable;
with Rho.Render;

package Rho.Geometry is

   type Vertex_Index is new Positive;

   type Vertex_Index_Array is array (Positive range <>) of Vertex_Index;

   type Root_Geometry_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
   with private;

   overriding procedure Load
     (Geometry : in out Root_Geometry_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   procedure Vertex
     (Geometry : in out Root_Geometry_Type'Class;
      Vector   : Rho.Matrices.Vector_3);

   procedure Vertex
     (Geometry : in out Root_Geometry_Type'Class;
      X, Y, Z  : Real);

   procedure Normal
     (Geometry : in out Root_Geometry_Type'Class;
      Vector   : Rho.Matrices.Normal_Vector_3);

   procedure Normal
     (Geometry : in out Root_Geometry_Type'Class;
      X, Y, Z  : Signed_Unit_Real);

   procedure Texture
     (Geometry : in out Root_Geometry_Type'Class;
      U, V     : Unit_Real);

   procedure Face
     (Geometry : in out Root_Geometry_Type'Class;
      A, B, C  : Vertex_Index);

   procedure Face
     (Geometry : in out Root_Geometry_Type'Class;
      Vertices : Vertex_Index_Array);

   type Geometry_Type is access all Root_Geometry_Type'Class;

   function Create_Geometry return Geometry_Type;

private

   type Root_Geometry_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface with
      record
         Vertices : Rho.Buffers.Buffer_Type;
         Normals  : Rho.Buffers.Buffer_Type;
         UVs      : Rho.Buffers.Buffer_Type;
         Faces    : Rho.Buffers.Buffer_Type;
      end record;

   overriding procedure Execute_Render
     (Geometry : in out Root_Geometry_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding function Class_Name
     (Geometry : Root_Geometry_Type)
      return String
   is ("geometry");

end Rho.Geometry;
