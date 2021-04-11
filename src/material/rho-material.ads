private with Ada.Containers.Vectors;

with Rho.Objects;
with Rho.Render;
with Rho.Renderable;

with Rho.Shaders.Slices;
with Rho.Shaders.Programs;

with Rho.Textures;

package Rho.Material is

   type Root_Material_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Slices.Slice_Container_Interface
   with private;

   overriding function Shader_Slices
     (Material : Root_Material_Type)
      return Rho.Shaders.Slices.Slice_Array;

   overriding procedure Add_Slice
     (Material : in out Root_Material_Type;
      Slice : Rho.Shaders.Slices.Slice_Type);

   type Material_Type is access all Root_Material_Type'Class;

private

   package Texture_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Rho.Textures.Texture_Type,
        "="          => Rho.Textures."=");

   type Root_Material_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Slices.Slice_Container_Interface with
      record
         Program   : Rho.Shaders.Programs.Program_Type;
         Textures  : Texture_Vectors.Vector;
         Slices    : Rho.Shaders.Slices.Slice_Container;
         Loaded    : Boolean := False;
         Compiled  : Boolean := False;
      end record;

   overriding procedure Compile
     (Material : in out Root_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Load
     (Material : in out Root_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Before_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding procedure Execute_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding function Class_Name
     (Material : Root_Material_Type)
      return String
   is ("material");

   overriding function Shader_Slices
     (Material : Root_Material_Type)
      return Rho.Shaders.Slices.Slice_Array
   is (Material.Slices.Shader_Slices);

end Rho.Material;
