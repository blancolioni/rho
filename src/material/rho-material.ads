private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

private with Tau.Compiler;

with Rho.Objects;
with Rho.Render;
with Rho.Renderable;

with Rho.Shaders.Slices;
with Rho.Shaders.Programs;

with Rho.Textures;

with Tau.Shaders;

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

   procedure Add_Shader
     (Material : in out Root_Material_Type'Class;
      Shader   : Tau.Shaders.Tau_Shader);

   procedure Add_Shader
     (Material    : in out Root_Material_Type'Class;
      Shader_Name : String);

   type Material_Type is access all Root_Material_Type'Class;

private

   package Texture_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Rho.Textures.Texture_Type,
        "="          => Rho.Textures."=");

   package Shader_Name_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Root_Material_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Slices.Slice_Container_Interface with
      record
         Program      : Rho.Shaders.Programs.Program_Type;
         Textures     : Texture_Vectors.Vector;
         Slices       : Rho.Shaders.Slices.Slice_Container;
         Shader_Names : Shader_Name_Lists.List;
         Compiler     : Tau.Compiler.Tau_Compiler;
         Loaded       : Boolean := False;
         Compiled     : Boolean := False;
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
