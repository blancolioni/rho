private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with WL.String_Sets;

with Rho.Objects;
with Rho.Render;
with Rho.Renderable;
with Rho.Values;

with Rho.Shaders.Partial;
with Rho.Shaders.Programs;

with Rho.Textures;

package Rho.Material is

   type Root_Material_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Partial.Partial_Shader_Source
   with private;

   type Material_Type is access all Root_Material_Type'Class;

   type Material_Array is array (Positive range <>) of Material_Type;

private

   package Texture_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Rho.Textures.Texture_Type,
        "="          => Rho.Textures."=");

   type Static_Binding (Of_Type : Rho.Values.Value_Type) is
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Rho.Values.Rho_Value (Of_Type);
      end record;

   package Static_Binding_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Static_Binding);

   type Root_Material_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Partial.Partial_Shader_Source with
      record
         Program         : Rho.Shaders.Programs.Program_Type;
         Textures        : Texture_Vectors.Vector;
         Shader_Names    : WL.String_Sets.Set;
         Partials        : Rho.Shaders.Partial.Partial_Shader_Container;
         Static_Bindings : Static_Binding_Lists.List;
         Loaded          : Boolean := False;
         Compiled        : Boolean := False;
      end record;

   procedure Default_Shaders (This : in out Root_Material_Type'Class);

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

   overriding procedure Iterate
     (Material   : Root_Material_Type;
      Stage      : Shader_Stage;
      Process    : not null access
        procedure (Partial : Rho.Shaders.Partial.Partial_Shader_Type));

end Rho.Material;
