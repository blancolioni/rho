private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with WL.String_Sets;
private with Rho.Properties.Bags;

with Rho.Objects;
with Rho.Properties;
with Rho.Render;
with Rho.Renderable;
with Rho.Values;

with Rho.Shaders.Partial;
with Rho.Shaders.Programs;

with Rho.Textures;

package Rho.Material is

   subtype Parent is Rho.Objects.Root_Object_Type;

   type Instance is new Parent
     and Rho.Properties.Property_Value_List
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Partial.Partial_Shader_Source
   with private;

   subtype Any_Instance is Instance'Class;

   type Reference is access all Instance'Class;

   type Reference_Array is array (Positive range <>) of Reference;

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

   type Instance is new Parent
     and Rho.Properties.Property_Value_List
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Partial.Partial_Shader_Source with
      record
         Program         : Rho.Shaders.Programs.Program_Type;
         Textures        : Texture_Vectors.Vector;
         Shader_Names    : WL.String_Sets.Set;
         Partials        : Rho.Shaders.Partial.Partial_Shader_Container;
         Static_Bindings : Static_Binding_Lists.List;
         Property_Bag    : Rho.Properties.Bags.Reference;
         Loaded          : Boolean := False;
         Compiled        : Boolean := False;
      end record;

   procedure Default_Shaders (This : in out Instance'Class);

   overriding procedure Compile
     (Material : in out Instance;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Load
     (Material : in out Instance;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Before_Render
     (Material   : in out Instance;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding procedure Execute_Render
     (Material   : in out Instance;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding function Get_Value
     (This : Instance;
      Prop : Rho.Properties.Property)
      return String;

   overriding procedure Set_Value
     (This  : not null access Instance;
      Prop  : Rho.Properties.Property;
      Value : String);

   overriding function Class_Name
     (Material : Instance)
      return String
   is ("material");

   overriding procedure Iterate
     (Material   : Instance;
      Stage      : Shader_Stage;
      Process    : not null access
        procedure (Partial : Rho.Shaders.Partial.Partial_Shader_Type));

end Rho.Material;
