private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with WL.String_Sets;

with Rho.Shaders.Partial;
with Rho.Shaders.Variables;

package Rho.Shaders.Builder is

   type Root_Builder_Type is tagged limited private;

   procedure Add
     (This    : in out Root_Builder_Type;
      Partial : Rho.Shaders.Partial.Partial_Shader_Type);

   procedure Build
     (This : in out Root_Builder_Type);

   function Source
     (This  : Root_Builder_Type)
      return String;

   function Has_Attribute
     (This : Root_Builder_Type;
      Name : String)
      return Boolean;

   function Has_Uniform
     (This : Root_Builder_Type;
      Name : String)
      return Boolean;

private

   type Shader_Section is
     (Shader_Preamble,
      Shader_Global,
      Shader_Local,
      Shader_Main);

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Shader_Section_Array is
     array (Shader_Section) of String_Lists.List;

   package Partial_Shader_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Shaders.Partial.Partial_Shader_Type,
        Rho.Shaders.Partial."=");

   package Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Shaders.Variables.Variable_Type,
        Rho.Shaders.Variables."=");

   type Root_Builder_Type is tagged limited
      record
         Names      : WL.String_Sets.Set;
         Partials   : Partial_Shader_Lists.List;
         Sections   : Shader_Section_Array;
         Uniforms   : WL.String_Sets.Set;
         Attributes : WL.String_Sets.Set;
         Source     : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Source
     (This : Root_Builder_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (This.Source));

   function Has_Attribute
     (This : Root_Builder_Type;
      Name : String)
      return Boolean
   is (This.Attributes.Contains (Name));

   function Has_Uniform
     (This : Root_Builder_Type;
      Name : String)
      return Boolean
   is (This.Uniforms.Contains (Name));

end Rho.Shaders.Builder;
