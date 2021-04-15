private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Rho.Objects;

with Rho.Shaders.Slices;

package Rho.Shaders.Stages is

   type Root_Shader_Type is
     new Rho.Objects.Root_Object_Type with private;

   function Stage (Shader : Root_Shader_Type) return Shader_Stage;

   procedure Build (Shader : in out Root_Shader_Type);

   function Has_Source (Shader : Root_Shader_Type) return Boolean;
   function Shader_Source (Shader : Root_Shader_Type) return String;

   procedure Add_Slice
     (Shader : in out Root_Shader_Type;
      Slice : Rho.Shaders.Slices.Slice_Type);

   procedure Add_Slices
     (Shader    : in out Root_Shader_Type;
      Slices : Rho.Shaders.Slices.Slice_Array);

   procedure Add_Slices
     (Shader    : in out Root_Shader_Type;
      Slices    : Rho.Shaders.Slices.Slice_Container_Interface'Class);

   procedure Set_Source
     (Shader : in out Root_Shader_Type;
      Source : String);

   type Shader_Type is access all Root_Shader_Type'Class;

   function Create
     (Name   : String;
      Stage  : Shader_Stage;
      Source : String := "")
      return Shader_Type;

   type Shader_Array is array (Positive range <>) of Shader_Type;

private

   package Fragment_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Shaders.Slices.Slice_Type,
        Rho.Shaders.Slices."=");

   type Fragment_List_Array is
     array (Rho.Shaders.Slices.Shader_Source_Section)
     of Fragment_Lists.List;

   type Root_Shader_Type is
     new Rho.Objects.Root_Object_Type with
      record
         Stage     : Shader_Stage;
         Source    : Ada.Strings.Unbounded.Unbounded_String;
         Slices : Fragment_List_Array;
      end record;

   overriding function Class_Name
     (Shader : Root_Shader_Type)
      return String
   is ("shader");

   function Stage (Shader : Root_Shader_Type) return Shader_Stage
   is (Shader.Stage);

   function Shader_Source (Shader : Root_Shader_Type) return String
   is (Ada.Strings.Unbounded.To_String (Shader.Source));

   function Has_Source (Shader : Root_Shader_Type) return Boolean
   is (Ada.Strings.Unbounded."/="
       (Shader.Source,
        Ada.Strings.Unbounded.Null_Unbounded_String));

end Rho.Shaders.Stages;
