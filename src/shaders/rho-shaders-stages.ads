private with Ada.Strings.Unbounded;

with Rho.Objects;
with Rho.Shaders.Partial;

package Rho.Shaders.Stages is

   type Root_Shader_Type is
     new Rho.Objects.Root_Object_Type with private;

   function Stage (Shader : Root_Shader_Type) return Shader_Stage;

   function Has_Source (Shader : Root_Shader_Type) return Boolean;
   function Shader_Source (Shader : Root_Shader_Type) return String;

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

   type Root_Shader_Type is
     new Rho.Objects.Root_Object_Type with
      record
         Stage     : Shader_Stage;
         Source    : Ada.Strings.Unbounded.Unbounded_String;
         Partials  : Rho.Shaders.Partial.Partial_Shader_Container;
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
