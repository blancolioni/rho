private with Ada.Containers.Doubly_Linked_Lists;

private with Tau.Declarations.Lists;
private with Tau.Statements.Lists;

with Rho;

with Tau.Objects;
with Tau.Declarations;
with Tau.Statements;

package Tau.Shaders is

   type Stage_Declaration_Class is
     (In_Variable, Out_Variable, Uniform_Variable,
      Provide_Name, Require_Name,
      Local_Name);

   type Root_Tau_Shader_Stage is
     new Tau.Objects.Root_Tau_Object with private;

   type Tau_Shader_Stage is access all Root_Tau_Shader_Stage'Class;

   function Stage
     (Shader : Root_Tau_Shader_Stage'Class)
      return Rho.Shader_Stage;

   procedure Iterate_Declarations
     (Shader : Root_Tau_Shader_Stage'Class;
      Class  : Stage_Declaration_Class;
      Process : not null access
        procedure (Declaration : Tau.Declarations.Tau_Declaration));

   procedure Iterate_Statements
     (Shader : Root_Tau_Shader_Stage'Class;
      Process : not null access
        procedure (Statement : Tau.Statements.Tau_Statement));

   type Root_Tau_Shader is
     new Tau.Objects.Root_Tau_Object with private;

   type Tau_Shader is access all Root_Tau_Shader'Class;

   function Has_Stage
     (Shader       : Root_Tau_Shader'Class;
      Shader_Stage : Rho.Shader_Stage)
      return Boolean;

   function Stage
     (Shader : Root_Tau_Shader'Class;
      Shader_Stage : Rho.Shader_Stage)
      return Tau_Shader_Stage;

   function Has_Vertex_Shader
     (Shader : Root_Tau_Shader'Class)
      return Boolean;

   function Vertex_Shader
     (Shader : Root_Tau_Shader'Class)
      return Tau_Shader_Stage;

   function Has_Fragment_Shader
     (Shader : Root_Tau_Shader'Class)
      return Boolean;

   function Fragment_Shader
     (Shader : Root_Tau_Shader'Class)
      return Tau_Shader_Stage;

private

   type Declaration_Class_Array is
     array (Stage_Declaration_Class) of Tau.Declarations.Lists.List;

   type Root_Tau_Shader_Stage is
     new Tau.Objects.Root_Tau_Object with
      record
         Stage        : Rho.Shader_Stage;
         Declarations : Declaration_Class_Array;
         Statements   : Tau.Statements.Lists.List;
      end record;

   overriding function Class_Name
     (Item : Root_Tau_Shader_Stage)
      return String
   is ("shader-stage");

   function Stage
     (Shader : Root_Tau_Shader_Stage'Class)
      return Rho.Shader_Stage
   is (Shader.Stage);

   type Shader_Stage_Array is
     array (Rho.Shader_Stage) of Tau_Shader_Stage;

   package Shader_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Tau_Shader);

   type Root_Tau_Shader is
     new Tau.Objects.Root_Tau_Object with
      record
         Is_Abstract : Boolean;
         Inherits    : Shader_Lists.List;
         Arguments   : Tau.Declarations.Lists.List;
         Stages      : Shader_Stage_Array;
      end record;

   overriding function Class_Name
     (Item : Root_Tau_Shader)
      return String
   is ("shader");

   function Has_Stage
     (Shader       : Root_Tau_Shader'Class;
      Shader_Stage : Rho.Shader_Stage)
      return Boolean
   is (Shader.Stages (Shader_Stage) /= null);

   function Stage
     (Shader : Root_Tau_Shader'Class;
      Shader_Stage : Rho.Shader_Stage)
      return Tau_Shader_Stage
   is (Shader.Stages (Shader_Stage));

   function Has_Vertex_Shader
     (Shader : Root_Tau_Shader'Class)
      return Boolean
   is (Shader.Has_Stage (Rho.Vertex_Shader));

   function Vertex_Shader
     (Shader : Root_Tau_Shader'Class)
      return Tau_Shader_Stage
   is (Shader.Stage (Rho.Vertex_Shader));

   function Has_Fragment_Shader
     (Shader : Root_Tau_Shader'Class)
      return Boolean
   is (Shader.Has_Stage (Rho.Fragment_Shader));

   function Fragment_Shader
     (Shader : Root_Tau_Shader'Class)
      return Tau_Shader_Stage
   is (Shader.Stage (Rho.Fragment_Shader));

end Tau.Shaders;
