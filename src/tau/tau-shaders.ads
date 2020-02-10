private with Tau.Declarations.Lists;
private with Tau.Statements.Lists;

with Tau.Environment;
with Tau.Generators;
with Tau.Objects;

with Rho;

package Tau.Shaders is

   subtype Tau_Shader_Stage is Rho.Shader_Stage;

   type Root_Tau_Shader is
     new Tau.Objects.Root_Tau_Object with private;

   function Check
     (Shader : Root_Tau_Shader)
      return Boolean;

   procedure Check
     (Shader      : Root_Tau_Shader;
      Environment : Tau.Environment.Tau_Environment);

   function Compile
     (Shader     : Root_Tau_Shader;
      Bindings   : Tau.Environment.Tau_Environment;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
     return Boolean;

   function Stage
     (Shader : Root_Tau_Shader)
      return Rho.Shader_Stage;

   type Tau_Shader is access all Root_Tau_Shader'Class;

private

   type Root_Tau_Shader is new Tau.Objects.Root_Tau_Object with
      record
         Is_Abstract        : Boolean;
         Stage              : Tau_Shader_Stage;
         Abstract_Arguments : Tau.Declarations.Lists.List;
         Arguments          : Tau.Declarations.Lists.List;
         Declarations       : Tau.Declarations.Lists.List;
         Statements         : Tau.Statements.Lists.List;
      end record;

   function Stage
     (Shader : Root_Tau_Shader)
      return Rho.Shader_Stage
   is (Shader.Stage);

end Tau.Shaders;
