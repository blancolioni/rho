private with Tau.Declarations.Lists;
private with Tau.Statements.Lists;

with Tau.Environment;
with Tau.Generators;
with Tau.Objects;
with Tau.Values;

with Tau.Expressions;

with Rho;

package Tau.Shaders is

   subtype Tau_Shader_Stage is Rho.Shader_Stage;

   type Root_Tau_Shader is
     new Tau.Objects.Root_Tau_Object with private;

   type Tau_Shader is access all Root_Tau_Shader'Class;

   overriding function Children
     (Shader : Root_Tau_Shader)
      return Tau_Node_Array;

   function Bind
     (Shader   : Root_Tau_Shader;
      Bindings : Tau.Environment.Tau_Environment)
      return Tau_Shader;

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

   function To_Value
     (Shader : Tau_Shader)
      return Tau.Values.Tau_Value;

private

   type Root_Tau_Shader is new Tau.Objects.Root_Tau_Object with
      record
         Is_Abstract        : Boolean;
         Stage              : Tau_Shader_Stage;
         Value              : Tau.Values.Tau_Value;
         Abstract_Arguments : Tau.Declarations.Lists.List;
         Arguments          : Tau.Declarations.Lists.List;
         Declarations       : Tau.Declarations.Lists.List;
         Statements         : Tau.Statements.Lists.List;
         Return_Value       : Tau.Expressions.Tau_Expression;
         Bindings           : Tau.Environment.Tau_Environment;
      end record;

   overriding function To_Source
     (Shader    : Root_Tau_Shader;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String;

   function Stage
     (Shader : Root_Tau_Shader)
      return Rho.Shader_Stage
   is (Shader.Stage);

end Tau.Shaders;
