with Tau.Statements;

package Tau.Shaders.Builder is

   function New_Shader_Stage
     (Position : GCS.Positions.File_Position;
      Stage    : Rho.Shader_Stage)
      return Tau_Shader_Stage;

   procedure Add_In_Variable
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration);

   procedure Add_Out_Variable
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration);

   procedure Add_Uniform_Variable
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration);

   procedure Add_Require
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration);

   procedure Add_Provide
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration);

   procedure Add_Local
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration);

   procedure Add_Statement
     (Stage     : Tau_Shader_Stage;
      Statement : Tau.Statements.Tau_Statement);

   function New_Shader
     (Position    : GCS.Positions.File_Position;
      Name        : String;
      Is_Abstract : Boolean)
      return Tau_Shader;

   procedure Add_Argument
     (Shader   : Tau_Shader;
      Argument : Tau.Declarations.Tau_Declaration);

   procedure Add_Stage
     (Shader : Tau_Shader;
      Stage  : Tau_Shader_Stage)
     with Pre => not Has_Stage (Shader.all, Stage.Stage),
     Post => Shader.Has_Stage (Stage.Stage)
     and then Shader.Stage (Stage.Stage) = Stage;

end Tau.Shaders.Builder;
