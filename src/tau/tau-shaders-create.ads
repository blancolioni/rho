with Tau.Declarations.Lists;
with Tau.Statements.Lists;

package Tau.Shaders.Create is

   function New_Shader
     (Declaration        : GCS.Positions.File_Position;
      Name               : String;
      Stage              : Rho.Shader_Stage;
      Arguments          : Tau.Declarations.Lists.List;
      Declarations       : Tau.Declarations.Lists.List;
      Statements         : Tau.Statements.Lists.List)
      return Tau_Shader;

end Tau.Shaders.Create;
