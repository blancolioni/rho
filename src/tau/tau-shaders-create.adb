pragma Ada_2012;
package body Tau.Shaders.Create is

   ----------------
   -- New_Shader --
   ----------------

   function New_Shader
     (Declaration  : GCS.Positions.File_Position;
      Name         : String;
      Stage        : Rho.Shader_Stage;
      Arguments    : Tau.Declarations.Lists.List;
      Declarations : Tau.Declarations.Lists.List;
      Statements   : Tau.Statements.Lists.List)
      return Tau_Shader
   is
      Shader : constant Tau_Shader :=
        new Root_Tau_Shader'
          (Tau.Objects.Root_Tau_Object with
           Is_Abstract        => False,
           Stage              => Stage,
           Abstract_Arguments => <>,
           Arguments          => Arguments,
           Declarations       => Declarations,
           Statements         => Statements);
   begin
      Shader.Initialize_Object (Declaration, Name);
      return Shader;
   end New_Shader;

end Tau.Shaders.Create;
