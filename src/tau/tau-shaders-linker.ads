private with Tau.Shaders.Lists;

package Tau.Shaders.Linker is

   type Shader_Link_Type is tagged private;

   procedure Add
     (Linker : in out Shader_Link_Type;
      Shader : Tau_Shader);

   procedure Link
     (Linker : in out Shader_Link_Type);

   function Shader (Stage : Rho.Shader_Stage) return Tau_Shader;

private

   type Shader_Link_Type is tagged
      record
         List : Tau.Shaders.Lists.List;
      end record;

end Tau.Shaders.Linker;
