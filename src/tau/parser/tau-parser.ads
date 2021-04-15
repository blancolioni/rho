with Tau.Objects;
with Tau.Shaders;

package Tau.Parser is

   function Load_File
     (Path : String)
      return Tau.Objects.Tau_Object;

   function Load_Shader
     (Path : String)
      return Tau.Shaders.Tau_Shader
   is (Tau.Shaders.Tau_Shader (Load_File (Path)));

end Tau.Parser;
