package Rho.Shaders.Partial.Loader is

   function Load_Partial_Shader
     (Stage : Shader_Stage;
      Path  : String)
      return Partial_Shader_Type;

end Rho.Shaders.Partial.Loader;
