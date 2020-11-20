with Tau.Objects;
with Tau.Material;
with Tau.Shaders;
with Tau.Textures;

package Tau.Parser is

   function Load_File
     (Path : String)
      return Tau.Objects.Tau_Object;

   function Load_Material
     (Path : String)
      return Tau.Material.Tau_Material
   is (Tau.Material.Tau_Material (Load_File (Path)));

   function Load_Shader
     (Path : String)
      return Tau.Shaders.Tau_Shader
   is (Tau.Shaders.Tau_Shader (Load_File (Path)));

   function Load_Texture
     (Path : String)
      return Tau.Textures.Tau_Texture
   is (Tau.Textures.Tau_Texture (Load_File (Path)));

end Tau.Parser;
