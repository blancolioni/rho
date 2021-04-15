with Rho.Textures;

package Rho.Material.Simple is

   function Create_Simple_Material
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Material_Type;

end Rho.Material.Simple;
