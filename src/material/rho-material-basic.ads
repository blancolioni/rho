with Rho.Color;
with Rho.Textures;

package Rho.Material.Basic is

   function Create_Basic_Material
     (Color : Rho.Color.Color_Type)
      return Material_Type;

   function Create_Basic_Material
     (Texture : Rho.Textures.Texture_Type)
      return Material_Type;

end Rho.Material.Basic;
