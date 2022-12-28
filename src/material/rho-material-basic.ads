with Rho.Color;
with Rho.Textures;

package Rho.Material.Basic is

   function Create
     (Color : Rho.Color.Color_Type)
      return Reference;

   function Create
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Reference;

end Rho.Material.Basic;
