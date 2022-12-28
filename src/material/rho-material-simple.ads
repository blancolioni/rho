with Rho.Textures;

package Rho.Material.Simple is

   function Create
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Reference;

end Rho.Material.Simple;
