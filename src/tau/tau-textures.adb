package body Tau.Textures is

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Texture          : Root_Tau_Texture)
      return Rho.Textures.Texture_Type
   is
   begin
      return null;
      --  return Rho.Textures.Create_From_Image_Name
      --    (Texture_Name => Texture.Link_Name,
      --     Image_Name   => Texture.Get_Property ("image"),
      --     Format       => Texture.Get_Property ("format"),
      --  Order        => Positive'Value (Texture.Get_Property ("dimensions;
   end Instantiate;

end Tau.Textures;
