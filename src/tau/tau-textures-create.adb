package body Tau.Textures.Create is

   -----------------
   -- New_Texture --
   -----------------

   function New_Texture
     (Declaration : GCS.Positions.File_Position; Name : String)
      return Tau_Texture
   is
   begin
      return Texture : constant Tau_Texture :=
        new Root_Tau_Texture
      do
         Texture.Initialize_Object (Declaration, Name);
         Texture.Initial_Property ("image");
         Texture.Initial_Property ("format", "png");
         Texture.Initial_Property ("dimensions", "2");
      end return;
   end New_Texture;

end Tau.Textures.Create;
