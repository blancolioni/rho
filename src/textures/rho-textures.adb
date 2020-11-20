package body Rho.Textures is

   ----------------------------
   -- Create_From_Image_Name --
   ----------------------------

   function Create_From_Image_Name
     (Texture_Name : String;
      Image_Name   : String;
      Format       : String;
      Order        : Positive)
      return Texture_Type
   is
   begin
      return Texture : constant Texture_Type :=
        new Root_Texture_Type'
          (Rho.Objects.Root_Object_Type with
             Image_Name =>
               Ada.Strings.Unbounded.To_Unbounded_String (Image_Name),
           Image_Format =>
             Ada.Strings.Unbounded.To_Unbounded_String (Format),
           Dimensions   => Order)
      do
         Texture.Set_Name (Texture_Name);
      end return;
   end Create_From_Image_Name;

end Rho.Textures;
