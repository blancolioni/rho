with System;

package Rho.Textures.Creator is

   type Texture_Creator_Interface is limited interface;

   function Create_Packed_Greyscale
     (This          : Texture_Creator_Interface;
      Width, Height : Positive;
      Bitmap        : System.Address)
      return Texture_Type
      is abstract;

end Rho.Textures.Creator;
