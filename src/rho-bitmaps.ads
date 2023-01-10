package Rho.Bitmaps is

   type Greyscale_Value is mod 256;

   type Greyscale_Bitmap is array (Positive range <>) of Greyscale_Value
     with Pack;

   type Greyscale_Reference is access Greyscale_Bitmap;

end Rho.Bitmaps;
