package Rho.UI.Surface.Primitives is

   function Draw_Color
     (Color : Rho.Color.Color_Type)
      return Draw_Primitive'Class;

   function Move_To
     (X, Y : Real;
      Draw : Boolean)
      return Draw_Primitive'Class;

   function Rectangle
     (Width, Height : Non_Negative_Real;
      Stencil       : Rho.Bitmaps.Greyscale_Reference)
      return Draw_Primitive'Class;

end Rho.UI.Surface.Primitives;
