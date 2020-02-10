package body Rho.Rectangles is

   --------------------------
   -- Initialize_Rectangle --
   --------------------------

   procedure Initialize_Rectangle
     (Rectangle     : in out Root_Rectangle_Type'Class;
      X, Y          : Real;
      Width, Height : Non_Negative_Real)
   is
   begin
      Rectangle.X := X;
      Rectangle.Y := Y;
      Rectangle.Width := Width;
      Rectangle.Height := Height;
   end Initialize_Rectangle;

end Rho.Rectangles;
