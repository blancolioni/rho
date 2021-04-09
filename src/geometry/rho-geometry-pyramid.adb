package body Rho.Geometry.Pyramid is

   ----------------------
   -- Pyramid_Geometry --
   ----------------------

   function Pyramid_Geometry
     (Base_Size_X, Base_Size_Y, Height : Non_Negative_Real)
      return Geometry_Type
   is
      X2 : constant Real := Base_Size_X / 2.0;
      X1 : constant Real := -X2;
      Y2 : constant Real := Base_Size_Y / 2.0;
      Y1 : constant Real := -Y2;
      Z2 : constant Real := Height / 2.0;
      Z1 : constant Real := -Z2;
      Geometry     : constant Geometry_Type := Create_Geometry;
   begin
      Geometry.Vertex (X1, Y1, Z1);
      Geometry.Vertex (X1, Y2, Z1);
      Geometry.Vertex (X2, Y2, Z1);
      Geometry.Vertex (X2, Y1, Z1);
      Geometry.Vertex (0.0, 0.0, Z2);

      Geometry.Face (1, 2, 3);
      Geometry.Face (1, 3, 4);
      Geometry.Face (2, 1, 5);
      Geometry.Face (3, 2, 5);
      Geometry.Face (4, 3, 5);
      Geometry.Face (1, 4, 5);

      return Geometry;
   end Pyramid_Geometry;

end Rho.Geometry.Pyramid;
