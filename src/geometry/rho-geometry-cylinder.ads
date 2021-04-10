package Rho.Geometry.Cylinder is

   function Cylinder_Geometry
     (Top_Radius      : Real := 1.0;
      Bottom_Radius   : Real := 1.0;
      Height          : Real := 1.0;
      Radial_Segments : Positive := 8;
      Height_Segments : Positive := 1)
      return Geometry_Type
     with Pre => Height /= 0.0;

end Rho.Geometry.Cylinder;
