package Rho.Geometry.Cone is

   function Cone_Geometry
     (Radius          : Real;
      Height          : Real;
      Radial_Segments : Natural;
      Height_Segments : Natural)
      return Geometry_Type;

end Rho.Geometry.Cone;
