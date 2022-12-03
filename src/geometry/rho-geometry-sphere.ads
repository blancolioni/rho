package Rho.Geometry.Sphere is

   function Sphere_Geometry
     (Radius          : Real := 1.0;
      Width_Segments  : Positive := 8;
      Height_Segments : Positive := 8;
      Theta_Start     : Real := 0.0;
      Theta_Length    : Real := 180.0;
      Phi_Start       : Real := 0.0;
      Phi_Length      : Real := 360.0)
      return Geometry_Type
     with Pre => Radius /= 0.0
     and then Width_Segments >= 3
     and then Height_Segments >= 2;

end Rho.Geometry.Sphere;
