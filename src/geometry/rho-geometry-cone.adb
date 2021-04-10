with Rho.Geometry.Cylinder;

package body Rho.Geometry.Cone is

   -------------------
   -- Cone_Geometry --
   -------------------

   function Cone_Geometry
     (Radius          : Real;
      Height          : Real;
      Radial_Segments : Natural;
      Height_Segments : Natural)
      return Geometry_Type
   is
   begin
      return Rho.Geometry.Cylinder.Cylinder_Geometry
        (Top_Radius      => 0.0,
         Bottom_Radius   => Radius,
         Height          => Height,
         Radial_Segments => Radial_Segments,
         Height_Segments => Height_Segments);
   end Cone_Geometry;

end Rho.Geometry.Cone;
