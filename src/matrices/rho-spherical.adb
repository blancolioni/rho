with Rho.Elementary_Functions;

package body Rho.Spherical is

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Spherical_Position) return Spherical_Position is
      use type Rho.Trigonometry.Angle;
   begin
      return (Left.Radius, Left.Phi + Right.Phi, Left.Theta + Right.Theta);
   end "+";

   ------------------
   -- To_Spherical --
   ------------------

   function To_Spherical
     (V : Rho.Matrices.Vector_3)
      return Spherical_Position
   is
      use Rho.Matrices;
   begin
      return To_Spherical (X (V), Y (V), Z (V));
   end To_Spherical;

   ------------------
   -- To_Spherical --
   ------------------

   function To_Spherical (X, Y, Z : Real)
                          return Spherical_Position
   is
      use Rho.Trigonometry;
      Radius : constant Non_Negative_Real :=
                 Rho.Elementary_Functions.Sqrt (X * X + Y * Y + Z * Z);
   begin
      if Radius = 0.0 then
         return Spherical_Position'
           (Radius, From_Degrees (0.0), From_Degrees (0.0));
      else
         return Spherical_Position'
           (Radius => Radius,
            Theta  => Arctan (X, Z),
            Phi    => Arccos (Signed_Unit_Clamp (Y / Radius)));
      end if;
   end To_Spherical;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (S : Spherical_Position) return Rho.Matrices.Vector_3 is
      use Rho.Trigonometry;
      Sin_Phi_Radius : constant Real := Sin (S.Phi) * S.Radius;
   begin
      return Rho.Matrices.To_Vector
        (Sin_Phi_Radius * Sin (S.Theta),
         Cos (S.Phi) * S.Radius,
         Sin_Phi_Radius * Cos (S.Theta));
   end To_Vector;

end Rho.Spherical;
