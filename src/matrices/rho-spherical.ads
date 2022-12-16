with Rho.Matrices;
with Rho.Trigonometry;

package Rho.Spherical is

   type Spherical_Position is private;

   function Create (Radius     : Non_Negative_Real;
                    Phi, Theta : Rho.Trigonometry.Angle :=
                      Rho.Trigonometry.From_Degrees (0.0))
                    return Spherical_Position;

   function To_Spherical
     (V : Rho.Matrices.Vector_3)
      return Spherical_Position;

   function To_Spherical
     (X, Y, Z : Real)
      return Spherical_Position;

   function To_Vector (S : Spherical_Position) return Rho.Matrices.Vector_3;
   function Radius (This : Spherical_Position) return Non_Negative_Real;
   function Phi (This : Spherical_Position) return Rho.Trigonometry.Angle;
   function Theta (This : Spherical_Position) return Rho.Trigonometry.Angle;

   function "+" (Left, Right : Spherical_Position) return Spherical_Position;

private

   type Spherical_Position is
      record
         Radius : Non_Negative_Real := 1.0;
         Phi    : Rho.Trigonometry.Angle :=
                    Rho.Trigonometry.From_Degrees (0.0);
         Theta  : Rho.Trigonometry.Angle :=
                    Rho.Trigonometry.From_Degrees (0.0);
      end record;

   function Radius (This : Spherical_Position) return Non_Negative_Real
   is (This.Radius);

   function Phi (This : Spherical_Position) return Rho.Trigonometry.Angle
   is (This.Phi);

   function Theta (This : Spherical_Position) return Rho.Trigonometry.Angle
   is (This.Theta);

   function Create (Radius     : Non_Negative_Real;
                    Phi, Theta : Rho.Trigonometry.Angle :=
                      Rho.Trigonometry.From_Degrees (0.0))
                    return Spherical_Position
   is ((Radius, Phi, Theta));

end Rho.Spherical;
