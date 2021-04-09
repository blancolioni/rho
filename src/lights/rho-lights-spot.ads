with Rho.Trigonometry;

package Rho.Lights.Spot is

   type Root_Spot_Light is
     new Root_Light_Type with private;

   type Spot_Light_Type is access all Root_Spot_Light'Class;

   function Spot_Light
     (Color     : Rho.Color.Color_Type := Rho.Color.White;
      Intensity : Unit_Real            := 1.0;
      Maximum_Range : Non_Negative_Real       := 0.0;
      Dispersion    : Rho.Trigonometry.Angle  :=
        Rho.Trigonometry.From_Degrees (60.0);
      Penumbra      : Unit_Real               := 0.0;
      Decay         : Non_Negative_Real       := 1.0)
      return Spot_Light_Type;

private

   type Root_Spot_Light is
     new Root_Light_Type with
      record
         Maximum_Range : Non_Negative_Real       := 0.0;
         Dispersion    : Rho.Trigonometry.Angle  :=
                           Rho.Trigonometry.From_Degrees (60.0);
         Penumbra      : Unit_Real               := 0.0;
         Decay         : Non_Negative_Real       := 1.0;
      end record;

end Rho.Lights.Spot;
