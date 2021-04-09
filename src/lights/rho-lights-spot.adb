package body Rho.Lights.Spot is

   ----------------
   -- Spot_Light --
   ----------------

   function Spot_Light
     (Color         : Rho.Color.Color_Type   := Rho.Color.White;
      Intensity     : Unit_Real := 1.0;
      Maximum_Range : Non_Negative_Real := 0.0;
      Dispersion    : Rho.Trigonometry.Angle :=
        Rho.Trigonometry.From_Degrees (60.0);
      Penumbra      : Unit_Real := 0.0;
      Decay         : Non_Negative_Real := 1.0)
      return Spot_Light_Type
   is
   begin
      return Light : constant Spot_Light_Type :=
        new Root_Spot_Light'
          (Rho.Nodes.Root_Node_Type with
             Color         => Color,
             Intensity     => Intensity,
             Maximum_Range => Maximum_Range,
             Dispersion    => Dispersion,
             Penumbra      => Penumbra,
             Decay         => Decay)
      do
         null;
      end return;
   end Spot_Light;

end Rho.Lights.Spot;
