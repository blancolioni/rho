with Rho.Matrices;
with Rho.Values;

package body Rho.Lights.Spot is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Light : in out Root_Spot_Light;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Root_Light_Type (Light).Load (Target);

      Target.Add_Uniform
        (Name  => "spotPosition",
         Value =>
           Rho.Values.Vector_Value (Light.Position));
      Target.Add_Uniform
        (Name  => "spotColor",
         Value =>
           Rho.Values.Vector_Value
             (Rho.Matrices.To_Vector
                  (Light.Color.R, Light.Color.G, Light.Color.B)));
      Target.Add_Uniform
        (Name  => "attenuation",
         Value =>
           Rho.Values.Real_Value (Light.Decay));
   end Load;

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

      Light : constant Spot_Light_Type :=
                new Root_Spot_Light'
                  (Rho.Nodes.Root_Node_Type with
                   Color         => Color,
                   Intensity     => Intensity,
                   Maximum_Range => Maximum_Range,
                   Dispersion    => Dispersion,
                   Penumbra      => Penumbra,
                   Decay         => Decay);
   begin
      return Light;
   end Spot_Light;

end Rho.Lights.Spot;
