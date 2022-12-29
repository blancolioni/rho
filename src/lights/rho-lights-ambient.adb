with Rho.Matrices;
with Rho.Values;

package body Rho.Lights.Ambient is

   -------------------
   -- Ambient_Light --
   -------------------

   function Ambient_Light
     (Color     : Rho.Color.Color_Type;
      Intensity : Unit_Real := 1.0)
      return Ambient_Light_Type
   is
   begin
      return Light : constant Ambient_Light_Type :=
        new Root_Ambient_Light
      do
         Light.Initialize (Color, Intensity);
      end return;
   end Ambient_Light;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Light : in out Root_Ambient_Light;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Root_Light_Type (Light).Load (Target);

      Target.Set_Uniform
        (Name  => "ambientColor",
         Value =>
           Rho.Values.Vector_Value
             (Rho.Matrices.To_Vector
                  (Light.Color.R, Light.Color.G, Light.Color.B)));
      Target.Set_Uniform
        (Name  => "ambientCoefficient",
         Value =>
           Rho.Values.Real_Value (Light.Intensity));
   end Load;

end Rho.Lights.Ambient;
