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

end Rho.Lights.Ambient;
