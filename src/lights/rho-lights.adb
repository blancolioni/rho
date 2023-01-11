package body Rho.Lights is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Light     : not null access Root_Light_Type'Class;
      Color     : Rho.Color.Color_Type;
      Intensity :        Unit_Real := 1.0)
   is
   begin
      Light.Initialize (Is_Light => True);
      Light.Color := Color;
      Light.Intensity := Intensity;
   end Initialize;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Light : in out Root_Light_Type;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Rho.Nodes.Root_Node_Type (Light).Load (Target);
   end Load;

end Rho.Lights;
