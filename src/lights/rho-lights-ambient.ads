package Rho.Lights.Ambient is

   type Root_Ambient_Light is
     new Root_Light_Type with private;

   type Ambient_Light_Type is access all Root_Ambient_Light'Class;

   function Ambient_Light
     (Color     : Rho.Color.Color_Type;
      Intensity : Unit_Real := 1.0)
      return Ambient_Light_Type;

private

   type Root_Ambient_Light is
     new Root_Light_Type with
      record
         null;
      end record;

   overriding procedure Load
     (Light : in out Root_Ambient_Light;
      Target : not null access Rho.Render.Render_Target'Class);

end Rho.Lights.Ambient;
