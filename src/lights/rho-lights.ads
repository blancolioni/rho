with Rho.Color;
with Rho.Nodes;
with Rho.Render;

package Rho.Lights is

   type Root_Light_Type is
     abstract new Rho.Nodes.Root_Node_Type
   with private;

   overriding function Class_Name
     (Light : Root_Light_Type)
      return String
   is ("light");

   type Light_Type is access all Root_Light_Type'Class;

   procedure Initialize
     (Light     : in out Root_Light_Type'Class;
      Color     : Rho.Color.Color_Type;
      Intensity : Unit_Real := 1.0);

private

   type Root_Light_Type is
     abstract new Rho.Nodes.Root_Node_Type with
      record
         Color     : Rho.Color.Color_Type;
         Intensity : Unit_Real := 1.0;
      end record;

   overriding procedure Load
     (Light : in out Root_Light_Type;
      Target : not null access Rho.Render.Render_Target'Class);

end Rho.Lights;
