package body Rho.Renderable is

   ------------
   -- Render --
   ------------

   procedure Render
     (Renderable : in out Renderable_Interface'Class;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Renderable.Before_Render (Target);
      Renderable.Execute_Render (Target);
      Renderable.After_Render (Target);
   end Render;

end Rho.Renderable;
