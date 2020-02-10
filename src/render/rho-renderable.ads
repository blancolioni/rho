with Rho.Render;

package Rho.Renderable is

--     Signal_Before_Render : constant Rho.Signals.Signal_Type :=
--       "signal-before-render";
--
--     Signal_After_Render  : constant Rho.Signals.Signal_Type :=
--       "signal-after-render";

   type Renderable_Interface is limited interface;

   procedure Load
     (Renderable : in out Renderable_Interface;
      Target     : not null access Rho.Render.Render_Target'Class)
   is abstract;

   procedure Unload
     (Renderable : in out Renderable_Interface;
      Target     : not null access Rho.Render.Render_Target'Class)
   is null;

   procedure Before_Render
     (Renderable : in out Renderable_Interface;
      Target     : not null access Rho.Render.Render_Target'Class)
   is null;

   procedure After_Render
     (Renderable : in out Renderable_Interface;
      Target     : not null access Rho.Render.Render_Target'Class)
   is null;

   procedure Execute_Render
     (Renderable : in out Renderable_Interface;
      Target     : not null access Rho.Render.Render_Target'Class)
   is null;

   procedure Render
     (Renderable : in out Renderable_Interface'Class;
      Target     : not null access Rho.Render.Render_Target'Class);

end Rho.Renderable;
