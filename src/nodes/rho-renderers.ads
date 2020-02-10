with Rho.Cameras;
with Rho.Scenes;
with Rho.Windows;

package Rho.Renderers is

   type Root_Renderer_Type is abstract tagged limited private;

   procedure Render
     (Renderer : in out Root_Renderer_Type;
      Window   : not null access Rho.Windows.Root_Window_Type'Class;
      Scene    : not null access Rho.Scenes.Root_Scene_Type'Class;
      Camera   : not null access Rho.Cameras.Root_Camera_Type'Class)
   is abstract;

   type Renderer_Type is access all Root_Renderer_Type'Class;

private

   type Root_Renderer_Type is abstract tagged limited
      record
         null;
      end record;

end Rho.Renderers;
