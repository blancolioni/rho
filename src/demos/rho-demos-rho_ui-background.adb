with Rho.Cameras;
with Rho.Lights.Ambient;
with Rho.Lights.Spot;
with Rho.Nodes;
with Rho.UI.Builder;

with Rho.Paths;

package body Rho.Demos.Rho_UI.Background is

   type Background_UI_Demo is
     new Root_Demo_Type with
      record
         Handle    : Rho.Handles.Handle;
         Window    : Rho.Windows.Window_Type;
         Scene     : Rho.Scenes.Scene_Type;
         Ambient   : Rho.Lights.Ambient.Ambient_Light_Type;
         Spotlight : Rho.Lights.Spot.Spot_Light_Type;
      end record;

   overriding function Category
     (Demo : Background_UI_Demo)
      return String
   is ("ui");

   overriding function Name
     (Demo : Background_UI_Demo)
      return String
   is ("background");

   overriding function Description
     (Demo : Background_UI_Demo)
      return String
   is ("UI with colored background");

   overriding procedure Execute
     (Demo   : not null access Background_UI_Demo;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Demo   : not null access Background_UI_Demo;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type)
   is
      Scene    : constant Rho.Scenes.Scene_Type :=
        Rho.Scenes.Create_Scene;
      Camera   : constant Rho.Cameras.Camera_Type :=
                   Rho.Cameras.Perspective_Camera
                     (Field_Of_View => 45.0,
                      Aspect_Ratio  => Window.Aspect_Ratio,
                      Near          => 0.1,
                      Far           => 100.0);

      Builder : constant Rho.UI.Builder.Reference :=
                  Rho.UI.Builder.Load_Html
                    (Rho.Paths.Config_File ("gui/demos/ui/background.html"));
   begin

      Window.Add_UI (Builder.Top);

      Scene.Set_Name ("scene");
      Camera.Set_Name ("camera");
      Camera.Set_Position (0.0, 0.0, 1.0);
      Demo.Handle := Handle;
      Demo.Window := Window;
      Demo.Camera := Camera;
      Demo.Scene := Scene;

      Window.Set_Scene (Demo.Scene);
      Window.Set_Camera (Demo.Camera);

      Demo.Start (Handle, Window, Demo.Scene);

      Handle.Main_Loop;

   end Execute;

   ----------
   -- Load --
   ----------

   function Load return Demo_Type is
   begin
      return new Background_UI_Demo;
   end Load;

end Rho.Demos.Rho_UI.Background;
