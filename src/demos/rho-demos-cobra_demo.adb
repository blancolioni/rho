with Rho.Cameras;
with Rho.Color;
with Rho.Lights.Ambient;
with Rho.Lights.Spot;
with Rho.Material.Simple;
with Rho.Nodes;
with Rho.Signals;

with Rho.Loaders.Dat;

with Rho.Paths;

package body Rho.Demos.Cobra_Demo is

   type Cobra_Demo_Type is new Root_Demo_Type with
      record
         Handle    : Rho.Handles.Handle;
         Window    : Rho.Windows.Window_Type;
         Scene     : Rho.Scenes.Scene_Type;
         Camera    : Rho.Cameras.Camera_Type;
         Ambient   : Rho.Lights.Ambient.Ambient_Light_Type;
         Spotlight : Rho.Lights.Spot.Spot_Light_Type;
      end record;

   overriding function Category
     (Demo : Cobra_Demo_Type)
      return String
   is ("mesh");

   overriding function Name
     (Demo : Cobra_Demo_Type)
      return String
   is ("cobra");

   overriding function Description
     (Demo : Cobra_Demo_Type)
      return String
   is ("A cobra mesh from Oolite");

   overriding procedure Execute
     (Demo   : not null access Cobra_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type);

   procedure On_Before_Scene_Render
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Demo   : not null access Cobra_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type)
   is
      Scene    : constant Rho.Scenes.Scene_Type :=
        Rho.Scenes.Create_Scene;
      Camera   : constant Rho.Cameras.Camera_Type :=
        Rho.Cameras.Perspective_Camera
          (Field_Of_View => 45.0,
           Aspect_Ratio  => Window.Aspect_Ratio,
           Near          => 10.0,
           Far           => 400.0);

      Mat_1 : constant Rho.Material.Material_Type :=
                Rho.Material.Simple.Create_Simple_Material
                  (Handle.Assets.Create_Texture_From_Image
                     ("config/textures/oolite_cobra3_diffuse"));
      Mat_2 : constant Rho.Material.Material_Type :=
                Rho.Material.Simple.Create_Simple_Material
                  (Handle.Assets.Create_Texture_From_Image
                     ("config/textures/oolite_cobra3_subents"));

      Node : constant Rho.Nodes.Node_Type :=
               Rho.Loaders.Dat.Load
                 (Path   => Rho.Paths.Config_File
                    ("models/oolite_cobra3.dat"),
                  Material => (Mat_1, Mat_2));

      Handler_Id : constant Rho.Signals.Handler_Id :=
        Handle.Current_Renderer.Add_Handler
          (Object  => Node,
           Signal  => Rho.Handles.Signal_Before_Render,
           Handler => On_Before_Scene_Render'Access,
           Data    => Rho.Signals.No_Signal_Data)
        with Unreferenced;

   begin

      Scene.Set_Name ("scene");
      Camera.Set_Name ("camera");
      Camera.Set_Position (0.0, 0.0, 200.0);

      Scene.Add (Node);

      Demo.Ambient :=
        Rho.Lights.Ambient.Ambient_Light
          (Color     => (1.0, 1.0, 1.0, 1.0),
           Intensity => 1.0);

      Demo.Spotlight :=
        Rho.Lights.Spot.Spot_Light
          (Color         => Rho.Color.White,
           Intensity     => 1.0,
           Decay         => 0.001);
      Demo.Spotlight.Set_Position (0.0, 100.0, 0.0);
      Scene.Add (Demo.Spotlight);

      Scene.Add (Demo.Ambient);

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
      return new Cobra_Demo_Type;
   end Load;

   ----------------------------
   -- On_Before_Scene_Render --
   ----------------------------

   procedure On_Before_Scene_Render
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (User_Data);
      Render_Data : Rho.Handles.Render_Signal_Type renames
                      Rho.Handles.Render_Signal_Type (Signal_Data);
      Node : constant Rho.Nodes.Node_Type :=
        Rho.Nodes.Node_Type (Object);
   begin
      Node.Rotate_Z (7.0 * Real (Render_Data.Time_Since_Last_Event));
      Node.Rotate_Y (5.3 * Real (Render_Data.Time_Since_Last_Event));
   end On_Before_Scene_Render;

end Rho.Demos.Cobra_Demo;
