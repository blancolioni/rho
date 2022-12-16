with Rho.Cameras;
with Rho.Color;
with Rho.Geometry.Sphere;
with Rho.Lights.Ambient;
with Rho.Lights.Spot;
with Rho.Material.Simple;
with Rho.Meshes;
with Rho.Nodes;
with Rho.Signals;

with Rho.Paths;

package body Rho.Demos.Earth is

   type Earth_Demo_Type is new Root_Demo_Type with
      record
         Handle    : Rho.Handles.Handle;
         Window    : Rho.Windows.Window_Type;
         Scene     : Rho.Scenes.Scene_Type;
         Camera    : Rho.Cameras.Camera_Type;
         Ambient   : Rho.Lights.Ambient.Ambient_Light_Type;
         Spotlight : Rho.Lights.Spot.Spot_Light_Type;
      end record;

   overriding function Category
     (Demo : Earth_Demo_Type)
      return String
   is ("mesh");

   overriding function Name
     (Demo : Earth_Demo_Type)
      return String
   is ("earth");

   overriding function Description
     (Demo : Earth_Demo_Type)
      return String
   is ("A rotating earth");

   overriding procedure Execute
     (Demo   : not null access Earth_Demo_Type;
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
     (Demo   : not null access Earth_Demo_Type;
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

      Material : constant Rho.Material.Material_Type :=
                   Rho.Material.Simple.Create_Simple_Material
                     (Handle.Assets.Create_Texture_From_Image
                        (Rho.Paths.Config_File
                           ("images/earth-physical-4096x2048")));
      Geometry : constant Rho.Geometry.Geometry_Type :=
                   Rho.Geometry.Sphere.Sphere_Geometry
                     (Width_Segments => 64,
                      Height_Segments => 32);

      Node     : constant Rho.Nodes.Node_Type :=
                   Rho.Nodes.Create_Node;
      Mesh     : constant Rho.Meshes.Mesh_Type :=
                   Rho.Meshes.Create_Mesh (Geometry);

      Handler_Id : constant Rho.Signals.Handler_Id :=
                     Handle.Current_Renderer.Add_Handler
                       (Object  => Mesh,
                        Signal  => Rho.Handles.Signal_Before_Render,
                        Handler => On_Before_Scene_Render'Access,
                        Data    => Rho.Signals.No_Signal_Data)
        with Unreferenced;

   begin

      Scene.Set_Name ("scene");
      Camera.Set_Name ("camera");
      Mesh.Set_Name ("mesh");

      Camera.Set_Position (0.0, 0.0, 4.0);

      Mesh.Add_Material (Material);

      Node.Add (Mesh);
      Scene.Add (Node);

      Node.Rotate_X (-23.4);

      Demo.Ambient :=
        Rho.Lights.Ambient.Ambient_Light
          (Color     => (1.0, 1.0, 1.0, 1.0),
           Intensity => 0.2);

      Demo.Spotlight :=
        Rho.Lights.Spot.Spot_Light
          (Color         => Rho.Color.White,
           Intensity     => 1.0,
           Decay         => 0.001);
      Demo.Spotlight.Set_Position (-10.0, 0.0, 4.0);
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
      return new Earth_Demo_Type;
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
      --  Node.Rotate_Z (7.0 * Real (Render_Data.Time_Since_Last_Event));
      Node.Rotate_Y (-5.3 * Real (Render_Data.Time_Since_Last_Event));
   end On_Before_Scene_Render;

end Rho.Demos.Earth;
