with Rho.Cameras;
with Rho.Color;
with Rho.Devices.Keyboard;
with Rho.Lights.Ambient;
with Rho.Lights.Spot;
with Rho.Material.Simple;
with Rho.Matrices;
with Rho.Nodes;
with Rho.Signals.Keyboard;

with Rho.Loaders.Dat;

with Rho.Paths;

package body Rho.Demos.Cobra_Demo is

   type Cobra_Demo_Type is new Root_Demo_Type with
      record
         Handle    : Rho.Handles.Handle;
         Window    : Rho.Windows.Window_Type;
         Scene     : Rho.Scenes.Scene_Type;
         Node      : Rho.Nodes.Node_Type;
         Ambient   : Rho.Lights.Ambient.Ambient_Light_Type;
         Spotlight : Rho.Lights.Spot.Spot_Light_Type;
         Look_At   : Rho.Matrices.Vector_3;
      end record;

   type Cobra_Demo_Access is access all Cobra_Demo_Type'Class;

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

   type Event_User_Data is
     new Rho.Signals.Signal_Data_Interface with
      record
         Demo : Cobra_Demo_Access;
      end record;

   procedure On_Key_Press
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

      Mat_1 : constant Rho.Material.Reference :=
                Rho.Material.Simple.Create
                  (Handle.Assets.Create_Texture_From_Image
                     ("config/textures/oolite_cobra3_diffuse"));
      Mat_2 : constant Rho.Material.Reference :=
                Rho.Material.Simple.Create
                  (Handle.Assets.Create_Texture_From_Image
                     ("config/textures/oolite_cobra3_subents"));

      Node : constant Rho.Nodes.Node_Type :=
               Rho.Loaders.Dat.Load
                 (Path   => Rho.Paths.Config_File
                    ("models/oolite_cobra3.dat"),
                  Material => (Mat_1, Mat_2));

      User_Data    : constant Event_User_Data :=
                       Event_User_Data'(Demo => Cobra_Demo_Access (Demo));
      Key_Press_Id : constant Rho.Signals.Handler_Id :=
                       Handle.Current_Renderer.Add_Handler
                         (Object  => Scene,
                          Signal  =>
                            Rho.Signals.Keyboard.Press_Signal,
                          Handler => On_Key_Press'Access,
                          Data    => User_Data)
        with Unreferenced;
   begin

      Scene.Set_Name ("scene");
      Camera.Set_Name ("camera");
      Camera.Set_Position (0.0, 0.0, 200.0);

      Demo.Look_At := Rho.Matrices.To_Vector (0.0, 0.0, 0.0);
      Camera.Look_At (Demo.Look_At);
      --  Node.Look_At (Demo.Look_At);

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
      Demo.Node := Node;

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

   ------------------
   -- On_Key_Press --
   ------------------

   procedure On_Key_Press
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
      use Rho.Matrices;
      Data : Rho.Signals.Keyboard.Signal_Data renames
               Rho.Signals.Keyboard.Signal_Data (Signal_Data);
      Demo : constant Cobra_Demo_Access :=
               Event_User_Data (User_Data).Demo;
   begin
      case Data.Key is
         when Rho.Devices.Keyboard.Up =>
            Demo.Look_At :=
              Demo.Look_At + To_Vector (0.0, 1.0, 0.0);
            Demo.Camera.Look_At (Demo.Look_At);
         when Rho.Devices.Keyboard.Down =>
            Demo.Look_At :=
              Demo.Look_At + To_Vector (0.0, -1.0, 0.0);
            Demo.Camera.Look_At (Demo.Look_At);
         when others =>
            null;
      end case;
   end On_Key_Press;

end Rho.Demos.Cobra_Demo;
