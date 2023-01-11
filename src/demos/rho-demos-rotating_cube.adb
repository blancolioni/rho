with Rho.Geometry.Box;
with Rho.Meshes;
with Rho.Scenes;
with Rho.Signals;

package body Rho.Demos.Rotating_Cube is

   function On_Before_Scene_Render
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Result;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Demo     : not null access Rotating_Cube_Demo'Class;
      Handle   : Rho.Handles.Handle;
      Window   : Rho.Windows.Window_Type;
      Material : Rho.Material.Reference)
   is
   begin
      Demo.Initialize (Handle, Window, (1 => Material));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Demo     : not null access Rotating_Cube_Demo'Class;
      Handle   : Rho.Handles.Handle;
      Window   : Rho.Windows.Window_Type;
      Material : Rho.Material.Reference_Array)
   is
      Scene    : constant Rho.Scenes.Scene_Type :=
        Rho.Scenes.Create_Scene;
      Camera   : constant Rho.Cameras.Camera_Type :=
        Rho.Cameras.Perspective_Camera
          (Field_Of_View => 45.0,
           Aspect_Ratio  => Window.Aspect_Ratio,
           Near          => 0.1,
           Far           => 100.0);
      Geometry : constant Rho.Geometry.Geometry_Type :=
        Rho.Geometry.Box.Box_Geometry;
      Mesh     : constant Rho.Meshes.Mesh_Type :=
        Rho.Meshes.Create_Mesh (Geometry);

      Handler_Id : constant Rho.Signals.Handler_Id :=
        Mesh.Add_Handler
          (Signal  => Rho.Handles.Signal_Before_Render,
           Handler => On_Before_Scene_Render'Access,
           Data    => Rho.Signals.No_Signal_Data)
        with Unreferenced;

   begin

      Scene.Set_Name ("scene");
      Camera.Set_Name ("camera");
      Mesh.Set_Name ("mesh");
      Camera.Set_Position (0.0, 0.0, 4.0);

      for M of Material loop
         Mesh.Add_Material (M);
      end loop;

      Scene.Add (Mesh);

      Demo.Handle := Handle;
      Demo.Window := Window;
      Demo.Camera := Camera;
      Demo.Scene := Scene;

      Window.Set_Scene (Demo.Scene);
      Window.Set_Camera (Demo.Camera);

      Demo.Start (Handle, Window, Demo.Scene);

   end Initialize;

   ----------------------------
   -- On_Before_Scene_Render --
   ----------------------------

   function On_Before_Scene_Render
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Result
   is
      pragma Unreferenced (User_Data);
      Render_Data : Rho.Handles.Render_Signal_Type renames
                      Rho.Handles.Render_Signal_Type (Signal_Data);
      Mesh : constant Rho.Meshes.Mesh_Type :=
        Rho.Meshes.Mesh_Type (Object);
   begin
      Mesh.Rotate_Z (7.0 * Real (Render_Data.Time_Since_Last_Event));
      Mesh.Rotate_Y (5.3 * Real (Render_Data.Time_Since_Last_Event));
      return Rho.Signals.Propagate;
   end On_Before_Scene_Render;

   -----------
   -- Start --
   -----------

   procedure Start
     (Demo     : not null access Rotating_Cube_Demo'Class)
   is
   begin
      Demo.Handle.Main_Loop;
   end Start;

end Rho.Demos.Rotating_Cube;
