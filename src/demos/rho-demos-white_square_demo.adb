with Tau.Shaders.Library;

with Rho.Cameras;
with Rho.Color;
with Rho.Geometry.Box;
with Rho.Material.Basic;
with Rho.Meshes;
with Rho.Scenes;

package body Rho.Demos.White_Square_Demo is

   type White_Square_Demo_Type is
     new Root_Demo_Type with
      record
         Shader : Tau.Shaders.Tau_Shader;
      end record;

   overriding function Category
     (Demo : White_Square_Demo_Type)
      return String
   is ("static");

   overriding function Name
     (Demo : White_Square_Demo_Type)
      return String
   is ("white_square");

   overriding function Description
     (Demo : White_Square_Demo_Type)
      return String
   is ("A static white square");

   overriding procedure Execute
     (Demo   : not null access White_Square_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Demo   : not null access White_Square_Demo_Type;
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
      Geometry : constant Rho.Geometry.Geometry_Type :=
        Rho.Geometry.Box.Box_Geometry;
      Material : constant Rho.Material.Material_Type :=
        Rho.Material.Basic.Create_Basic_Material
          (Color => (1.0, 1.0, 1.0, 1.0));
      Mesh     : constant Rho.Meshes.Mesh_Type :=
        Rho.Meshes.Create_Mesh (Geometry, Material);

   begin
      Scene.Set_Name ("scene");
      Camera.Set_Name ("camera");
      Mesh.Set_Name ("mesh");
      Camera.Set_Position (0.0, 0.0, 3.0);
      Scene.Add (Mesh);

      Window.Set_Scene (Scene);
      Window.Set_Camera (Camera);

      Demo.Start (Handle, Window, Scene);
      Handle.Main_Loop;
   end Execute;

   ----------
   -- Load --
   ----------

   function Load return Demo_Type is
   begin
      return new White_Square_Demo_Type'
        (Frame_Count => 0,
         Elapsed     => 0.0,
         Shader      =>
           Tau.Shaders.Library.Single_Color_Shader
             (Color => Rho.Color.White));
   end Load;

end Rho.Demos.White_Square_Demo;
