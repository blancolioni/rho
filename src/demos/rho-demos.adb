with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.String_Maps;

with Rho.Controls.Orbit;
with Rho.Signals;

--  with Rho.Demos.Ambient_Light;
with Rho.Demos.Cobra_Demo;
with Rho.Demos.Cone;
with Rho.Demos.Cube_Demo;
--  with Rho.Demos.Depth_Demo;
with Rho.Demos.Earth;
with Rho.Demos.Spotlight_Demo;
with Rho.Demos.Texture_Demo;
with Rho.Demos.White_Square_Demo;

with Rho.Demos.Rho_UI.Background;

package body Rho.Demos is

   package Demo_Maps is
     new WL.String_Maps (Demo_Type);

   package Demo_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Demo_Type);

   Map : Demo_Maps.Map;
   List : Demo_Lists.List;

   type Demo_Signal_Data is
     new Rho.Signals.Signal_Data_Interface with
      record
         Demo : Demo_Type;
      end record;

   procedure Update_FPS
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class);

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
   begin
      return Map.Contains (Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Demo_Type is
   begin
      return Map.Element (Name);
   end Get;

   -------------------
   -- Iterate_Demos --
   -------------------

   procedure Iterate_Demos
     (Process : not null access procedure (Demo : Demo_Type))
   is
   begin
      for Demo of List loop
         Process (Demo);
      end loop;
   end Iterate_Demos;

   ----------------
   -- Load_Demos --
   ----------------

   procedure Load_Demos is

      procedure Load (Demo : Demo_Type);

      ----------
      -- Load --
      ----------

      procedure Load (Demo : Demo_Type) is
      begin
         Map.Insert (Demo.Name, Demo);
         List.Append (Demo);
      end Load;

   begin
      Load (White_Square_Demo.Load);
--        Load (Ambient_Light.Load);
      Load (Cone.Load);
      Load (Cube_Demo.Load);
      Load (Cobra_Demo.Load);
      Load (Earth.Load);
      --  Load (Depth_Demo.Load);
      Load (Spotlight_Demo.Load);
      Load (Texture_Demo.Load);
      Load (Rho_UI.Background.Load);
   end Load_Demos;

   -----------
   -- Start --
   -----------

   procedure Start
     (Demo   : not null access Root_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type;
      Scene  : Rho.Scenes.Scene_Type)
   is
      Orbit_Control : constant Rho.Controls.Orbit.Reference :=
                        Rho.Controls.Orbit.Create
                          (Handle, Scene, Demo.Camera)
                          with Unreferenced;
      Handler_Id : constant Rho.Signals.Handler_Id :=
        Handle.Current_Renderer.Add_Handler
          (Object  => Scene,
           Signal  => Rho.Handles.Signal_Before_Render,
           Handler => Update_FPS'Access,
           Data    =>
             Demo_Signal_Data'(Demo => Demo_Type (Demo)))
        with Unreferenced;

   begin
      null;
   end Start;

   ----------------
   -- Update_FPS --
   ----------------

   procedure Update_FPS
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
      Demo : constant Demo_Type :=
        Demo_Signal_Data (User_Data).Demo;
      Render_Data : Rho.Handles.Render_Signal_Type renames
        Rho.Handles.Render_Signal_Type (Signal_Data);
   begin
      Demo.Frame_Count := Demo.Frame_Count + 1;
      Demo.Elapsed := Demo.Elapsed + Render_Data.Time_Since_Last_Event;

      if Demo.Elapsed > 2.0 then
         Ada.Text_IO.Put_Line
           ("fps:"
            & Natural'Image
              (Natural (Real (Demo.Frame_Count) / Real (Demo.Elapsed))));
         Demo.Elapsed := 0.0;
         Demo.Frame_Count := 0;
      end if;
   end Update_FPS;

end Rho.Demos;
