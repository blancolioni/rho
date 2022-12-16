private with Ada.Calendar;
private with Ada.Containers.Doubly_Linked_Lists;

with Rho.Cameras;
with Rho.Color;
with Rho.Rectangles;
with Rho.Render;
with Rho.Scenes;

with Rho.UI.Widget;

package Rho.Windows is

   type Root_Window_Type is
     abstract new Rho.Rectangles.Root_Rectangle_Type with private;

   function Render_Target
     (Window : Root_Window_Type)
      return access Rho.Render.Render_Target'Class
      is abstract;

   procedure Before_Render
     (Window : in out Root_Window_Type)
   is null;

   procedure After_Render
     (Window : in out Root_Window_Type)
   is null;

   procedure Initialize_Window
     (Window        : in out Root_Window_Type'Class;
      X, Y          : Real;
      Width, Height : Non_Negative_Real);

   function Wireframe
     (Window : Root_Window_Type'Class)
      return Boolean;

   procedure Set_Wireframe
     (Window : in out Root_Window_Type'Class;
      Value  : Boolean);

   function Clear_Color
     (Window : Root_Window_Type'Class)
      return Rho.Color.Color_Type;

   procedure Set_Clear_Color
     (Window : in out Root_Window_Type'Class;
      Color  : Rho.Color.Color_Type);

   procedure Set_Camera
     (Window : in out Root_Window_Type'Class;
      Camera : not null access Rho.Cameras.Root_Camera_Type'Class);

   function Scene
     (Window : Root_Window_Type'Class)
      return Rho.Scenes.Scene_Type;

   procedure Set_Scene
     (Window : in out Root_Window_Type'Class;
      Scene  : not null access Rho.Scenes.Root_Scene_Type'Class);

   procedure Render
     (Window : in out Root_Window_Type);

   type Window_Type is access all Root_Window_Type'Class;

   procedure Add_UI
     (Window : not null access Root_Window_Type'Class;
      Top    : Rho.UI.Widget.Reference);

   procedure Remove_UI
     (Window : not null access Root_Window_Type'Class;
      Top    : Rho.UI.Widget.Reference);

private

   package Rho_Widget_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.UI.Widget.Reference, Rho.UI.Widget."=");

   type Root_Window_Type is
     abstract new Rho.Rectangles.Root_Rectangle_Type with
      record
         Clear_Color        : Rho.Color.Color_Type :=
           (0.0, 0.0, 0.0, 1.0);
         Camera             : Rho.Cameras.Camera_Type;
         UI_Camera          : Rho.Cameras.Camera_Type;
         Scene              : Rho.Scenes.Scene_Type;
         Wireframe_Changed  : Boolean := True;
         Current_Wireframe  : Boolean := False;
         Before_Render_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
         After_Render_Time  : Ada.Calendar.Time := Ada.Calendar.Clock;
         UIs                : Rho_Widget_Lists.List;
      end record;

   function Clear_Color
     (Window : Root_Window_Type'Class)
      return Rho.Color.Color_Type
   is (Window.Clear_Color);

   function Scene
     (Window : Root_Window_Type'Class)
      return Rho.Scenes.Scene_Type
   is (Window.Scene);

   function Wireframe
     (Window : Root_Window_Type'Class)
      return Boolean
   is (Window.Current_Wireframe);

end Rho.Windows;
