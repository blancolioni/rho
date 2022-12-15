with Rho.Handles;
with Rho.UI.Widget.Main_Root;

package body Rho.Windows is

   ------------
   -- Add_UI --
   ------------

   procedure Add_UI
     (Window : not null access Root_Window_Type'Class;
      Top    : Rho.UI.Widget.Reference)
   is
   begin
      Window.UIs.Append (Top);
      Top.Configure;
      Top.Map (Window);
   end Add_UI;

   -----------------------
   -- Initialize_Window --
   -----------------------

   procedure Initialize_Window
     (Window        : in out Root_Window_Type'Class;
      X, Y          : Real;
      Width, Height : Non_Negative_Real)
   is
   begin
      Window.Initialize_Rectangle (X, Y, Width, Height);
   end Initialize_Window;

   ---------------
   -- Remove_UI --
   ---------------

   procedure Remove_UI
     (Window : not null access Root_Window_Type'Class;
      Top    : Rho.UI.Widget.Reference)
   is
      use Rho_Widget_Lists;
      Position : Cursor := Window.UIs.Find (Top);
   begin
      if Has_Element (Position) then
         Window.UIs.Delete (Position);
      end if;
   end Remove_UI;

   ------------
   -- Render --
   ------------

   procedure Render
     (Window : in out Root_Window_Type)
   is
      W : Root_Window_Type'Class renames Root_Window_Type'Class (Window);
      Target : constant access Rho.Render.Render_Target'Class :=
        W.Render_Target;

   begin

      Target.Emit_Signal
        (null,  Rho.Handles.Signal_Before_Render,
         Rho.Handles.Render_Signal_Data (W.Before_Render_Time));

      W.Before_Render_Time := Ada.Calendar.Clock;

      W.Before_Render;

      Window.Camera.Render (Target);
      Window.Camera.Set_Root_Object (Window.Scene);
      Window.Scene.Render (Target);

      if not W.UIs.Is_Empty then
         declare
            use type Rho.Cameras.Camera_Type;
         begin
            if W.UI_Camera = null then
               W.UI_Camera :=
                 Rho.Cameras.Orthographic_Camera
                   (0.0, 0.0, W.Width, W.Height);
            end if;
         end;

         W.UI_Camera.Render (Target);
         for UI of W.UIs loop
            Rho.UI.Widget.Main_Root.Any_Instance (UI.all)
              .Root_Node.Render (Target);
         end loop;
      end if;

      W.After_Render;

      Target.Emit_Signal
        (null,  Rho.Handles.Signal_After_Render,
         Rho.Handles.Render_Signal_Data (W.After_Render_Time));
      W.After_Render_Time := Ada.Calendar.Clock;

      W.Wireframe_Changed := False;
   end Render;

   ----------------
   -- Set_Camera --
   ----------------

   procedure Set_Camera
     (Window : in out Root_Window_Type'Class;
      Camera : not null access Rho.Cameras.Root_Camera_Type'Class)
   is
   begin
      Window.Camera := Rho.Cameras.Camera_Type (Camera);
   end Set_Camera;

   ---------------------
   -- Set_Clear_Color --
   ---------------------

   procedure Set_Clear_Color
     (Window : in out Root_Window_Type'Class;
      Color  : Rho.Color.Color_Type)
   is
   begin
      Window.Clear_Color := Color;
   end Set_Clear_Color;

   ---------------
   -- Set_Scene --
   ---------------

   procedure Set_Scene
     (Window : in out Root_Window_Type'Class;
      Scene  : not null access Rho.Scenes.Root_Scene_Type'Class)
   is
   begin
      Window.Scene := Rho.Scenes.Scene_Type (Scene);
   end Set_Scene;

   -------------------
   -- Set_Wireframe --
   -------------------

   procedure Set_Wireframe
     (Window : in out Root_Window_Type'Class;
      Value  : Boolean)
   is
   begin
      Window.Wireframe_Changed :=
        Window.Wireframe_Changed
          or else (Value /= Window.Current_Wireframe);
      Window.Current_Wireframe := Value;
   end Set_Wireframe;

end Rho.Windows;
