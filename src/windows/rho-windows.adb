with Rho.Handles;
with Rho.UI.Events;
with Rho.UI.Surface;

with Rho.Signals.Pointer;

package body Rho.Windows is

   function On_Mouse_Move
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Result;

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

      declare
         Surface : constant Rho.UI.Surface.Reference :=
                     Rho.UI.Surface.Create
                       (Real (Top.Get_Layout_Position.X),
                        Real (Top.Get_Layout_Position.Y),
                        Real (Top.Get_Layout_Size.Width),
                        Real (Top.Get_Layout_Size.Height));
      begin
         Top.Map (Surface);
      end;

   end Add_UI;

   -----------------------
   -- Initialize_Window --
   -----------------------

   procedure Initialize_Window
     (Window        : not null access Root_Window_Type'Class;
      X, Y          : Real;
      Width, Height : Non_Negative_Real)
   is
   begin
      Window.Initialize_Rectangle (X, Y, Width, Height);
      Window.Initialize_Signals;
      Window.Mouse_Move_Handler :=
        Window.Add_Handler
          (Signal  => Rho.Signals.Pointer.Move_Signal,
           Handler => On_Mouse_Move'Access,
           Data    => Rho.Signals.No_Signal_Data);
   end Initialize_Window;

   -------------------
   -- On_Mouse_Move --
   -------------------

   function On_Mouse_Move
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Result
   is
      pragma Unreferenced (User_Data);
      use Rho.UI.Widget;
      Window : constant Window_Type := Window_Type (Object);
      Data   : constant Rho.Signals.Pointer.Signal_Data :=
                 Rho.Signals.Pointer.Signal_Data (Signal_Data);
      X      : constant Real := Real (Data.X);
      Y      : constant Real := Real (Data.Y);
      Old_W  : constant Rho.UI.Widget.Reference := Window.Current_Widget;
   begin

      Window.Current_Widget := null;

      for UI of Window.UIs loop
         declare
            W : constant Reference :=
                  UI.Get_Widget_At_Point (X, Y);
         begin
            if W /= null then
               Window.Current_Widget := W;
               if Old_W /= W then
                  if Old_W /= null then
                     Old_W.Send (Rho.UI.Events.Leave_Notify_Event (X, Y));
                  end if;
                  W.Send (Rho.UI.Events.Enter_Notify_Event (X, Y));
               else
                  if X /= Window.Pointer_X or else Y /= Window.Pointer_Y then
                     W.Send (Rho.UI.Events.Motion_Notify_Event (X, Y));
                     Window.Pointer_X := X;
                     Window.Pointer_Y := Y;
                  end if;
               end if;

               exit;
            end if;
         end;
      end loop;

      Window.Scene.Emit_Signal (Rho.Signals.Pointer.Move_Signal, Signal_Data);

      return Rho.Signals.Propagate;

   end On_Mouse_Move;

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

      W.Emit_Signal
        (Rho.Handles.Signal_Before_Render,
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
                   (0.0, W.Height, W.Width, -W.Height);
            end if;
         end;

         W.UI_Camera.Render (Target);
         for UI of W.UIs loop
            UI.Show (Target);
         end loop;
      end if;

      W.After_Render;

      W.Emit_Signal
        (Rho.Handles.Signal_After_Render,
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
