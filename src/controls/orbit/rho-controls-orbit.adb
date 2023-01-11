with Rho.Signals.Buttons;
with Rho.Signals.Pointer;

package body Rho.Controls.Orbit is

   procedure On_Mouse_Button_Press
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class);

   procedure On_Mouse_Button_Release
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class);

   procedure On_Pointer_Move
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class);

   ------------
   -- Create --
   ------------

   function Create
     (Root      : not null access Rho.Nodes.Root_Node_Type'Class;
      Camera    : Rho.Cameras.Camera_Type)
      return Reference
   is
      Result : constant Reference := new Instance'
        (Parent with
         Root              => Rho.Nodes.Node_Type (Root),
         Camera            => Camera,
         Target            => Root.Position,
         others            => <>);

      User_Data : constant Orbit_User_Data :=
                    (This => Result);

      Button_Press_Id : constant Rho.Signals.Handler_Id :=
                          Root.Add_Handler
                            (Signal  => Rho.Signals.Buttons.Press_Signal,
                             Handler => On_Mouse_Button_Press'Access,
                             Data    => User_Data);

      Button_Release_Id : constant Rho.Signals.Handler_Id :=
                            Root.Add_Handler
                              (Signal  =>
                                 Rho.Signals.Buttons.Release_Signal,
                               Handler => On_Mouse_Button_Release'Access,
                               Data    => User_Data);

      Pointer_Move_Id : constant Rho.Signals.Handler_Id :=
                          Root.Add_Handler
                            (Signal  =>
                               Rho.Signals.Pointer.Move_Signal,
                             Handler => On_Pointer_Move'Access,
                             Data    => User_Data);

   begin
      Result.Button_Press_Id := Button_Press_Id;
      Result.Button_Release_Id := Button_Release_Id;
      Result.Pointer_Move_Id := Pointer_Move_Id;
      return Result;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (This : Reference) is
   begin
      null;
   end Destroy;

   ---------------------------
   -- On_Mouse_Button_Press --
   ---------------------------

   procedure On_Mouse_Button_Press
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
      Data : Rho.Signals.Buttons.Signal_Data renames
               Rho.Signals.Buttons.Signal_Data (Signal_Data);
      This : constant Reference := Orbit_User_Data'Class (User_Data).This;
   begin
      case Data.Button is
         when 0 =>
            This.Dragging := True;
            This.Last_X := Data.X;
            This.Last_Y := Data.Y;
         when others =>
            null;
      end case;
   end On_Mouse_Button_Press;

   -----------------------------
   -- On_Mouse_Button_Release --
   -----------------------------

   procedure On_Mouse_Button_Release
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
      Data : Rho.Signals.Buttons.Signal_Data renames
               Rho.Signals.Buttons.Signal_Data (Signal_Data);
      This : constant Reference := Orbit_User_Data'Class (User_Data).This;
   begin
      case Data.Button is
         when 0 =>
            This.Dragging := False;
         when others =>
            null;
      end case;
   end On_Mouse_Button_Release;

   ---------------------
   -- On_Pointer_Move --
   ---------------------

   procedure On_Pointer_Move
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
      Data : Rho.Signals.Pointer.Signal_Data renames
               Rho.Signals.Pointer.Signal_Data (Signal_Data);
      This : constant Reference := Orbit_User_Data'Class (User_Data).This;
   begin
      if This.Dragging then
         declare
            use Rho.Matrices, Rho.Spherical, Rho.Trigonometry;
            DX : constant Integer := Data.X - This.Last_X;
            DY : constant Integer := Data.Y - This.Last_Y;
            R  : constant Vector_3 := This.Camera.Position - This.Target;
            D  : constant Spherical_Position :=
                   Create (abs R,
                           From_Degrees (Real (DX) / 10.0),
                           From_Degrees (Real (DY)) / 10.0);
            S  : constant Spherical_Position := To_Spherical (R) + D;
         begin
            This.Camera.Set_Position (To_Vector (S) + This.Target);
            This.Camera.Look_At (This.Target);
         end;

         This.Last_X := Data.X;
         This.Last_Y := Data.Y;
      end if;
   end On_Pointer_Move;

end Rho.Controls.Orbit;
