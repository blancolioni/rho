private with Ada.Numerics;
private with Rho.Matrices;
private with Rho.Signals;
private with Rho.Spherical;

with Rho.Cameras;
with Rho.Handles;
with Rho.Nodes;
with Rho.Trigonometry;

package Rho.Controls.Orbit is

   subtype Parent is Rho.Controls.Instance;

   type Instance is new Parent with private;

   type Reference is access all Instance'Class;

   function Create
     (Handle    : Rho.Handles.Handle;
      Root      : not null access Rho.Nodes.Root_Node_Type'Class;
      Camera    : Rho.Cameras.Camera_Type)
      return Reference;

   procedure Destroy
     (This : Reference);

   function Polar_Angle
     (This : Instance'Class)
      return Rho.Trigonometry.Angle;

   function Azimuth_Angle
     (This : Instance'Class)
      return Rho.Trigonometry.Angle;

   function Distance
     (This : Instance'Class)
      return Non_Negative_Real;

private

   subtype Dispatch is Parent'Class;

   type Orbit_User_Data is new Rho.Signals.Signal_Data_Interface with
      record
         This : Reference;
      end record;

   type Instance is new Parent with
      record
         Root              : Rho.Nodes.Node_Type;
         Camera            : Rho.Cameras.Camera_Type;
         Target            : Rho.Matrices.Vector_3;
         Min_Distance      : Non_Negative_Real := 0.0;
         Max_Distance      : Non_Negative_Real := Non_Negative_Real'Last;
         Min_Zoom          : Non_Negative_Real := 0.0;
         Max_Zoom          : Non_Negative_Real := Non_Negative_Real'Last;
         Min_Polar_Angle   : Non_Negative_Real := 0.0;
         Max_Polar_Angle   : Non_Negative_Real := Ada.Numerics.Pi;
         Min_Azimuth_Angle : Real              := Real'First;
         Max_Azimuth_ANgle : Real              := Real'Last;
         Enable_Damping    : Boolean           := False;
         Damping_Factor    : Unit_Real         := 0.05;
         Enable_Zoom       : Boolean           := True;
         Zoom_Speed        : Non_Negative_Real := 1.0;
         Enable_Rotate     : Boolean           := True;
         Rotate_Speed      : Non_Negative_Real := 1.0;
         Enable_Pan        : Boolean           := True;
         Pan_Speed         : Non_Negative_Real := 1.0;
         Spherical_Pos     : Rho.Spherical.Spherical_Position;
         Spherical_Delta   : Rho.Spherical.Spherical_Position;
         Button_Press_Id   : Rho.Signals.Handler_Id :=
                               Rho.Signals.Null_Handler_Id;
         Button_Release_Id : Rho.Signals.Handler_Id :=
                               Rho.Signals.Null_Handler_Id;
         Pointer_Move_Id   : Rho.Signals.Handler_Id :=
                               Rho.Signals.Null_Handler_Id;
         Dragging          : Boolean := False;
         Last_X            : Integer;
         Last_Y            : Integer;
      end record;

   function Polar_Angle
     (This : Instance'Class)
      return Rho.Trigonometry.Angle
   is (Rho.Spherical.Phi (This.Spherical_Pos));

   function Azimuth_Angle
     (This : Instance'Class)
      return Rho.Trigonometry.Angle
   is (Rho.Spherical.Theta (This.Spherical_Pos));

   function Distance
     (This : Instance'Class)
      return Non_Negative_Real
   is (Rho.Spherical.Radius (This.Spherical_Pos));

end Rho.Controls.Orbit;
