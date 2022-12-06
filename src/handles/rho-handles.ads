private with Ada.Containers.Doubly_Linked_Lists;

with Ada.Calendar;

with Rho.Assets;
with Rho.Render;
with Rho.Signals;
with Rho.Windows;

package Rho.Handles is

   function Signal_Before_Render return Rho.Signals.Signal_Type;
   function Signal_After_Render return Rho.Signals.Signal_Type;

   type Render_Signal_Type is
     new Rho.Signals.Signal_Data_Interface with private;

   function Time_Since_Last_Event
     (Data : Render_Signal_Type'Class)
      return Duration;

   function Render_Signal_Data
     (Previous : Ada.Calendar.Time)
      return Render_Signal_Type;

   type Root_Handle_Type is abstract tagged limited private;

   function Create_Window
     (Handle : in out Root_Handle_Type;
      X      : Real;
      Y      : Real;
      Width  : Non_Negative_Real;
      Height : Non_Negative_Real;
      Full   : Boolean)
      return Rho.Windows.Window_Type
      is abstract;

   function Current_Renderer
     (Handle : Root_Handle_Type)
      return not null access Rho.Render.Render_Target'Class
      is abstract;

   procedure Main_Loop
     (Handle : in out Root_Handle_Type)
   is abstract;

   procedure Set_Active_Window
     (Handle : in out Root_Handle_Type;
      Window : Rho.Windows.Window_Type);

   function Active_Window
     (Handle : Root_Handle_Type)
      return Rho.Windows.Window_Type;

   function Assets
     (Handle : Root_Handle_Type)
      return Rho.Assets.Asset_Container_Type;

   type Handle is access all Root_Handle_Type'Class;

private

   package Window_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Windows.Window_Type, Rho.Windows."=");

   type Root_Handle_Type is abstract tagged limited
      record
         Windows : Window_Lists.List;
         Active  : Rho.Windows.Window_Type;
         Assets  : Rho.Assets.Asset_Container_Type;
      end record;

   function Active_Window
     (Handle : Root_Handle_Type)
      return Rho.Windows.Window_Type
   is (Handle.Active);

   function Assets
     (Handle : Root_Handle_Type)
      return Rho.Assets.Asset_Container_Type
   is (Handle.Assets);

   type Render_Signal_Type is
     new Rho.Signals.Signal_Data_Interface with
      record
         Previous_Event : Ada.Calendar.Time;
      end record;

   function Time_Since_Last_Event
     (Data : Render_Signal_Type'Class)
      return Duration
   is (Ada.Calendar."-" (Ada.Calendar.Clock, Data.Previous_Event));

   function Render_Signal_Data
     (Previous : Ada.Calendar.Time)
      return Render_Signal_Type
   is (Render_Signal_Type'(Previous_Event => Previous));

end Rho.Handles;
