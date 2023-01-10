with Ada.Calendar;

package Rho.UI.Events is

   type Event_Type is
     (Delete,
      Motion_Notify,
      Enter_Notify,
      Leave_Notify,
      Button_Press,
      Button_Release,
      Key_Press,
      Key_Release,
      Scroll
     );

   subtype Delete_Event_Type is
     Event_Type range Delete .. Delete;

   subtype Motion_Event_Type is
     Event_Type range Motion_Notify .. Leave_Notify;

   subtype Button_Event_Type is
     Event_Type range Button_Press .. Button_Release;

   subtype Key_Event_Type is
     Event_Type range Key_Press .. Key_Release;

   subtype Scroll_Event_Type is
     Event_Type range Scroll .. Scroll;

   type Event (Class : Event_Type) is private;

   type Handler_Result is (Propagate, Stop);

   type Button_Type is (Primary, Middle, Secondary);

   type Scroll_Direction is (Up, Down, Left, Right);

   type Scroll_Unit is (Wheel, Surface);

   type Event_User_Data is interface;

   type Handler_Id is private;
   Null_Handler_Id : constant Handler_Id;

   function Enter_Notify_Event (X, Y : Real) return Event;
   function Leave_Notify_Event (X, Y : Real) return Event;
   function Motion_Notify_Event (X, Y : Real) return Event;

private

   type Event (Class : Event_Type) is
      record
         X, Y : Real;
         Time : Ada.Calendar.Time;
         case Class is
            when Delete_Event_Type =>
               null;
            when Motion_Event_Type =>
               null;
            when Button_Event_Type =>
               Button : Button_Type;
            when Key_Event_Type =>
               Key    : Character;
            when Scroll_Event_Type =>
               Direction : Scroll_Direction;
               Unit      : Scroll_Unit;
         end case;
      end record;

   type Handler_Id is new Natural;

   Null_Handler_Id : constant Handler_Id := 0;

   function Enter_Notify_Event (X, Y : Real) return Event
   is (Enter_Notify, X, Y, Ada.Calendar.Clock);

   function Leave_Notify_Event (X, Y : Real) return Event
   is (Leave_Notify, X, Y, Ada.Calendar.Clock);

   function Motion_Notify_Event (X, Y : Real) return Event
   is (Motion_Notify, X, Y, Ada.Calendar.Clock);

end Rho.UI.Events;
