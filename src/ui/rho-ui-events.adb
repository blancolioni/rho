package body Rho.UI.Events is

   type Null_Event_Data_Record is new Event_User_Data with null record;

   --------------------------
   -- Null_Event_User_Data --
   --------------------------

   function Null_Event_User_Data return Event_User_Data'Class is
      This : constant Null_Event_Data_Record := (null record);
   begin
      return This;
   end Null_Event_User_Data;

end Rho.UI.Events;
