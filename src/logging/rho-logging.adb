with Ada.Text_IO;

package body Rho.Logging is

   Log_File     : Ada.Text_IO.File_Type;
   Log_Started  : Boolean := False;
   Log_Priority : Message_Priority := Default_Minimum_Priority;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      Log (Default_Minimum_Priority, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Priority : Message_Priority; Message : String) is
   begin
      if Log_Started
        and then Priority >= Log_Priority
      then
         Ada.Text_IO.Put_Line
           (Log_File, Message);
      end if;
   end Log;

   ----------------------
   -- Set_Log_Priority --
   ----------------------

   procedure Set_Log_Priority (Minimum_Priority : Message_Priority) is
   begin
      Log_Priority := Minimum_Priority;
   end Set_Log_Priority;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging
     (Priority : Message_Priority := Default_Minimum_Priority)
   is
   begin
      Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File, "rho.log");
      Log_Started := True;
      Log_Priority := Priority;
   end Start_Logging;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      Ada.Text_IO.Close (Log_File);
      Log_Started := False;
   end Stop_Logging;

end Rho.Logging;
