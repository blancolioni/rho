with Ada.Exceptions;
with Ada.Text_IO;

package body Rho.Signals is

   type No_Data_Record is new Signal_Data_Interface with null record;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (Dispatch : in out Signal_Dispatcher;
      Object   : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal   : Rho.Signals.Signal_Type;
      Handler  : Rho.Signals.Handler_Type;
      Data     : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Id
   is
      S : constant String := Signal_Name (Signal);
   begin
      if not Dispatch.Map.Contains (S) then
         Dispatch.Map.Insert (S, Handler_Lists.Empty_List);
      end if;

      return Id : constant Handler_Id := Dispatch.Next_Id do
         Dispatch.Next_Id := Dispatch.Next_Id + 1;
         Dispatch.Map (S).Append
           (Handler_Record'
              (Id      => Id,
               Handler => Handler,
               Object  => Signal_Object_Access (Object),
               Data    => Data_Holder.To_Holder (Data)));
      end return;
   end Add_Handler;

   -----------------
   -- Emit_Signal --
   -----------------

   overriding procedure Emit_Signal
     (Dispatch : in out Signal_Dispatcher;
      Object   : access Rho.Signals.Signal_Object_Interface'Class;
      Signal   : Rho.Signals.Signal_Type;
      Data     : Rho.Signals.Signal_Data_Interface'Class)
   is
      S : constant String := Signal_Name (Signal);
   begin
      if Dispatch.Map.Contains (S) then
         declare
            List : Handler_Lists.List renames Dispatch.Map (S);
         begin
            for Handler of List loop
               declare
                  use type WL.Guids.Guid;
               begin
                  if Object = null
                    or else Handler.Object.Guid = Object.Guid
                  then
                     Handler.Handler
                       (Handler.Object, Data, Handler.Data.Element);
                  end if;
               exception
                  when E : others =>
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "exception while calling handler"
                        & Handler.Id'Image
                        & " for "
                        & S
                        & ": "
                        & Ada.Exceptions.Exception_Message (E));
               end;
            end loop;
         end;
      end if;
   end Emit_Signal;

   --------------------
   -- No_Signal_Data --
   --------------------

   function No_Signal_Data return Signal_Data_Interface'Class is
   begin
      return Result : No_Data_Record;
   end No_Signal_Data;

   --------------------
   -- Remove_Handler --
   --------------------

   overriding procedure Remove_Handler
     (Dispatch : in out Signal_Dispatcher;
      Signal   : Rho.Signals.Signal_Type;
      Id       : Rho.Signals.Handler_Id)
   is
      S    : constant String := Signal_Name (Signal);
      List : Handler_Lists.List renames Dispatch.Map (S);
      Pos  : Handler_Lists.Cursor := Handler_Lists.No_Element;
   begin
      for Position in List.Iterate loop
         if Handler_Lists.Element (Position).Id = Id then
            Pos := Position;
            exit;
         end if;
      end loop;

      if Handler_Lists.Has_Element (Pos) then
         List.Delete (Pos);
      else
         raise Constraint_Error with
           "no such handler" & Id'Image & " for signal " & S;
      end if;
   end Remove_Handler;

end Rho.Signals;
