with Ada.Exceptions;
with Ada.Text_IO;

package body Rho.Signals is

   type No_Data_Record is new Signal_Data_Interface with null record;

   -----------------
   -- Add_Handler --
   -----------------

   function Add_Handler
     (This     : in out Signal_Dispatcher'Class;
      Signal   : Signal_Type;
      Handler  : Handler_Type;
      Data     : Signal_Data_Interface'Class)
      return Handler_Id
   is
      S : constant String := Signal_Name (Signal);
   begin
      if not This.Map.Contains (S) then
         This.Map.Insert (S, Handler_Lists.Empty_List);
      end if;

      return Id : constant Handler_Id := This.Next_Id do
         This.Next_Id := This.Next_Id + 1;
         This.Map (S).Append
           (Handler_Record'
              (Id      => Id,
               Handler => Handler,
               Data    => Data_Holder.To_Holder (Data)));
      end return;
   end Add_Handler;

   -----------------
   -- Emit_Signal --
   -----------------

   procedure Emit_Signal
     (This     : Signal_Dispatcher'Class;
      Signal   : Signal_Type;
      Data     : Signal_Data_Interface'Class)
   is
      S : constant String := Signal_Name (Signal);
   begin
      if This.Map.Contains (S) then
         declare
            List : Handler_Lists.List renames This.Map (S);
         begin
            for Handler of reverse List loop
               begin
                  Handler.Handler
                    (This.Object, Data, Handler.Data.Element);
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This   : in out Signal_Dispatcher'Class;
      Object : not null access Signal_Object_Interface'Class)
   is
   begin
      This.Object := Signal_Object_Access (Object);
   end Initialize;

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

   procedure Remove_Handler
     (This     : in out Signal_Dispatcher'Class;
      Signal   : Signal_Type;
      Id       : Handler_Id)
   is
      S    : constant String := Signal_Name (Signal);
      List : Handler_Lists.List renames This.Map (S);
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
