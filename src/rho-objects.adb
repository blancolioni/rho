with Ada.Strings.Fixed;

with WL.String_Maps;

package body Rho.Objects is

   function To_Name
     (Base  : String;
      Index : Positive)
      return String;

   package Name_Index_Maps is
     new WL.String_Maps (Positive);

   protected Names is
      procedure Get_Next_Name
        (Base : String;
         Name : out Ada.Strings.Unbounded.Unbounded_String);
   private
      Name_Index_Map : Name_Index_Maps.Map;
   end Names;

   protected body Names is

      -------------------
      -- Get_Next_Name --
      -------------------

      procedure Get_Next_Name
        (Base : String;
         Name : out Ada.Strings.Unbounded.Unbounded_String)
      is
         use Ada.Strings.Unbounded;
         use Name_Index_Maps;
         Position : constant Cursor := Name_Index_Map.Find (Base);
         Index    : Positive := 1;
      begin
         if Has_Element (Position) then
            declare
               N : Positive renames Name_Index_Map (Position);
            begin
               N := N + 1;
               Index := N;
            end;
         else
            Name_Index_Map.Insert (Base, Index);
         end if;

         Name := To_Unbounded_String (To_Name (Base, Index));
      end Get_Next_Name;

   end Names;

   procedure Ref
     (This : not null access Root_Object_Type)
   is null;

   procedure Unref
     (This : not null access Root_Object_Type)
   is null;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (Object   : in out Root_Object_Type;
      Signal   : Rho.Signals.Signal_Type;
      Handler  : Rho.Signals.Handler_Type;
      Data     : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Id
   is
   begin
      return Object.Dispatcher.Add_Handler
        (Signal, Handler, Data);
   end Add_Handler;

   -----------------
   -- Emit_Signal --
   -----------------

   overriding procedure Emit_Signal
     (Object   : Root_Object_Type;
      Signal   : Rho.Signals.Signal_Type;
      Data     : Rho.Signals.Signal_Data_Interface'Class)
   is
   begin
      Object.Dispatcher.Emit_Signal (Signal, Data);
   end Emit_Signal;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Root_Object_Type) is
   begin
      Names.Get_Next_Name ("object", Object.Name);
   end Initialize;

   ------------------------
   -- Initialize_Signals --
   ------------------------

   procedure Initialize_Signals
     (This : not null access Root_Object_Type)
   is
   begin
      This.Dispatcher.Initialize (This);
   end Initialize_Signals;

   --------------------
   -- Remove_Handler --
   --------------------

   overriding procedure Remove_Handler
     (Object   : in out Root_Object_Type;
      Signal   : Rho.Signals.Signal_Type;
      Id       : Rho.Signals.Handler_Id)
   is
   begin
      Object.Dispatcher.Remove_Handler (Signal, Id);
   end Remove_Handler;

   ----------------
   -- Set_Loaded --
   ----------------

   procedure Set_Loaded (Object : in out Root_Object_Type'Class) is
   begin
      Object.Loaded := True;
   end Set_Loaded;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Object : in out Root_Object_Type'Class;
      Name   : String)
   is
   begin
      Object.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (Base  : String;
      Index : Positive)
      return String
   is
   begin
      return Base & "_"
        & Ada.Strings.Fixed.Trim (Index'Image, Ada.Strings.Left);
   end To_Name;

end Rho.Objects;
