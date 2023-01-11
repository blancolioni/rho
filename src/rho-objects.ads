private with Ada.Strings.Unbounded;

with WL.Guids;

with Ada.Finalization;

with Rho.Signals;

package Rho.Objects is

   type Root_Object_Type is
     abstract new Ada.Finalization.Limited_Controlled
     and Rho.Signals.Signal_Object_Interface
   with private;

   overriding procedure Emit_Signal
     (Object   : Root_Object_Type;
      Signal   : Rho.Signals.Signal_Type;
      Data     : Rho.Signals.Signal_Data_Interface'Class);

   overriding function Add_Handler
     (Object   : in out Root_Object_Type;
      Signal   : Rho.Signals.Signal_Type;
      Handler  : Rho.Signals.Handler_Type;
      Data     : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Id;

   overriding procedure Remove_Handler
     (Object   : in out Root_Object_Type;
      Signal   : Rho.Signals.Signal_Type;
      Id       : Rho.Signals.Handler_Id);

   procedure Initialize_Signals
     (This : not null access Root_Object_Type);

   function Guid
     (Object : Root_Object_Type'Class)
      return WL.Guids.Guid;

   function Name
     (Object : Root_Object_Type'Class)
      return String;

   function Class_Name
     (Object : Root_Object_Type)
      return String
      is abstract;

   procedure Set_Name
     (Object : in out Root_Object_Type'Class;
      Name   : String);

   function Is_Loaded
     (Object : Root_Object_Type'Class)
      return Boolean;

   procedure Set_Loaded (Object : in out Root_Object_Type'Class);

   function Same_As
     (Left  : Root_Object_Type'Class;
      Right : not null access Root_Object_Type'Class)
      return Boolean;

   procedure Ref
     (This : not null access Root_Object_Type);

   procedure Unref
     (This : not null access Root_Object_Type);

private

   subtype Object_Name is Ada.Strings.Unbounded.Unbounded_String;

   type Root_Object_Type is
     abstract new Ada.Finalization.Limited_Controlled
     and Rho.Signals.Signal_Object_Interface with
      record
         Guid       : WL.Guids.Guid := WL.Guids.New_Guid;
         Name       : Object_Name;
         Loaded     : Boolean       := False;
         References : Natural := 0;
         Dispatcher : Rho.Signals.Signal_Dispatcher;
      end record;

   overriding procedure Initialize (Object : in out Root_Object_Type);

   function Guid
     (Object : Root_Object_Type'Class)
      return WL.Guids.Guid
   is (Object.Guid);

   function Is_Loaded
     (Object : Root_Object_Type'Class)
      return Boolean
   is (Object.Loaded);

   function Same_As
     (Left  : Root_Object_Type'Class;
      Right : not null access Root_Object_Type'Class)
      return Boolean
   is (WL.Guids."=" (Left.Guid, Right.Guid));

   function Name
     (Object : Root_Object_Type'Class)
      return String
   is (Ada.Strings.Unbounded.To_String (Object.Name));

end Rho.Objects;
