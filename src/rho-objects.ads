private with Ada.Strings.Unbounded;

with WL.Guids;

with Ada.Finalization;

with Rho.Signals;

package Rho.Objects is

   type Root_Object_Type is
     abstract new Ada.Finalization.Limited_Controlled
     and Rho.Signals.Signal_Object_Interface
   with private;

   overriding function Guid
     (Object : Root_Object_Type)
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

   procedure Reference
     (This : not null access Root_Object_Type);

   procedure Unreference
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
      end record;

   overriding procedure Initialize (Object : in out Root_Object_Type);

   overriding function Guid
     (Object : Root_Object_Type)
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
