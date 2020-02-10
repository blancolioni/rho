private with Ada.Strings.Unbounded;

with GCS.Positions;

package Tau.Objects is

   type Root_Tau_Object is abstract new Root_Tau_Node with private;

   function Name
     (Object : Root_Tau_Object)
      return String;

   procedure Initialize_Object
     (Object      : in out Root_Tau_Object'Class;
      Declaration : GCS.Positions.File_Position;
      Name        : String);

   type Tau_Object is access all Root_Tau_Object'Class;

private

   type Root_Tau_Object is abstract new Root_Tau_Node with
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Name
     (Object : Root_Tau_Object)
      return String
   is (Ada.Strings.Unbounded.To_String (Object.Object_Name));

end Tau.Objects;
