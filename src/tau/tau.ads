private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with GCS.Positions;

package Tau is

   type Root_Tau_Node is abstract tagged private;

   function Class_Name
     (Node : Root_Tau_Node)
      return String
      is abstract;

   function Defined_At
     (Node : Root_Tau_Node'Class)
      return GCS.Positions.File_Position;

   function Has_Errors
     (Node : Root_Tau_Node'Class)
      return Boolean;

   procedure Error
     (Node    : in out Root_Tau_Node'Class;
      Message : String);

   procedure Clear_Errors
     (Node    : in out Root_Tau_Node'Class);

   type Tau_Node is access all Root_Tau_Node'Class;
   type Constant_Tau_Node is access constant Root_Tau_Node'Class;

   procedure Iterate_Errors
     (Node    : not null access constant Root_Tau_Node'Class;
      Process : not null access
        procedure (Node : Constant_Tau_Node;
                   Message : String));

   type Tau_Node_Array is array (Positive range <>) of Tau_Node;

   function Children
     (Node : Root_Tau_Node)
      return Tau_Node_Array;

private

   package Error_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Root_Tau_Node is abstract tagged
      record
         File_Position : GCS.Positions.File_Position;
         Errors        : Error_Lists.List;
      end record;

   procedure Initialize_Node
     (Object      : in out Root_Tau_Node'Class;
      Declaration : GCS.Positions.File_Position);

   function Defined_At
     (Node : Root_Tau_Node'Class)
      return GCS.Positions.File_Position
   is (Node.File_Position);

   subtype Tau_String is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (X : String) return Tau_String
   is (Ada.Strings.Unbounded.To_Unbounded_String (X));

   function "-" (X : Tau_String) return String
   is (Ada.Strings.Unbounded.To_String (X));

end Tau;
