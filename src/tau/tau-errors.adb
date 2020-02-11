with Ada.Text_IO;

package body Tau.Errors is

   use type GCS.Positions.File_Position;

   type Error_Record (Message_Length : Natural) is
      record
         Position : GCS.Positions.File_Position;
         Message  : String (1 .. Message_Length);
      end record;

   package Error_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Error_Record);

   function Before (Left, Right : Error_Record) return Boolean
   is (Left.Position < Right.Position);

   package Error_Sorting is
     new Error_Lists.Generic_Sorting (Before);

   ------------------
   -- Write_Errors --
   ------------------

   procedure Write_Errors
     (Node : not null access constant Root_Tau_Node'Class)
   is
      List : Error_Lists.List;

      procedure Add (Error_Node : Constant_Tau_Node;
                     Message    : String);

      ---------
      -- Add --
      ---------

      procedure Add (Error_Node : Constant_Tau_Node;
                     Message    : String)
      is
      begin
         List.Append
           (Error_Record'
              (Message_Length => Message'Length,
               Position       => Error_Node.Defined_At,
               Message        => Message));
      end Add;

   begin
      Node.Iterate_Errors (Add'Access);
      Error_Sorting.Sort (List);
      for Item of List loop
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            GCS.Positions.Image (Item.Position)
            & ": " & Item.Message);
      end loop;
   end Write_Errors;

end Tau.Errors;
