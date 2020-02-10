with GCS.Positions;

package Tau is

   type Root_Tau_Node is abstract tagged private;

   function Defined_At
     (Node : Root_Tau_Node'Class)
      return GCS.Positions.File_Position;

   type Tau_Node is access all Root_Tau_Node'Class;

private

   type Root_Tau_Node is abstract tagged
      record
         File_Position : GCS.Positions.File_Position;
      end record;

   procedure Initialize_Node
     (Object      : in out Root_Tau_Node'Class;
      Declaration : GCS.Positions.File_Position);

   function Defined_At
     (Node : Root_Tau_Node'Class)
      return GCS.Positions.File_Position
   is (Node.File_Position);

end Tau;
