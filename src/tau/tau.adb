package body Tau is

   ---------------------
   -- Initialize_Node --
   ---------------------

   procedure Initialize_Node
     (Object      : in out Root_Tau_Node'Class;
      Declaration : GCS.Positions.File_Position)
   is
   begin
      Object.File_Position := Declaration;
   end Initialize_Node;

end Tau;
