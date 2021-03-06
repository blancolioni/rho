package body Tau is

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors
     (Node    : in out Root_Tau_Node'Class)
   is
   begin
      Node.Errors.Clear;
      for Child of Node.Children loop
         Child.Clear_Errors;
      end loop;
   end Clear_Errors;

   ----------------
   -- Depends_On --
   ----------------

   function Depends_On
     (Node : Root_Tau_Node;
      Name : String)
      return Boolean
   is
   begin
      return (for some Child of Root_Tau_Node'Class (Node).Children =>
                Child.Depends_On (Name));
   end Depends_On;

   -----------
   -- Error --
   -----------

   procedure Error
     (Node    : in out Root_Tau_Node'Class;
      Message : String)
   is
   begin
--        Rho.Logging.Log
--          (GCS.Positions.Image (Node.Defined_At)
--           & ": " & Message);
      Node.Errors.Append (Message);
   end Error;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors
     (Node : Root_Tau_Node'Class)
      return Boolean
   is
   begin
      if not Node.Errors.Is_Empty then
         return True;
      end if;
      for Child of Node.Children loop
         if Child.Has_Errors then
            return True;
         end if;
      end loop;
      return False;
   end Has_Errors;

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

   --------------------
   -- Iterate_Errors --
   --------------------

   procedure Iterate_Errors
     (Node    : not null access constant Root_Tau_Node'Class;
      Process : not null access
        procedure (Node : Constant_Tau_Node;
                   Message : String))
   is
   begin
      for Error of Node.Errors loop
         Process (Constant_Tau_Node (Node), Error);
      end loop;
      for Child of Node.Children loop
         Child.Iterate_Errors (Process);
      end loop;
   end Iterate_Errors;

end Tau;
