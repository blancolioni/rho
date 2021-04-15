with Tau.Statements.Assignment_Statements;
with Tau.Statements.Return_Statements;

package body Tau.Statements is

   --------------------
   -- Assigned_Value --
   --------------------

   function Assigned_Value
     (Statement : Root_Tau_Statement'Class)
      return Tau.Expressions.Tau_Expression
   is
   begin
      return Assignment_Statements.Assigned_Value (Statement);
   end Assigned_Value;

   --------------------------
   -- Assignment_Statement --
   --------------------------

   function Assignment_Statement
     (Position : GCS.Positions.File_Position;
      Name     : String;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement
   is
   begin
      return Tau.Statements.Assignment_Statements.Create
        (Position, Name, Value);
   end Assignment_Statement;

   ----------------------
   -- Return_Statement --
   ----------------------

   function Return_Statement
     (Position : GCS.Positions.File_Position;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement
   is
   begin
      return Tau.Statements.Return_Statements.Create (Position, Value);
   end Return_Statement;

   ------------------
   -- Return_Value --
   ------------------

   function Return_Value
     (Statement : Root_Tau_Statement)
      return Tau.Expressions.Tau_Expression
   is
      pragma Unreferenced (Statement);
   begin
      return null;
   end Return_Value;

   ------------
   -- Target --
   ------------

   function Target
     (Statement : Root_Tau_Statement'Class)
      return String
   is
   begin
      return Assignment_Statements.Assignment_Target (Statement);
   end Target;

end Tau.Statements;
