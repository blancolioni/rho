with Tau.Statements.Assignment_Statements;
with Tau.Statements.Return_Statements;

package body Tau.Statements is

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

end Tau.Statements;
