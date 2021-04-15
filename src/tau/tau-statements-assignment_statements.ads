private package Tau.Statements.Assignment_Statements is

   function Create
     (Position : GCS.Positions.File_Position;
      Target   : String;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement;

   function Assignment_Target
     (Statement : Root_Tau_Statement'Class)
      return String;

   function Assigned_Value
     (Statement : Root_Tau_Statement'Class)
      return Tau.Expressions.Tau_Expression;

end Tau.Statements.Assignment_Statements;
