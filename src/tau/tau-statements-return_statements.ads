private package Tau.Statements.Return_Statements is

   function Create
     (Position : GCS.Positions.File_Position;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement;

end Tau.Statements.Return_Statements;
