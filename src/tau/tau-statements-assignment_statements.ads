private package Tau.Statements.Assignment_Statements is

   function Create
     (Position : GCS.Positions.File_Position;
      Target   : String;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement;

end Tau.Statements.Assignment_Statements;
