private package Tau.Expressions.References is

   function Reference_Expression
     (Position    : GCS.Positions.File_Position;
      Name        : String)
      return Tau_Expression;

end Tau.Expressions.References;
