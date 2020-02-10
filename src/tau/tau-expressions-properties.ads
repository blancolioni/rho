private package Tau.Expressions.Properties is

   function Property_Expression
     (Position  : GCS.Positions.File_Position;
      Left      : Tau_Expression;
      Property  : String)
      return Tau_Expression;

end Tau.Expressions.Properties;
