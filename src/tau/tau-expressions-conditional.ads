private package Tau.Expressions.Conditional is

   function Conditional_Expression
     (Position       : GCS.Positions.File_Position;
      Condition      : Tau_Expression;
      True_Value     : Tau_Expression;
      False_Value    : Tau_Expression)
      return Tau_Expression;

end Tau.Expressions.Conditional;
