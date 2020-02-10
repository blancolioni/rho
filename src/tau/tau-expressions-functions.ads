private package Tau.Expressions.Functions is

   function Apply
     (Position      : GCS.Positions.File_Position;
      Function_Name : String;
      Values        : Tau_Expression_Array)
      return Tau_Expression;

end Tau.Expressions.Functions;
