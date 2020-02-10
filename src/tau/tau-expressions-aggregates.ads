private package Tau.Expressions.Aggregates is

   function Aggregate
     (Position       : GCS.Positions.File_Position;
      Aggregate_Type : Tau.Types.Tau_Type;
      Values         : Tau_Expression_Array)
      return Tau_Expression;

end Tau.Expressions.Aggregates;
