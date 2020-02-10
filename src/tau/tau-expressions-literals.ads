private package Tau.Expressions.Literals is

   function Literal_Expression
     (Position : GCS.Positions.File_Position;
      Value    : Float)
      return Tau_Expression;

   function Literal_Expression
     (Position : GCS.Positions.File_Position;
      Value    : Integer)
      return Tau_Expression;

private

   type Root_Literal_Expression is
     abstract new Root_Tau_Expression with null record;

end Tau.Expressions.Literals;
