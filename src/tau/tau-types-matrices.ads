private package Tau.Types.Matrices is

   function Matrix (Row_Order, Column_Order : Positive) return Tau_Type
     with Pre => Row_Order <= 4
       and then Column_Order <= 4;

end Tau.Types.Matrices;
