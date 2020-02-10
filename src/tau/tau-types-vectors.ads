package Tau.Types.Vectors is

   function Vector (Order : Positive) return Tau_Type
     with Pre => Order in 2 .. 4;

end Tau.Types.Vectors;
