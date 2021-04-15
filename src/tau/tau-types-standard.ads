package Tau.Types.Standard is

   function Tau_Boolean return Tau_Type;
   function Tau_Float return Tau_Type;
   function Tau_Integer return Tau_Type;

   function Tau_Vector (Order : Positive) return Tau_Type;
   function Tau_Matrix (Row_Order, Column_Order : Positive) return Tau_Type;

end Tau.Types.Standard;
