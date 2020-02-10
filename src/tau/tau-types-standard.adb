with Tau.Types.Scalar;
with Tau.Types.Matrices;
with Tau.Types.Vectors;

package body Tau.Types.Standard is

   ---------------
   -- Tau_Float --
   ---------------

   function Tau_Float return Tau_Type is
   begin
      return Tau.Types.Scalar.Tau_Float;
   end Tau_Float;

   -----------------
   -- Tau_Integer --
   -----------------

   function Tau_Integer return Tau_Type is
   begin
      return Tau.Types.Scalar.Tau_Integer;
   end Tau_Integer;

   ----------------
   -- Tau_Matrix --
   ----------------

   function Tau_Matrix (Row_Order, Column_Order : Positive) return Tau_Type is
   begin
      return Tau.Types.Matrices.Matrix (Row_Order, Column_Order);
   end Tau_Matrix;

   ----------------
   -- Tau_Vector --
   ----------------

   function Tau_Vector (Order : Positive) return Tau_Type is
   begin
      return Tau.Types.Vectors.Vector (Order);
   end Tau_Vector;

end Tau.Types.Standard;
