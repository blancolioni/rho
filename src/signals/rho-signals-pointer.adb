package body Rho.Signals.Pointer is

   Signal_Pointer_Move : constant Signal_Type :=
                           New_Signal ("signal-pointer-move");
   -----------------
   -- Move_Signal --
   -----------------

   function Move_Signal return Signal_Type is
   begin
      return Signal_Pointer_Move;
   end Move_Signal;

end Rho.Signals.Pointer;
