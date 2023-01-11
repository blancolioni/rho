package body Rho.UI.Signals is

   Signal_Draw : constant Rho.Signals.Signal_Type :=
                   Rho.Signals.New_Signal ("signal-draw");

   -----------------
   -- Draw_Signal --
   -----------------

   function Draw_Signal return Rho.Signals.Signal_Type is
   begin
      return Signal_Draw;
   end Draw_Signal;

end Rho.UI.Signals;
