package body Rho.Signals.Configure is

   Signal_Configure : constant Signal_Type :=
                           New_Signal ("signal-configure");

   ----------------------
   -- Configure_Signal --
   ----------------------

   function Configure_Signal return Signal_Type is
   begin
      return Signal_Configure;
   end Configure_Signal;

end Rho.Signals.Configure;
