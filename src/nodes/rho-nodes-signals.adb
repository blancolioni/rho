package body Rho.Nodes.Signals is

   Signal_Button_Press : constant Rho.Signals.Signal_Type :=
                           Rho.Signals.New_Signal ("signal-button-press");

   Signal_Button_Release : constant Rho.Signals.Signal_Type :=
                             Rho.Signals.New_Signal ("signal-button-release");

   -------------------------
   -- Button_Press_Signal --
   -------------------------

   function Button_Press_Signal return Rho.Signals.Signal_Type is
   begin
      return Signal_Button_Press;
   end Button_Press_Signal;

   ---------------------------
   -- Button_Release_Signal --
   ---------------------------

   function Button_Release_Signal return Rho.Signals.Signal_Type is
   begin
      return Signal_Button_Release;
   end Button_Release_Signal;

end Rho.Nodes.Signals;
