package body Rho.Signals.Buttons is

   Signal_Button_Press : constant Signal_Type :=
                           New_Signal ("signal-button-press");

   Signal_Button_Release : constant Signal_Type :=
                             New_Signal ("signal-button-release");

   ------------------
   -- Press_Signal --
   ------------------

   function Press_Signal return Signal_Type is
   begin
      return Signal_Button_Press;
   end Press_Signal;

   --------------------
   -- Release_Signal --
   --------------------

   function Release_Signal return Signal_Type is
   begin
      return Signal_Button_Release;
   end Release_Signal;

end Rho.Signals.Buttons;
