package body Rho.Signals.Keyboard is

   Signal_Key_Press : constant Signal_Type :=
                        New_Signal ("signal-key-press");

   Signal_Key_Release : constant Signal_Type :=
                          New_Signal ("signal-key-release");

   ------------------
   -- Press_Signal --
   ------------------

   function Press_Signal return Signal_Type is
   begin
      return Signal_Key_Press;
   end Press_Signal;

   --------------------
   -- Release_Signal --
   --------------------

   function Release_Signal return Signal_Type is
   begin
      return Signal_Key_Release;
   end Release_Signal;

end Rho.Signals.Keyboard;
