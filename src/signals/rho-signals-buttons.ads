package Rho.Signals.Buttons is

   type Signal_Data is new Signal_Data_Interface with
      record
         Button    : Natural;
         X, Y      : Integer;
      end record;

   function Press_Signal return Signal_Type;
   function Release_Signal return Signal_Type;

end Rho.Signals.Buttons;
