with Rho.Devices.Keyboard;

package Rho.Signals.Keyboard is

   type Signal_Data is new Signal_Data_Interface with
      record
         Key   : Rho.Devices.Keyboard.Key_Type;
         X, Y  : Integer;
      end record;

   function Press_Signal return Signal_Type;
   function Release_Signal return Signal_Type;

end Rho.Signals.Keyboard;
