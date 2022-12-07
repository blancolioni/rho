package Rho.Signals.Pointer is

   type Signal_Data is new Signal_Data_Interface with
      record
         X, Y      : Integer;
      end record;

   function Move_Signal return Signal_Type;

end Rho.Signals.Pointer;
