package Rho.Signals.Configure is

   type Configure_Data is new Signal_Data_Interface with
      record
         Width, Height : Natural;
      end record;

   function Configure_Signal return Signal_Type;

end Rho.Signals.Configure;
