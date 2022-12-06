with Rho.Signals;

package Rho.Nodes.Signals is

   type Button_Signal_Data is
     new Rho.Signals.Signal_Data_Interface with
      record
         Button    : Natural;
         X, Y      : Integer;
      end record;

   function Button_Press_Signal return Rho.Signals.Signal_Type;
   function Button_Release_Signal return Rho.Signals.Signal_Type;

end Rho.Nodes.Signals;
