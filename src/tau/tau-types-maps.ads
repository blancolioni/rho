private package Tau.Types.Maps is

   type Map_Type (Arity : Positive) is
     new Root_Tau_Type with
      record
         From : Tau_Type_Array (1 .. Arity);
         To   : Tau_Type;
      end record;

end Tau.Types.Maps;
