private package Tau.Types.Maps is

   type Map_Type (Arity : Positive) is
     new Root_Tau_Type with
      record
         From : Tau_Type_Array (1 .. Arity);
         To   : Tau_Type;
      end record;

   overriding function Map_From
     (Item : Map_Type)
      return Tau_Type_Array
   is (Item.From);

   overriding function Map_To
     (Item : not null access Map_Type)
      return Tau_Type
   is (Item.To);

end Tau.Types.Maps;
