package Tau.Types.Textures is

   function Texture (Order : Positive) return Tau_Type
     with Pre => Order <= 3;

end Tau.Types.Textures;
