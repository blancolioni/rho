private package Tau.Types.Library is

   function Exists (Name : String) return Boolean;

   procedure Insert
     (Name : String;
      Item : Tau_Type)
     with Pre => not Exists (Name);

   function Get (Name : String) return Tau_Type
     with Pre => Exists (Name);

end Tau.Types.Library;
