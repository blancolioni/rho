package Rho.UI is

   type Instance is abstract tagged private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   type Property (<>) is private;

   function Create_Property (Name : String) return Property;

private

   type Instance is abstract tagged
      record
         null;
      end record;

   type Property is new String;

   function Create_Property (Name : String) return Property
   is (Property (Name));

end Rho.UI;
