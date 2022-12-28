package Rho.UI is

   type Instance is abstract tagged private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

private

   type Instance is abstract tagged
      record
         null;
      end record;

end Rho.UI;
