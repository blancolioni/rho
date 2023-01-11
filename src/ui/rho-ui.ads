with Rho.Objects;

package Rho.UI is

   subtype Parent is Rho.Objects.Root_Object_Type;

   type Instance is abstract new Parent with private;

   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

private

   type Instance is abstract new Parent with
      record
         null;
      end record;

end Rho.UI;
