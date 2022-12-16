package Rho.UI.Widget.Draw is

   subtype Parent is Widget.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         null;
      end record;

end Rho.UI.Widget.Draw;
