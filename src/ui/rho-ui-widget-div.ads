package Rho.UI.Widget.Div is

   subtype Parent is Widget.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Create
     return Reference;

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         null;
      end record;

   function Create
     return Reference
   is (new Instance);

end Rho.UI.Widget.Div;
