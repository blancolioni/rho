with Partoe.DOM;
with Rho.UI.Widget;

package Rho.UI.Builder is

   subtype Parent is UI.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Load_Html
     (Path : String)
      return Reference;

   function Get
     (This : Instance'Class;
      Id   : String)
      return Rho.UI.Widget.Reference;

   function Top
     (This : Instance'Class)
      return Rho.UI.Widget.Reference;

   type Widget_Creator is access
     function (Element : not null access constant
                 Partoe.DOM.Root_Partoe_Node'Class)
               return Rho.UI.Widget.Reference;

   procedure Register
     (Tag_Name : String;
      Create   : Widget_Creator);

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         Top : Rho.UI.Widget.Reference;
      end record;

   function Top
     (This : Instance'Class)
      return Rho.UI.Widget.Reference
   is (This.Top);

end Rho.UI.Builder;
