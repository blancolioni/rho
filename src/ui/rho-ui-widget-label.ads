with Partoe.DOM;

with Rho.UI.Properties;

package Rho.UI.Widget.Label is

   subtype Parent is Widget.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Create
     (Text    : String)
      return Reference;

   function Create_From_Node
     (Element : not null access constant
        Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   Label_Property : constant Rho.UI.Properties.String_Property;

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         null;
      end record;

   Label_Property : constant Rho.UI.Properties.String_Property :=
                      Rho.UI.Properties.Create_Property ("label");

end Rho.UI.Widget.Label;
