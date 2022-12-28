with Partoe.DOM;

with Rho.Properties;

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

   Label_Property : constant Rho.Properties.Property;

   function Get_Label
     (This : Any_Instance)
      return String;

   procedure Set_Label
     (This  : in out Any_Instance;
      Value : String);

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         null;
      end record;

   Label_Properties : Widget_Property_Maps.Instance;

   Label_Property : constant Rho.Properties.Property :=
                      Label_Properties.Create_Property
                        ("label", On_Property_Change_Resize'Access, "");

end Rho.UI.Widget.Label;
