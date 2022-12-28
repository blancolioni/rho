private with WL.String_Maps;

package Rho.Properties.Bags is

   subtype Parent is Rho.Properties.Property_Value_List;

   type Instance is new Parent with private;

   type Reference is access all Instance'Class;

private

   package Value_Maps is
     new WL.String_Maps (String);

   type Instance is new Parent with
      record
         Map : Value_Maps.Map;
      end record;

   overriding function Get_Value
     (This : Instance;
      Prop : Property)
      return String
   is (This.Map.Element (String (Prop)));

   overriding procedure Set_Value
     (This  : not null access Instance;
      Prop  : Property;
      Value : String);

end Rho.Properties.Bags;
