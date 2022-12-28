package Rho.Properties is

   type Property (<>) is private;

   function Create_Property (Name : String) return Property;

   type Property_List is limited interface;

   type Property_Value_List is limited interface;

   function Get_Value
     (This : Property_Value_List;
      Prop : Property)
      return String
      is abstract;

   procedure Set_Value
     (This  : not null access Property_Value_List;
      Prop  : Property;
      Value : String)
      is abstract;

private

   type Property is new String;

   function Create_Property (Name : String) return Property
   is (Property (Name));

end Rho.Properties;
