private with WL.String_Maps;
private with Rho.Strings;

generic
   type Container_Type is
     abstract limited new Property_Value_List with private;
package Rho.Properties.Maps is

   type Property_Changed_Procedure is access
     procedure (Map : not null access Container_Type'Class;
                Value : String);

   type Instance is new Property_List with private;

   function Create_Property
     (This       : in out Instance'Class;
      Name       : String;
      On_Changed : Property_Changed_Procedure;
      Default    : String)
      return Property;

private

   use Rho.Strings;

   type Element_Type is
      record
         On_Changed : Property_Changed_Procedure;
         Default    : Rho_String;
      end record;

   package Element_Maps is
     new WL.String_Maps (Element_Type);

   type Instance is new Property_List with
      record
         Elements : Element_Maps.Map;
      end record;

end Rho.Properties.Maps;
