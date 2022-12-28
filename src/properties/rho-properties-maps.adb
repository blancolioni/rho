package body Rho.Properties.Maps is

   ---------------------
   -- Create_Property --
   ---------------------

   function Create_Property
     (This       : in out Instance'Class;
      Name       : String;
      On_Changed : Property_Changed_Procedure;
      Default    : String)
      return Property
   is
   begin
      This.Elements.Insert
        (Key => Name,
         New_Item => Element_Type'
           (On_Changed => On_Changed,
            Default    => +Default));
      return Property (Name);
   end Create_Property;

end Rho.Properties.Maps;
