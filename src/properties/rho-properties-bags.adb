package body Rho.Properties.Bags is

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (This : not null access Instance;
      Prop  : Property;
      Value : String)
   is
      Position : constant Value_Maps.Cursor := This.Map.Find (String (Prop));
   begin
      if Value_Maps.Has_Element (Position) then
         This.Map.Replace_Element (Position, Value);
      else
         This.Map.Insert (String (Prop), Value);
      end if;
   end Set_Value;

end Rho.Properties.Bags;
