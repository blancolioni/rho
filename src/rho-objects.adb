package body Rho.Objects is

   ----------------
   -- Set_Loaded --
   ----------------

   procedure Set_Loaded (Object : in out Root_Object_Type'Class) is
   begin
      Object.Loaded := True;
   end Set_Loaded;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Object : in out Root_Object_Type'Class;
      Name   : String)
   is
   begin
      Object.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Rho.Objects;
