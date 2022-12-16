package body Rho.Controls is

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Instance'Class) is
   begin
      This.Set_Enabled (False);
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Instance'Class) is
   begin
      This.Set_Enabled (True);
   end Enable;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled (This : in out Instance; Enabled : Boolean) is
   begin
      This.Enabled := Enabled;
   end Set_Enabled;

end Rho.Controls;
