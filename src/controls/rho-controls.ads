package Rho.Controls is

   type Instance is abstract tagged private;

   type Reference is access all Instance'Class;

   function Enabled (This : Instance'Class) return Boolean;

   procedure Set_Enabled
     (This    : in out Instance;
      Enabled : Boolean)
     with Post'Class => This.Enabled = Enabled;

   procedure Enable
     (This : in out Instance'Class)
     with Post => This.Enabled;

   procedure Disable
     (This : in out Instance'Class)
     with Post => not This.Enabled;

private

   type Instance is abstract tagged
      record
         Enabled : Boolean := True;
      end record;

   function Enabled (This : Instance'Class) return Boolean
   is (This.Enabled);

end Rho.Controls;
