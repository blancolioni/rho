with Rho.UI.Builder;

with Rho.UI.Widget.Label;

package body Rho.UI.Main is

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Rho.UI.Builder.Register
        ("label", Rho.UI.Widget.Label.Create_From_Node'Access);
   end Init;

end Rho.UI.Main;
