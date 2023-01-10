package body Rho.UI.Widget.Main_Root is

   ---------------
   -- Configure --
   ---------------

   overriding procedure Configure
     (This : not null access Instance)
   is
   begin
      Parent (This.all).Configure;
      Css.Apply_Layout (This);
   end Configure;

   ------------
   -- Create --
   ------------

   function Create return Reference is
   begin
      return new Instance;
   end Create;

   ---------
   -- Map --
   ---------

   overriding procedure Map
     (This : not null access Instance;
      Surface : not null access
        Rho.UI.Surface.Instance'Class)
   is
   begin
      Parent (This.all).Map (Surface);
   end Map;

   ----------
   -- Show --
   ----------

   overriding procedure Show
     (This   : not null access Instance;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Parent (This.all).Show (Target);
      This.Surface.Render (Target);
   end Show;

end Rho.UI.Widget.Main_Root;
