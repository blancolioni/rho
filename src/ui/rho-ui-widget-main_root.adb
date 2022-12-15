package body Rho.UI.Widget.Main_Root is

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
     (This    : not null access Instance;
      Surface : not null access constant Rho.Rectangles.Rectangle_Interface'
        Class)
   is
   begin
      Parent (This.all).Map (Surface);
      This.Scene := Rho.Scenes.Create_Scene;
      This.Scene.Add (This.Node);
      Css.Apply_Layout (This);
   end Map;

end Rho.UI.Widget.Main_Root;
