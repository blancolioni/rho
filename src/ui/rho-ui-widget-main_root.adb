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
      This.Z_Index := 0.0;
      This.Scene := Rho.Scenes.Create_Scene;
      This.Scene.Set_Name ("scene: " & This.Short_Description);
      This.Top_Node := Rho.Nodes.Create_Node;
      This.Scene.Add (This.Top_Node);
      Parent (This.all).Map (Surface);
      Css.Apply_Layout (This);
   end Map;

end Rho.UI.Widget.Main_Root;
