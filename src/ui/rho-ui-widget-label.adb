package body Rho.UI.Widget.Label is

   ------------
   -- Create --
   ------------

   function Create (Text : String) return Reference is
      pragma Unreferenced (Text);
   begin
      return new Instance;
   end Create;

   ----------------------
   -- Create_From_Node --
   ----------------------

   function Create_From_Node
     (Element : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
   begin
      return Widget.Reference (Create (Element.Text));
   end Create_From_Node;

end Rho.UI.Widget.Label;
