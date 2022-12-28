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

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (This : Any_Instance)
      return String
   is
   begin
      return This.Get_Value (Label_Property);
   end Get_Label;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (This  : in out Any_Instance;
      Value : String)
   is
   begin
      This.Set_Value (Label_Property, Value);
   end Set_Label;

end Rho.UI.Widget.Label;
