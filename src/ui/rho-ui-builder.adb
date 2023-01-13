with Ada.Strings.Fixed;

with WL.String_Maps;

with Rho.UI.Widget.Div;
with Rho.UI.Widget.Head;
with Rho.UI.Widget.Label;
with Rho.UI.Widget.Link;
with Rho.UI.Widget.Main_Root;
with Rho.UI.Widget.Title;

package body Rho.UI.Builder is

   package Widget_Creator_Maps is
     new WL.String_Maps (Widget_Creator);

   Widget_Creator_Map : Widget_Creator_Maps.Map;

   function Create_Widget
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   function Create_Children
     (Parent : not null access Rho.UI.Widget.Instance'Class;
      Root   : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   function Create_Div
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   function Create_Head
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   function Create_Main_Root
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   function Create_Link
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   function Create_Title
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   ---------------------
   -- Create_Children --
   ---------------------

   function Create_Children
     (Parent : not null access Rho.UI.Widget.Instance'Class;
      Root   : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
   begin
      Parent.Initialize (Root);
      for Child_Node of Root.Children loop
         declare
            use type Rho.UI.Widget.Reference;
            Child : constant Rho.UI.Widget.Reference :=
                      Create_Widget (Child_Node);
         begin
            if Child /= null then
               Parent.Add_Child (Child);
            end if;
         end;
      end loop;
      return Rho.UI.Widget.Reference (Parent);
   end Create_Children;

   ----------------
   -- Create_Div --
   ----------------

   function Create_Div
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
      Text : constant String :=
               Ada.Strings.Fixed.Trim
                 (Root.Text, Ada.Strings.Both);
      W    : constant Rho.UI.Widget.Reference :=
               (if Text /= ""
                then Rho.UI.Widget.Reference (Rho.UI.Widget.Label.Create)
                else Rho.UI.Widget.Reference (Rho.UI.Widget.Div.Create));
   begin
      return Create_Children (W, Root);
   end Create_Div;

   -----------------
   -- Create_Head --
   -----------------

   function Create_Head
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
      Head : constant Rho.UI.Widget.Head.Reference :=
               Rho.UI.Widget.Head.Create;
   begin
      return Create_Children (Head, Root);
   end Create_Head;

   -----------------
   -- Create_Link --
   -----------------

   function Create_Link
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
      Link : constant Rho.UI.Widget.Link.Reference :=
               Rho.UI.Widget.Link.Create
                 (Working_Path => Root.File_Path,
                  Relation     => Root.Attribute ("rel").Text,
                  Href         => Root.Attribute ("href").Text);
   begin
      return Create_Children (Link, Root);
   end Create_Link;

   ----------------------
   -- Create_Main_Root --
   ----------------------

   function Create_Main_Root
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
      Html : constant Rho.UI.Widget.Main_Root.Reference :=
               Rho.UI.Widget.Main_Root.Create;
   begin
      return Create_Children (Html, Root);
   end Create_Main_Root;

   ------------------
   -- Create_Title --
   ------------------

   function Create_Title
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
      Title : constant Rho.UI.Widget.Title.Reference :=
               Rho.UI.Widget.Title.Create;
   begin
      return Create_Children (Title, Root);
   end Create_Title;

   -------------------
   -- Create_Widget --
   -------------------

   function Create_Widget
     (Root : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
      use Widget_Creator_Maps;
      Position : constant Cursor := Widget_Creator_Map.Find (Root.Name);
   begin
      if Has_Element (Position) then
         return Element (Position) (Root);
      elsif Root.Child_Count > 0 then
         return Create_Widget (Root.Child (1));
      else
         return null;
      end if;
   end Create_Widget;

   ---------
   -- Get --
   ---------

   function Get
     (This : Instance'Class; Id : String) return Rho.UI.Widget.Reference
   is
   begin
      return This.Top.Find_By_Id (Id);
   end Get;

   ---------------
   -- Load_Html --
   ---------------

   function Load_Html (Path : String) return Reference is
      Doc : constant Partoe.DOM.Partoe_Document :=
              Partoe.DOM.Load (Path);
      This : constant Reference := new Instance;
   begin
      This.Top := Create_Widget (Doc);
      return This;
   end Load_Html;

   --------------
   -- Register --
   --------------

   procedure Register
     (Tag_Name : String;
      Create   : Widget_Creator)
   is
   begin
      Widget_Creator_Map.Insert (Tag_Name, Create);
   end Register;

begin
   Register ("div", Create_Div'Access);
   Register ("head", Create_Head'Access);
   Register ("html", Create_Main_Root'Access);
   Register ("link", Create_Link'Access);
   Register ("title", Create_Title'Access);
end Rho.UI.Builder;
