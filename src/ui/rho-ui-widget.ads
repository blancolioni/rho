private with Ada.Strings.Unbounded;
private with WL.Guids;

with Ada.Containers.Doubly_Linked_Lists;

with Partoe.DOM;

with Cairo;
with Css;

with Rho.Rectangles;
with Rho.Nodes;

package Rho.UI.Widget is

   subtype Parent is UI.Instance;

   type Instance is abstract new Parent
     and Css.Css_Element_Interface
   with private;

   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   procedure Initialize
     (This : in out Instance;
      Node : not null access constant Partoe.DOM.Root_Partoe_Node'Class);

   procedure Configure
     (This : not null access Instance);

   procedure Map
     (This : not null access Instance;
      Surface : not null access constant
        Rho.Rectangles.Rectangle_Interface'Class);

   procedure Show
     (This : not null access Instance);

   procedure Add_Child
     (This  : not null access Instance;
      Child : not null access Instance'Class);

   package Widget_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   function Children
     (This : Instance'Class)
      return Widget_Lists.List;

   function Find
     (Top : not null access Instance'Class;
      Match : not null access
        function (This : Reference) return Boolean)
      return Reference;

   function Find_By_Id
     (Top : not null access Instance'Class;
      Id  : String)
      return Reference;

   procedure Set_Size
     (This          : in out Instance;
      Width, Height : Non_Negative_Real);

   procedure On_Configure
     (This          : in out Instance'Class;
      Handler       : not null access
        function (This : not null access Instance'Class;
                  Width, Height : Natural)
      return Boolean);

   procedure On_Draw
     (This          : in out Instance'Class;
      Handler       : not null access
        function (This : not null access Instance'Class;
                  Cr   : Cairo.Cairo_Context)
      return Boolean);

private

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   type Render_Surface is access constant
     Rho.Rectangles.Rectangle_Interface'Class;

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent
     and Css.Css_Element_Interface with
      record
         Guid         : WL.Guids.Guid := WL.Guids.New_Guid;
         Tag          : Ada.Strings.Unbounded.Unbounded_String;
         Id           : Ada.Strings.Unbounded.Unbounded_String;
         File_Path    : Ada.Strings.Unbounded.Unbounded_String;
         Classes      : Ada.Strings.Unbounded.Unbounded_String;
         Parent       : Reference;
         Children     : Widget_Lists.List;
         Style_Map    : Css.Css_Style_Map;
         Position     : Css.Layout_Position;
         Size         : Css.Layout_Size;
         Content_Size : Css.Layout_Size;
         Rules        : Css.Css_Rule;
         Surface      : Render_Surface;
         Node         : Rho.Nodes.Node_Type;
      end record;

   overriding function Tag (This : Instance) return String;
   overriding function Id  (This : Instance) return String;
   overriding function Classes  (This : Instance) return String;

   overriding function Get_Layout_Position
     (This : Instance)
      return Css.Layout_Position;

   overriding procedure Set_Layout_Position
     (This     : in out Instance;
      Position : Css.Layout_Position);

   overriding function Get_Layout_Size
     (This : Instance)
      return Css.Layout_Size;

   overriding procedure Set_Layout_Size
     (This   : in out Instance;
      Size   : Css.Layout_Size);

   overriding function Minimum_Size
     (This       : Instance;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size;

   overriding function Required_Parent_Tag
     (This       : Instance)
      return String;

   overriding function Parent_Element
     (This       : Instance)
      return access Css.Css_Element_Interface'Class;

   overriding function Child_Index
     (This       : Instance)
      return Natural;

   overriding function Is_Table (This : Instance) return Boolean;

   overriding function Inline_Style_Rules
     (This : Instance)
      return Css.Css_Rule;

   overriding function Child_Elements
     (This : Instance)
      return Css.Array_Of_Elements;

   overriding function Contents_Layout_Size
     (This       : Instance)
      return Css.Layout_Size;

   overriding procedure Set_Contents_Layout_Size
     (This    : in out Instance;
      Size    : Css.Layout_Size);

   overriding procedure Create_Style
     (This   : in out Instance;
      Name   : String);

   overriding procedure Set_Style
     (This   : in out Instance;
      Name   : String;
      State  : String;
      Value  : Css.Css_Element_Value);

   overriding function Style
     (This : Instance;
      Name : String)
      return Css.Css_Element_Value;

   overriding function Default_Style_Value
     (This : Instance;
      Name : String)
      return Css.Css_Element_Value;

   function Children
     (This : Instance'Class)
      return Widget_Lists.List
   is (This.Children);

   overriding function Is_Table (This : Instance) return Boolean
   is (False);

   overriding function Inline_Style_Rules
     (This : Instance)
      return Css.Css_Rule
   is (This.Rules);

   overriding function Contents_Layout_Size
     (This       : Instance)
      return Css.Layout_Size
   is (This.Content_Size);

end Rho.UI.Widget;
