private with WL.Guids;
private with Rho.Geometry;
private with Rho.Color;
private with Rho.Properties.Bags;
private with Rho.Properties.Maps;
private with Rho.Strings;

with Ada.Containers.Doubly_Linked_Lists;

with Partoe.DOM;

with Cairo;
with Css;

with Rho.Fonts;
with Rho.Properties;
with Rho.Render;

with Rho.UI.Surface;

package Rho.UI.Widget is

   subtype Parent is UI.Instance;

   type Instance is abstract new Parent
     and Rho.Properties.Property_Value_List
     and Css.Css_Element_Interface
   with private;

   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Container_Widget
     (This : Instance'Class)
      return Reference;

   procedure Initialize
     (This    : in out Instance;
      Id      : String;
      Tag     : String;
      Classes : String);

   procedure Initialize
     (This : in out Instance;
      Node : not null access constant Partoe.DOM.Root_Partoe_Node'Class);

   procedure Finalize
     (This : in out Instance);

   procedure Configure
     (This : not null access Instance);

   procedure Map
     (This : not null access Instance;
      Surface : not null access
        Rho.UI.Surface.Instance'Class);

   procedure Show
     (This   : not null access Instance;
      Target : not null access Rho.Render.Render_Target'Class);

   procedure Queue_Redraw
     (This : not null access Instance);

   procedure Queue_Resize
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

   procedure On_Property_Change_Resize
     (This  : not null access Instance'Class;
      Value : String);

   procedure On_Property_Change_Redraw
     (This  : not null access Instance'Class;
      Value : String);

   function Get_Font
     (This : Instance'Class)
      return Rho.Fonts.Reference;

private

   subtype Dispatch is Instance'Class;

   type Property_Bag_Access is access Rho.Properties.Bags.Instance;

   type Instance is abstract new Parent
     and Rho.Properties.Property_Value_List
     and Css.Css_Element_Interface with
      record
         Guid         : WL.Guids.Guid := WL.Guids.New_Guid;
         Tag          : Rho.Strings.Rho_String;
         Id           : Rho.Strings.Rho_String;
         File_Path    : Rho.Strings.Rho_String;
         Classes      : Rho.Strings.Rho_String;
         Property_Bag : Property_Bag_Access;
         Parent       : Reference;
         Children     : Widget_Lists.List;
         Style_Map    : Css.Css_Style_Map;
         Position     : Css.Layout_Position;
         Size         : Css.Layout_Size;
         Content_Size : Css.Layout_Size;
         Rules        : Css.Css_Rule;
         Surface      : Rho.UI.Surface.Reference;
         Context      : Rho.UI.Surface.Context;
         Meta_Element : Boolean := True;
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

   overriding function Get_Value
     (This : Instance;
      Prop : Rho.Properties.Property)
      return String;

   overriding procedure Set_Value
     (This  : not null access Instance;
      Prop  : Rho.Properties.Property;
      Value : String);

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

   function Container_Widget
     (This : Instance'Class)
      return Reference
   is (This.Parent);

   package Widget_Property_Maps is
     new Rho.Properties.Maps (Instance);

   function Square_Geometry
     (Width, Height : Non_Negative_Real)
     return Rho.Geometry.Geometry_Type;

   function Get_Color
     (From : Css.Css_Element_Value)
      return Rho.Color.Color_Type;

end Rho.UI.Widget;
