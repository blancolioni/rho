private with Ada.Strings.Unbounded;

with Ada.Containers.Doubly_Linked_Lists;

with Cairo;
with Css;

package Rho.UI.Widget is

   subtype Parent is UI.Instance;

   type Instance is abstract new Parent
     and Css.Css_Element_Interface
   with private;

   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

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

   subtype Dispatch is Parent'Class;

   type Instance is abstract new Parent
     and Css.Css_Element_Interface with
      record
         Tag       : Ada.Strings.Unbounded.Unbounded_String;
         Id        : Ada.Strings.Unbounded.Unbounded_String;
         Classes   : Ada.Strings.Unbounded.Unbounded_String;
         Children  : Widget_Lists.List;
         Style_Map : Css.Css_Style_Map;
         Position  : Css.Layout_Position;
         Size      : Css.Layout_Size;
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

end Rho.UI.Widget;
