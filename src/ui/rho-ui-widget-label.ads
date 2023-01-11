private with Rho.Meshes;

with Partoe.DOM;

with Rho.Properties;

package Rho.UI.Widget.Label is

   subtype Parent is Widget.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Create_From_Node
     (Element : not null access constant
        Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference;

   Label_Property : constant Rho.Properties.Property;

   function Get_Label
     (This : Any_Instance)
      return String;

   procedure Set_Label
     (This  : in out Any_Instance;
      Value : String);

private

   type Glyph_Mesh is
     new Rho.Meshes.Root_Mesh_Type with
      record
         X, Y          : Non_Negative_Real;
         Width, Height : Non_Negative_Real;
      end record;

   type Glyph_Mesh_Access is access all Glyph_Mesh'Class;

   package Glyph_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Glyph_Mesh_Access);

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         Glyphs : Glyph_Lists.List;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("Rho.UI.Widget.Label");

   overriding function Minimum_Size
     (This       : Instance;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size;

   overriding procedure Map
     (This    : not null access Instance;
      Surface : not null access Rho.UI.Surface.Instance'Class);

   overriding procedure Set_Layout_Position
     (This     : in out Instance;
      Position : Css.Layout_Position);

   Label_Properties : Widget_Property_Maps.Instance;

   Label_Property : constant Rho.Properties.Property :=
                      Label_Properties.Create_Property
                        ("label", On_Property_Change_Resize'Access, "");

end Rho.UI.Widget.Label;
