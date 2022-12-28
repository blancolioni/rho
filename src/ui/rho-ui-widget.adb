with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

--  with WL.String_Maps;

with Rho.Geometry;
with Rho.Material.Custom;
with Rho.Matrices;
with Rho.Meshes;
with Rho.Shaders.Stages;

with Rho.Paths;

package body Rho.UI.Widget is

   Local_Square_Geometry : Rho.Geometry.Geometry_Type;
   Local_Vertex_Shader   : Rho.Shaders.Stages.Shader_Type;
   Local_Fragment_Shader : Rho.Shaders.Stages.Shader_Type;

   function Square_Geometry return Rho.Geometry.Geometry_Type;
   function Vertex_Shader return Rho.Shaders.Stages.Shader_Type;
   function Fragment_Shader return Rho.Shaders.Stages.Shader_Type;

   function Load_Standard_Shader_Source
     (Name : String)
      return String;

   overriding function Classes  (This : Instance) return String
   is (-This.Classes);

   overriding function Id  (This : Instance) return String
   is (-This.Id);

   overriding function Tag (This : Instance) return String
   is (-This.Tag);

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (This  : not null access Instance;
      Child : not null access Instance'Class)
   is
   begin
      This.Children.Append (Reference (Child));
      Child.Parent := Reference (This);
   end Add_Child;

   --------------------
   -- Child_Elements --
   --------------------

   overriding function Child_Elements
     (This : Instance)
      return Css.Array_Of_Elements
   is
   begin
      return Children : Css.Array_Of_Elements
        (1 .. Natural (This.Children.Length))
      do
         declare
            use Widget_Lists;
            Position : Cursor := This.Children.First;
         begin
            for Item of Children loop
               Item := Css.Css_Element (Element (Position));
               Next (Position);
            end loop;
         end;
      end return;
   end Child_Elements;

   -----------------
   -- Child_Index --
   -----------------

   overriding function Child_Index
     (This       : Instance)
      return Natural
   is
   begin
      if This.Parent = null then
         return 0;
      else
         declare
            use type WL.Guids.Guid;
            Index : Natural := 0;
         begin
            for Child of This.Parent.Children loop
               Index := Index + 1;
               exit when Child.Guid = This.Guid;
            end loop;
            pragma Assert (Index > 0, "child not contained by parent");
            return Index;
         end;
      end if;
   end Child_Index;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This : not null access Instance)
   is
   begin
      Css.Current_Style_Sheet.Load_Style_Rules (This.all);
      for Child of This.Children loop
         Child.Configure;
      end loop;
   end Configure;

   ------------------
   -- Create_Style --
   ------------------

   overriding procedure Create_Style
     (This   : in out Instance;
      Name   : String)
   is
   begin
      This.Style_Map.Create_Style (Name);
   end Create_Style;

   -------------------------
   -- Default_Style_Value --
   -------------------------

   overriding function Default_Style_Value
     (This : Instance;
      Name : String)
      return Css.Css_Element_Value
   is
   begin
      return Css.Default_Style_Value (Name);
   end Default_Style_Value;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (This : in out Instance)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Rho.Properties.Bags.Instance, Property_Bag_Access);
   begin
      Free (This.Property_Bag);
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find
     (Top   : not null access Instance'Class;
      Match : not null access function (This : Reference) return Boolean)
      return Reference
   is
   begin
      if Match (Reference (Top)) then
         return Reference (Top);
      else
         for Child of Top.Children loop
            declare
               Result : constant Reference := Child.Find (Match);
            begin
               if Result /= null then
                  return Result;
               end if;
            end;
         end loop;
         return null;
      end if;
   end Find;

   ----------------
   -- Find_By_Id --
   ----------------

   function Find_By_Id
     (Top : not null access Instance'Class;
      Id  : String)
      return Reference
   is
      function Match_Id (W : Reference) return Boolean
      is (-W.Id = Id);

   begin
      return Top.Find (Match_Id'Access);
   end Find_By_Id;

   ---------------------
   -- Fragment_Shader --
   ---------------------

   function Fragment_Shader return Rho.Shaders.Stages.Shader_Type is
      use type Rho.Shaders.Stages.Shader_Type;
   begin
      if True or else Local_Fragment_Shader = null then
         Local_Fragment_Shader :=
           Rho.Shaders.Stages.Create
             (Name   => "ui-bg-fragment",
              Stage  => Rho.Fragment_Shader,
              Source =>
                Load_Standard_Shader_Source ("ui-bg-fragment"));
      end if;
      return Local_Fragment_Shader;
   end Fragment_Shader;

   -------------------------
   -- Get_Layout_Position --
   -------------------------

   overriding function Get_Layout_Position
     (This : Instance)
      return Css.Layout_Position
   is
   begin
      return This.Position;
   end Get_Layout_Position;

   ---------------------
   -- Get_Layout_Size --
   ---------------------

   overriding function Get_Layout_Size
     (This : Instance)
      return Css.Layout_Size
   is
   begin
      return This.Size;
   end Get_Layout_Size;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (This : Instance;
      Prop : Rho.Properties.Property)
      return String
   is
   begin
      return This.Property_Bag.Get_Value (Prop);
   end Get_Value;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out Instance;
      Node : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
   is
      function Attr (Name : String) return String;

      ----------
      -- Attr --
      ----------

      function Attr (Name : String) return String is
      begin
         if Node.Has_Attribute (Name) then
            return Node.Attribute (Name).Text;
         else
            return "";
         end if;
      end Attr;
   begin
      This.Id := +Attr ("id");
      This.Tag := +Node.Name;
      This.Classes := +Attr ("class");
      This.Property_Bag := new Rho.Properties.Bags.Instance;
   end Initialize;

   ---------------------------------
   -- Load_Standard_Shader_Source --
   ---------------------------------

   function Load_Standard_Shader_Source
     (Name : String)
      return String
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      File : File_Type;
      Source : Unbounded_String;
   begin
      Open (File, In_File,
            Rho.Paths.Config_File ("shaders/gl/" & Name & ".txt"));
      while not End_Of_File (File) loop
         Source := Source & Get_Line (File) & Character'Val (10);
      end loop;
      Close (File);
      return To_String (Source);
   end Load_Standard_Shader_Source;

   ---------
   -- Map --
   ---------

   procedure Map
     (This : not null access Instance;
      Surface : not null access constant
        Rho.Rectangles.Rectangle_Interface'Class)
   is
      Mesh : constant Rho.Meshes.Mesh_Type :=
               Rho.Meshes.Create_Mesh
                 (Geometry => Square_Geometry,
                  Material =>
                    Rho.Material.Custom.Create_Custom_Material
                      (Shaders => (Vertex_Shader, Fragment_Shader)));
   begin
      This.Surface := Render_Surface (Surface);
      This.Node := Rho.Nodes.Node_Type (Mesh);

      if This.Parent /= null then
         declare
            W : Reference := This.Parent;
         begin
            while W.Parent /= null loop
               W := W.Parent;
            end loop;
            W.Node.Add (This.Node);
         end;
      end if;
      for Child of Dispatch (This.all).Children loop
         Child.Map (Surface);
      end loop;
   end Map;

   ------------------
   -- Minimum_Size --
   ------------------

   overriding function Minimum_Size
     (This       : Instance;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size
   is
   begin
      return (others => <>);
   end Minimum_Size;

   ------------------
   -- On_Configure --
   ------------------

   procedure On_Configure
     (This    : in out Instance'Class;
      Handler :        not null access function
        (This : not null access Instance'Class; Width, Height : Natural)
         return Boolean)
   is null;

   -------------
   -- On_Draw --
   -------------

   procedure On_Draw
     (This    : in out Instance'Class;
      Handler :        not null access function
        (This : not null access Instance'Class; Cr : Cairo.Cairo_Context)
         return Boolean)
   is null;

   -------------------------------
   -- On_Property_Change_Redraw --
   -------------------------------

   procedure On_Property_Change_Redraw
     (This  : not null access Instance'Class;
      Value : String)
   is
      pragma Unreferenced (Value);
   begin
      This.Queue_Redraw;
   end On_Property_Change_Redraw;

   -------------------------------
   -- On_Property_Change_Resize --
   -------------------------------

   procedure On_Property_Change_Resize
     (This  : not null access Instance'Class;
      Value : String)
   is
      pragma Unreferenced (Value);
   begin
      This.Queue_Resize;
   end On_Property_Change_Resize;

   --------------------
   -- Parent_Element --
   --------------------

   overriding function Parent_Element
     (This       : Instance)
      return access Css.Css_Element_Interface'Class
   is
   begin
      return This.Parent;
   end Parent_Element;

   ------------------
   -- Queue_Redraw --
   ------------------

   procedure Queue_Redraw
     (This : not null access Instance)
   is
   begin
      null;
   end Queue_Redraw;

   procedure Queue_Resize
     (This : not null access Instance)
   is
   begin
      null;
   end Queue_Resize;

   -------------------------
   -- Required_Parent_Tag --
   -------------------------

   overriding function Required_Parent_Tag
     (This       : Instance)
      return String
   is
   begin
      return "";
   end Required_Parent_Tag;

   ------------------------------
   -- Set_Contents_Layout_Size --
   ------------------------------

   overriding procedure Set_Contents_Layout_Size
     (This    : in out Instance;
      Size    : Css.Layout_Size)
   is
   begin
      This.Content_Size := Size;
   end Set_Contents_Layout_Size;

   -------------------------
   -- Set_Layout_Position --
   -------------------------

   overriding procedure Set_Layout_Position
     (This     : in out Instance;
      Position : Css.Layout_Position)
   is
   begin
      This.Position := Position;
      This.Node.Set_Position
        (Real (Position.X),
         This.Surface.Height - Real (Position.Y) - Real (This.Size.Height),
         0.0);
   end Set_Layout_Position;

   ---------------------
   -- Set_Layout_Size --
   ---------------------

   overriding procedure Set_Layout_Size
     (This   : in out Instance;
      Size   : Css.Layout_Size)
   is
   begin
      This.Size := Size;
      This.Node.Scale
        (Real (Size.Width), Real (Size.Height), 1.0);
      This.Node.Set_Position
        (Rho.Matrices.X (This.Node.Position),
         This.Surface.Height - Real (This.Position.Y) - Real (Size.Height),
         0.0);
   end Set_Layout_Size;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (This          : in out Instance;
      Width, Height : Non_Negative_Real)
   is
   begin
      Dispatch (This).Set_Layout_Size
        ((True, True, Css.Css_Float (Width), Css.Css_Float (Height)));
   end Set_Size;

   ---------------
   -- Set_Style --
   ---------------

   overriding procedure Set_Style
     (This   : in out Instance;
      Name   : String;
      State  : String;
      Value  : Css.Css_Element_Value)
   is
   begin
      This.Log ("set: " & Name);
      This.Style_Map.Set_Style (Name, State, Value);
   end Set_Style;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (This  : not null access Instance;
      Prop  : Rho.Properties.Property;
      Value : String)
   is
   begin
      This.Property_Bag.Set_Value (Prop, Value);
   end Set_Value;

   ----------
   -- Show --
   ----------

   procedure Show
     (This : not null access Instance)
   is
   begin
      for Child of This.Children loop
         Child.Show;
      end loop;
   end Show;

   ---------------------
   -- Square_Geometry --
   ---------------------

   function Square_Geometry return Rho.Geometry.Geometry_Type is

      procedure Create;

      ------------
      -- Create --
      ------------

      procedure Create is
         use Rho.Geometry;
         Geometry     : constant Geometry_Type := Create_Geometry;

         procedure Point (X, Y : Unit_Real);

         -----------
         -- Point --
         -----------

         procedure Point (X, Y : Unit_Real) is
         begin
            Geometry.Vertex (X, Y, 0.0);
            Geometry.Texture (X, Y);
            Geometry.Normal (0.0, 0.0, 1.0);
         end Point;

      begin

         Point (0.0, 0.0);
         Point (1.0, 0.0);
         Point (1.0, 1.0);
         Point (0.0, 1.0);

         Geometry.Face (1, 3, 2);
         Geometry.Face (3, 4, 1);

         Local_Square_Geometry := Geometry;

      end Create;

      use type Rho.Geometry.Geometry_Type;
   begin
      if Local_Square_Geometry = null then
         Create;
      end if;

      return Local_Square_Geometry;
   end Square_Geometry;

   -----------
   -- Style --
   -----------

   overriding function Style
     (This : Instance;
      Name : String)
      return Css.Css_Element_Value
   is
   begin
      return This.Style_Map.Style (Name);
   end Style;

   -------------------
   -- Vertex_Shader --
   -------------------

   function Vertex_Shader return Rho.Shaders.Stages.Shader_Type is
      use type Rho.Shaders.Stages.Shader_Type;
   begin
      if True or else Local_Vertex_Shader = null then
         Local_Vertex_Shader :=
           Rho.Shaders.Stages.Create
             (Name   => "ui-bg-vertex",
              Stage  => Rho.Vertex_Shader,
              Source =>
                Load_Standard_Shader_Source ("ui-bg-vertex"));
      end if;
      return Local_Vertex_Shader;
   end Vertex_Shader;

end Rho.UI.Widget;
