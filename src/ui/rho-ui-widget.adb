with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Css.Logging;
with Css.Parser;

with Rho.Handles;
with Rho.Shaders.Stages;

with Rho.UI.Main;

with Rho.Images;
with Rho.Paths;

package body Rho.UI.Widget is

   use Rho.Strings;

   Local_Vertex_Shader   : Rho.Shaders.Stages.Shader_Type;
   Local_Fragment_Shader : Rho.Shaders.Stages.Shader_Type;

   package Style_Maps is
     new WL.String_Maps (Css.Css_Element_Value, Css."=");

   Default_Style_Map : Style_Maps.Map;

   procedure Load_Default_Styles;

   function Vertex_Shader return Rho.Shaders.Stages.Shader_Type
     with Unreferenced;

   function Fragment_Shader return Rho.Shaders.Stages.Shader_Type
     with Unreferenced;

   function Load_Standard_Shader_Source
     (Name : String)
      return String;

   overriding function Classes  (This : Instance) return String
   is (-This.Classes);

   overriding function Id  (This : Instance) return String
   is (-This.Id);

   overriding function Tag (This : Instance) return String
   is (-This.Tag);

   function On_Enter_Notify
     (Widget    : not null access Instance'Class;
      Event     : Rho.UI.Events.Event;
      User_Data : Rho.UI.Events.Event_User_Data'Class)
      return Rho.UI.Events.Handler_Result;

   function On_Leave_Notify
     (Widget    : not null access Instance'Class;
      Event     : Rho.UI.Events.Event;
      User_Data : Rho.UI.Events.Event_User_Data'Class)
      return Rho.UI.Events.Handler_Result;

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

   -----------------
   -- Add_Handler --
   -----------------

   function Add_Handler
     (This      : not null access Instance'Class;
      For_Event : Rho.UI.Events.Event_Type;
      User_Data : Rho.UI.Events.Event_User_Data'Class;
      Handler   : Handler_Type)
      return Rho.UI.Events.Handler_Id
   is
   begin
      This.Handlers (For_Event).Append
        ((Handler, Data_Holder.To_Holder (User_Data)));
      return Rho.UI.Events.Null_Handler_Id;
   end Add_Handler;

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
      This.Log ("configure");
      This.Initialize_Signals;
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
      if Default_Style_Map.Contains (Name) then
         return Default_Style_Map.Element (Name);
      else
         return Css.Default_Style_Value (Name);
      end if;
   end Default_Style_Value;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
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

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
     (From : Css.Css_Element_Value)
      return Rho.Color.Color_Type
   is
   begin
      if Css.Is_String (From) then
         declare
            Css_Color : constant Css.Css_Color :=
                          Css.To_Color (From);
         begin
            Css.Logging.Log ("color: " & Css.To_String (From));
            return (Real (Css_Color.Red) / 255.0,
                    Real (Css_Color.Green) / 255.0,
                    Real (Css_Color.Blue) / 255.0,
                    Real (Css_Color.Alpha) / 255.0);
         end;
      else
         return (1.0, 0.0, 1.0, 1.0);
      end if;

   end Get_Color;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
     (This : Instance'Class)
      return Rho.Fonts.Reference
   is

      function Get (Tag : String) return String
      is (This.Style_To_String (Tag));

      Font_Family : constant String := Get ("font-family");
      --  Font_Size   : constant String := Get ("font-size");
      --  Font_Weight : constant String := Get ("font-weight");
      --  Font_Style  : constant String := Get ("font-style");

      Pixel_Size  : constant Css.Css_Float :=
                      This.Measure (This.Style ("font-size"),
                                    This.Size.Width);
   begin
      return Rho.Fonts.Load
        (Name         => Font_Family,
         Pixel_Height => Natural (Pixel_Size),
         Weight       => Rho.Fonts.Normal,
         Style        => Rho.Fonts.Normal,
         Loader       => Rho.UI.Main.UI_Handle.Assets.all,
         Textures     => Rho.UI.Main.UI_Handle.Assets.all);
   end Get_Font;

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

   -------------------------
   -- Get_Widget_At_Point --
   -------------------------

   function Get_Widget_At_Point
     (This : not null access Instance'Class;
      X, Y : Real)
      return Reference
   is
      Left : constant Real := Real (This.Position.X);
      Top  : constant Real := Real (This.Position.Y);
      Right : constant Real := Left + Real (This.Size.Width);
      Bottom : constant Real := Top + Real (This.Size.Height);
   begin
      if X in Left .. Right
        and then Y in Top .. Bottom
      then
         for Child of This.Children loop
            declare
               W : constant Reference :=
                     Child.Get_Widget_At_Point (X - Left, Y - Top);
            begin
               if W /= null then
                  return W;
               end if;
            end;
         end loop;
         return Reference (This);
      else
         return null;
      end if;
   end Get_Widget_At_Point;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This    : in out Instance;
      Id      : String;
      Tag     : String;
      Classes : String)
   is
   begin
      This.Id := +Id;
      This.Tag := +Tag;
      This.Classes := +Classes;
      This.Property_Bag := new Rho.Properties.Bags.Instance;
   end Initialize;

   -------------------------
   -- Load_Default_Styles --
   -------------------------

   procedure Load_Default_Styles is

      procedure Add (Name : String;
                     Value : String);

      ---------
      -- Add --
      ---------

      procedure Add (Name  : String;
                     Value : String)
      is
      begin
         Default_Style_Map.Insert (Name, Css.Parser.Parse_Value (Value));
      end Add;

   begin
      Add ("background-color", "transparent");
      Add ("color", "black");
      Add ("font-family", "SegoeUI");
      Add ("font-size", "48px");
   end Load_Default_Styles;

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
      Surface : not null access
        Rho.UI.Surface.Instance'Class)
   is
      Pos    : constant Css.Layout_Position := This.Get_Layout_Position;
      Size   : constant Css.Layout_Size := This.Get_Layout_Size;
      Width  : constant Real := Real (Size.Width);
      Height : constant Real := Real (Size.Height);
   begin
      This.Surface := Rho.UI.Surface.Reference (Surface);

      if This.Parent = null then
         This.Context :=
           Rho.UI.Surface.Create
             (This.Surface, 0.0, 0.0, Width, Height);
      else
         This.Context :=
           This.Parent.Context.Create
             (Real (Pos.X), Real (Pos.Y),
              Real (Size.Width), Real (Size.Height));
      end if;

      if Width > 0.0 and then Height > 0.0 then
         This.Context.Draw_Color
           (Get_Color (This.Style ("background-color")));
         This.Context.Rectangle (Width, Height, null);

         This.Log ("draw rectangle: "
                   & Rho.Images.Image (Width)
                   & " "
                   & Rho.Images.Image (Height));
      end if;

      This.Enter_Id :=
        This.Add_Handler
          (For_Event => Rho.UI.Events.Enter_Notify,
           User_Data => Rho.UI.Events.Null_Event_User_Data,
           Handler   => On_Enter_Notify'Access);

      This.Leave_Id :=
        This.Add_Handler
          (For_Event => Rho.UI.Events.Leave_Notify,
           User_Data => Rho.UI.Events.Null_Event_User_Data,
           Handler   => On_Leave_Notify'Access);

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

   ---------------------
   -- On_Enter_Notify --
   ---------------------

   function On_Enter_Notify
     (Widget    : not null access Instance'Class;
      Event     : Rho.UI.Events.Event;
      User_Data : Rho.UI.Events.Event_User_Data'Class)
      return Rho.UI.Events.Handler_Result
   is
      pragma Unreferenced (Event, User_Data);
   begin
      Ada.Text_IO.Put_Line
        (Widget.Short_Description & ": enter");
      return Rho.UI.Events.Propagate;
   end On_Enter_Notify;

   ---------------------
   -- On_Leave_Notify --
   ---------------------

   function On_Leave_Notify
     (Widget    : not null access Instance'Class;
      Event     : Rho.UI.Events.Event;
      User_Data : Rho.UI.Events.Event_User_Data'Class)
      return Rho.UI.Events.Handler_Result
   is
      pragma Unreferenced (Event, User_Data);
   begin
      Ada.Text_IO.Put_Line
        (Widget.Short_Description & ": leave");
      return Rho.UI.Events.Propagate;
   end On_Leave_Notify;

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

   ----------
   -- Send --
   ----------

   procedure Send
     (This  : not null access Instance'Class;
      Event : Rho.UI.Events.Event)
   is
   begin
      for Rec of reverse This.Handlers (Event.Class) loop
         declare
            use Rho.UI.Events;
            Result : constant Handler_Result :=
                       Rec.Handler (This, Event, Rec.Data.Element);
         begin
            exit when Result = Stop;
         end;
      end loop;
   end Send;

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
      if Rho.Strings."-" (This.Tag) = "label" then
         This.Log ("setting label position");
      end if;

      This.Position := Position;

      --  declare
      --     Container_Y : constant Real :=
      --                     (if This.Parent = null
      --                      then This.Surface.Height
      --                      else Real (This.Parent.Get_Layout_Size.Height));
      --  begin
      --     This.Node.Set_Position
      --       (Real (Position.X),
      --        Container_Y - Real (Position.Y) - Real (This.Size.Height),
      --        This.Z_Index);
      --  end;
      --
      --  This.Log ("set layout position: "
      --            & Rho.Matrices.Image (This.Node.Position));
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
     (This   : not null access Instance;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      for Child of This.Children loop
         Child.Show (Target);
      end loop;
   end Show;

   ---------------------
   -- Square_Geometry --
   ---------------------

   function Square_Geometry
     (Width, Height : Non_Negative_Real)
      return Rho.Geometry.Geometry_Type
   is
      use Rho.Geometry;
      Geometry     : constant Geometry_Type := Create_Geometry;

      procedure Point (X, Y : Non_Negative_Real);

      -----------
      -- Point --
      -----------

      procedure Point (X, Y : Non_Negative_Real) is
      begin
         Geometry.Vertex (X, Y, 0.0);
         Geometry.Texture ((if X = 0.0 then 0.0 else 1.0),
                           (if Y = 0.0 then 0.0 else 1.0));
         Geometry.Normal (0.0, 0.0, 1.0);
      end Point;

   begin

      Point (0.0, 0.0);
      Point (Width, 0.0);
      Point (Width, Height);
      Point (0.0, Height);

      Geometry.Face (1, 3, 2);
      Geometry.Face (3, 4, 1);

      return Geometry;
   end Square_Geometry;

   -----------
   -- Style --
   -----------

   overriding function Style
     (This : Instance;
      Name : String)
      return Css.Css_Element_Value
   is
      use Css;
      Value : constant Css_Element_Value :=
                This.Style_Map.Style (Name);
   begin
      if Value = Null_Element_Value then
         return Dispatch (This).Default_Style_Value (Name);
      else
         return Value;
      end if;
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

begin
   Load_Default_Styles;
end Rho.UI.Widget;
