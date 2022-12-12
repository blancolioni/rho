package body Rho.UI.Widget is

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
   end Initialize;

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

end Rho.UI.Widget;
