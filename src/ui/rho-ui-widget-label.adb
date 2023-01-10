with Ada.Strings.Fixed;

with Rho.Bitmaps;

--  with Rho.Images;

package body Rho.UI.Widget.Label is

   ----------------------
   -- Create_From_Node --
   ----------------------

   function Create_From_Node
     (Element : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
      return Rho.UI.Widget.Reference
   is
      Text  : constant String :=
                Ada.Strings.Fixed.Trim
                  (Element.Text, Ada.Strings.Both);
      Label : constant Reference := new Instance;
   begin
      Label.Initialize
        (Id      => "",
         Tag     => "label",
         Classes => "");
      Label.Set_Label (Text);
      return Widget.Reference (Label);
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

   ---------
   -- Map --
   ---------

   overriding procedure Map
     (This    : not null access Instance;
      Surface : not null access Rho.UI.Surface.Instance'Class)
   is
      Font : constant Rho.Fonts.Reference := Dispatch (This.all).Get_Font;
      Text : constant String := This.Get_Label;
      Left : Non_Negative_Real := 0.0;
      Base : constant Non_Negative_Real := Real (Font.Pixel_Height);

      procedure Draw_Glyph
        (Bitmap                : Rho.Bitmaps.Greyscale_Reference;
         Width, Height, Stride : Natural;
         Bearing_X, Bearing_Y  : Integer;
         Advance               : Natural);

      ----------------
      -- Draw_Glyph --
      ----------------

      procedure Draw_Glyph
        (Bitmap                : Rho.Bitmaps.Greyscale_Reference;
         Width, Height, Stride : Natural;
         Bearing_X, Bearing_Y  : Integer;
         Advance               : Natural)
      is
         pragma Unreferenced (Stride);
      begin
         This.Context.Move_To (Left + Real (Bearing_X),
                               Base - Real (Bearing_Y));
         This.Context.Rectangle
           (Width  => Real (Width),
            Height  => Real (Height),
            Stencil => Bitmap);
         Left := Left + Real (Advance);
      end Draw_Glyph;

   begin
      Parent (This.all).Map (Surface);
      This.Context.Draw_Color
        (Get_Color (This.Style ("color")));
      Font.Render (Text, Draw_Glyph'Access);
   end Map;

   ------------------
   -- Minimum_Size --
   ------------------

   overriding function Minimum_Size
     (This       : Instance;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size
   is
      Font : constant Rho.Fonts.Reference := Dispatch (This).Get_Font;
      Height : constant Css.Css_Float :=
                 Css.Css_Float (Font.Pixel_Height);
      Width  : constant Css.Css_Float :=
                 Css.Css_Float (Font.Measure (This.Get_Label));
   begin
      This.Log (Font.Description & " [" & This.Get_Label & "]: width"
                & Natural'Image (Natural (Width))
                & "; height" & Natural'Image (Natural (Height)));

      return Css.Layout_Size'
        (Constrained_Width  => True,
         Constrained_Height => True,
         Width              => Width,
         Height             => Height);
   end Minimum_Size;

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

   -------------------------
   -- Set_Layout_Position --
   -------------------------

   overriding procedure Set_Layout_Position
     (This     : in out Instance;
      Position : Css.Layout_Position)
   is
   begin
      Parent (This).Set_Layout_Position (Position);
   end Set_Layout_Position;

   --     procedure Move
   --       (Child : not null access Rho.Nodes.Root_Node_Type'Class);
   --
   --     procedure Move
   --       (Child : not null access Rho.Nodes.Root_Node_Type'Class)
   --     is
   --        use Rho.Images;
   --
   --        Glyph : Glyph_Mesh'Class renames Glyph_Mesh'Class (Child.all);
   --        Pos   : constant Rho.Matrices.Vector_3 :=
   --                  Glyph.Position;
   --     begin
   --        Glyph.Scale
   --          (Real (Glyph.Width) / Real (This.Get_Layout_Size.Width),
   --           Real (Glyph.Height) / Real (This.Get_Layout_Size.Height),
   --           1.0);
   --        Glyph.Set_Position
   --          (Rho.Matrices.To_Vector
   --             (Rho.Matrices.X (Pos),
   --              Real (This.Get_Layout_Size.Height) - Glyph.Y - Glyph.Height,
   --              0.0));
   --        This.Log ("rescale glyph: "
   --                  & Image (Rho.Matrices.X (Glyph.Position))
   --                  & " "
   --                  & Image (Rho.Matrices.Y (Glyph.Position))
   --                  & " "
   --                  & Image (Rho.Matrices.Z (Glyph.Position))
   --                  & Integer'Image (Integer (Glyph.Width))
   --                  & Integer'Image (Integer (Glyph.Height)));
   --     end Rescale;
   --
   --  begin
   --     Parent (This).Set_Layout_Size (Size);
   --     This.Node.Iterate_Children (Rescale'Access);
   --  end Set_Layout_Size;

end Rho.UI.Widget.Label;
