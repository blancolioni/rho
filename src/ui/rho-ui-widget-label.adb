with Ada.Strings.Fixed;

with Rho.Bitmaps;
with Rho.UI.Signals;

package body Rho.UI.Widget.Label is

   function On_Draw
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Result;

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

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This : in out Instance;
      Node : not null access constant Partoe.DOM.Root_Partoe_Node'Class)
   is
      Text  : constant String :=
                Ada.Strings.Fixed.Trim
                  (Node.Text, Ada.Strings.Both);
   begin
      Parent (This).Initialize (Node);
      This.Set_Label (Text);
   end Initialize;

   ---------
   -- Map --
   ---------

   overriding procedure Map
     (This    : not null access Instance;
      Surface : not null access Rho.UI.Surface.Instance'Class)
   is
   begin
      Parent (This.all).Map (Surface);
      This.Label_Draw_Id :=
        This.Add_Handler
          (Signal  => Rho.UI.Signals.Draw_Signal,
           Handler => On_Draw'Access,
           Data    => Rho.Signals.No_Signal_Data);
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

   -------------
   -- On_Draw --
   -------------

   function On_Draw
     (Object      : not null access Rho.Signals.Signal_Object_Interface'Class;
      Signal_Data : Rho.Signals.Signal_Data_Interface'Class;
      User_Data   : Rho.Signals.Signal_Data_Interface'Class)
      return Rho.Signals.Handler_Result
   is
      pragma Unreferenced (Signal_Data, User_Data);
      This : constant Reference := Reference (Object);
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
           (Width   => Real (Width),
            Height  => Real (Height),
            Stencil => Bitmap);
         Left := Left + Real (Advance);
      end Draw_Glyph;

   begin
      This.Context.Draw_Color
        (Get_Color (This.Style ("color")));
      Font.Render (Text, Draw_Glyph'Access);
      return Rho.Signals.Propagate;
   end On_Draw;

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

end Rho.UI.Widget.Label;
