with Glib;

with Cairo.Image_Surface;

package body Rho.UI.Surface.Primitives is

   type Draw_Color_Primitive is new Draw_Primitive with
      record
         Color : Rho.Color.Color_Type;
      end record;

   overriding procedure Bounding_Box
     (This          : Draw_Color_Primitive;
      DX, DY        : out Real;
      Width, Height : out Non_Negative_Real);

   overriding procedure Draw
     (This   : Draw_Color_Primitive;
      Target : in out Context'Class);

   type Move_Primitive is new Draw_Primitive with
      record
         X, Y : Real;
         Draw : Boolean;
      end record;

   overriding procedure Bounding_Box
     (This          : Move_Primitive;
      DX, DY        : out Real;
      Width, Height : out Non_Negative_Real);

   overriding procedure Draw
     (This   : Move_Primitive;
      Target : in out Context'Class);

   type Rectangle_Primitive is new Draw_Primitive with
      record
         Width, Height : Non_Negative_Real;
         Stencil       : Rho.Bitmaps.Greyscale_Reference;
      end record;

   overriding procedure Bounding_Box
     (This          : Rectangle_Primitive;
      DX, DY        : out Real;
      Width, Height : out Non_Negative_Real);

   overriding procedure Draw
     (This   : Rectangle_Primitive;
      Target : in out Context'Class);

   ------------------
   -- Bounding_Box --
   ------------------

   overriding procedure Bounding_Box
     (This          : Draw_Color_Primitive;
      DX, DY        : out Real;
      Width, Height : out Non_Negative_Real)
   is
   begin
      DX := 0.0;
      DY := 0.0;
      Width := 0.0;
      Height := 0.0;
   end Bounding_Box;

   ------------------
   -- Bounding_Box --
   ------------------

   overriding procedure Bounding_Box
     (This          : Move_Primitive;
      DX, DY        : out Real;
      Width, Height : out Non_Negative_Real)
   is
   begin
      if This.Draw then
         DX := Real'Min (This.X, 0.0);
         DY := Real'Min (This.Y, 0.0);
         Width := abs This.X;
         Height := abs This.Y;
      else
         DX := 0.0;
         DY := 0.0;
         Width := 0.0;
         Height := 0.0;
      end if;
   end Bounding_Box;

   ------------------
   -- Bounding_Box --
   ------------------

   overriding procedure Bounding_Box
     (This          : Rectangle_Primitive;
      DX, DY        : out Real;
      Width, Height : out Non_Negative_Real)
   is
   begin
      DX := 0.0;
      DY := 0.0;
      Width := This.Width;
      Height := This.Height;
   end Bounding_Box;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This   : Draw_Color_Primitive;
      Target : in out Context'Class)
   is
   begin
      Target.Color := This.Color;
   end Draw;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This   : Move_Primitive;
      Target : in out Context'Class)
   is
   begin
      if This.Draw then
         declare
            Cr : constant Cairo.Cairo_Context :=
                   Cairo.Create (Target.Surface.Surface);
         begin
            Cairo.Move_To (Cr,
                           Glib.Gdouble (Target.Left + Target.X),
                           Glib.Gdouble (Target.Top + Target.Y));
            Cairo.Set_Source_Rgba
              (Cr,
               Glib.Gdouble (Target.Color.R),
               Glib.Gdouble (Target.Color.G),
               Glib.Gdouble (Target.Color.B),
               Glib.Gdouble (Target.Color.A));
            Cairo.Line_To  (Cr,
                            Glib.Gdouble (Target.Left + This.X),
                            Glib.Gdouble (Target.Top + This.Y));
            Cairo.Stroke (Cr);
            Cairo.Destroy (Cr);
         end;
      end if;
      Target.X := This.X;
      Target.Y := This.Y;
   end Draw;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This   : Rectangle_Primitive;
      Target : in out Context'Class)
   is
      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (Target.Surface.Surface);
      X  : constant Glib.Gdouble := Glib.Gdouble (Target.Left + Target.X);
      Y  : constant Glib.Gdouble := Glib.Gdouble (Target.Top + Target.Y);
      W  : constant Glib.Gdouble := Glib.Gdouble (This.Width);
      H  : constant Glib.Gdouble := Glib.Gdouble (This.Height);
      R  : constant Glib.Gdouble := Glib.Gdouble (Target.Color.R);
      G  : constant Glib.Gdouble := Glib.Gdouble (Target.Color.G);
      B  : constant Glib.Gdouble := Glib.Gdouble (Target.Color.B);
      A  : constant Glib.Gdouble := Glib.Gdouble (Target.Color.A);
      use type Rho.Bitmaps.Greyscale_Reference;
   begin
      Cairo.Set_Source_Rgba (Cr, R, G, B, A);

      if This.Stencil /= null then
         declare
            Width   : constant Natural := Natural (Real'Ceiling (This.Width));
            Stride  : constant Natural :=
                        (if Width mod 4 = 0
                         then Width
                         else Width + 4 - Width mod 4);
            Surface : constant Cairo.Cairo_Surface :=
                        Cairo.Image_Surface.Create_For_Data_Generic
                          (Data   => This.Stencil.all'Address,
                           Format => Cairo.Image_Surface.Cairo_Format_A8,
                           Width  => Glib.Gint (Width),
                           Height => Glib.Gint (This.Height),
                           Stride => Glib.Gint (Stride));
         begin
            if True then
               Cairo.Mask_Surface (Cr, Surface, X, Y);
            end if;
            Cairo.Surface_Destroy (Surface);
         end;
      else
         Cairo.Rectangle (Cr, X, Y, W, H);
      end if;

      Cairo.Fill (Cr);
      Cairo.Destroy (Cr);

   end Draw;

   ----------------
   -- Draw_Color --
   ----------------

   function Draw_Color
     (Color : Rho.Color.Color_Type) return Draw_Primitive'Class
   is
      Result : constant Draw_Color_Primitive :=
                 Draw_Color_Primitive'
                   (Color => Color);
   begin
      return Result;
   end Draw_Color;

   -------------
   -- Move_To --
   -------------

   function Move_To (X, Y : Real;
                     Draw : Boolean)
                     return Draw_Primitive'Class
   is
      Result : constant Move_Primitive :=
                 Move_Primitive'
                   (X, Y, Draw);
   begin
      return Result;
   end Move_To;

   ---------------
   -- Rectangle --
   ---------------

   function Rectangle
     (Width, Height : Non_Negative_Real;
      Stencil       : Rho.Bitmaps.Greyscale_Reference)
      return Draw_Primitive'Class
   is
      Result : constant Rectangle_Primitive :=
                 Rectangle_Primitive'
                   (Width, Height, Stencil);
   begin
      return Result;
   end Rectangle;

end Rho.UI.Surface.Primitives;
