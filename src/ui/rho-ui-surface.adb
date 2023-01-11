with System.Storage_Elements;

with Glib;

with Cairo.Image_Surface;
with Cairo.Surface;

with Cairo.Png;

with Rho.Images;
with Rho.Logging;

with Rho.Formats;
with Rho.Geometry;
with Rho.Material.Basic;
with Rho.Textures;

with Rho.UI.Surface.Primitives;
with Rho.UI.Main;

package body Rho.UI.Surface is

   Tile_Width : constant := 64;
   Tile_Height : constant := 64;

   Local_Square_Geometry : Rho.Geometry.Geometry_Type;

   function Square_Geometry return Rho.Geometry.Geometry_Type;

   function Create_Tile_Mesh
     (This          : not null access Instance'Class;
      Left, Top     : Integer;
      Width, Height : Natural)
      return Rho.Meshes.Mesh_Type;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (This      : in out Context;
      Primitive : Draw_Primitive'Class)
   is
      DX, DY : Real;
      W, H   : Non_Negative_Real;
   begin
      Primitive.Bounding_Box (DX, DY, W, H);
      if W > 0.0 and then H > 0.0 then
         This.Surface.Tiles.Invalidate
           (This.X + DX, This.Y + DY, W, H);
      end if;
      Primitive.Draw (This);
   end Apply;

   ------------
   -- Create --
   ------------

   function Create
     (X, Y          : Real;
      Width, Height : Non_Negative_Real)
      return Reference
   is
   begin
      return Surface : constant Reference :=
        new Instance'
          (Rho.Rectangles.Rectangle_Object with
             Scene => Rho.Scenes.Create_Scene,
             Top   => Rho.Nodes.Create_Node,
             Tiles => <>,
             Surface =>
               Cairo.Image_Surface.Create
                 (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
                  Width  => Glib.Gint (Real'Ceiling (Width)),
                  Height => Glib.Gint (Real'Ceiling (Height))))
      do
         Surface.Initialize_Rectangle (X, Y, Width, Height);
         Surface.Scene.Add (Surface.Top);
         Surface.Tiles.Create (Width, Height, Tile_Width, Tile_Height);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (This : not null access Instance'Class;
      X, Y : Real;
      W, H : Non_Negative_Real)
      return Context
   is
   begin
      return Context'
        (Surface => Reference (This),
         Left    => X + This.X,
         Top     => Y + This.Y,
         Right   => X + This.X + W,
         Bottom  => Y + This.Y + H,
         X       => 0.0,
         Y       => 0.0,
         Color   => <>);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (From : Context'Class;
      X, Y : Real;
      W, H : Non_Negative_Real)
      return Context
   is
   begin
      return Context'
        (Surface => From.Surface,
         Left    => X + From.Left,
         Top     => Y + From.Top,
         Right   => X + From.Left + W,
         Bottom  => Y + From.Top + H,
         X       => 0.0,
         Y       => 0.0,
         Color   => <>);
   end Create;

   ----------------------
   -- Create_Tile_Mesh --
   ----------------------

   function Create_Tile_Mesh
     (This          : not null access Instance'Class;
      Left, Top     : Integer;
      Width, Height : Natural)
      return Rho.Meshes.Mesh_Type
   is
      use Cairo.Image_Surface;
      use System.Storage_Elements;
      Stride   : constant Storage_Offset :=
                   Storage_Offset (Get_Stride (This.Surface));
      Offset   : constant Storage_Count :=
                   Stride * Storage_Count (Top)
                   + Storage_Count (Left) * 4;
      Texture  : constant Rho.Textures.Texture_Type :=
                   Rho.UI.Main.UI_Handle.Assets.Create_Texture
                     (Image_Buffer  => Get_Data_Generic (This.Surface),
                      Buffer_Length => Stride * Storage_Count (This.Height),
                      Width         => Width,
                      Height        => Height,
                      Offset        => Offset,
                      Stride        => Stride,
                      Format        => Rho.Formats.ARGB,
                      Flip_Vertical => False);
      Material : constant Rho.Material.Reference :=
                   Rho.Material.Basic.Create
                     (Texture);
   begin
      Rho.Logging.Log
        ("create mesh: left" & Left'Image
         & "; top" & Top'Image
         & "; width" & Width'Image
         & "; height" & Height'Image
         & "; stride" & Stride'Image
         & "; offset" & Offset'Image);
      return Rho.Meshes.Create_Mesh
        (Geometry => Square_Geometry,
         Material => Material);
   end Create_Tile_Mesh;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (This : in out Reference) is
   begin
      Cairo.Surface_Destroy (This.Surface);
      This.Scene.Unref;
      This := null;
   end Destroy;

   ----------------
   -- Draw_Color --
   ----------------

   procedure Draw_Color
     (This  : in out Context;
      Color : Rho.Color.Color_Type)
   is
   begin
      This.Apply (Primitives.Draw_Color (Color));
   end Draw_Color;

   ----------
   -- Line --
   ----------

   procedure Line (This   : in out Context;
                   X1, Y1 : Real;
                   X2, Y2 : Real)
   is
   begin
      This.Move_To (X1, Y1);
      This.Line_To (X2, Y2);
   end Line;

   -------------
   -- Line_To --
   -------------

   procedure Line_To (This : in out Context; X, Y : Real) is
   begin
      This.Apply (Primitives.Move_To (X, Y, True));
   end Line_To;

   -------------
   -- Move_To --
   -------------

   procedure Move_To (This : in out Context; X, Y : Real) is
   begin
      This.Apply (Primitives.Move_To (X, Y, False));
   end Move_To;

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle
     (This          : in out Context;
      Width, Height : Non_Negative_Real;
      Stencil       : Rho.Bitmaps.Greyscale_Reference)
   is
   begin
      This.Apply (Primitives.Rectangle (Width, Height, Stencil));
   end Rectangle;

   First : Boolean := True;

   ------------
   -- Render --
   ------------

   procedure Render
     (This   : not null access Instance'Class;
      Target : not null access Rho.Render.Render_Target'Class)
   is
      procedure Render_Tile
        (X, Y : Real;
         W, H : Non_Negative_Real;
         Mesh : in out Rho.Meshes.Mesh_Type);

      -----------------
      -- Render_Tile --
      -----------------

      procedure Render_Tile
        (X, Y : Real;
         W, H : Non_Negative_Real;
         Mesh : in out Rho.Meshes.Mesh_Type)
      is
         use type Rho.Meshes.Mesh_Type;
      begin
         Rho.Logging.Log
           ("rendering: " & Rho.Images.Image (X)
                & " "
                & Rho.Images.Image (Y));
         if Mesh = null then
            Mesh :=
              Create_Tile_Mesh
                (This,
                 Integer (Real'Floor (X)),
                 Integer (Real'Floor (Y)),
                 Natural (Real'Ceiling (W)),
                 Natural (Real'Ceiling (H)));
            This.Top.Add (Mesh);
            Mesh.Set_Position (X, Y, 0.0);
         end if;
      end Render_Tile;

   begin
      Cairo.Surface.Flush (This.Surface);
      This.Tiles.Iterate_Invalid_Tiles
        (Render_Tile'Access);
      if First then
         declare
            Status : constant Cairo.Cairo_Status :=
                       Cairo.Png.Write_To_Png (This.Surface, "render.png");
         begin
            pragma Unreferenced (Status);
         end;

         First := False;
      end if;

      This.Scene.Render (Target);

   end Render;

   ---------------------
   -- Square_Geometry --
   ---------------------

   function Square_Geometry return Rho.Geometry.Geometry_Type is
      use Rho.Geometry;
   begin
      if Local_Square_Geometry = null then
         declare
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
            Point (Real (Tile_Width), 0.0);
            Point (Real (Tile_Width), Real (Tile_Height));
            Point (0.0, Real (Tile_Height));

            Geometry.Face (1, 3, 2);
            Geometry.Face (3, 4, 1);

            Local_Square_Geometry := Geometry;
         end;
      end if;

      return Local_Square_Geometry;
   end Square_Geometry;

end Rho.UI.Surface;
