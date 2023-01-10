with Ada.Directories;
with Ada.Text_IO;

with WL.String_Maps;

with Rho.Textures;

with FT.Glyphs;

package body Rho.Fonts is

   FT_Library : FT.Library_Reference;

   package Face_Maps is
     new WL.String_Maps (FT.Faces.Face_Reference,
                         FT.Faces."=");

   Face_Map : Face_Maps.Map;

   type Glyph_Record is
      record
         Bitmap    : Rho.Bitmaps.Greyscale_Reference;
         Width     : Natural;
         Height    : Natural;
         Stride    : Natural;
         Bearing_X : Integer;
         Bearing_Y : Integer;
         Advance   : Natural;
      end record;

   ----------
   -- Load --
   ----------

   function Load
     (Name         : String;
      Pixel_Height : Positive;
      Weight       : Font_Weight;
      Style        : Font_Style;
      Loader       : Rho.Loaders.Loader_Interface'Class;
      Textures     : Rho.Textures.Creator.Texture_Creator_Interface'Class)
      return Reference
   is
      pragma Unreferenced (Weight, Style, Textures);
      Face : FT.Faces.Face_Reference;
      Path : constant String := Loader.Find_File (Name, "ttf");

      function Load_Glyph (Index : Natural) return Glyph_Access;

      ----------------
      -- Load_Glyph --
      ----------------

      function Load_Glyph (Index : Natural) return Glyph_Access is
      begin
         FT.Faces.Load_Character
           (Face, FT.ULong (Index), FT.Faces.Load_Render);

         declare
            Glyph : constant FT.Glyph_Slot_Reference :=
                      FT.Faces.Glyph_Slot (Face);
            Bitmap : constant FT.Bitmap_Record :=
                       FT.Glyphs.Bitmap (Glyph);
            Width : constant Natural :=
                       Natural (Bitmap.Width);
            Height : constant Natural :=
                       Natural (Bitmap.Rows);
            Stride : constant Natural :=
                       (if Width mod 4 = 0 then Width
                        else Width + 4 - Width mod 4);
            Length : constant Natural := Width * Height;
            Raw_Data : Rho.Bitmaps.Greyscale_Bitmap (1 .. Length);
            for Raw_Data'Address use Bitmap.Buffer;
            pragma Import (Ada, Raw_Data);
            Glyph_Data : Rho.Bitmaps.Greyscale_Bitmap (1 .. Height * Stride) :=
                           (others => 0);
         begin
            for Y in 0 .. Height - 1 loop
               for X in 0 .. Width - 1 loop
                  Glyph_Data (Y * Stride + X + 1) :=
                    Raw_Data (Y * Width + X + 1);
               end loop;
            end loop;

            return new Glyph_Record'
              (Bitmap  => new Rho.Bitmaps.Greyscale_Bitmap'(Glyph_Data),
               Width   => Width,
               Height  => Height,
               Stride  => Stride,
               Bearing_X => Integer (FT.Glyphs.Bitmap_Left (Glyph)),
               Bearing_Y => Integer (FT.Glyphs.Bitmap_Top (Glyph)),
               Advance   => Natural (FT.Glyphs.Advance (Glyph).X));
         exception
            when others =>
               Ada.Text_IO.Put_Line
                 ("error loading [" & Character'Val (Index) & "]");
               raise;
         end;
      end Load_Glyph;

   begin
      if Path = "" then
         return null;
      end if;

      if not Face_Map.Contains (Path) then
         FT.Faces.New_Face
           (Library        => FT_Library,
            File_Path_Name => Path,
            Face_Index     => 0,
            Object         => Face);
         Face_Map.Insert (Path, Face);
      end if;

      Face := Face_Map (Path);

      FT.Faces.Set_Pixel_Sizes (Face, 0, FT.UInt (Pixel_Height));

      return This : constant Reference := new Instance do
         This.Set_Name (Ada.Directories.Base_Name (Path));
         This.Face := Face;
         This.Pixel_Height := Pixel_Height;
         for Index in This.Glyphs.U_0000_U_007F'Range loop
            This.Glyphs.U_0000_U_007F (Index) := Load_Glyph (Index);
         end loop;

      end return;
   end Load;

   -------------
   -- Measure --
   -------------

   function Measure
     (This : Instance'Class;
      Text : String)
      return Natural
   is
      Result : Natural := 0;
   begin
      for Ch of Text loop
         declare
            Glyph : constant Glyph_Access :=
                      This.Glyphs.U_0000_U_007F (Character'Pos (Ch));
         begin
            Result := Result + Glyph.Advance / 64;
         end;
      end loop;
      return Result;
   end Measure;

   ------------
   -- Render --
   ------------

   procedure Render
     (This     : Instance'Class;
      Text     : String;
      On_Glyph : not null access
        procedure (Bitmap : Rho.Bitmaps.Greyscale_Reference;
                   Width, Height, Stride : Natural;
                   Bearing_X, Bearing_Y : Integer;
                   Advance : Natural))
   is
   begin
      for Ch of Text loop
         declare
            Glyph : constant Glyph_Access :=
                      This.Glyphs.U_0000_U_007F (Character'Pos (Ch));
         begin
            On_Glyph (Glyph.Bitmap, Glyph.Width, Glyph.Height, Glyph.Stride,
                      Glyph.Bearing_X, Glyph.Bearing_Y,
                      Glyph.Advance / 64);
         end;
      end loop;
   end Render;

begin
   FT.Init (FT_Library);
end Rho.Fonts;
