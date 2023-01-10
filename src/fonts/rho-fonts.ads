private with FT.Faces;
private with Rho.Strings;

with Rho.Bitmaps;
with Rho.Loaders;
--  with Rho.Material;
with Rho.Objects;
with Rho.Textures.Creator;

package Rho.Fonts is

   type Font_Weight is
     (Thin,
      Ultralight,
      Light,
      Semilight,
      Book,
      Normal,
      Medium,
      Semibold,
      Bold,
      Ultrabold,
      Heavy,
      Ultraheavy);

   type Font_Style is (Normal, Oblique, Italic);

   subtype Parent is Rho.Objects.Root_Object_Type;
   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Description (This : Instance'Class) return String;
   function Pixel_Height (This : Instance'Class) return Positive;

   function Measure
     (This : Instance'Class;
      Text : String)
      return Natural;

   procedure Render
     (This     : Instance'Class;
      Text     : String;
      On_Glyph : not null access
        procedure (Bitmap : Rho.Bitmaps.Greyscale_Reference;
                   Width, Height, Stride : Natural;
                   Bearing_X, Bearing_Y : Integer;
                   Advance : Natural));

   function Load
     (Name         : String;
      Pixel_Height : Positive;
      Weight       : Font_Weight;
      Style        : Font_Style;
      Loader       : Rho.Loaders.Loader_Interface'Class;
      Textures     : Rho.Textures.Creator.Texture_Creator_Interface'Class)
      return Reference;

   --  function Font_Material return Rho.Material.Reference;

private

   type Glyph_Record;
   type Glyph_Access is access Glyph_Record;

   type Single_Byte_Glyphs is array (0 .. 127) of Glyph_Access;

   type Glyph_Map is
      record
         U_0000_U_007F : Single_Byte_Glyphs;
      end record;

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         Face         : FT.Faces.Face_Reference;
         Family       : Rho.Strings.Rho_String;
         Pixel_Height : Positive;
         Glyphs       : Glyph_Map;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("font");

   function Description (This : Instance'Class) return String
   is (Rho.Strings."-" (This.Family) & This.Pixel_Height'Image & "px");

   function Pixel_Height (This : Instance'Class) return Positive
   is (This.Pixel_Height);

end Rho.Fonts;
