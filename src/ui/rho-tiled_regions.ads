private with Ada.Containers.Vectors;

generic
   type Element_Type is private;
package Rho.Tiled_Regions is

   type Tiled_Region is tagged private;

   procedure Create
     (This                    : in out Tiled_Region;
      Width, Height           : Non_Negative_Real;
      Tile_Width, Tile_Height : Positive);

   procedure Invalidate
     (This : in out Tiled_Region;
      X, Y : Real;
      W, H : Non_Negative_Real);

   procedure Iterate_Invalid_Tiles
     (This    : in out Tiled_Region;
      Process : not null access
        procedure (X, Y : Real;
                   W, H : Non_Negative_Real;
                   Element : in out Element_Type));

private

   type Tile_Record is
      record
         Element : Element_Type;
         Invalid : Boolean := True;
      end record;

   package Tile_Vectors is
     new Ada.Containers.Vectors (Natural, Tile_Record);

   type Tiled_Region is tagged
      record
         Width        : Non_Negative_Real;
         Height       : Non_Negative_Real;
         Tile_Width   : Positive;
         Tile_Height  : Positive;
         Tiles_Across : Positive;
         Tiles        : Tile_Vectors.Vector;
      end record;

end Rho.Tiled_Regions;
