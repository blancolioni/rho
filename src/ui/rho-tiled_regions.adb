package body Rho.Tiled_Regions is

   ------------
   -- Create --
   ------------

   procedure Create
     (This                    : in out Tiled_Region;
      Width, Height           : Non_Negative_Real;
      Tile_Width, Tile_Height : Positive)
   is
      Across_Count : constant Natural :=
                       Natural (Real'Ceiling (Width));
      Down_Count   : constant Natural :=
                       Natural (Real'Ceiling (Height));
      Tiles_Across : constant Natural :=
                       (if Across_Count mod Tile_Width = 0
                        then Across_Count / Tile_Width
                        else Across_Count / Tile_Width + 1);
      Tiles_Down   : constant Natural :=
                       (if Down_Count mod Tile_Height = 0
                        then Down_Count / Tile_Height
                        else Down_Count / Tile_Height + 1);
   begin
      for I in 1 .. Tiles_Across * Tiles_Down loop
         declare
            T : Tile_Record;
         begin
            This.Tiles.Append (T);
         end;
      end loop;
      This.Width := Width;
      This.Height := Height;
      This.Tile_Width := Tile_Width;
      This.Tile_Height := Tile_Height;
      This.Tiles_Across := Tiles_Across;
   end Create;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (This : in out Tiled_Region;
      X, Y : Real;
      W, H : Non_Negative_Real)
   is
   begin
      for I in 0 .. This.Tiles.Last_Index loop
         declare
            Tile_X : constant Natural := I mod This.Tiles_Across;
            Tile_Y : constant Natural := I / This.Tiles_Across;
            Left   : constant Real := Real (Tile_X * This.Tile_Width);
            Top    : constant Real := Real (Tile_Y * This.Tile_Height);
         begin
            if X < Left + Real (This.Tile_Width) and then X + W > Left
              and then Y < Top + Real (This.Tile_Width)
              and then Y + H > Top
            then
               This.Tiles (I).Invalid := True;
            end if;
         end;
      end loop;
   end Invalidate;

   ---------------------------
   -- Iterate_Invalid_Tiles --
   ---------------------------

   procedure Iterate_Invalid_Tiles
     (This    : in out Tiled_Region;
      Process :        not null access procedure
        (X, Y : Real;
         W, H : Non_Negative_Real;
         Element : in out Element_Type))
   is
   begin
      for I in 0 .. This.Tiles.Last_Index loop
         declare
            Tile_X : constant Natural := I mod This.Tiles_Across;
            Tile_Y : constant Natural := I / This.Tiles_Across;
            Left   : constant Real := Real (Tile_X * This.Tile_Width);
            Top    : constant Real := Real (Tile_Y * This.Tile_Height);
         begin
            if This.Tiles (I).Invalid then
               Process (Left, Top,
                        Real (This.Tile_Width),
                        Real (This.Tile_Height),
                        This.Tiles (I).Element);
               This.Tiles (I).Invalid := False;
            end if;
         end;
      end loop;
   end Iterate_Invalid_Tiles;

end Rho.Tiled_Regions;
