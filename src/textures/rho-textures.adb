package body Rho.Textures is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Texture    : in out Root_Texture_Type'Class;
      Identifier : String;
      Order      : Texture_Dimension_Count;
      Width      : Positive;
      Height     : Positive;
      Depth      : Positive := 1;
      S_Border   : Texture_Address_Mode := Wrap;
      T_Border   : Texture_Address_Mode := Wrap;
      Mag_Filter : Texture_Filter_Type  := Linear)
   is
   begin
      Texture.Identifier :=
        Ada.Strings.Unbounded.To_Unbounded_String (Identifier);
      Texture.Dimensions := Order;
      Texture.Width := Width;
      Texture.Height := Height;
      Texture.Depth := Depth;
      Texture.S_Border := S_Border;
      Texture.T_Border := T_Border;
      Texture.Mag_Filter := Mag_Filter;
   end Initialize;

end Rho.Textures;
