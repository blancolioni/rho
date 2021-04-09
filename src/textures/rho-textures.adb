with Rho.Shaders.Slices.Attributes;
with Rho.Shaders.Slices.Main;
with Rho.Shaders.Slices.Uniforms;

package body Rho.Textures is

   ---------------
   -- Add_Slice --
   ---------------

   overriding procedure Add_Slice
     (Texture : in out Root_Texture_Type;
      Slice    : Rho.Shaders.Slices.Slice_Type)
   is
   begin
      Texture.Slices.Add_Slice (Slice);
   end Add_Slice;

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

      Texture.Add_Slice
        (Rho.Shaders.Slices.Attributes.In_Attribute_Fragment
           (Stage     => Vertex_Shader,
            Name      => "vertexTextureCoord",
            Type_Name => "vec2"));
      Texture.Add_Slice
        (Rho.Shaders.Slices.Attributes.Out_Attribute_Fragment
           (Stage     => Vertex_Shader,
            Name      => "fragmentTextureCoord",
            Type_Name => "vec2"));
      Texture.Add_Slice
        (Rho.Shaders.Slices.Attributes.In_Attribute_Fragment
           (Stage     => Fragment_Shader,
            Name      => "fragmentTextureCoord",
            Type_Name => "vec2"));
      Texture.Add_Slice
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Stage     => Fragment_Shader,
            Name      => Texture.Name,
            Type_Name => "sampler2D"));
      Texture.Add_Slice
        (Rho.Shaders.Slices.Main.Shader_Line
           (Stage    => Vertex_Shader,
            Priority => 1,
            Name     => "pass texture coordinates to fragment shader",
            Line     =>
              "fragmentTextureCoord = vertexTextureCoord"));
      Texture.Add_Slice
        (Rho.Shaders.Slices.Main.Shader_Line
           (Stage    => Fragment_Shader,
            Priority => 1,
            Name     => "get color from texture",
            Line     =>
              "vec4 fragmentColor = "
            & "texture (" & Texture.Name & ", fragmentTextureCoord)"));
   end Initialize;

end Rho.Textures;
