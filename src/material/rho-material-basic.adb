with Tau.Shaders.Library;

with Rho.Shaders.Slices.Attributes;
with Rho.Shaders.Slices.Main;
--  with Rho.Shaders.Slices.Uniforms;

package body Rho.Material.Basic is

   ---------------------------
   -- Create_Basic_Material --
   ---------------------------

   function Create_Basic_Material
     (Color : Rho.Color.Color_Type)
      return Material_Type
   is
   begin
      return Material : constant Material_Type :=
        new Root_Material_Type
      do
         Material.Compiler.Add_Shader
           (Tau.Shaders.Library.Single_Color_Shader (Color));

         Material.Add_Slice
           (Rho.Shaders.Slices.Attributes.Out_Attribute_Fragment
              (Vertex_Shader, "fragmentColor", "vec4"));
         Material.Add_Slice
           (Rho.Shaders.Slices.Main.Shader_Line
              (Stage    => Vertex_Shader,
               Priority => 1,
               Name     => "pass color to fragment shader",
               Line     =>
                 "fragmentColor = "
               & Rho.Color.To_Shader_Value (Color) & ";"));

         Material.Add_Slice
           (Rho.Shaders.Slices.Attributes.In_Attribute_Fragment
              (Fragment_Shader, "fragmentColor", "vec4"));
         Material.Add_Slice
           (Rho.Shaders.Slices.Attributes.Out_Attribute_Fragment
              (Fragment_Shader, "finalColor", "vec4"));
         Material.Add_Slice
           (Rho.Shaders.Slices.Main.Shader_Line
              (Stage    => Fragment_Shader,
               Priority => 1,
               Name     => "set final color",
               Line     => "finalColor = fragmentColor"));

      end return;
   end Create_Basic_Material;

   ---------------------------
   -- Create_Basic_Material --
   ---------------------------

   function Create_Basic_Material
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Material_Type
   is
   begin
      return Material : constant Material_Type :=
        new Root_Material_Type
      do
         Material.Add_Slice
           (Rho.Shaders.Slices.Attributes.Out_Attribute_Fragment
              (Fragment_Shader, "finalColor", "vec4"));
         Material.Add_Slice
           (Rho.Shaders.Slices.Main.Shader_Line
              (Stage    => Fragment_Shader,
               Priority => Rho.Shaders.Slices.Shader_Source_Priority'Last,
               Name     => "set final color",
               Line     => "finalColor = fragmentColor"));
         Material.Textures.Append (Rho.Textures.Texture_Type (Texture));
      end return;
   end Create_Basic_Material;

end Rho.Material.Basic;
