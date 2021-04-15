with Rho.Shaders.Slices.Attributes;
with Rho.Shaders.Slices.Main;

package body Rho.Material.Simple is

   ----------------------------
   -- Create_Simple_Material --
   ----------------------------

   function Create_Simple_Material
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Material_Type
   is
   begin
      return Material : constant Material_Type :=
        new Root_Material_Type
      do
         Material.Add_Shader ("rho-shaders-material-textured");

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
   end Create_Simple_Material;

end Rho.Material.Simple;
