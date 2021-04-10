with Rho.Shaders.Slices.Attributes;
with Rho.Shaders.Slices.Main;

package body Rho.Material.Simple is

   ----------------------------
   -- Create_Simple_Material --
   ----------------------------

   function Create_Simple_Material
     (Texture : Rho.Textures.Texture_Type) return Material_Type
   is
   begin
      return Material : constant Material_Type :=
        new Root_Material_Type'
          (Rho.Objects.Root_Object_Type with
             Slices => <>,
           Program   => <>,
           Textures => <>)
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
         Material.Textures.Append (Texture);
      end return;
   end Create_Simple_Material;

end Rho.Material.Simple;
