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
         Material.Default_Shaders;
         Material.Shader_Names.Include ("light");
         Material.Shader_Names.Include ("ambient");
         Material.Shader_Names.Include ("spot");
         Material.Shader_Names.Include ("textured");
         Material.Textures.Append (Rho.Textures.Texture_Type (Texture));
      end return;
   end Create_Simple_Material;

end Rho.Material.Simple;
