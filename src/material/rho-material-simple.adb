package body Rho.Material.Simple is

   function Create
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Reference
   is
   begin
      return Material : constant Reference := new Instance do
         Material.Default_Shaders;
         Material.Shader_Names.Include ("light");
         Material.Shader_Names.Include ("ambient");
         Material.Shader_Names.Include ("spot");
         Material.Shader_Names.Include ("textured");
         Material.Textures.Append (Rho.Textures.Texture_Type (Texture));
      end return;
   end Create;

end Rho.Material.Simple;
